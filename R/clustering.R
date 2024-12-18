
#' Run (scaled) k-means on a dataset.
#'
#' Observations are `scale()`-ed before clustering.
#'
#' @param data a dataframe
#' @param k number of clusters to create
#' @param vars variable selection for clustering. Select multiple variables
#'   with `c()`, e.g., `c(x, y)`. The selection supports tidyselect semantics
#'   [tidyselect::select_helpers], e.g., `c(x, starts_with("mean_")`.
#' @param args_kmeans additional arguments passed to `stats::kmeans()`.
#' @return the original `data` but augmented with additional columns for
#'   clustering details. including `.kmeans_cluster` (cluster number of each
#'   observation, as a factor) and `.kmeans_k` (selected number of clusters).
#'
#'   Cluster-level information is also included. For example, suppose that
#'   we cluster using the variable `x`. Then the output will have a
#'   column `.kmeans_x` giving the cluster mean for `x` and
#'   `.kmeans_rank_x` giving the cluster labels reordered using the cluster
#'   means for `x`. The column `.kmeans_sort` contains the cluster sorted
#'   using the first principal component of the scaled variables. All columns
#'   of cluster indices are a `factor()` so that they can be plotted as
#'   discrete variables.
#' @export
#' @concept other-stats
#' @details Note that each variable is `scaled()` before clustering
#' and then cluster means are unscaled to match the original data scale.
#'
#' This function provides the original kmeans labels as `.kmeans_cluster` but
#' other alternative labeling based on different sortings of the data. These are
#' provided in order to deal with label-swapping in Bayesian models. See
#' bootstrapping example below.
#'
#' @examples
#' data_kmeans <- fit_kmeans(mtcars, 3, c(mpg, wt, hp))
#'
#' library(ggplot2)
#' ggplot(data_kmeans) +
#'   aes(x = wt, y = mpg) +
#'   geom_point(aes(color = .kmeans_cluster))
#'
#' ggplot(data_kmeans) +
#'   aes(x = wt, y = mpg) +
#'   geom_point(aes(color = .kmeans_rank_wt))
#'
#' # Example of label swapping
#' set.seed(123)
#' data_boots <- lapply(
#'   1:10,
#'   function(x) {
#'     rows <- sample(seq_len(nrow(mtcars)), replace = TRUE)
#'     data <- mtcars[rows, ]
#'     data$.bootstrap <- x
#'     data
#'   }
#' ) |>
#'   lapply(fit_kmeans, k = 3, c(mpg, wt, hp)) |>
#'   dplyr::bind_rows() |>
#'   dplyr::select(.bootstrap, dplyr::starts_with(".kmeans_")) |>
#'   dplyr::distinct()
#'
#' # Clusters start off in random locations and move to center, so the labels
#' # differ between model runs and across bootstraps.
#' ggplot(data_boots) +
#'   aes(x = .kmeans_wt, y = .kmeans_mpg) +
#'   geom_point(aes(color = .kmeans_cluster)) +
#'   labs(title = "k-means centers on 10 bootstraps")
#'
#' # Labels sorted using first principal component
#' # so the labels are more consistent.
#' ggplot(data_boots) +
#'   aes(x = .kmeans_wt, y = .kmeans_mpg) +
#'   geom_point(aes(color = .kmeans_sort)) +
#'   labs(title = "k-means centers on 10 bootstraps")
fit_kmeans <- function(data, k, vars, args_kmeans = list()) {
  fct_rank <- function(x) factor(dplyr::row_number(x))
  vars_model <- tidyselect::eval_select(enquo(vars), data)

  data_model <- data[, vars_model, drop = FALSE] |> scale()

  args <- utils::modifyList(
    list(x = data_model, centers = k),
    args_kmeans
  )

  model <- do.call(stats::kmeans, args)

  # Unscale cluster means
  z_means <- attr(data_model, "scaled:center")
  z_sds <- attr(data_model, "scaled:scale")
  zm <- matrix(z_means, nrow = k, ncol = length(z_means), byrow = TRUE)
  zsd <- matrix(z_sds, nrow = k, ncol = length(z_sds), byrow = TRUE)

  data_means <- ((model$centers * zsd) + zm) |>
    as.data.frame()
  data_means2 <- data_means |>
    lapply(fct_rank) |>
    rlang::set_names(function(x) paste0("rank_", x)) |>
    as.data.frame()

  data_means[["k"]] <- k
  data_means[["cluster"]] <- k |> seq_len() |> factor()

  # Try to make labels more similar by sorting on first principal component
  get_column <- function(x, j) x[, j, drop = TRUE]
  data_means[["sort"]] <- data_model |>
    stats::princomp(fix_sign = TRUE) |>
    stats::predict(newdata = model$centers) |>
    get_column(1) |>
    fct_rank()

  data_means <- cbind(data_means, data_means2) |>
    rlang::set_names(function(x) paste0(".kmeans_", x))

  # Not using left_join() or merge() because I noticed rownames dropped
  # on mtcars example
  data[[".kmeans_cluster"]] <- factor(model$cluster)
  data[names(data_means)] <- data_means[data$.kmeans_cluster, ]
  data
}


# fit_pam <- function(data, k, ..., pam_args = list()) {
#   data_model <- data |>
#     dplyr::select(...)
#
#   args <- utils::modifyList(
#     list(x = data_model, k = k),
#     pam_args
#   )
#   model <- do.call(cluster::pam, args)
#
#   data$.pam_cluster <- model$clustering
#   data$.pam_is_medoid <- FALSE
#   data$.pam_is_medoid[model$id.med] <- TRUE
#
#   data_medoids <- model$medoids |>
#     as.data.frame() |>
#     dplyr::mutate(
#       across(everything(), dplyr::row_number, .names = "{.col}_rank"),
#       cluster = seq_len(k),
#       cluster_size = model$clusinfo[, "size"],
#     ) |>
#     rlang::set_names(
#       function(x) paste0(".pam_", x)
#     )
#
#   data |>
#     dplyr::left_join(data_medoids, by = ".pam_cluster")
# }
