# Changelog

## wisclabmisc 0.1.1.9000 (dev version)

- Suppressed warnings from
  [`sessioninfo::session_info()`](https://sessioninfo.r-lib.org/reference/session_info.html)
  in
  [`mem_gamlss()`](https://www.tjmahr.com/wisclabmisc/reference/mem_gamlss.md).
- Updated regex for
  [`tocs_item()`](https://www.tjmahr.com/wisclabmisc/reference/tocs_item.md).
- Added
  [`compute_overlap_rate()`](https://www.tjmahr.com/wisclabmisc/reference/compute_overlap_rate.md).
- Added
  [`compute_sens_spec_from_ecdf()`](https://www.tjmahr.com/wisclabmisc/reference/compute_sens_spec_from_ecdf.md)
  to compute sensitivity, specificity and AUC on weighted data.
- Updated
  [`compute_empirical_roc()`](https://www.tjmahr.com/wisclabmisc/reference/compute_empirical_roc.md)
  to handle multilevel data (like a rating scale) if the two levels to
  be compared are provided in `levels`.
- Updated
  [`compute_empirical_roc()`](https://www.tjmahr.com/wisclabmisc/reference/compute_empirical_roc.md)
  to support aliases for `direction`: `"case-low"`/`"control-high"` and
  `"case-high"`/`"control-low"`. These forms help if you think about the
  direction or comparison as “which group gets a low/high score?”
- Add
  [`skip_block()`](https://www.tjmahr.com/wisclabmisc/reference/skip_block.md).

## wisclabmisc 0.1.1

- Added
  [`file_rename_with()`](https://www.tjmahr.com/wisclabmisc/reference/file_rename_with.md)
  and
  [`file_replace_name()`](https://www.tjmahr.com/wisclabmisc/reference/file_rename_with.md).
- Added
  [`info_surprisal()`](https://www.tjmahr.com/wisclabmisc/reference/information.md)
  and friends for computing entropy.
- Deprecated
  [`check_sample_centiles()`](https://www.tjmahr.com/wisclabmisc/reference/check_sample_centiles.md)
  deprecated. Use newly added
  [`check_model_centiles()`](https://www.tjmahr.com/wisclabmisc/reference/check_model_centiles.md)
  instead.
- Added
  [`check_computed_centiles()`](https://www.tjmahr.com/wisclabmisc/reference/check_model_centiles.md)
  for calibrating precomputed centiles (allows examining centiles
  outside of GAMLSS).

## wisclabmisc 0.1.0

- Package features up to November 2024
