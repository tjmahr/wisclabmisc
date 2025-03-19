

#' Intelligibility development from 30 to 119 months
#'
#' A dataset of speech intelligibility scores from 538 children.
#'
#' Citation:
#'
#' Hustad, K. C., Mahr, T. J., Natzke, P., & Rathouz, P. J. (2021). Speech
#' Development Between 30 and 119 Months in Typical Children I:
#' Intelligibility Growth Curves for Single-Word and Multiword Productions.
#' *Journal of Speech, Language, and Hearing Research*, 1–13.
#' <https://doi.org/10.1044/2021_jslhr-21-00142>
#'
#'
#' @noRd
#' @format A data frame with 1076 rows and 4 variables:
#' \describe{
#'   \item{child}{unique child id. each child has two rows in the dataset}
#'   \item{age_months}{child's age in months}
#'   \item{mean_intelligibility}{child's intelligibility, the proportion of
#'   words correctly transcribed by two naive listeners}
#'   \item{intelligibility_type}{type of speech stimuli, either `single-word` or
#'   `multiword`}
#' }
#' @source Associated article: <https://doi.org/10.1044/2021_jslhr-21-00142>
#' @concept datasets
NULL
# "intelligibility_growth"


#' Fake speaking rate data
#'
#' A dataset of fake speaking rate measures for testing and demonstrating
#' modeling functions. These were created by randomly
#' sampling 200 rows of a speaking rate dataset and adding random noise to the
#' `age_months` and `speaking_sps` variables. These values do not measure any
#' real children but represent plausible age and rate measurements from our
#' kind of work.
#'
#' @format A data frame with 200 rows and 2 variables:
#' \describe{
#'   \item{age_months}{child's age in months}
#'   \item{speaking_sps}{child's speaking rate in syllables per second}
#' }
#' @concept datasets
"data_fake_rates"


#' Fake intelligibility data
#'
#' A dataset of fake intelligibility scores for testing and demonstrating
#' modeling functions. These were created by randomly sampling 200 rows of an
#' intelligibility dataset and adding random noise to the `age_months` and
#' `intelligibility` variables. These values do not measure any real children
#' but represent plausible age and intelligibility measurements from our kind of
#' work.
#'
#' @format A data frame with 200 rows and 2 variables:
#' \describe{
#'   \item{age_months}{child's age in months}
#'   \item{intelligibility}{child's intelligibility (proportion of words said
#'   by the child that were correctly transcribed by two listeners)}
#' }
#' @concept datasets
"data_fake_intelligibility"


#' Simulated intelligibility scores by utterance length
#'
#' A dataset of simulated intelligibility scores for testing and demonstrating
#' modeling functions. These were created by fitting a Bayesian model of the raw
#' Hustad and colleagues (2020) and drawing 1 sample from the posterior
#' distribution of expected predictions (i.e., "epreds). In other words, these
#' values are model predictions of the original dataset. They are correlated
#' with original dataset values at *r* = .86. We might think of the simulation
#' as adding random noise to the original dataset.
#'
#' @format A data frame with 694 rows and 5 variables:
#' \describe{
#'   \item{child}{identifier for the child}
#'   \item{age_months}{child's age in months}
#'   \item{length_longest}{length of the child's longest utterance}
#'   \item{tocs_level}{utterance length}
#'   \item{sim_intelligibility}{child's intelligibility for the given utterance
#'   length (proportion of words said by the child that were correctly
#'   transcribed by two listeners)}
#' }
#' @references Hustad, K. C., Mahr, T., Natzke, P. E. M., & Rathouz, P. J.
#'   (2020). Development of Speech Intelligibility Between 30 and 47 Months in
#'   Typically Developing Children: A Cross-Sectional Study of Growth. *Journal
#'   of Speech, Language, and Hearing Research*, *63*(6), 1675–1687.
#'   https://doi.org/10.1044/2020_JSLHR-20-00008
#' @concept datasets
"data_example_intelligibility_by_length"



#' Phonetic and acquisition features of consonants and vowels
#'
#' These are dataframes that contain information about the consonants and
#' vowels in American English. The *phonetic* features are conventional
#' descriptions of how sounds are produced. The *acquisition* (`acq`) features
#' (try to) describe the expected acquisition or speech-motor difficulty of
#' speech sounds.
#'
#'
#' @details
#'
#' Most of the phonetic features are self-evident and definitional. For example, /p/ is
#' *the* bilabial voiceless stop. For fuzzier features, I consulted the general
#' IPA chart and the Wikipedia page on English phonology. These issues included
#' things like: *what are the lax vowels again?* or *the last two rows of the
#' consonant tables are approximants, so /r,l,j/ are approximants.*
#'
#' Some features have alternative feature sets in order to accommodate degrees
#' of aggregation. For example, /r,l,j,w/ are *approximant* in `manner`
#' but divided into *liquid* and *glide* in `manner_alt`.
#'
#' ## Phonetic features of consonants
#'
#'  `data_features_consonants` is a dataframe with 24 rows and
#' 10 variables.
#'
#' ```{r}
#' knitr::kable(data_features_consonants)
#' ```
#'
#' Description of each column:
#'
#' \describe{
#'   \item{phone}{phone in IPA}
#'   \item{cmubet}{phone in the CMU alphabet}
#'   \item{wiscbet}{phone in an older system used by our lab}
#'   \item{voicing}{*voiced* versus *voiceless*}
#'   \item{voicing_alt}{*spread_glottis* versus *plain*}
#'   \item{manner}{manner of articulation}
#'   \item{manner_alt}{alternative manner coding that separates *approximants*
#'   into *liquids* and *glides*}
#'   \item{place}{place of articulation}
#'   \item{place_fct}{place coded as a factor and ordered based on
#'   frontness of the articulators. *labiovelar* is recoded as `NA`.}
#'   \item{sonorance}{*obstruent* versus *sonorant*}
#'   \item{sonorance_alt}{*obstruant* versus *sonorant* versus *strident*.}
#' }
#'
#' Levels of the factor columns:
#'
#' ```{r}
#' data_features_consonants |>
#'   lapply(levels) |>
#'   Filter(length, x = _)
#' ```
#'
#' ## Considerations about consonant phonetic features
#'
#' The CMU alphabet does not include a glottal stop.
#'
#' Here /f,v/ are coded as *strident* following
#' [Wikipedia](https://en.wikipedia.org/wiki/Sibilant) and
#' *Sound Pattern of English*. If this feature value doesn't seem right, we
#' should probably use an alternative feature of *sibilant* for the
#' stridents minus /f,v/.
#'
#' The alternative voicing scheme was suggested by a colleague because of how
#' the voice-voiceless phonetic contrast is achieved with different articulatory
#' strategies in different languages. Note that `voicing_alt` does not assign
#' a feature to nasals or approximants.
#'
#' ## Phonetic features of vowels
#'
#' `data_features_vowels` is a dataframe with 17 rows and
#' 11 variables.
#'
#' ```{r}
#' knitr::kable(data_features_vowels)
#' ```
#'
#' Description of each column:
#'
#' \describe{
#'   \item{phone}{phone in IPA}
#'   \item{cmubet}{phone in the CMU alphabet}
#'   \item{wiscbet}{phone in an older system used by our lab}
#'   \item{hint}{a word containing the selected vowel}
#'   \item{manner}{manner of articulation}
#'   \item{manner_alt}{alternative manner with *vowel*, *diphthong* and *r-colored*}
#'   \item{tenseness}{*tense* versus *lax* (versus *diphthong* and *r-colored*)}
#'   \item{height}{vowel height on a four-level scale}
#'   \item{height_fct}{height coded as a factor ordered *high*,
#'   *mid-high*, *mid-low*, *low*. *diphthong* is recoded to `NA`.}
#'   \item{height_alt}{vowel height on a three-level scale}
#'   \item{backness}{vowel backness}
#'   \item{backness_fct}{backness coded as a factor ordered *front*,
#'   *central*, *back*. *diphthong* is recoded to `NA`.}
#'   \item{rounding}{*unrounded* versus *rounded* (versus *diphthong* and
#'   *r-colored*)}
#' }
#'
#' Levels of the factor columns:
#'
#' ```{r}
#' data_features_vowels |>
#'   lapply(levels) |>
#'   Filter(length, x = _)
#' ```
#'
#' ## Considerations about vowel features
#'
#' I don't consider /eɪ/ and /oʊ/ to be diphthongs, but perhaps `manner_alt`
#' could encode the difference of these vowels from the others.
#'
#' In the CMU alphabet and ARPAbet, vowels can include a number to indicate
#' vowel stress, so `AH1` or `AH2` is /ʌ/ but `AH0` is /ə/.
#'
#' The vowel features for General American English,
#' [according to Wikipedia](https://en.wikipedia.org/wiki/English_phonology),
#' are as follows:
#'
#' <table style="text-align: center;">
#'<tbody>
#' <tr class="header">
#' <th rowspan="2"></th>
#' <th colspan="2">Front</th>
#' <th colspan="2">Central</th>
#' <th colspan="2">Back</th>
#' </tr>
#' <tr class="odd">
#' <th>lax</th>
#' <th>tense</th>
#' <th>lax</th>
#' <th>tense</th>
#' <th>lax</th>
#' <th>tense</th>
#' </tr>
#'
#' <tr class="odd">
#'<th>Close</th>
#'<td>ɪ</td>
#'<td>i</td>
#'<td></td>
#'<td></td>
#'<td>ʊ</td>
#'<td>u</td>
#'</tr>
#'<tr class="even">
#'<th>Mid</th>
#'<td>ɛ</td>
#'<td>eɪ</td>
#'<td>ə</td>
#'<td>(ɜ)</td>
#'<td></td>
#'<td>oʊ</td>
#'</tr>
#'<tr class="odd">
#'<th>Open</th>
#'<td>æ</td>
#'<td></td>
#'<td>(ʌ)</td>
#'<td>ɑ</td>
#'<td></td>
#'<td>(ɔ)</td>
#'</tr>
#'<tr class="even">
#'<th>Diphthongs</th>
#'<td colspan="6">aɪ ɔɪ aʊ</td>
#'</tr>
#'</tbody>
#'</table>
#'
#' I adapted these features in this way:
#'
#' - A four-level breakdown of height (high, mid-high, mid-low, low) was used
#'   instead of a three-level one (close, mid, open).
#' - *tense* and *lax* features were directly borrowed. Diphthongs and
#'   r-colored vowels are were not assign a tenseness.
#' - /ɑ/ moved to *back* (following the general IPA)
#' - diphthongs have no backness or height
#' - r-colored vowels were given the backness and height of the /ʌ,ə/
#'
#' Based on the assumption that /ʌ,ə/ are the same general vowel with
#' differing stress, these vowels have the same features. This definition
#' clashes with the general IPA chart which places /ʌ/ as a back vowel. However,
#' /ʌ/ is a conventional notation. Quoting
#' [Wikipedia](https://en.wikipedia.org/wiki/English_phonology) again:
#' "Although the notation /ʌ/ is used for the vowel of STRUT in RP and General
#' American, the actual pronunciation is closer to a near-open central
#' vowel \[ɐ\] in RP and advanced back \[ʌ̟\] in General American." That is,
#' /ʌ/ is fronted in American English (hence, *mid*) in American English.
#'
#' ## Acquisition features of consonants
#'
#' `data_acq_consonants` provides
#'
#' ```{r}
#' knitr::kable(data_acq_consonants)
#' ```
#'
#' Description of each column:
#'
#' \describe{
#'   \item{phone}{phone in IPA}
#'   \item{cmubet}{phone in the CMU alphabet}
#'   \item{wiscbet}{phone in an older system used by our lab}
#'   \item{cm2020_90_age_mean, cm2020_90_age_sd, cm2020_90_age_min,
#'   cm2020_90_age_max}{Age of acquisition statistics reported by Crowe &
#'   McLeod (2020). Statistics are the mean, SD, min and max age (in months)
#'   when children reached 90% accuracy on a consonant.}
#'   \item{cm2020_90_num_studies}{Number of studies used by Crowe & McLeod
#'   (2020) to compute the corresponding statistics.}
#'   \item{cm2020_90_stage }{Developmental stage assigned to the consonant by
#'   Crowe & McLeod (2020). Sounds with an `age_mean` before 48 months are
#'   `early`, before 60 months are `middle`, and of 60 or older are `late`.}
#' }
#'
#' Crowe and McLeod (2020, `cm2020_` variables) provides a systematic review and
#' summary statistics for age of acquisition norms for English consonants. They
#' scoured the literature of acquisition ages for individual consonants and
#' computed summary statistics on them. They considered just accuracy of sounds
#' when produced in single words. Their sources include a mix of a journal
#' articles and norms for articulation assessments. They do not weight
#' statistics from individual studies by sample size or sampling procedure.
#'
#' I prepared the Crowe and McLeod (2020) data by copying the relevant numbers
#' from their Table 2 making the following changes: 1) rounding mean and SD
#' values to 1 decimal point (3 days for ages in months), 2) dropping /ʍ/, 3)
#' using /r/, /g/, /tʃ/, /dʒ/ for IPA characters instead of the specialized
#' characters used in the article. I believe these changes are so minimal that
#' they do not qualify as transformative and hence do not break the "no
#' derivatives"
#'
#' ### Acquisition references
#'
#' Crowe, K., & McLeod, S. (2020). Children’s English Consonant Acquisition in
#' the United States: A Review. *American Journal of Speech-Language Pathology*,
#' *29*(4), 2155–2169. <https://doi.org/10.1044/2020_AJSLP-19-00168>
#'
#' @concept datasets
"data_features_consonants"

#' @rdname data_features_consonants
"data_features_vowels"

#' @rdname data_features_consonants
"data_acq_consonants"
