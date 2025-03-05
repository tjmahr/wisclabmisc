

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



#' Phonetic features of consonants and vowels
#'
#' These are two dataframes that contain conventional phonetic features of
#' the consonants and vowels used by CMU phonetic alphabet.
#'
#' @details
#' d
#'
#' ## What are phonetic features?
#'
#' We produce most speech sounds by manipulating airflow from the lungs through
#' the larynx and through the oral or nasal cavities. We need a system for
#' describing how that airflow is shaped and filtered to make sounds, and phonetic
#' features are the conventional system for describing sounds. First, let's take
#' for granted that there is a basic distinction between vowels and consonants. For vowels,
#' the airflow is almost-always resonant (voicing from the larynx) and largely
#' unobstructed but tweaked by the position of the tongue.  For
#' consonants, there is some constriction along the vocal tract.
#'
#' We describe the consonants by using phonetic features that specify the
#' constriction in the vocal tract along with the state of larynx.
#'
#' * **place of articulation**: where the constriction occurs. For example,
#'   if both lips are
#'   used, the place is *bilabial*. When the tongue is involved, the place
#'   describes where the tongue creates the constriction: *dental* (with the
#'   teeth), *alveolar* (ridge), *velar* (velum), among others.
#' * **manner of articulation**: how much constriction occurs and whether
#'   anything else special is going on. We consider three main degrees of
#'   constriction. Sounds that barely constrict are *approximants*
#'   (or semivowels). Sounds where the constriction fully stop airflow are
#'   *stops* (or plosives). Sounds that constrict enough to produce turbulent
#'   airflow are *fricatives*. Other manners of articulation are variations
#'   on these three levels of constriction. For example, *nasal* stops are
#'   stops that allow airflow through the nasal cavity.
#' * **voicing**: whether or not the larynx is closed (and vibrating) to
#'   produce voicing. /s/ and /z/ are two sounds that the same place and
#'   manner of articulation, but differ in their voicing. /s/ is *voiceless*,
#'   /z/ is *voiced*.
#'
#' The chart of the [International Phonetic
#' Alphabet](https://en.wikipedia.org/wiki/International_Phonetic_Alphabet_chart)
#' arranges consonant sounds according to the above three features of place of
#' articulation (columns), manner of articulation (rows) and voicing (left and
#' right position in a cell).
#'
#' I want to point out that phonetic features are **descriptive** and
#' **classificatory**. The features are descriptive: /p/ is *the* voiceless,
#' bilabial stop. Every Wikipedia article of an individual speech sound (like
#' [/p/](https://en.wikipedia.org/wiki/Voiceless_bilabial_plosive)) begins by
#' enumerating its features. On the flipside, the features define natural
#' classes or classifications/categories of sounds: The "stops" are the
#' consonants that are made using a complete constriction of airflow at some
#' point along the vocal tract.
#'
#' Note that all of these features reflect degrees of some tendency. That is,
#' there is some coherent way in which can order the individual features in a
#' set of phonetic feature. Place of articulation can be ordered from front
#' (lips and teeth) to back (tongue root and larynx). Manner can be ordered by
#' degree of constriction from stops (closure) to fricatives (near closure) to
#' approximants (much less constrictions), but there are secondary manner
#' features (nasal or lateral airflow, for example) that complicate a
#' single-dimensional ordering. Voicing is generally a binary category (voiced
#' versus voiceless), but that is a simplification and we might consider
#' degrees of airflow or vibration.
#'
#'
#' Vowels are trickier. First they are described along three main axes:
#'
#' * **height** of the tongue body. /i/ (as in "beet") is a *high* vowel,
#'   /ɛ/ (as in "bet") is a *mid* vowel, and /æ/ (as in "bat") is a *low*
#'   vowel.
#' * **backness** of the tongue body. /i/ (as in "beet") is a *front* vowel,
#'   /ʌ/ (as in "butt") is a *central* vowel, and /u/ (as in "but") is a
#'   *back* vowel.
#' * **rounding** of the lips. Rounding or protrusion of the lips lengthens
#'   the vocal tract which affects the resonances of the vowels. In American
#'   English, /ɑ/ (as in "bot") is a low back *unrounded* vowel and /ɔ/ (as
#'   in "bought") is a low back *rounded* vowel, but there is a tendency
#'   for these two vowels to [merge into one sound](https://en.wikipedia.org/wiki/Cot%E2%80%93caught_merger).
#'
#' The chart of the [International Phonetic
#' Alphabet](https://en.wikipedia.org/wiki/International_Phonetic_Alphabet_chart)
#' for vowels arranges the sounds into the so-called vowel quadrilateral and
#' within each height-backness combination, differentiates between an unrounded
#' and rounded vowel in that position.
#'
#' In English, these three sets of features cannot differentiate /i/ and // (beet versus bit) or /u/ and // (boot versus book)
#'
#' Phonologists (who study how systems of speech sound work) will tack on other
#' superset features or carve up place and manner into different sets of
#' distinctions.
#'
#' (Every statement, I make here is reductive and English centric, so assume there are exception for every tendency I mention. For example, voiceless vowels exist as do sounds that get their airflow without using the lungs.)
#'
#'
#' Notes on the features chosen:
#'
#' Most of the features are self-evident and definitional. For example, as I
#' said above, /p/ is *the* bilabial voiceless stop. For fuzzier features, I consulted the general
#' IPA chart and the Wikipedia page on English phonology. These issues included
#' things like: *what are the lax vowels again?* or *the last two rows of the
#' consonant tables are approximants, so /r,l,j/ are approximants.*
#'
#' Some features have alternative feature sets in order to accommodate degrees
#' of aggregation. For example, /r,l,j,w/ are *approximant* in `manner`
#' but divided into *liquid* and *glide* in `manner_alt`.
#'
#' ## Consonant features
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
#' ## Considerations about consonant features
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
#' ## Vowels
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
#'   \item{height}{vowel height}
#'   \item{height_fct}{height coded as a factor ordered *high*,
#'   *mid*, *low*. *diphthong* is recoded to `NA`.}
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
#' - *tense* and *lax* features were directly borrowed. Diphthongs and
#'   r-colored vowels are were not assign a tenseness.
#' - /ʌ,ɔ/ raised to *mid* (following the general IPA chart)
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
#' @concept datasets
"data_features_consonants"

#' @rdname data_features_consonants
"data_features_vowels"
