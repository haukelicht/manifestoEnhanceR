
#' Enhance quasi-sentence of manifesto data frame
#'
#' @description \code{enhance_manifesto_df} helper
#'
#' @param qs.df the subset of quasi-sentences (non-header) rows in a manifesto dataframe
#'
#' @return The input \code{qs.df} \code{\link[tibble]{tibble}} enhanced by columns
#'      'qs_nr' (running quasi-sentence counter) and
#'      'sent_nr' (running sentence counter)
#'
#' @import dplyr
enhance_qs_df_ <- function(qs.df) {
  qs.df %>%
    mutate(
      qs_nr = row_number()
      # flag: is sentence start?
      , sent_start_ = grepl("^\\s*[[:upper:]0-9]", text)
      # flag: is sentence end?
      , sent_end_ = grepl("\\W\\w+[[:lower:]][.!?]\\s*$", text)
      # flag: is previous row ending a sentence?
      , prev_sent_end_ = lag(sent_end_, default = TRUE)
      # sentence number
      , sent_nr = cumsum(sent_start_ & prev_sent_end_)
    ) %>%
    select(-ends_with("_"))
}

#' Enhance meta data (non-quasi-sentence rows) of manifesto data frame
#'
#' @description \code{enhance_manifesto_df} helper
#'
#' @param meta.df the subset of non-quasi-sentences (headers/titles) rows in a manifesto dataframe
#'
#' @return The input \code{meta.df} \code{\link[tibble]{tibble}} enhanced by column
#'      'role' (indicator 'title', 'header', or 'meta')
#'
#' @import dplyr
enhance_meta_df_ <- function(meta.df) {
  no_H <- all(meta.df$cmp_code != "H" | is.na(meta.df$cmp_code))

  out <- mutate(meta.df, group_nr_ = cumsum(idx - lag(idx, default = 0) > 1))

  if (no_H) {
    out <- out %>%
      mutate(
        role = case_when(
          min(idx) == 1L & group_nr_ == min(group_nr_) ~ "title"
          , is.na(cmp_code) ~ "header"
          , TRUE ~ NA_character_
        )
      )
  } else {
    out <- out %>%
      mutate(
        role = case_when(
          min(idx) == 1L & group_nr_ == min(group_nr_) ~ "title"
          , cmp_code == "H" | group_nr_ == min(group_nr_) ~ "header"
          , is.na(cmp_code) ~ "meta"
          , TRUE ~ NA_character_
        )
      )
  }

  return(out)
}

#' Enhance uncoded manifesto data frame
#'
#' @description \code{enhance_manifesto_df} helper
#'
#' @param u.df the 1-row manifesto dataframe recording the entire manifesto text in a unit-length character column 'text'
#'
#' @return The input \code{u.df} \code{\link[tibble]{tibble}} enhanced by column
#'      'qs_nr' (running quasi-sentence counter),
#'      'sent_nr' (running sentence counter),
#'      'role' (indicator, here 'qs' for all rows), and
#'      'bloc_nr'
#'
#' @note The function takes the unit-legnth character contained in column 'text' of the 1-row data frame,
#'      and splits it into sentences.
#'      Sentences are then numbered, and because the manifesto is uncoded, \code{qs_nr == sent_nr},
#'      'role' is 'qs' and bloc_nr is 1 for all rows.
#'
#' @import dplyr
#' @importFrom stringi stri_split_boundaries stri_opts_brkiter
enhance_uncoded_manifesto_ <- function(u.df) {

  sents <- stri_split_boundaries(u.df$text, opts_brkiter = stri_opts_brkiter("sentence"))
  sents <- unlist(sents)
  nrs <- seq_along(sents)

  out <- tibble(
    text = sents
    , qs_nr = nrs
    , sent_nr = nrs
    , role = "qs"
    , bloc_nr = 1L
  )

  out <- as_tibble(merge(select(u.df, -text), out, by = 0, all = TRUE)[-1])
  out <- fill(out, !!names(u.df), .direction = "down")
  select(out, !!names(u.df), qs_nr, sent_nr, role, bloc_nr)
}

#' Enhance manifesto data frame
#'
#' @description Functon takes a manifesto data frame
#'      (see \code{as_tibble.ManifestoCorpus} and \code{as_tibble.ManifestoDocument})
#'      and enhances it with quasi-sentence, sentence, and bloc counters as well as a
#'      role indicator distinguishing quasi-sentence text (value 'qs'), from title, header and meta text.
#'
#'      This text-level information is infered from columns 'text' and 'cmp_code'.
#'
#' @param x A manifesto data frame with the two required columns: 'text' and 'cmp_code'
#'
#' @return The input \code{x} as \code{manifesto.df} object (inherits from \code{\link[tibble]{tibble}}),
#'      enhanced by column
#'      'qs_nr' (running quasi-sentence counter),
#'      'sent_nr' (running sentence counter),
#'      'role' (indicator, here 'qs' for all rows), and
#'      'bloc_nr' (enumerates consecutive rows by 'role')
#'
#'      In addition, the returned \code{manifesto.df} obejct has two attributes:
#'      \enumerate{
#'        \item 'annotated': indicates wehtehr or not the input manifesto has been annotated/coded by CMP experts.
#'        \item 'extra_cols': names of columns added by enhancing the input data frame.
#'      }
#'
#' @note As one natrual sentence may contain multiple quasi-sentences, the latter map m:1 to the former.
#'
#'      For each row, the indicator variable 'role' may assume either of four values:
#'      \enumerate{
#'        \item 'qs': quasi-sentence
#'        \item 'title': the first row(s) with CMP code 'H' or \code{NA} (only in annotated manifestos)
#'        \item 'header': subsequent rows with CMP code 'H' or \code{NA}  (only in annotated manifestos)
#'        \item 'meta': in annotated manifestos containing 'H' codes, the row(s) between 'title' and the first 'header' rows
#'      }
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' library(tibble)
#' library(manifestoEnhanceR)
#'
#' man <- tribble(
#'   ~manifesto_id, ~text, ~cmp_code,
#'   "123", "main title", "H",
#'   "123", "sub title", "H",
#'   "123", "Publisher etc", NA_character,
#'   "123", "first section", "H",
#'   "123", "This is the first full sentence.", "000",
#'   "123", "This is the second,", "000",
#'   "123", "but splitted sentence.", "000",
#'   "123", "second section", "H",
#'   "123", "This is the third sentence.", "000"
#' )
#'
#' enhanced <- enhance_manifesto_df(man)
#' class(enhanced)
#' nrow(man) == nrow(enhanced)
#' ncol(man) < ncol(enhanced)
#' attr(enhanced, "annotated")
#' attr(enhanced, "extra_cols")
#' }
enhance_manifesto_df <- function(x) {

  # fallback case: all manifesto data in one line (often the case for old, non-annotated manifestos)
  if (nrow(x) == 1 && is.na(x$cmp_code)) {
    out <- enhance_uncoded_manifesto_(x)

    attr(out, "annotated") <- FALSE
    attr(out, "extra_cols") <- setdiff(names(out), names(x))

    # return as 'manifesto.df'
    return(structure(out, class = c("manifesto.df", class(x))))
  }

  splits <- x %>%
    select(text, cmp_code) %>%
    mutate(
      idx = row_number()
      , is_qs = grepl("^\\d+", as.character(cmp_code))
    ) %>%
    split(.$is_qs)

  meta <- if ("FALSE" %in% names(splits)) enhance_meta_df_(splits[["FALSE"]]) else tibble(idx = integer())
  qs <- if ("TRUE" %in% names(splits)) enhance_qs_df_(splits[["TRUE"]]) else tibble(idx = integer())

  out <- bind_rows(meta, qs) %>%
    arrange(idx) %>%
    mutate(
      role = ifelse(is_qs, "qs", role)
      , bloc_nr = cumsum(role != lag(role, default = role[1]))
    ) %>%
    select(-is_qs, -ends_with("_"))

  out <- bind_cols(x, select(out, -text, -cmp_code))
  out$idx <- NULL

  attr(out, "annotated") <- TRUE
  attr(out, "extra_cols") <- setdiff(names(out), names(x))

  # return as 'manifesto.df'
  return(structure(out, class = c("manifesto.df", class(out))))
}
