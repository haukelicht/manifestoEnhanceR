#' XML-ify manifesto meta content bloc
#'
#' @description \code{manifesto_df_to_xml} helper
#'
#' @param x rows of \code{manifesto.df} with role 'title', header' or 'meta'
#'
#' @return A XML-formatted unit-length character vector
#'     Out-most tag named like 'role' of row ('title', header' or 'meta').
#'     Inner tag is 'p' and contains text lines.

xmlify_meta_ <- function(x) {
  role <- unique(x$role)
  stopifnot(length(role) == 1)
  sprintf('\n    <%s>%s\n    </%s>', role, paste0("\n      <p>", x$text, "</p>", collapse = ""), role)
}

#' XML-ify manifesto quasi-sentences bloc
#'
#' @description \code{manifesto_df_to_xml} helper
#'
#' @param x rows of \code{manifesto.df} with role 'qs'
#'
#' @return A character vector, elements are XML-formatted
#'     Out-most and only tag named 'quasi-sentence' (w/ attribute 'nr' and 'cmp-code' if CMP code is not \code{NA}).
#'
#' @import dplyr
xmlify_qs_ <- function(x) {
  x %>%
    mutate(
      node = paste0(
        sprintf(
          '\n      <quasi-sentence nr="%d"%s%s>'
          , qs_nr
          , ifelse(is.na(cmp_code), "", sprintf(' cmp-code="%s"', cmp_code))
          , ifelse(is.na(eu_code), "", sprintf(' eu-code="%s"', eu_code))
        )
        , text
        , "</quasi-sentence>"
      )
    ) %>%
    group_by(sent_nr) %>%
    summarize(text = paste0(node, collapse = "")) %>%
    ungroup() %>%
    summarize(text = paste0(paste0(sprintf('\n    <sentence nr="%d">', sent_nr), text, "\n    </sentence>"), collapse = "")) %>%
    .$text
}

#' XML-ify manifesto chapter
#'
#' @description \code{manifesto_df_to_xml} helper
#'
#' @param x unit-length, XML-fromatted character vector sentenc-quasi-sentence nestings
#'
#' @param chap.nr unit-length integer, specifying the chapter number
#'
#' @return A XML-formatted unit-length character vector
#'     Out-most tag named 'chapter' (w/ attribute 'nr'),
#'     inner tags are be 'sentence' and 'quasi-sentence' (nested in this order).
#'     'quasi-sentence' nodes wrap text lines.
xmlify_chapter_ <- function(x, chap.nr) {
  sprintf('\n  <chapter nr="%d">%s\n  </chapter>', chap.nr, x)
}

#' XML-ify manifesto meta data
#'
#' @description \code{manifesto_df_to_xml} helper
#'
#' @param x first row of \code{manifesto.df}
#'
#' @param title a character vector specifying the manifesto title
#'     If \code{NULL} (the default) or \code{length(title) == 0},
#'     the title is taken from the input data frame if column exists.
#'
#' @return A XML-formatted unit-length character vector.
#'     Out-most tag named 'head', inner tags (in this order) may be 'title' and 'p'.
#'     'p' nodes wrap title lines.
xmlify_head_ <- function(x, title = NULL) {

  req <- c("manifesto_id", "language")
  if (!is.list(x) || !all(req %in% names(x)))
    stop("`x` must be a data frame/list containing columns/having elements ", paste(sQuote(req), collapse = ", "))

  if ("title" %in% names(x) && all(lengths(x$title) > 0) & all(nchar(x$title) > 0) && (length(title) == 0 | is.null(title))) {
    title <- paste0("\n    <title>", paste0("\n      <p>", x$title, "</p>", collapse = ""), "\n    </title>")
    x$title <- NULL
  }

  if ("text" %in% names(x))
    x$text <- NULL

  attrs <- map2_chr(
    lapply(x, as.character)
    , names(x)
    , function(e, nm) {
      if (is.na(e) || e == "") return("")
      sprintf(' %s="%s"', gsub("[^[:alnum:]]+", "-", nm), e)
    })

  out <- paste0(
    "\n  <head"
    , paste0(attrs, collapse = "")
    , ">"
    , title
    , ifelse(length(title) == 0 | is.null(title), "</head>", "\n  </head>")
  )

  return(out)
}

#' Combine XML head and body
#'
#' @description \code{manifesto_df_to_xml} helper
#'
#' @param head A unit-length XML-formatted string of the data contained in the document head node.
#' @param body A unit-length XML-formatted string of the data (chapter, header, sentence, and quasi-sentence nodes)
#'      contained in the document body node.
#'
#' @return A XML-formatted unit-length character vector.
#'     Out-most tag named 'manifesto', inner tags (in this order) are 'head' and 'body'.
combine_head_and_body_ <- function(head, body) {
  paste0(
    '<manifesto>'
    , head
    , body
    , "\n</manifesto>"
  )
}

#' Convert manifesto to XML
#'
#' @description Function takes \code{manifesto.df} and outputs
#'     either an XML-formatted string
#'     or, if \code{parse = TRUE} (the default), a \code{\link[xml2]{xm_document}}.
#'
#' @seealso \code{\link{enhance_manifesto_df}}
#'
#' @param man.df A \code{manifesto.df} object (see \code{\link{enhance_manfiesto_df}}).
#'
#' @param parse logical. If \code{TRUE} (the default), a \code{\link[xml2]{xm_document}} is returned.
#'     Otherwise, an XML-formatted, unit-length character vector
#'
#' @param .xml.encode character vector specifying plain text special characters to be converted to XML-compatible encoded.
#'     Conversion is performed by \code{\link[textutils]{HTMLencode}}.
#'     Defaults to '&', '<', '>', '#' and '"'.
#'     Ignored if \code{NULL}.
#'
#' @return A \code{\link[xml2]{xm_document}} with parent node 'manifesto',
#'     or an XML-formatted string if \code{parse = FALSE}.
#'
#' @importFrom textutils HTMLencode
#' @import dplyr
#' @import purrr
#' @import stringr
#' @importFrom tibble enframe
#' @importFrom xml2 read_xml
#'
#' @export
#'
#' @examples
#'
#' library(tibble)
#' library(manifestoEnhanceR)
#' library(xml2)
#'
#' man <- tribble(
#'   ~manifesto_id, ~party, ~text, ~cmp_code,
#'   "123", "Bloc Party", "main title", "H",
#'   "123", "Bloc Party", "sub title", "H",
#'   "123", "Bloc Party", "Publisher etc", NA_character,
#'   "123", "Bloc Party", "first section", "H",
#'   "123", "Bloc Party", "This is the first full sentence.", "000",
#'   "123", "Bloc Party", "This is the second,", "000",
#'   "123", "Bloc Party", "but splitted sentence.", "000",
#'   "123", "Bloc Party", "second section", "H"
#'   "123", "Bloc Party", "This is the third sentence.", "000"
#' )
#'
#' enhanced <- enhance_manifesto_df(man)
#' xml <- manifesto_df_to_xml(enhanced)
#' class(xml)
#' xml_attrs(xml)
#'
#' xml_string <- manifesto_df_to_xml(enhanced, parse = FALSE)
#' length(xml_string)
#' cat(xml_string)
manifesto_df_to_xml <- function(man.df, parse = TRUE, .xml.encode = c("&", "<", ">", "#", '"')) {
  err_msg <- "Could not create XML from manifesto data frame."

  is_enhanced <- attr(man.df, "enhanced")
  if (!is.null(.xml.encode) && is.character(.xml.encode) && length(.xml.encode) > 0)
    man.df <- mutate(man.df, text = str_replace_all(text, fixed(setNames(tolower(map_chr(.xml.encode, HTMLencode)), .xml.encode))))


  # split into blocs
  blocs <- split(man.df, man.df$bloc_nr)

  # determine bloc rolse
  roles <- map_chr(blocs, function(b) unique(b$role))

  # assign chapter numbers to blocs
  chap_nrs <- cumsum(roles == "header")

  first_header_ids <- which(roles == "header")[1]
  if (is.na(first_header_ids))
    first_header_ids <- length(roles)

  blocs_before_firts_header <- roles[1:(first_header_ids-1)]
  if ("qs" %in% blocs_before_firts_header) {
    chap_nrs[first_header_ids:length(chap_nrs)] <- chap_nrs[first_header_ids:length(chap_nrs)] + 1L
    chap_nrs[names(which(blocs_before_firts_header == "qs"))] <- 1L
  }

  # XML-ify blocs
  is_meta <- roles != "qs"

  xmlified <- tryCatch(
    enframe(map2_chr(blocs, is_meta, function(b, m) ifelse(m, xmlify_meta_, xmlify_qs_)(b)), name = NULL)
    , error = function(err) err
  )

  if (inherits(xmlified, "error"))
    stop(err_msg, " Reason: ", xmlified$message)

  xmlified$chap_nr <- chap_nrs
  xmlified$role <- roles

  # XML-ify chapters
  chaps <- xmlified %>%
    filter(role != "title") %>%
    select(-role)

  body <- tryCatch(
    paste0("\n<body>", paste0(map2_chr(chaps$value, chaps$chap_nr, xmlify_chapter_), collapse = ""), "\n</body>")
    , error = function(err) err
  )

  if (inherits(body, "error"))
    stop(err_msg, " Reason: ", body$message)

  # add one more indentation level
  body <- gsub("\n", "\n  ", body)

  # XML-ify head
  title <- xmlified %>%
    filter(role == "title") %>%
    .$value

  first_row <- man.df[1, setdiff(names(man.df), attr(man.df, "extra_cols"))]
  first_row$text <- NULL

  head <- tryCatch(xmlify_head_(c(first_row, annotated = is_enhanced), title), error = function(err) err)

  if (inherits(head, "error"))
    stop(err_msg, " Reason: ", head$message)

  out <- combine_head_and_body_(head, body)

  if (parse){
    xml <- tryCatch(read_xml(out), error = function(err) err)

    if (inherits(xml, "error"))
      warning("Cannot parse. Returning XML string instead.\nReason: ", xml$message)
    else
      return(xml)
  }

  return(out)

}
