#' \code{\link[dplyr]{as_tibble}} method for \code{\link[manifestoR]{ManifestoCorpus}} class
#'
#' @param x A 'ManifestoCorpus' object.
#'
#' @return A \code{\link[tibble]{tibble}} with
#'      rows uniquely identifyied by column 'manifesto_id',
#'      list-column 'data' containing mainfesto text and codes, and
#'      all other columns recording manifesto meta data.
#'
#' @import dplyr
#' @import tidyr
#' @export
as_tibble.ManifestoCorpus <- function(x) {

  if (!inherits(x, "ManifestoCorpus"))
    stop("No `as_tibble()` method implemented for object of class ", sQuote(class(x)[1]))

  man_sents <- as_tibble(map_dfr(as.list(x), "content", .id = "manifesto_id"))

  man_meta <- as_tibble(map_dfr(map(as.list(x), "meta"), as.data.frame.list, stringsAsFactors = FALSE))

  out <- left_join(man_sents, man_meta, by = "manifesto_id")

  out <- nest(out, data = names(man_sents)[-1])

  return(out)
}

#' \code{\link[dplyr]{as_tibble}} method for \code{\link[manifestoR]{ManifestoDocument}} class
#'
#' @param x A 'ManifestoDocument' object.
#'
#' @return A \code{\link[tibble]{tibble}} with
#'      rows uniquely identifyied by column 'manifesto_id',
#'      list-column 'data' containing mainfesto text and codes, and
#'      all other columns recording manifesto meta data.
#'
#' @import dplyr
#' @import tidyr
#' @export
as_tibble.ManifestoDocument <- function(x) {

  if (!inherits(x, "ManifestoDocument"))
    stop("No `as_tibble()` method implemented for object of class ", sQuote(class(x)[1]))

  out <- as_tibble(as.data.frame.list(as.list(x$meta)))
  out$data <- list(as_tibble(x$content))

  return(out)
}


