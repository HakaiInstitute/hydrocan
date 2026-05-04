#' Cite a hydrocan data source
#'
#' Returns a citation for a registered data source, formatted as a
#' [bibentry()] object. The output behaves like [citation()]: it prints a
#' human-readable reference and a BibTeX entry, and can be passed to
#' [toBibtex()].
#'
#' @param source Single character string naming the data source to cite.
#'   See [hc_list_sources()] for available names.
#'
#' @return A [bibentry()] object.
#' @export
#'
#' @examples
#' \dontrun{
#' hc_citation("cehq")
#' toBibtex(hc_citation("hydroquebec"))
#' }
hc_citation <- function(source) {
  adapter <- get0(source, envir = .hydrocan_registry)
  if (is.null(adapter)) {
    stop("No data source registered with name '", source, "'.", call. = FALSE)
  }
  .adapter_bibentry(adapter)
}

# Build a bibentry for one adapter. Fields that are NULL in the adapter are
# omitted rather than passed as NA, since bibentry() handles missing fields
# better than explicit NAs.
.adapter_bibentry <- function(adapter) {
  note <- "Accessed via the hydrocan R package"
  if (!is.null(adapter$license)) {
    note <- paste0(note, " License: ", adapter$license)
  }

  args <- list(
    bibtype = "Misc",
    title = adapter$title %||% adapter$name,
    year = format(Sys.Date(), "%Y"),
    note = note
  )
  if (!is.null(adapter$publisher)) {
    args$author <- utils::person(family = adapter$publisher)
  }
  if (!is.null(adapter$docs_url)) {
    args$url <- adapter$docs_url
  }

  entry <- do.call(utils::bibentry, args)

  # Promote to "citation" class so print() renders the header and BibTeX
  # section in the same style as citation('pkg').
  class(entry) <- c("citation", "bibentry")
  attr(entry, "mheader") <- paste0(
    "To cite '",
    adapter$name,
    "' data in publications use:"
  )
  entry
}

# Minimal null-coalescing operator used only within this file.
`%||%` <- function(x, y) if (is.null(x)) y else x
