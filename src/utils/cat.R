#' Print a large console title for a script or run.
#'
#' @param title Optional plot title.
#' @return The value produced by `cat.script_title`.
cat.script_title <- function(title) {
  len <- nchar(title)
  spacer <- strrep("%", len)

  cat(paste("\n%%%", spacer, "%%%", sep = ""))
  cat(paste("\n%% ", title, " %%", sep = ""))
  cat(paste("\n%%%", spacer, "%%%\n\n", sep = ""))
}
#' Print a console title for a major run section.
#'
#' @param title Optional plot title.
#' @return The value produced by `cat.section_title`.
cat.section_title <- function(title) {
  len <- nchar(title)
  spacer <- strrep("|", len)

  cat(paste("\n|||", spacer, "|||", sep = ""))
  cat(paste("\n|| ", title, " ||", sep = ""))
  cat(paste("\n|||", spacer, "|||\n\n", sep = ""))
}
#' Print a compact console title for a subsection.
#'
#' @param title Optional plot title.
#' @return The value produced by `cat.subsection_title`.
cat.subsection_title <- function(title) {
  cat(paste("\n# ", title, "\n\n", sep = ""))
}
#' Print a JSON-like R object with indentation.
#'
#' @param json_obj JSON-like object or nested list to print.
#' @param indent Number of spaces per indentation level.
#' @return The value produced by `cat.json`.
cat.json <- function(json_obj, indent = 0) {

  #' Recursively print one branch of a JSON-like object.
  #'
  #' @param json_obj JSON-like object or nested list to print.
  #' @param indent_level Current recursion depth.
  #' @return The value produced by `printRecursive`.
  printRecursive <- function(json_obj, indent_level) {
    if (is.list(json_obj)) {
      cat("\n")
      keys <- names(json_obj)
      for (key in keys) {
        cat(paste(rep(" ", indent_level * indent), collapse = ""))
        cat(sprintf("- %s : ", key))
        printRecursive(json_obj[[key]], indent_level + 1)
        cat("\n")
      }
      cat(paste(rep(" ", (indent_level - 1) * indent), collapse = ""))
    } else if (is.atomic(json_obj)) {
      cat(sprintf("%s", json_obj))
    }
  }

  printRecursive(json_obj, 1)
  cat("\n")
}