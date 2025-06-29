#' @title Helper function to make latex long tables.
#'
#' @import gt table1 parseLatex
#' @name repeat_header_gt
#'
#' @export
library(gt)
library(parseLatex)

repeat_header_gt <- function(gt, continuation = "\\textit{(continued...)}") {
  latex <- as_latex(gt)
  parsed <- parseLatex(latex)
  table <- find_tabular(parsed)
  toprule <- find_macro(parsed[[table]], "\\toprule")[1]
  midrule <- find_macro(parsed[[table]], "\\midrule")[1]
  header <- parsed[[table]][toprule:midrule]
  range <- LaTeX2range(table, midrule)
  parsed <- set_range(parsed, range,
                      paste0("\\midrule\n\\endfirsthead\n\\caption*{",
                             continuation,
                             "}\\\\\n", deparseLatex(header), "\\endhead\n"))
  newlatex <- deparseLatex(parsed)
  attributes(newlatex) <- attributes(latex)
  newlatex
}
