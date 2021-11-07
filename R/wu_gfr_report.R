#' Generate elements for a Google Form Summary
#'
#' @param resp_tbl Responses tibble
#' @param rpt_tbl Report template
#' @param show_msg Show additional messages
#'
#' @export
#' @importFrom rmarkdown render

wu_gfr_report <- function(gfr_elems, output_fn, output_dir = ".", rpt_title = "Google Form Summary", report_rmd = NULL) {

  ## Get the Rmd template
  if (is.null(report_rmd)) {
        report_rmd <- system.file("gfs-report.Rmd", package="wrkshputils")
      }
  if (!file.exists(report_rmd)) stop("Cant find the report template")


  report_fn <- render(input = report_rmd,
                      output_dir = output_dir, output_file = output_fn,
                      params = list(rpt_title = rpt_title,
                                    rpt_elemnts = gfr_elems))

  invisible(report_fn)

}
