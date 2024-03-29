% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wu_popup.R
\name{wu_popup}
\alias{wu_popup}
\title{Plot a png or jpg image file}
\usage{
wu_popup(x, height = 360, bgcol = "#222")
}
\arguments{
\item{x}{A filename or URL of a PNG or JPG image}

\item{height}{The (minimum) height of the viewer pane}

\item{bgcol}{Background color}
}
\description{
Plot a png or jpg image file
}
\details{
This will display a PNG or JPG image in the RStudio viewer pane.
\code{x} can be either a local file or a URL. This gives you the ability to
create 'popup' slides in a R script that you work through during a lesson.

\code{height} determines the minimum height in screen pixels that the viewer pane will
resize itself to (if needed). Set to \code{NULL} to disable.

\code{bgcol} can be a named HTML color (e.g., \code{gray}) or a hexadecimal HTML color value (e.g., \code{#FF0034}).
}
\examples{
\dontrun{
wu_popup("http://placekitten.com/400/300")
}

}
