% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agg_dev.R
\name{agg_ppm}
\alias{agg_ppm}
\title{Draw to a PPM file}
\usage{
agg_ppm(
  filename = "Rplot\%03d.ppm",
  width = 480,
  height = 480,
  units = "px",
  pointsize = 12,
  background = "white",
  res = 72,
  scaling = 1,
  snap_rect = TRUE,
  bg
)
}
\arguments{
\item{filename}{The name of the file. Follows the same semantics as the file
naming in \code{\link[grDevices:png]{grDevices::png()}}, meaning that you can provide a \code{\link[=sprintf]{sprintf()}}
compliant string format to name multiple plots (such as the default value)}

\item{width, height}{The dimensions of the device}

\item{units}{The unit \code{width} and \code{height} is measured in, in either pixels
(\code{'px'}), inches (\code{'in'}), millimeters (\code{'mm'}), or centimeter (\code{'cm'}).}

\item{pointsize}{The default pointsize of the device in pt. This will in
general not have any effect on grid graphics (including ggplot2) as text
size is always set explicitly there.}

\item{background}{The background colour of the device}

\item{res}{The resolution of the device. This setting will govern how device
dimensions given in inches, centimeters, or millimeters will be converted
to pixels. Further, it will be used to scale text sizes and linewidths}

\item{scaling}{A scaling factor to apply to the rendered line width and text
size. Useful for getting the right dimensions at the resolution that you
need. If e.g. you need to render a plot at 4000x3000 pixels for it to fit
into a layout, but you find that the result appears to small, you can
increase the \code{scaling} argument to make everything appear bigger at the
same resolution.}

\item{snap_rect}{Should axis-aligned rectangles drawn with only fill snap to
the pixel grid. This will prevent anti-aliasing artifacts when two
rectangles are touching at their border.}

\item{bg}{Same as \code{background} for compatibility with old graphic device APIs}
}
\description{
The PPM (Portable Pixel Map) format defines one of the simplest storage
formats available for
image data. It is basically a raw 8bit RGB stream with a few bytes of
information in the start. It goes without saying, that this file format is
horribly inefficient and should only be used if you want to play around with
a simple file format, or need a file-based image stream.
}
\examples{
file <- tempfile(fileext = '.ppm')
agg_ppm(file)
plot(sin, -pi, 2*pi)
dev.off()

}
