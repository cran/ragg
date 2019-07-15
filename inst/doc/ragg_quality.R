## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7
)
has_xlib <- capabilities()['X11']

## ----setup, message=FALSE------------------------------------------------
library(ragg)
library(grid)
library(magick)
img_zoom <- function(path) {
  img <- image_read(path)
  img <- image_crop(img, '300x150+150+75')
  img <- image_sample(img, '600x300')
  img
}

## ------------------------------------------------------------------------
circle_quality <- function(device, name, file, ...) {
  device(file, width = 600, height = 300, ...)
  grid.circle(
    x = c(0.25, 0.75), 
    r = 0.4, 
    gp = gpar(col = c('black', NA), fill = c(NA, 'black'), lwd = 2)
  )
  grid.text(y = 0.1, label = name, gp = gpar(cex = 2))
  invisible(dev.off())
}

## ------------------------------------------------------------------------
ragg_circle <- knitr::fig_path('.png')

circle_quality(agg_png, 'ragg', ragg_circle)

knitr::include_graphics(ragg_circle)

## ---- echo=FALSE---------------------------------------------------------
img_zoom(ragg_circle)

## ---- eval=has_xlib, include=has_xlib------------------------------------
xlib_circle <- knitr::fig_path('.png')

circle_quality(png, 'Xlib', xlib_circle, type = 'Xlib')

knitr::include_graphics(xlib_circle)

## ---- eval=has_xlib, include=has_xlib, echo=FALSE------------------------
img_zoom(xlib_circle)

## ------------------------------------------------------------------------
cairo_none_circle <- knitr::fig_path('.png')

circle_quality(png, 'cairo no AA', cairo_none_circle, 
               type = 'cairo', antialias = 'none')

knitr::include_graphics(cairo_none_circle)

## ---- echo=FALSE---------------------------------------------------------
img_zoom(cairo_none_circle)

## ------------------------------------------------------------------------
cairo_gray_circle <- knitr::fig_path('.png')

circle_quality(png, 'cairo gray AA', cairo_gray_circle, 
               type = 'cairo', antialias = 'gray')

knitr::include_graphics(cairo_gray_circle)

## ---- echo=FALSE---------------------------------------------------------
img_zoom(cairo_gray_circle)

## ------------------------------------------------------------------------
cairo_subpixel_circle <- knitr::fig_path('.png')

circle_quality(png, 'cairo subpixel AA', cairo_subpixel_circle, 
               type = 'cairo', antialias = 'subpixel')

knitr::include_graphics(cairo_subpixel_circle)

## ---- echo=FALSE---------------------------------------------------------
img_zoom(cairo_subpixel_circle)

## ------------------------------------------------------------------------
text_quality <- function(device, name, file, rotation = 0, ...) {
  text <- 'The quick blue R jumped over the lazy snake'
  vp <- viewport(angle = rotation)
  device(file, width = 600, height = 300, ...)
  pushViewport(vp)
  grid.text(text, x = 0.1, y = 0.2, just = 'left', gp = gpar(fontfamily = 'serif', cex = 0.5))
  grid.text(text, x = 0.1, y = 0.4, just = 'left', gp = gpar(fontfamily = 'serif', cex = 1))
  grid.text(text, x = 0.1, y = 0.6, just = 'left', gp = gpar(fontfamily = 'serif', cex = 1.5))
  grid.text(text, x = 0.1, y = 0.8, just = 'left', gp = gpar(fontfamily = 'serif', cex = 2))
  popViewport()
  grid.text(x = 0.9, y = 0.1, label = name, just = 'right', gp = gpar(cex = 2))
  invisible(dev.off())
}

## ------------------------------------------------------------------------
ragg_text <- knitr::fig_path('.png')

text_quality(agg_png, 'ragg', ragg_text)

knitr::include_graphics(ragg_text)

## ---- echo=FALSE---------------------------------------------------------
img_zoom(ragg_text)

## ------------------------------------------------------------------------
ragg_text_rot <- knitr::fig_path('.png')

text_quality(agg_png, 'ragg', ragg_text_rot, rotation = 27)

knitr::include_graphics(ragg_text_rot)

## ---- echo=FALSE---------------------------------------------------------
img_zoom(ragg_text_rot)

## ---- eval=has_xlib, include=has_xlib------------------------------------
xlib_text <- knitr::fig_path('.png')

text_quality(png, 'Xlib', xlib_text, type = 'Xlib')

knitr::include_graphics(xlib_text)

## ---- eval=has_xlib, include=has_xlib, echo=FALSE------------------------
img_zoom(xlib_text)

## ---- eval=has_xlib, include=has_xlib------------------------------------
xlib_text_rot <- knitr::fig_path('.png')

text_quality(png, 'Xlib', xlib_text_rot, rotation = 27, type = 'Xlib')

knitr::include_graphics(xlib_text_rot)

## ---- eval=has_xlib, include=has_xlib, echo=FALSE------------------------
img_zoom(xlib_text_rot)

## ------------------------------------------------------------------------
cairo_none_text <- knitr::fig_path('.png')

text_quality(png, 'cairo no AA', cairo_none_text, 
             type = 'cairo', antialias = 'none')

knitr::include_graphics(cairo_none_text)

## ---- echo=FALSE---------------------------------------------------------
img_zoom(cairo_none_text)

## ------------------------------------------------------------------------
cairo_none_text_rot <- knitr::fig_path('.png')

text_quality(png, 'cairo no AA', cairo_none_text_rot, rotation = 27, 
             type = 'cairo', antialias = 'none')

knitr::include_graphics(cairo_none_text_rot)

## ---- echo=FALSE---------------------------------------------------------
img_zoom(cairo_none_text_rot)

## ------------------------------------------------------------------------
cairo_gray_text <- knitr::fig_path('.png')

text_quality(png, 'cairo gray AA', cairo_gray_text, 
             type = 'cairo', antialias = 'gray')

knitr::include_graphics(cairo_gray_text)

## ---- echo=FALSE---------------------------------------------------------
img_zoom(cairo_gray_text)

## ------------------------------------------------------------------------
cairo_gray_text_rot <- knitr::fig_path('.png')

text_quality(png, 'cairo gray AA', cairo_gray_text_rot, rotation = 27, 
             type = 'cairo', antialias = 'gray')

knitr::include_graphics(cairo_gray_text_rot)

## ---- echo=FALSE---------------------------------------------------------
img_zoom(cairo_gray_text_rot)

## ------------------------------------------------------------------------
cairo_subpixel_text <- knitr::fig_path('.png')

text_quality(png, 'cairo subpixel AA', cairo_subpixel_text, 
             type = 'cairo', antialias = 'subpixel')

knitr::include_graphics(cairo_subpixel_text)

## ---- echo=FALSE---------------------------------------------------------
img_zoom(cairo_subpixel_text)

## ------------------------------------------------------------------------
cairo_subpixel_text_rot <- knitr::fig_path('.png')

text_quality(png, 'cairo subpixel AA', cairo_subpixel_text_rot, rotation = 27, 
             type = 'cairo', antialias = 'subpixel')

knitr::include_graphics(cairo_subpixel_text_rot)

## ---- echo=FALSE---------------------------------------------------------
img_zoom(cairo_subpixel_text_rot)

## ------------------------------------------------------------------------
blend_quality <- function(device, name, file, ...) {
  device(file, width = 600, height = 300, ...)
  grid.rect(x = 0.35, y = 0.4, width = 0.5, height = 0.5, gp = gpar(fill = '#FF0000'))
  grid.rect(x = 0.65, y = 0.6, width = 0.5, height = 0.5, gp = gpar(fill = '#00FF001A'))
  grid.text(x = 0.9, y = 0.1, label = name, just = 'right', gp = gpar(cex = 2))
  invisible(dev.off())
}

## ------------------------------------------------------------------------
ragg_blend <- knitr::fig_path('.png')

blend_quality(agg_png, 'ragg', ragg_blend)

knitr::include_graphics(ragg_blend)

## ------------------------------------------------------------------------
cairo_blend <- knitr::fig_path('.png')

blend_quality(png, 'cairo', cairo_blend, type = 'cairo')

knitr::include_graphics(cairo_blend)

## ------------------------------------------------------------------------
raster_quality <- function(device, name, file, ...) {
  reds <- matrix(hcl(0, 80, seq(50, 80, 10)),
                        nrow = 4, ncol = 5)
  device(file, width = 600, height = 300, ...)
  grid.raster(reds, vp = viewport(0.25, 0.25, 0.5, 0.5, angle = 27))
  grid.raster(reds, interpolate = FALSE, 
              vp = viewport(0.75, 0.75, 0.5, 0.5, angle = 27))
  grid.text(x = 0.9, y = 0.1, label = name, just = 'right', gp = gpar(cex = 2))
  invisible(dev.off())
}

## ------------------------------------------------------------------------
ragg_raster <- knitr::fig_path('.png')

raster_quality(agg_png, 'ragg', ragg_raster)

knitr::include_graphics(ragg_raster)

## ---- eval=has_xlib, include=has_xlib------------------------------------
xlib_raster <- knitr::fig_path('.png')

raster_quality(png, 'Xlib', xlib_raster, type = 'Xlib')

knitr::include_graphics(xlib_raster)

## ------------------------------------------------------------------------
cairo_raster <- knitr::fig_path('.png')

raster_quality(png, 'cairo', cairo_raster, type = 'cairo')

knitr::include_graphics(cairo_raster)

## ------------------------------------------------------------------------
sessioninfo::session_info()

