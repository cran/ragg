## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7
)
has_xlib <- capabilities()['X11']

## ----setup, message=FALSE------------------------------------------------
library(ragg)
library(devoid)
library(ggplot2)

## ---- message=FALSE------------------------------------------------------
file <- tempfile(fileext = '.png')
res <- bench::mark(
  ragg = {agg_png(file); plot.new(); dev.off()},
  cairo = {png(file, type = 'cairo'); plot.new(); dev.off()},
  cairo_png = {png(file, type = 'cairo-png'); plot.new(); dev.off()},
  Xlib = if (has_xlib) {png(file, type = 'Xlib'); plot.new(); dev.off()} else NULL,
  check = FALSE
)
if (!has_xlib) {
  res <- res[-4, ]
}
plot(res, type = 'ridge') + ggtitle('Open and close performance')

## ---- message=FALSE------------------------------------------------------
render_bench <- function(dev_f, ...) {
  dots <- rlang::enexprs(...)
  force(dev_f)
  on.exit(dev.off())
  plot.new()
  rlang::eval_tidy(expr(bench::mark(!!!dots, min_iterations = 10)))
}
all_render_bench <- function(expr, xlib = TRUE) {
  file <- tempfile()
  expr <- rlang::enexpr(expr)
  res <- list(
    render_bench(agg_png(file), ragg = !!expr),
    render_bench(png(file, type = "cairo", antialias = 'none'), 
                 cairo_none = !!expr),
    render_bench(png(file, type = "cairo", antialias = 'gray'), 
                 cairo_gray = !!expr),
    render_bench(png(file, type = "cairo", antialias = 'subpixel'), 
                 cairo_subpixel = !!expr),
    if (has_xlib && xlib) render_bench(png(file, type = "Xlib"), xlib = !!expr) else NULL
  )
  expr <- unlist(lapply(res, `[[`, 'expression'), recursive = FALSE)
  res <- suppressWarnings(dplyr::bind_rows(res))
  res$expression <- expr
  class(res$expression) <- c('bench_expr', 'expression')
  res$Anti_aliased <- c(TRUE, FALSE, TRUE, TRUE, FALSE)[seq_len(nrow(res))]
  res
}
plot_bench <- function(x, title, baseline) {
  plot(x, type = 'ridge', aes(fill = Anti_aliased)) + 
    facet_null() +
    geom_vline(xintercept = baseline['elapsed'], linetype = 2, colour = 'grey') + 
    annotate('text', x = baseline['elapsed'], y = -Inf, label = ' baseline',
             vjust = 0, hjust = 0, colour = 'grey') +
    scale_fill_brewer(labels = c('No', 'Yes'), type = 'qual') +
    labs(title = title, fill = 'Anti-aliased', x = NULL, y = NULL) + 
    theme_minimal() + 
    theme(panel.grid.major.y = element_blank(),
          legend.position = 'bottom') + 
    scale_x_continuous(labels = bench::bench_time)
}

## ---- warning=FALSE, message=FALSE---------------------------------------
x <- runif(1000)
y <- runif(1000)
pch <- 1

void_dev()
plot.new()
b <- system.time(points(x, y, pch = pch))
invisible(dev.off())
res <- all_render_bench(points(x, y, pch = pch))
plot_bench(res, 'Unfilled circle performance', b)

## ---- message=FALSE------------------------------------------------------
pch <- 19

void_dev()
plot.new()
b <- system.time(points(x, y, pch = pch))
invisible(dev.off())
res <- all_render_bench(points(x, y, pch = pch))
plot_bench(res, 'Filled circle performance', b)

## ---- message=FALSE------------------------------------------------------
pch <- 4

void_dev()
plot.new()
b <- system.time(points(x, y, pch = pch))
invisible(dev.off())
res <- all_render_bench(points(x, y, pch = pch))
plot_bench(res, 'Line segment performance', b)

## ---- message=FALSE------------------------------------------------------
void_dev()
plot.new()
b <- system.time(lines(x, y))
invisible(dev.off())
res <- all_render_bench(lines(x, y))
plot_bench(res, 'Connected line performance', b)

## ---- message=FALSE------------------------------------------------------
void_dev()
plot.new()
b <- system.time(lines(x, y, lty = 4))
invisible(dev.off())
res <- all_render_bench(lines(x, y, lty = 4))
plot_bench(res, 'Patterned connected line performance', b)

## ---- message=FALSE------------------------------------------------------
pch <- 0

void_dev()
plot.new()
b <- system.time(points(x, y, pch = pch))
invisible(dev.off())
res <- all_render_bench(points(x, y, pch = pch))
plot_bench(res, 'Unfilled rectangle performance', b)

## ---- message=FALSE------------------------------------------------------
pch <- 15

void_dev()
plot.new()
b <- system.time(points(x, y, pch = pch))
invisible(dev.off())
res <- all_render_bench(points(x, y, pch = pch))
plot_bench(res, 'Filled rectangle performance', b)

## ---- message=FALSE------------------------------------------------------
pch <- 2

void_dev()
plot.new()
b <- system.time(points(x, y, pch = pch))
invisible(dev.off())
res <- all_render_bench(points(x, y, pch = pch))
plot_bench(res, 'Simple polygon performance', b)

## ---- message=FALSE------------------------------------------------------
void_dev()
plot.new()
b <- system.time(polygon(x, y))
invisible(dev.off())
res <- all_render_bench(polygon(x, y))
plot_bench(res, 'Unfilled complex polygon performance', b)

## ---- message=FALSE------------------------------------------------------
void_dev()
plot.new()
b <- system.time(polygon(x, y, border = 'gray', col = 'black'))
invisible(dev.off())
res <- all_render_bench(polygon(x, y, border = 'gray', col = 'black'))
plot_bench(res, 'Filled complex polygon performance', b)

## ---- message=FALSE------------------------------------------------------
section <- rep(1:10, each = 100)
x_path <- unlist(lapply(split(x, section), function(x) c(x, NA)))
y_path <- unlist(lapply(split(y, section), function(x) c(x, NA)))
x_path <- x_path[-length(x_path)]
y_path <- y_path[-length(y_path)]

void_dev()
plot.new()
b <- system.time(polypath(x_path, y_path, rule = 'evenodd'))
invisible(dev.off())
res <- all_render_bench(polypath(x_path, y_path, rule = 'evenodd'), 
                        xlib = FALSE)
plot_bench(res, 'Unfilled path performance', b)

## ---- message=FALSE------------------------------------------------------
void_dev()
plot.new()
b <- system.time(polypath(x_path, y_path, rule = 'evenodd', border = 'gray', 
                          col = 'black'))
invisible(dev.off())
res <- all_render_bench(polypath(x_path, y_path, rule = 'evenodd', 
                                border = 'gray', col = 'black'),
                        xlib = FALSE)
plot_bench(res, 'Filled path performance', b)

## ---- message=FALSE------------------------------------------------------
raster <- matrix(hcl(0, 80, seq(50, 80, 10)), nrow = 4, ncol = 5)

void_dev()
plot.new()
b <- system.time(rasterImage(raster, xleft = rep(0.25, 100), ybottom = 0.25, 
                             xright = 0.75, ytop = 0.75, interpolate = FALSE))
invisible(dev.off())
res <- all_render_bench(rasterImage(raster, xleft = 0.25, ybottom = 0.25, 
                                    xright = 0.75, ytop = 0.75, 
                                    interpolate = FALSE))
plot_bench(res, 'Non-interpolated, non-rotated raster performance', b)

## ---- message=FALSE------------------------------------------------------
void_dev()
plot.new()
b <- system.time(rasterImage(raster, xleft = rep(0.25, 100), ybottom = 0.25, 
                             xright = 0.75, ytop = 0.75, interpolate = FALSE, 
                             angle = 27))
invisible(dev.off())
res <- all_render_bench(rasterImage(raster, xleft = 0.25, ybottom = 0.25, 
                                    xright = 0.75, ytop = 0.75, 
                                    interpolate = FALSE, angle = 27))
plot_bench(res, 'Non-interpolated, rotated raster performance', b)

## ---- message=FALSE------------------------------------------------------
void_dev()
plot.new()
b <- system.time(rasterImage(raster, xleft = rep(0.25, 100), ybottom = 0.25, 
                             xright = 0.75, ytop = 0.75, interpolate = TRUE))
invisible(dev.off())
res <- all_render_bench(rasterImage(raster, xleft = 0.25, ybottom = 0.25, 
                                    xright = 0.75, ytop = 0.75, 
                                    interpolate = TRUE))
plot_bench(res, 'Interpolated, non-rotated raster performance', b)

## ---- message=FALSE------------------------------------------------------
void_dev()
plot.new()
b <- system.time(rasterImage(raster, xleft = rep(0.25, 100), ybottom = 0.25, 
                             xright = 0.75, ytop = 0.75, interpolate = TRUE, 
                             angle = 27))
invisible(dev.off())
res <- all_render_bench(rasterImage(raster, xleft = 0.25, ybottom = 0.25, 
                                    xright = 0.75, ytop = 0.75, 
                                    interpolate = TRUE, angle = 27))
plot_bench(res, 'Interpolated, rotated raster performance', b)

## ---- message=FALSE------------------------------------------------------
pch <- "#"

void_dev()
plot.new()
b <- system.time(points(x, y, pch = pch))
invisible(dev.off())
res <- all_render_bench(points(x, y, pch = pch))
plot_bench(res, 'Single character performance', b)

## ---- message=FALSE------------------------------------------------------
void_dev()
plot.new()
b <- system.time(text(x, y, label = 'abcdefghijk'))
invisible(dev.off())
res <- all_render_bench(text(x, y, label = 'abcdefghijk'))
plot_bench(res, 'Text string performance', b)

## ---- fig.height=5, message=FALSE----------------------------------------
p <- ggplot(diamonds, aes(carat, price)) + 
  geom_hex() + 
  geom_point(shape = 1, size = 0.05, colour = 'white') + 
  geom_smooth() +
  facet_wrap(~clarity) + 
  labs(title = '5 things you didn\'t knew about the diamonds dataset',
       subtitle = 'You won\'t believe number 4',
       caption = 'Source: The ggplot2 package')
p

## ---- message=FALSE, warning=FALSE---------------------------------------
p <- ggplotGrob(p)
void_dev()
b <- system.time(plot(p))
invisible(dev.off())

res <- all_render_bench(plot(p))
plot_bench(res, 'Complex composite performance', b)

## ------------------------------------------------------------------------
sessioninfo::session_info()

