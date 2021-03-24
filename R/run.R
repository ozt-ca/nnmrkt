#' Run a simple script by nnmrkt
#'
#' @description
#' Run a simple script by nnmkrt, implemented with NN
#'
#' @examples
#' run_nnmrkt()
run_nnmrkt <- function(){
  dat <- read.csv('https://raw.githubusercontent.com/ozt-ca/tjo.hatenablog.samples/master/r_samples/public_lib/jp/example_nnmrkt.csv')
  dat$label <- as.factor(dat$label)

  fit <- e1071::svm(label~., dat, gamma = 550)

  px <- seq(-0.2, 1.0, by = 0.01)
  py <- seq(-0.5, 1.5, by = 0.01)
  pgrid <- expand.grid(px, py)
  names(pgrid) <- names(dat)[-3]
  out <- predict(fit, newdata = pgrid)

  contour(px, py, array(out, c(length(px), length(py))), levels = 0.5,
        col = 'red', lwd = 2, drawlabels = F)
}
