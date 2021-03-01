#' the function is not yet ready
#'
#' @param model a model object that is acceptable in semPlot::semPaths
#'
#' @return
#' @export
#'
#' @examples
#'
SEM_Plot = function(model) {
  semPlot::semPaths(model,
                    whatLabels = 'std',
                    layout = 'tree3',
                    nCharNodes = 0,
                    edge.color = 'black',
                    edge.label.cex = 0.5)
}
