#' semPlot::semPaths rewrite
#'
#' @param model a model object that is acceptable in semPlot::semPaths
#'
#' @return
#' @export
#'
#' @examples
SEM_Plot = function(model, rotation = 2) {
  semPlot::semPaths(model,
                    whatLabels = 'std',
                    layout = 'tree3',
                    rotation = rotation,
                    nCharNodes = 0,
                    edge.color = 'black',
                    edge.label.cex = 0.5)
}
