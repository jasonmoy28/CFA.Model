#' Measurement Invariance
#'
#' This function uses the semTools::compareFit. For now, it will only compute the test measures between the configural and metric model
#'
#' @param model explicit lavaan model. use lavaan syntax. If model is not explictly specified, the function will run a CFA assuming all columns belong to one latent variable.
#' @param data dataframe
#' @param group character. required for metric model.
#' @param items vector or quos(). default to NULL. create a lavaan formula if the formula is not explicitly set in model. if you want to use tidyselect syntax, wrap it in quos() (e.g. quos(contains('Q')))
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#' @param group_partial items for partial equivalence. form should be c('DV =~ item1', 'DV =~ item2').
#'
#' @return
#' return the compareFit object
#' @export
#'
#' @examples
#'
measurement_invariance = function(model = NULL,
                                  data,
                                  items = NULL,
                                  group,
                                  ordered = F,
                                  group_partial = NULL) {
  print('Computing for configural model')
  config_model = cfa_summary(
    model = model,
    data = data,
    group = group,
    items = items,
    ordered = ordered,
    group_partial = group_partial
  )
  print('Computing for metric model')
  metric_model = cfa_metric_summary(
    model = model,
    data = data,
    group = group,
    items = items,
    ordered = ordered,
    group_partial = group_partial
  )
  fit = semTools::compareFit(config_model, metric_model)
  return(fit)
}
