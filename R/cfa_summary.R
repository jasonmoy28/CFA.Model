#' CFA summary
#'
#' The function is a wrapper of the lavaan::cfa. By default, it fits a lavaan model based on the data by treating everything columns measure a single latent variable.
#'
#' @param model explicit lavaan model. use lavaan syntax. If model is not explictly specified, the function will run a CFA assuming all columns belong to one latent variable.
#' @param data dataframe
#' @param group optional character. default is NULL. the nested variable for multilevel dataset (e.g., Country)
#' @param items vector or quos(). default to NULL. create a lavaan formula if the formula is not explicitly set in model. if you want to use tidyselect syntax, wrap it in quos() (e.g. quos(contains('Q'))). This argument will be ignored if model is specified.
#' @param summary_item vector. default is cfi, rmsea, and tli. See lavaan for more option
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#' @param return_result character. default is 'model'. You can set it to 'short_summary' for fit measures
#' @param quite default as F. If set to true, it will not print the statement that tells you what the lavaan model is going to run.
#' @param group_partial items for partial equivalence. The form should be c('DV =~ item1', 'DV =~ item2').
#'
#' @return
#' @export
#'
#' @examples
#'
cfa_summary = function(model = NULL,
                       data,
                       group = NULL,
                       items = NULL,
                       summary_item = c('cfi', 'rmsea', 'tli'),
                       ordered = F,
                       return_result = 'model',
                       quite = F,
                       group_partial = NULL) {

  if (is.null(model)) {
    cfa_data = data %>% dplyr::select(!!!items, !!!group)
    cfa_items = data %>% dplyr::select(!!!items) %>% names(.)
    model = paste('DV =~', paste(cfa_items, collapse = ' + '))
  }

  cfa_data = data
  if (quite == F) {
    print(paste('Computing CFA using:',model))
  }
  cfa_model = lavaan::cfa(
    model = model,
    data = cfa_data,
    group = group,
    ordered = ordered,
    group.partial = group_partial
  )
  if (ordered) {
    summary_item = paste(summary_item, '.scaled', sep = '')
  }
  if (return_result == 'model') {
    return(cfa_model)
  } else if (return_result == 'short_summary') {
    cfa_short_summary = lavaan::fitMeasures(cfa_model)[summary_item]
    return(cfa_short_summary)
  } else if(return_result == 'long_summary') {
    summary(cfa_model,fit.measures = T, standardized = T)
  }
}
