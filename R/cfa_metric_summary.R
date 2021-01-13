#' Confirmatory factor analysis metric model
#'
#' The function is a wrapper of the lavaan::cfa. By default, it fits a lavaan model based on the data by treating everything columns measure a single latent variable.
#' @param model explicit lavaan model. use lavaan syntax. If model is not explictly specified, the function will run a CFA assuming all columns belong to one latent variable.
#' @param data dataframe
#' @param group character. required. the nested variable for multilevel dataset (e.g., Country)
#' @param items vector or quos(). default to NULL. create a lavaan formula if the formula is not explicitly set in model. if you want to use tidyselect syntax, wrap it in quos() (e.g. quos(contains('Q'))). This argument will be ignored if model is specified
#' @param summary_item vector. default is cfi, rmsea, and tli. See lavaan for more option
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#' @param return_result character. default is 'model'. You can set it to 'summary' for fit measures
#'
#' @return
#' @export
#'
#' @examples
#    #explicitly state the lavaan model (1 latent factor)
#'   cfa_summary(model = 'DV =~ IV1 + IV2 + IV3', data = data, group = 'Country')
#'
#'   # the equivalent without explictly stating the lavaan model (1 latent factor)
#'   cfa_summary(data = data, items = quos(IV1:IV3'), group = 'Country')
#'
#'   # multiple latent factor (must explicitly state the lavaan model)
#'   cfa_summary(model = 'DV1 =~ IV1 + IV2 + IV3; DV2 =~ IV4 + IV5 + IV6', data = data, group = 'Country')
#'
cfa_metric_summary = function(model = NULL,
                              data,
                              group,
                              items = NULL,
                              summary_item = c('cfi', 'rmsea', 'tli'),
                              ordered = F,
                              return_result = 'model',
                              quite = F) {
  # Create the lavaan formula if model is not explicitly specify
  if (is.null(model)) {
    cfa_data = data %>% select(!!!items, !!!group)
    cfa_items = data %>% select(!!!items) %>% names(.)
    model = paste('DV =~', paste(cfa_items, collapse = ' + '))
  }

  cfa_data = data
  if (quite == F) {
    print(paste('Computing CFA using: ',model))
  }

  # Lavaan modeling
  cfa_model = cfa(
    model = model,
    data = cfa_data,
    group = group,
    group.equal = 'loadings',
    ordered = ordered
  )

  if (ordered) {
    summary_item = paste(summary_item, '.scaled', sep = '')
  }
  if (return_result == 'model') {
    return(cfa_model)
  } else if (return_result == 'short_summary') {
    cfa_short_summary = fitMeasures(cfa_model)[summary_item]
    return(cfa_short_summary)
  } else if(return_result == 'long_summary') {
    cfa_long_summary = summary(cfa_model,fit.measure =T, standardized = T)
    return(cfa_long_summary)
  }
}
