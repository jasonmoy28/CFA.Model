#' Model Selection: cfa_groupwise
#'
#' This function will run a series of CFA (n = length(group)) with respect to each group. The function is intended to help you get a better understanding of which group has abnormal fit indicator
#'
#' @param model explicit lavaan model. use lavaan syntax. If model is not explictly specified, the function will run a CFA assuming all columns belong to one latent variable.
#' @param data dataframe. a multilevel dataset
#' @param group character. required. the variable uses for series of CFA.
#' @param items vector or quos(). default to NULL. create a lavaan formula if the formula is not explicitly set in model. if you want to use tidyselect syntax, wrap it in quos() (e.g. quos(contains('Q'))). This argument will be ignored if model is specified
#' @param ordered logical. default is `F`. If it is set to `T``, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#'
#' @return
#' return a dataframe with cfi, rmsea, tli for each group
#' @export
#'
#' @examples
#'

cfa_groupwise = function(model = NULL, data, group, items = NULL, ordered = F){

  if (is.null(model)) {
      data = data %>% dplyr::select(!!!items, !!!group)
      cfa_items = data %>% dplyr::select(!!!items) %>% names(.)
      model = paste('DV =~', paste(cfa_items, collapse = ' + '))
  }

  groups = data %>% dplyr::select(!!! group) %>% dplyr::distinct()
  groups = c(groups)[[1]]
  return_df = data.frame(group = NULL,cfi = NULL, rmsea = NULL, tli = NULL)
  for (i in groups) {
    cfa_data = data %>%
      dplyr::filter(dplyr::across(!!! group) == i)
    cfa_model_summary = cfa_summary(model = model, data = cfa_data,ordered = ordered,return_result = 'summary')
    cfa_model_summary = as.data.frame(cfa_model_summary)
    summary_df = data.frame(group = i, cfi = cfa_model_summary['cfi',], rmsea = cfa_model_summary['rmsea',],tli = cfa_model_summary['tli',])
    return_df = rbind(return_df,summary_df)
  }
  return(return_df)
}
