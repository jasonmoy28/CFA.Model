#' Model Selection: CFA Initial Items Optimizer (unstable function)
#'
#' This function uses a for loop to find the CFI for all possible combinations of the factors.
#' @param data dataframe
#' @param items vector or quos(). required. create a lavaan formula. if you want to use tidyselect syntax, wrap it in quos() (e.g. quos(contains('Q'))).
#' @param group optional character. the nested variable for multilevel dataset (e.g., Country). If it is specified, it may requires a very long computation time.
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#' @param fac_num numeric. number of initial factors to be selected. default to 4. It is the minimum number for overidentified (non-saturated) model, so the model can produce a CFI value.
#'
#' @return
#' @export
#'
#' @examples
#'
# cfa_model_optimize_init_factors = function(data,
#                                            items,
#                                            group = NULL,
#                                            ordered = F,
#                                            fac_num = 4){
#   items = ggplot2::enquo(items)
#   
#   col_names = data %>% dplyr::select(!!items) %>% names(.)
#   combination_df = tibble::as_tibble(utils::combn(col_names,m = fac_num))
#   print(paste('Start optimizing for the 4 initial factors (Requires ', ncol(combination_df), ' times computation)',sep = ''))
#   summary_vector = NULL
#   for (i in combination_df) {
#     model = paste('DV =~', paste(i, collapse = ' + '))
#     summary = cfa_summary(model = model, data = data,group = group, return_result = 'short_summary',quite = T)
#     summary_vector = c(summary_vector,summary['cfi'])
#   }
#   max_iteraction = which.max(summary_vector)
#   max_factors = c(combination_df[,max_iteraction])[[1]]
#   print(paste('The initial 4 factors are: ',paste(max_factors, collapse = ' ')))
#   return(max_factors)
# }
