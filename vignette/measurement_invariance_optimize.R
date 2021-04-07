#' Measurement Invariance optimize (unstable function)
#'
#' The function help you to dplyr::select the items for that is fit with measurement invariance using a data-driven approach. It will take very long since it needs to compute the configural and the metric CFA many many times.
#' @param data dataframe
#' @param items vector or quos(). required. create a lavaan formula. if you want to use tidyselect syntax, wrap it in quos() (e.g. quos(contains('Q'))).
#' @param threshold the delta_CFI threshold. Default is 0.001
#' @param group required character. the nested variable for multilevel dataset (e.g., Country).
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#'
#' @return
#' return a lavaan syntax model
#' @export
#'
#' @examples
#'
# measurement_invariance_optimize = function(data,
#                                            items,
#                                            threshold = 0.001,
#                                            group,
#                                            ordered = F) {
#   items = ggplot2::enquo(items)
# 
#   cfa_items = data %>% dplyr::select(!!items) %>% names(.)
#   delta_cfi_vec = NULL
#   max_item = NULL
#   max_list = list(model = NULL, value = NULL)
#   for (index in c(1:(length(cfa_items)-4))) {
#     if (!is.null(max_item)) {
#       cfa_items = cfa_items[-which(cfa_items == max_item)]
#       delta_cfi_vec = NULL
#     }
#     for (cfa_item in cfa_items) {
#       nested_item = cfa_items[-which(cfa_items == cfa_item)]
#       model = paste('DV =~', paste(nested_item, collapse = ' + '))
#       config_summary = cfa_summary(model = model, data = data, group = group,
#                                    items = items, ordered = ordered,return_result = 'short_summary',quite = T)
# 
#       metric_summary = cfa_metric_summary(model = model, data = data, group = group,
#                                           items = items, ordered = ordered,return_result = 'short_summary',quite = T)
#       delta_cfi =  metric_summary['cfi'] - config_summary['cfi']
#       delta_cfi_vec = c(delta_cfi_vec,delta_cfi)
#       print(paste(cfa_item,delta_cfi))
#     }
#     # Max values
#     max_iteraction = which.max(delta_cfi_vec)
#     max_value = delta_cfi_vec[max_iteraction]
#     max_item = cfa_items[max_iteraction]
#     max_items = cfa_items[-which(cfa_items == max_item)]
#     max_list[['model']] = c(max_list[['model']],paste('DV =~', paste(max_items, collapse = ' + ')))
#     max_list[['value']] = c(max_list[['value']],max_value)
# 
#     # Print dropping items
#     print(paste('Dropping item:',max_item))
#   }
# 
#   successful_model = max_list[[1]][which.max(max_list[[2]])]
#   successful_model_value = max_list[[2]][which.max(max_list[[2]])]
#   print(paste('Best model is ', successful_model, 'with a delta_cfi of', round(successful_model_value,6)))
#   return(successful_model)
# }
