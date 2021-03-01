#' Model Selection: CFA Items Optimizer
#'
#' The function help you to dplyr::select the best item for CFA using a data-driven approach. In short, it computed a series of CFA using a stepwise apporach, then it dplyr::select and retain the best observed factor. Then, it compute again for the best observed factor until the CFI is below the set threshold.
#'
#' @param data dataframe
#' @param items vector or quos(). required. create a lavaan formula. if you want to use tidyselect syntax, wrap it in quos() (e.g. quos(contains('Q'))).
#' @param threshold the CFI threshold for the optimizer to stop. Recommend setting to 0.95 or 0.9 (the recommended cutoff based on the literature)
#' @param optimize_indicator the optimize_indicator. default to CFI. You can probably change to rmsea or tli but you must be aware of the cutoff. Haven't tested other indicator except CFI
#' @param start_columns default function or vector of length 3 or greater. default as NULL. CFA required at least 3 observed factors. By default, it will use the cfa_model_optimize_init_factors to find the best 4 initial factors.
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#' @param group optional character. the nested variable for multilevel dataset (e.g., Country).  If it is specified, it may requires a very long computation time. You can specified the start_columns to avoid lengthy time of optimizing the initial 4 factors.
#'
#' @return return a vector of the optimized observed factors
#' @export
#'
#' @examples
#'
#'
cfa_items_optimize = function(data,
                              items,
                              threshold = 0.9,
                              optimize_indicator = 'cfi',
                              start_columns = NULL,
                              group = NULL,
                              ordered = F) {

  cfa_items = data %>% dplyr::select(!!!items) %>% names(.)
  return_df = data.frame(group = NULL,cfi = NULL,rmsea = NULL,tli = NULL)

  if (is.null(start_columns)) {
    start_columns = cfa_model_optimize_init_factors(data = data, items = items, group = group, ordered = ordered)
  }

  stepwise_item = data %>% dplyr::select(!!!start_columns) %>% names(.)
  nested_stepwise_item = stepwise_item
  init_length = length(stepwise_item)
  loop_vector = c(1:(length(cfa_items) - init_length))

  for (k in loop_vector) {
    cfa_items = cfa_items[-which(cfa_items %in% stepwise_item)]
    nested_stepwise_item = stepwise_item
    cfa_in_group_vector = NULL
    for (i in cfa_items) {
      nested_stepwise_item = c(nested_stepwise_item, i)
      model = paste('DV =~', paste(nested_stepwise_item, collapse = ' + '))
      cfa_model_summary = cfa_summary(
        model = model,
        data = data,
        ordered = ordered,
        group = group,
        return_result = 'short_summary',
        quite = T
      )
      cfa_in_group_vector = c(cfa_in_group_vector,cfa_model_summary['cfi'])
      nested_stepwise_item = stepwise_item
    }
    max_iteration = which.max(cfa_in_group_vector)
    max_item = cfa_items[max_iteration]
    max_value = cfa_in_group_vector[max_iteration]

    if (max_value < threshold) {
      print_model = paste('DV =~', paste(stepwise_item, collapse = ' + '))
      print(paste('Optimization Completed: Model is ',print_model,sep = ''))
      return(stepwise_item)
    }
    stepwise_item = c(stepwise_item, max_item)
  }
}
