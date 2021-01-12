#' Model Selection: CFA Items Optimizer
#'
#' The function help you to select the best item for CFA using a data-drive approach. In short, it computed a series of CFA using a stepwise apporach, then it select and retain the best observed factor. Then, it compute again for the best observed factor until the CFI is below the set threshold.
#' @param data dataframe
#' @param items vector or quos(). required. create a lavaan formula. if you want to use tidyselect syntax, wrap it in quos() (e.g. quos(contains('Q'))).
#' @param threshold the CFI threshold for the optimizer to stop. Recommend setting to 0.95 or 0.9 (the recommended cutoff based on the literature)
#' @param optimize_indicator the optimize_indicator. default to CFI. You can probably change to rmsea or tli but you must be aware of the cutoff. Haven't tested other indicator except CFI
#' @param start_columns vector of length 3. CFA required at least 3 observed factors.By default, it is the first 3 columns that is specified in the items argument. Currently, the function don't support optimizing best 3 initial observed factors.
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#'
#' @return return a vector of the optimized observed factors
#' @export
#'
#' @examples cfa_items_optimize(data = data, items = quos(IV1:IV3'))
#'
#'
cfa_items_optimize = function(data,
                              items,
                              threshold = 0.9,
                              optimize_indicator = 'cfi',
                              start_columns = c(1:3),
                              ordered = F) {
  cfa_items = data %>% select(!!!items) %>% names(.)
  return_df = data.frame(
    group = NULL,
    cfi = NULL,
    rmsea = NULL,
    tli = NULL,
    marginal_cfi = NULL
  )

  stepwise_item = data %>% select(!!!start_columns) %>% names(.)
  nested_stepwise_item = stepwise_item
  init_length = length(stepwise_item)
  loop_vector = c(1:(length(cfa_items) - init_length))

  for (k in loop_vector) {
    cfa_items = cfa_items[-which(cfa_items %in% stepwise_item)]
    nested_stepwise_item = stepwise_item
    cfa_in_group = NULL
    for (i in cfa_items) {
      nested_stepwise_item = c(nested_stepwise_item, i)
      model = paste('DV =~', paste(nested_stepwise_item, collapse = ' + '))
      cfa_model_summary = cfa_summary(
        model = model,
        data = data,
        ordered = ordered,
        return_result = 'summary'
      )
      cfa_model_summary = as.data.frame(cfa_model_summary)
      cfa_df = data.frame(item = i, cfi =  cfa_model_summary['cfi', ])
      cfa_in_group = rbind(cfa_in_group, cfa_df)
      nested_stepwise_item = stepwise_item
    }
    max_value = cfa_in_group %>% filter(cfi == max(cfi)) %>% select(cfi) %>% as.numeric(.)
    max_item = cfa_in_group %>% filter(cfi == max(cfi)) %>% select(item) %>% as.character(.)
    if (max_value < threshold) {
      print(paste(
        'Optimization Completed: ',
        length(stepwise_item),
        ' factors are identified',
        sep = ''
      ))
      return(stepwise_item)
    }
    stepwise_item = c(stepwise_item, max_item)
  }
}
