#' Model Selection: Stepwise CFA by items
#'
#' The model will run a series of CFA (similiar to stepwise regression) by adding observed factor to the CFA 1 by 1. Currently, it only support stepwise CFA for one latent factor CFA.
#'
#' @param data dataframe
#' @param items vector or quos(). default to NULL. create a lavaan formula. if you want to use tidyselect syntax, wrap it in quos() (e.g. quos(contains('Q'))).
#' @param group optional character. default is NULL. the nested variable for multilevel dataset (e.g., Country)
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#'
#' @return
#' @export
#'
#' @examples
#'

cfa_items_stepwise = function(data,
                              items,
                              group = NULL,
                              ordered = F) {


  cfa_items = data %>% dplyr::select(!!!items) %>% names(.)
  return_df = data.frame(group = NULL, cfi = NULL, rmsea = NULL, tli = NULL, marginal_cfi = NULL)

  stepwise_item = NULL
  j = 1
  for (i in cfa_items) {
    j = j + 1
    stepwise_item = c(stepwise_item, i)
    if (length(stepwise_item) > 3) {
      model = paste('DV =~', paste(stepwise_item, collapse = ' + '))
      cfa_model_summary = cfa_summary(
        model = model,
        data = data,
        ordered = ordered,
        return_result = 'short_summary',
        group = group,
        quite = T
      )

      if (length(stepwise_item) > 4) {
        marginal_cfi = return_df[j - 4 - 1, 'cfi'] - cfa_model_summary['cfi']
      } else{
        marginal_cfi = 0
      }
      summary_df = data.frame(
        stepwise_item = i,
        cfi = cfa_model_summary['cfi'],
        rmsea = cfa_model_summary['rmsea'],
        tli = cfa_model_summary['tli'],
        marginal_cfi = marginal_cfi
      )
      return_df = rbind(return_df, summary_df)
      rownames(return_df) = NULL
    }
  }
  return(return_df)
}
