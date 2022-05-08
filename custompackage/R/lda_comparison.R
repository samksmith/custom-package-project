#' @title tabular results from LDA
#' @description
#' @param df data frame to run LDA on. Must have column named "group"
#' @keywords to aid in searching functions
#' @export makes the function available for others to use when your package is loaded
#' @examples 

lda_comparison = function(df){
lda = lda(data = df, group ~ .)
predictions = predict(lda)
print(table(df$group, predictions$class))
#add some text output explaining the table here?
original_assignments = df %>% group_by(group) %>% summarise(n()) %>%
  mutate(original_assignments = `n()`) %>% dplyr::select(-`n()`)
lda_assignments = as.data.frame(predictions$class) %>%
  group_by(predictions$class) %>% summarise(n()) %>%
  mutate(group = `predictions$class`) %>% dplyr::select(-`predictions$class`) %>%
  mutate(lda_assignments = `n()`) %>% dplyr::select(-`n()`)
left_join(original_assignments, lda_assignments, by = "group")
}