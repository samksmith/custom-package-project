#' @title comparison tables from LDA
#' @description This function gives two outputs- the first compares the original groups that the samples are assigned to, and the group that have been assigned by the LDA. The second table shows how many samples were classified into each group in the original dataset and using the LDA.
#' @param df dataframe with column "group" and LDA to be run on all other variables
#' @keywords LDA tables comparison
#' @export
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
