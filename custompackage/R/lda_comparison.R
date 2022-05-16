#' @title comparison tables from LDA
#' @description This function gives two outputs- the first compares the original groups that the samples are assigned to, and the group that have been assigned by the LDA. The second table shows how many samples were classified into each group in the original dataset and using the LDA.
#' @param df dataframe with column "group" and LDA to be run on all other variables
#' @keywords LDA tables comparison
#' @export
#' @examples

lda_comparison = function(testing_df,predictions){
  if(nargs() != 2){
    stop("Must include testing data frame and predictions object.")
  }
  class <- as.data.frame(predictions$class)
  colnames(class) <- "group"
  original_assignments <- testing_df %>% group_by(group) %>% summarise("Number" = n()) %>%
    mutate(original_assignments = Number) %>% dplyr::select(-Number)
  lda_assignments <- class %>%
    group_by(group) %>% summarise("Number" = n()) %>%
    mutate(lda_assignments = Number) %>% dplyr::select(-Number)
  left_join(original_assignments,lda_assignments, by = "group")
}
