#' @title comparison tables from LDA
#' @description This function outputs a table that compares the original groups that the samples are assigned to, and the group that have been assigned by the LDA.
#' @param testing_df dataframe with column "group" and LDA to be run on all other variables
#' @param predictions variable in which results of lda() and predict() (from MASS package) are stored
#' @keywords LDA tables comparison
#' @export
#' @examples lda = lda(data = training, group ~ .)
#' @examplespredictions = predict(lda)
#' @examples lda_comparison(training, predictions)

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
