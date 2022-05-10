#' @title top 5 variables loading on to discriminant functions
#' @description this function performs an LDA on a dataset with three groups and then outputs the top 5 variables loading on to discriminant functions 1 (LD1) and 2 (LD2)
#' @param dataframe with column "group" and LDA to be run on all other variables
#' @keywords loadings LDA scores
#' @export
#' @examples

top5vars = function(df){
  lda= lda(data= df, group ~ .)
  lda_scores = as.data.frame(LDA$scaling)
  ld1_scores = lda_scores %>%
    mutate(vars = rownames(lda_scores)) %>%
    dplyr::select(LD1, vars) %>%
    arrange(desc(abs(LD1)))
  print("The top five variables that contribute to discriminant function 1 (LD1) are:")
  print(ld1_scores$vars[1:5])
  ld2_scores = lda_scores %>%
    mutate(vars = rownames(lda_scores)) %>%
    dplyr::select(LD2, vars) %>%
    arrange(desc(abs(LD2)))
  print("The top five variables that contribute to discriminant function 2 (LD2) are:")
  print(ld2_scores$vars[1:5])
}
