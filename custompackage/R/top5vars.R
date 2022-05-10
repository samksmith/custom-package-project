#' @title jackknife approach to evaluate an LDA
#' @description
#' @param one line for each parameter in our function
#' @keywords to aid in searching functions
#' @export makes the function available for others to use when your package is loaded
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
