#' @title jackknife approach to evaluate an LDA
#' @description
#' @param one line for each parameter in our function
#' @keywords to aid in searching functions
#' @export makes the function available for others to use when your package is loaded
#' @examples 

jackknife = function(df){
  results = data.frame()
  for(i in 1:nrow(df)){
    new_df = df %>%
      filter(!row_number() %in% i)
    lda = lda(data = new_df, group ~ .)
    lda_predictions = predict(lda)
    sample = 1:(nrow(df)-1)
    lda_results = as.data.frame(sample) %>%
      mutate(group = lda_predictions$class)
    results = bind_rows(results, lda_results)
  }
  ggplot(data = results, aes(x = sample, fill = group))+
    geom_bar()+
    scale_fill_brewer()
}