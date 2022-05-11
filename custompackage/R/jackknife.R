#' @title jackknife approach to validate an LDA
#' @description this function performs an LDA on a dataframe with groups in column "group". It performs multiple LDAs on the dataframe by removing one observation each time. The LDA is validated by comparing the results of these LDAs against one another. A graph is outputted from this function that shows how often each sample was sorted into a single group.
#' @param df dataframe with column "group" and LDA to be run on all other variables
#' @keywords lda validation
#' @export
#' @examples

jackknife = function(df,group){
  if(nargs() != 2){
    stop("Must include both data frame and group column name")
  }
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
    geom_bar(color="black")+
    scale_fill_brewer(palette="Dark2")
}
