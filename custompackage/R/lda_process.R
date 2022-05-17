#' @title pre processing testing and training datasets
#' @description this function performs an LDA on a dataframe with groups in column "group". It performs multiple LDAs on the dataframe by removing one observation each time. The LDA is validated by comparing the results of these LDAs against one another. A graph is outputted from this function that shows how often each sample was sorted into a single group.
#' @param df dataframe with column "group" and LDA to be run on all other variables
#' @param partition divides your data up to the percent specified as training dataset, and 1- percent specified as testing dataset. For a 80-20 splitfor example, enter 0.8
#' @keywords preProcess testing training
#' @export
#' @examples lda = lda(data = training, group ~ .)
#' @examples predictions = predict(lda)
#' @examples lda_comparison(data, predictions)

lda_process = function(df,partition = 0.8){
  set.seed(round(partition*nrow(df)))
  training.samples <- df$group %>% createDataPartition(p = partition, list = FALSE)
  # training dataset
  train.df <- df[training.samples,]
  # testing dataset
  test.df <- df[-training.samples,]
  # normalize the data
  preproc.param <- train.df %>% preProcess(method = c("center","scale"))
  # transform using estimated parameters
  train.transf <- preproc.param %>% predict(train.df)
  test.transf <- preproc.param %>% predict(test.df)
  transformed <- list("training" = train.transf,"testing" = test.transf)
  return(transformed)
}
