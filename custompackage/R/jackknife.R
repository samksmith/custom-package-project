#' @title jackknife approach to validate an LDA
#' @description this function performs an LDA on a dataframe with groups in column "group". It performs multiple LDAs on the dataframe by removing one observation each time. The LDA is validated by comparing the results of these LDAs against one another. A graph is outputted from this function that shows how often each sample was sorted into a single group.
#' @param df dataframe with column "group" and LDA to be run on all other variables
#' @keywords lda validation
#' @export
#' @examples

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