#' @title graphical results from LDA
#' @description This function performs an LDA on a dataset and outputs a scatterplot of LD1 vs LD2, and loading histograms (using ggplot, similar to outputs of MASS::ldahist) for each discriminant function
#' @param df datafrane with column "group" and LDA to be run on all other variables
#' @param lda_object the object in which lda() (from MASS package) results are stored
#' @keywords LDA plot histogram
#' @export
#' @examples lda = lda(data = training, group ~ .)
#' @examples lda_graphs(training, lda)

lda_graphs = function(df,lda_object){
  if(nargs() != 2){
    stop("Must include both data frame and lda object")
  }
  predictions = predict(lda_object)
  lds = as.data.frame(predictions$x) %>%
    mutate(class = predictions$class)
  main_plot = ggplot(data = lds, aes(x=LD1, y=LD2, color = class))+
    geom_point() + scale_color_brewer(palette = "Dark2")
  hist_df = as.data.frame(predictions$x) %>%
    mutate(group = df$group)
  ld1 = ggplot(data=hist_df, aes(x = LD1))+
    geom_histogram(binwidth = 0.5, fill = "cyan", color = "black")+
    facet_wrap(~ group, nrow = 3 ,strip.position = "bottom")+
    theme_bw()
  ld2 = ggplot(data=hist_df, aes(x = LD2))+
    geom_histogram(binwidth = 0.5, fill = "cyan", color = "black")+
    facet_wrap(~ group, nrow = 3 ,strip.position = "bottom")+
    theme_bw()
  main_plot / (ld1 + ld2)
}

