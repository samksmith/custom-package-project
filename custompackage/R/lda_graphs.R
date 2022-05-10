#' @title tabular results from LDA
#' @description
#' @param df data frame to run LDA on. Must have column named "group"
#' @keywords to aid in searching functionss
#' @export
#' @examples

lda_graphs = function(df){
  lda = lda(data=df, group ~ .)
  predictions = predict(lda)
  lds = as.data.frame(predictions$x) %>%
    mutate(class = predictions$class)
  main_plot = ggplot(data = lds, aes(x=LD1, y=LD2, color = class))+
    geom_point()
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

