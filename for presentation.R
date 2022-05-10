#Run the following code for all functions:
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
    geom_bar(color = "black") + scale_fill_brewer(palette = "Dark2")
}

lda_comparison = function(df){
  lda = lda(data = df, group ~ .)
  predictions = predict(lda)
  print(table(df$group, predictions$class))
  #add some text output explaining the table here?
  original_assignments = df %>% group_by(group) %>% summarise(n()) %>%
    mutate(original_assignments = `n()`) %>% dplyr::select(-`n()`)
  lda_assignments = as.data.frame(predictions$class) %>%
    group_by(predictions$class) %>% summarise(n()) %>%
    mutate(group = `predictions$class`) %>% dplyr::select(-`predictions$class`) %>%
    mutate(lda_assignments = `n()`) %>% dplyr::select(-`n()`)
  left_join(original_assignments, lda_assignments, by = "group")
}

lda_graphs = function(df){
  lda = lda(data=df, group ~ .)
  predictions = predict(lda)
  lds = as.data.frame(predictions$x) %>%
    mutate(class = predictions$class)
  main_plot = ggplot(data = lds, aes(x=LD1, y=LD2, color = class))+
    geom_point()+ scale_color_brewer(palette = "Dark2")
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

top5vars = function(df){
  lda= lda(data= df, group ~ .)
  lda_scores = as.data.frame(lda$scaling)
  ld1_scores = lda_scores %>%
    mutate(vars = rownames(lda_scores)) %>%
    dplyr::select(LD1, vars) %>%
    arrange(desc(abs(LD1)))
  print("The top five variables that contribute to discriminant function 1 (LD1) are:")
  print(ld1_scores$vars[1:10])
  ld2_scores = lda_scores %>%
    mutate(vars = rownames(lda_scores)) %>%
    dplyr::select(LD2, vars) %>%
    arrange(desc(abs(LD2)))
  print("The top five variables that contribute to discriminant function 2 (LD2) are:")
  print(ld2_scores$vars[1:10])
}

#call in datasets, rename group column and sort by group so jackknife graph is organized
acoustic = read_csv("acoustic_data.csv") %>%
  rename(group = Group) %>%
  arrange(group)
visual = read_csv("visual_data.csv") %>%
  rename(group = Group) %>%
  arrange(group)

#run all 4 functions on acoustic data
jackknife(acoustic)
lda_comparison(acoustic)
lda_graphs(acoustic)
top5vars(acoustic)

#run all 4 functions on acoustic data
jackknife(visual)
lda_comparison(visual)
lda_graphs(visual)
top5vars(visual)

