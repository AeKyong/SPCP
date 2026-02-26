data_augmentation = function(data, augMethod, nMultiple, nTarget){
# browser()

  # select split data
  train = data[["train"]]
  train_filtered = as.data.frame(train[,!names(train) %in% c("itemID")])

  # specs for synthetic data
  thr.rel = 0.9
  C.perc = list(nMultiple, 1.0) # x3 and x1
  pert = 0.1  # noise = sd*0.1

  # augmentation specs for UBL functions
  summaryY = summary(train_filtered$pct)
  thresholdY = summaryY["1st Qu."]

  control_matrix = matrix(c(
    summaryY["Min."],     1,
    thresholdY,         1,
    thresholdY + 0.001, 0,
    summaryY["Max."],     0
  ), ncol = 2, byrow = TRUE)

  # control_matrix <- matrix(
  #   c(summaryY["1st Qu."], 1,
  #     summaryY["Median"],  0,
  #     summaryY["3rd Qu."], 1),
  #   ncol = 2, byrow = TRUE)

  # specs for relevance function (phi)
  rel = UBL::phi.control(train_filtered$pct,
                          method = "range",  # linear interpolation between two points
                          control.pts = control_matrix # provide key points
                          # extr.type = "low"
                         )

  if(augMethod == "None") {
    trainBalanced = train_filtered

  } else if(augMethod == "RandOverRegress") {
    trainBalanced = RandOverRegress(pct~., train_filtered, thr.rel = thr.rel, C.perc=list(nMultiple), rel = rel)

  } else if(augMethod == "GaussNoiseRegress") {
    trainBalanced = GaussNoiseRegress(pct ~ ., train_filtered,  thr.rel = thr.rel, C.perc = C.perc, rel = rel)

  } else if(augMethod == "SmoteRegress") {
    trainBalanced = SmoteRegress(pct ~ ., train_filtered, thr.rel = thr.rel, C.perc = C.perc, rel = rel)

  } else if(augMethod == "SMOGNRegress") {
    trainBalanced = SMOGNRegress(pct~., train_filtered, thr.rel=thr.rel, C.perc=C.perc, rel = rel)

  } else if(augMethod == "WERCSRegress") {
    trainBalanced = WERCSRegress(pct~., train_filtered, thr.rel=thr.rel, C.perc=C.perc, rel = rel)

  } else if(augMethod == "augCAT") {
    items = names(train_filtered)[grep("v", names(train_filtered))]
    trainBalanced = augCAT(dat = train_filtered, target = "pct", predictors = items, nSyn =  (nMultiple*nTarget), threshold = thresholdY)
  }


    # ###########################
    # #U-shape Relevance Function
    # ###########################
    # # augmentation specs for UBL functions
    # summaryY = summary(train_filtered$pct)
    # # control_matrix <- matrix(
    # #   c(summaryY["1st Qu."], 1,
    # #     summaryY["Median"],  0,
    # #     summaryY["3rd Qu."], 0.5),
    # #   ncol = 2, byrow = TRUE)
    #
    # control_matrix <- matrix(
    #   c(summaryY["Min."],      1,   # 가장 낮은 값에 중요도 1
    #     summaryY["1st Qu."],   1,   # 1사분위수까지 중요도 1 유지
    #     summaryY["Median"],    0),  # 중앙값부터는 중요도 0 (생성 대상 제외)
    #   ncol = 2, byrow = TRUE)
    #
    #
    # # specs for relevance function (phi)
    # rel <- UBL::phi.control(train_filtered$pct,
    #                         method = "range",  # linear interpolation between two points
    #                         control.pts = control_matrix # provide key points
    #                         # extr.type = "low"
    # )
    # # 1. Create a sequence of target values spanning your data's range
    # We will calculate the relevance for each of these points.
    # x_values <- seq(from = summaryY["Min."], to = summaryY["Max."], by = 0.1)
    #
    # # 2. Calculate the relevance score for each x_value using your 'rel' object
    # # The phi() function does this calculation.
    # y_values_phi <- UBL::phi(x_values, control.parms = rel)
    #
    # # 3. Create a data frame for plotting
    # plot_data <- data.frame(
    #   TargetValue = x_values,
    #   RelevanceScore = y_values_phi
    # )
    #
    # # 4. Create the plot
    # relevancePlot =
    #   ggplot(plot_data, aes(x = TargetValue, y = RelevanceScore)) +
    #   geom_line(color = "blue", linewidth = 1.2) +
    #   # Add the horizontal threshold line at y = 0.8
    #   geom_hline(yintercept = 0.8, linetype = "dotdash", color = "black", linewidth = 1) +
    #   # Add a text label for the threshold line
    #   annotate("text", x = summaryY["Max."], y = 0.8, label = "Threshold = 0.8", hjust = 1.1, vjust = -0.5, fontface = "bold") +
    #
    #   labs(
    #     title = "U-Shaped Relevance Function",
    #     x = "Target Variable ('pct')",
    #     y = "Relevance Score (phi)"
    #   ) +
    #   theme_minimal() +
    #   theme_bw() +
    #   # Add vertical lines to show where your control points are
    #   geom_vline(xintercept = c(summaryY["1st Qu."], summaryY["Median"], summaryY["3rd Qu."]), linetype = "dashed", color = "red") +
    #   # Add text labels for the control points
    #   annotate("text", x = summaryY["1st Qu."], y = 0.5, label = "1st Quartile\n(Relevance = 1)", angle = 90, vjust = -0.5) +
    #   annotate("text", x = summaryY["Median"], y = 0.5, label = "Median\n(Relevance = 0)", angle = 90, vjust = -0.5) +
    #   annotate("text", x = summaryY["3rd Qu."], y = 0.5, label = "3rd Quartile\n(Relevance = 1)", angle = 90, vjust = -0.5)
    #
    # ggsave(plot = relevancePlot, paste0("Plot_", target_variable, "_", split, ".png"), width = 16, height = 10, dpi = 300)


   return(trainBalanced)

}
