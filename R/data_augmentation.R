data_augmentation = function(data, augMethod, nSyn){


  # specs for synthetic data
  thr.rel = 0.1
  under = 1.0
  over = 2.0
  C.perc = "balance"
  pert = 0.1  # noise = sd*0.1

  # select split data
  train = data[["train"]]
  train_filtered = as.data.frame(train[,!names(train) %in% c("itemID")])


  if(augMethod == "None") {
    trainBalanced = train_filtered

  } else if(augMethod == "RandOverRegress") {
    trainBalanced = RandOverRegress(pct~., train_filtered, thr.rel = thr.rel, C.perc=C.perc)

  } else if(augMethod == "GaussNoiseRegress") {
    trainBalanced = GaussNoiseRegress(pct ~ ., train_filtered,  thr.rel = thr.rel, C.perc = C.perc)

  } else if(augMethod == "SmoteRegress") {
    trainBalanced = SmoteRegress(pct ~ ., train_filtered, thr.rel = thr.rel, C.perc = C.perc)

  } else if(augMethod == "SMOGNRegress") {
    trainBalanced = SMOGNRegress(pct~., train_filtered, thr.rel=thr.rel, C.perc=C.perc)

  } else if(augMethod == "WERCSRegress") {
    trainBalanced = WERCSRegress(pct~., train_filtered, thr.rel=thr.rel, C.perc=C.perc)

  } else if(augMethod == "augMI") {
    items = names(train_filtered)[grep("v", names(train_filtered))]
    trainBalanced = augMI(dat = train_filtered, fix_vars=c(), ord_vars=c(items), syn_vars=c("pct", items)) # synthetic pct values are the same with the original

  } else if(augMethod == "augCAT") {
    items = names(train_filtered)[grep("v", names(train_filtered))]
    trainBalanced = augCAT(dat = train_filtered, target = "pct", predictors = items, nSyn = nSyn)

  } else if(augMethod == "augNN") {
    items = names(train_filtered)[grep("v", names(train_filtered))]
    trainBalanced = augNN(dat = train_filtered, target = "pct", predictors = items, nSyn = nSyn)

  }


    # ###########################
    # #U-shape Relevance Function
    # ###########################
    # # augmentation specs for UBL functions
    # summaryY = summary(train_filtered$pct)
    # control_matrix <- matrix(
    #   c(summaryY["1st Qu."], 1,
    #     summaryY["Median"],  0,
    #     summaryY["3rd Qu."], 1),
    #   ncol = 2, byrow = TRUE)
    #
    # # specs for relevance function (phi)
    # rel <- UBL::phi.control(train_filtered$pct,
    #                         method = "range",  # linear interpolation between two points
    #                         control.pts = control_matrix # provide key points
    #                         # extr.type = "low"
    # )
    # # 1. Create a sequence of target values spanning your data's range
    # # We will calculate the relevance for each of these points.
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
