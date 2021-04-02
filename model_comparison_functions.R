
# ************************************************
# F_plottingPR() :
#  Plotting Precision Recall Curve and RoC Curve for Final models
#  INPUT    :
#             LIST        - predicted:  list of  predicted values of all models
#             LIST        - expected:   list of  expected values of all models
#             LIST        - title:      list of name of all models
#
# ****************************************************************************
F_plottingPR<-function(predicted,expected,title){
  # fill.area = TRUE,
  PRCurve<-NULL
  ROCCurve<-NULL
  lengthOfList<-length(predicted)
  for (i in 1:lengthOfList ) {
    PRCurve[[i]]<-pr.curve(scores.class0 = predicted[[i]], weights.class0 = expected[[i]],
                           curve = TRUE, max.compute = T, min.compute = T, rand.compute = T)
    plot(PRCurve[[i]],main=title[[i]])
    
    ROCCurve[[i]]<-roc.curve(scores.class0 = predicted[[i]], weights.class0 = expected[[i]], curve = TRUE,
                             max.compute = T, min.compute = T, rand.compute = T)
    plot(ROCCurve[[i]],main=title[[i]])
  }
}

# ************************************************
# printingDecisionTreeAsPDF() :
#
# To write Tree Diagram into pdf
#
# INPUT1:  data frame - train_inputs: 
# INPUT2:  data frame - train_expected: 
#
# OUTPUT : PDF - Decision Tree plot
# ************************************************
printingDecisionTreeAsPDF<-function(train_inputs,train_expected,boost,title=title){
  title<-c(PDF_FILENAME,ModelCounter)
  #setting the values as global
  
  Global_train_inputs<<-train_inputs
  Global_train_expected<<-train_expected
  
  # :: is used to specify a function within the named package to avoid confusion
  tree<-C50::C5.0(x=Global_train_inputs,
                  factor(Global_train_expected),
                  trials=boost)
  
  # ::: is used to directly access a member of a package that is internal
  graphtree<-C50:::as.party.C5.0(tree)
  
  # The plot is large - so print to a big PDF file
  pdf(title, width=100, height=50, paper="special", onefile=F)
  
  # The number is the node level of the tree to print
  plot(graphtree[1])
  
  #This closes the PDF file
  dev.off()
}

# ************************************************
# F_plot_confusion_matrix() :
#
# To plot confusion matrix in ggplot
#
# INPUTs:  NUMERIC VALUES - TP,FN,FP,TN 
#           STRING        - Title of the confusion matrix
#
# OUTPUT : ggplot of confusion matrix
# ************************************************
F_plot_confusion_matrix<-function(TP,FN,FP,TN,title){
  #setting the indicators
  indicators <- c("1", "0")
  truth <- factor(rep(indicators, times = c(TP+FN, TN+FP)),   
                  levels = rev(indicators))
  pred <- factor(
    c(
      rep(indicators, times = c(TP, FN)),    #TP FN
      rep(indicators, times = c(FP, TN))),  #FP TN
    levels = rev(indicators))
  
  confusionMatrix(pred, truth)
  table <- data.frame(confusionMatrix(pred, truth)$table)
  
  plotTable <- table %>%
    mutate(goodbad = ifelse(table$Prediction == table$Reference, "CorrectPrediction", "IncorrectPrediction")) %>%
    group_by(Reference) 
  
  # fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
  plotOUtput<-ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad)) +
    geom_tile() +
    geom_text(aes(label = Freq), vjust = 0.5, fontface  = "bold", alpha = 1) +
    scale_fill_manual(values = c(CorrectPrediction = "#58D68D", IncorrectPrediction = "#EC7063")) +
    theme_bw() +ggtitle(title)+
    xlim(rev(levels(table$Reference)))
  
  #Adding theme to the text of the plot
  plotOUtput<-plotOUtput + theme(
    plot.title = element_text(color="#2D5445", size=18, face="bold.italic"),
    axis.title.x = element_text(color="#701026", size=15, face="bold"),
    axis.title.y = element_text(color="#993333", size=15, face="bold")
  )
  print(plotOUtput)
  return(plotOUtput)
}
# ************************************************
# F_print_view() :
#
# To plot confusion matrix in ggplot
#
# INPUTs:  NUMERIC VALUES - TP,FN,FP,TN 
#           STRING        - Title of the confusion matrix
#
# OUTPUT : ggplot of confusion matrix
# ************************************************
F_print_view<-function(results,title){
  #using reactables for table view
  print("Printing View")
  options(reactable.theme = reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    cellPadding = "2px 2px",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
    searchInputStyle = list(width = "100%")
  ))
  store<-reactable(results,pagination=FALSE,highlight=TRUE,bordered=TRUE)
  #adding with title
  finalResults <- htmlwidgets::prependContent(store, 
                                              h2(class = "title", title))
  print(finalResults)
}

