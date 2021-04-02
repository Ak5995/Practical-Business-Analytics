
# ************************************************
# NdetermineThreshold() :
#
# Calculating Confusion Matrix and determine threshold on min Euclidean Distance
# Plotting ROc and threshold comparison of youden index and euclidean distance
#
# INPUT   :   vector double  - test_predicted   - probability of being class 1
#         :   vector double  - test_expected    - dataset to evaluate
#         :   boolean        - plot             - TRUE=output charts
#         :   string         - title            - chart title
#
# OUTPUT  :   List       - Named evaluation measures from confusion matrix
#                        - Threshold at min Euclidean distance
#                        - AUC - area under the ROC curve
#                        - Predicted class probability
#
# ************************************************
NdetermineThreshold<-function(test_predicted,
                              test_expected,
                              plot=TRUE,
                              title=""){
  toPlot<-data.frame()
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-NEvaluateClassifier(test_predicted=test_predicted,
                                 test_expected=test_expected,
                                 threshold=threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$Recall))
  }
  
  #Finding and Plotting Youden Index and Euclidean Distance
  #Youden index
  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1
  # Finding Max youdan distance
  maxYoudan<-toPlot$x[which.max(toPlot$youdan)]
  
  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2) to perfect classifier
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2)) 
  # Finding Min Euclidean distance from the best classifier
  minEuclidean<-toPlot$x[which.min(toPlot$distance)]
  min_distance <- min(toPlot$distance)
  
  # ************************************************
  # Plot threshold graph
  if (plot==TRUE){
    # Sensitivity (TPR)
    plot(toPlot$x,toPlot$tpr,
         xlim=c(0, 1), ylim=c(0, 100),
         type="l",lwd=3, col="blue",
         xlab="Threshold",
         ylab="%Rate",
         main=paste("Threshold Perfomance Classifier Model",title))
    
    # Plot the specificity (1-FPR)
    lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)
    
    # The point where specificity and sensitivity are the same
    crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
    
    if (!is.na(crosspoint)){
      if ((crosspoint<1) & (crosspoint>0))
        abline(v=crosspoint,col="red",lty=3,lwd=2)
    }
    
    # Plot the Euclidean distance to "perfect" classifier (smallest the best)
    lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)
    
    # Plot the min distance, as might be more (311019NRT check it is within range)
    if ((minEuclidean<1) & (minEuclidean>0))
      abline(v=minEuclidean,col="green",lty=3,lwd=2)
    
    # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
    lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
    
    if ((maxYoudan<1) & (maxYoudan>0))
      abline(v=maxYoudan,col="purple",lty=3,lwd=2)
    
    legend("bottom",c("TPR","1-FPR","Distance","Youdan"),col=c("blue","red","green","purple"),lty=1:2,lwd=2)
    text(x=0,y=50, adj = c(-0.2,2),cex=1,col="black",paste("THRESHOLDS:\nEuclidean=",minEuclidean,"\nYoudan=",maxYoudan))
    
    # ************************************************
    # 121020NRT ROC graph
    
    sensitivityROC<-toPlot$tpr[which.min(toPlot$distance)]
    specificityROC<-100-toPlot$fpr[which.min(toPlot$distance)]
    
    auc<-auroc(score=test_predicted,bool=test_expected) # Estimate the AUC
    
    # Set origin point for plotting
    toPlot<-rbind(toPlot,data.frame(x=0,fpr=0,tpr=0, youdan=0,distance=0))
    
    plot(100-toPlot$fpr,toPlot$tpr,
         type="l",
         lwd=3,
         col="black",
         main=paste("ROC:",title),
         xlab="Specificity (1-FPR) %",
         ylab="Sensitivity (TPR) %",
         xlim=c(100,0),
         ylim=c(0,100)
    )
    axis(1, seq(0.0,100,10))
    axis(2, seq(0.0,100,10))
    
    #Add crosshairs to the graph
    abline(h=sensitivityROC,col="red",lty=3,lwd=2)
    abline(v=specificityROC,col="red",lty=3,lwd=2)
    
    annotate<-paste("Threshold: ",round(minEuclidean,digits=4L),
                    "\nTPR: ",round(sensitivityROC,digits=2L),
                    "%\n1-FPR: ",round(specificityROC,digits=2L),
                    "%\n AUC: ",round(auc,digits=2L),sep="")
    
    text(x=specificityROC, y=sensitivityROC, adj = c(-0.2,1.2),cex=1, col="red",annotate)
    
  } # endof if plotting
  
  # Select the threshold - I have chosen distance
  
  myThreshold<-minEuclidean      # Min Distance should be the same as analysis["threshold"]
  #Use the "best" distance threshold to evaluate classifier
  results<-NEvaluateClassifier(test_predicted=test_predicted,
                               test_expected=test_expected,
                               threshold=myThreshold)
  
  
  
  results$distance<-min_distance #0~100
  
  results$threshold<-myThreshold #0~1
  results$AUC<-auroc(score=test_predicted,bool=test_expected) # Estimate the AUC
  
  #returning predicted probabilities and testing data for ROC plotting
  
  results$y_predicted_list<-test_predicted
  results$y_expected_list<-test_expected
  results$model<-title
  return(results)
} #endof myPerformancePlot()





# ************************************************
# NEvaluateClassifier() :
#
# Use dataset to generate predictions from model
# Evaluate as classifier using threshold value
#
# INPUT   :   vector double     - test_predicted- predicted probability of being class 1
#             Data Frame        - test_expected - Testing dataset
#             double            - threshold     -cutoff (probability) for classification
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# ************************************************
NEvaluateClassifier<-function(test_predicted,test_expected,threshold) {
  
  predictedClass<-ifelse(test_predicted<threshold,0,1)
  testPredictedClass1<-predictedClass
  results<-NcalcConfusion(expectedClass=test_expected,
                          predictedClass=predictedClass)
  return(results)
} #endof NEvaluateClassifier()


# ************************************************
# NcalcConfusion() :
#
# Calculate a confusion matrix for 2-class classifier
# INPUT: vector - expectedClass  - {0,1}, Expected values for each row
#        vector - predictedClass - {0,1}, Predicted values for each row
#
# OUTPUT: A list with all measures of model)
# An Attrition is indicated when $Status=1 and no attrition when $Status=0
#                             ACTUAL
#                 ----------------------------
# PREDICTED          Attrition=1 | No Attrition=0
#                 ----------------------------
#     Attrition=1         TP     |    FP
#                 ============================
#     No Attrition=0      FN     |    TN
#
#
# ************************************************
NcalcConfusion<-function(expectedClass,predictedClass){
  
  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))
  
  # This "converts" the above into our preferred format
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  return(NcalcMeasures(TP,FN,FP,TN))
  
} #endof NcalcConfusion()
# ************************************************
# NcalcMeasures() :
#
# Evaluation measures for a confusion matrix
#
# INPUT: numeric  - TP, FN, FP, TN
#
# OUTPUT: A list with the following entries:
#        TP        - double - True Positive records
#        FP        - double - False Positive records
#        TN        - double - True Negative records
#        FN        - double - False Negative records
#        accuracy  - double - accuracy measure
#        Precision - double - precision for "good" (values are 1) measure
#        pbad      - double - precision for "bad" (values are 1) measure
#        FPR       - double - FPR measure
#        Recall    - double - TPR measure
#        Specificity - double - TNR measure
#        MCC       - double - Matthew's Correlation Coeficient
#         F1       - double - F-Score
#
# 080819NRT added TNR measure
# ************************************************
NcalcMeasures<-function(TP,FN,FP,TN){
  retList<-list(    "TP"=TP,
                    "FN"=FN,
                    "TN"=TN,
                    "FP"=FP,
                    "pbad"=   round(100.0*(TN/(FN+TN)),digits=4L),
                    "FPR"=     round(100.0*(FP/(FP+TN)),digits=4L),
                    "FNR"=     round(100.0*(FN/(FN+TP)),digits=4L),
                    "accuracy"=round(100.0*((TP+TN)/(TP+FP+FN+TN)),digits=4L),
                    "Specificity"=    round(100.0*(TN/(FP+TN)),digits=4L),
                    "Precision"=  round(100.0*(TP/(TP+FP)),digits=4L),
                    "Recall"=   round(100.0*(TP/(TP+FN)),digits=4L),
                    "F1"=round((2*TP)/((2*TP)+FP+FN),digits=4L),
                    "MCC"=    round(((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)),digits=4L)
  )
  return(retList)
}

# ************************************************
# Nauroc() :
#
# Calculate the Area Under Curve (AUC) for ROC
#
# INPUT   :   vector double     - score            - probability of being class 1
#             vector double     - bool             - Expected class of 0 or 1
#
# OUTPUT  :   double   - AUC
#
# ************************************************
auroc <- function(score, bool) {
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}

# ************************************************
# NprintMeasures()
#
# Output measures to the Viewer
#
# INPUT:    list -   results - results from NcalcConfusion()
#           string - title   - title of the table
#
# OUTPUT :  NONE
# ************************************************
NprintMeasures<-function(results,title){
 
  #This outputs our results into the "Viewer" in RStudio
  tidyTable<-data.frame(t(t(results)))
  #Removing unnecessary columns
  tidyTable<-tidyTable[names(tidyTable) %in% c("y_predicted_list", "y_expected_list") == FALSE]
  names(tidyTable)[1]<-title
  
  t<-formattable::formattable(tidyTable,list(
    TP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TP)),
    FN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FN)),
    TN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TN)),
    FP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FP))))
  print(t)
}
# ************************************************

# ************************************************
# DECISION TREE CONVERT DT RULES TO ASCII FORMATTED RULES
#
# <anticedent 1> AND <anticedent 2> ...
# Each anticedent is: [field][comparision][value]
#
# INPUT: Object - tree - Trained tree
#
# OUTPUT: data frame of rules, class and anticedents
# ************************************************
NDT5RuleOutput<-function(tree){
 
  x<-summary(tree)[1]
  x<-substr(x,regexpr("Rules:",x)[1]+8,nchar(x))
  x<-substr(x,1,regexpr("Evaluation on training data",x)[1]-1)
  x<-gsub("[\n\t]", "*", x)
  df_of_rules<-data.frame(matrix(ncol=3,nrow=tree$size),stringsAsFactors = FALSE)
  df_of_rules<-setNames(df_of_rules,c("Rule","Class","Anti"))
  
  numberofrules<-tree$size
  # For boosted DT
  if (length(numberofrules)>1){
    numberofrules<-numberofrules[1]
  }
  
  totalAnticedents<-0
  for (ruleNumber in 1:numberofrules){
    start<-regexpr("\\*\\*",x)[1]+2
    end<-regexpr("->",x)[1]-3
    onerule<-substr(x,start,end) #Single rule, anticedents seperated by '**'
    onerule<-gsub("\\*\\*"," AND ",onerule) #Rule now has "AND" between anticedents
    #onerule<-convertNormalisedDTRuleToRealWorld(onerule)
    NumAnticedents<-str_count(onerule,"AND")+1
    totalAnticedents=totalAnticedents+NumAnticedents
    classpos<-regexpr("class ",x)+6
    classID<-as.numeric(substr(x,classpos,classpos))  #This has the class of the rule, i.e. {0,1}
    df_of_rules$Rule[ruleNumber]<-onerule
    df_of_rules$Class[ruleNumber]<-ifelse(classID==0,"NoAttrition","Attrition") # Convert class to label
    df_of_rules$Anti[ruleNumber]<-NumAnticedents
    x<-substr(x,classpos,nchar(x))
    st<-regexpr("\\*\\*",x)[1]+2 #move past the rule ID
    x<-substr(x,st,nchar(x))
  }
  return(df_of_rules)
}