#-------------------------------------------------------------------------------
#FUNCTION: Optimization of Decision Tree
#-------------------------------------------------------------------------------

# *********************Optimizing Decision Tree*******************************
# Since,we do not know the number of boosted tree in which our dataset performs best,
# We are going to try different parameters in DT boosting to find the best one.
#
#  Tuning Parameters 
#             LOGICAL           -winnow   #TRUE or FALSE
#             LIST              -trials   #no of boosting. Eg: c(1,15,20)
#  INPUT   :
#             Data Frame        - trainDataset
#             Data Frame        - testDataset
#             LIST              - number_of_trees
#
# ****************************************************************************

F_optimize_tree<-function(trainDataset=trainDataset,
                          testDataset=testDataset,
                          boost=boost,
                          winnow=winnow){
  if(is_empty(boost) || is_empty(winnow)){
    print("Please enter boosting values to comapre")
  }
  else{
    # Setting local variables
    tuningDT<-NULL
    counter<-1
    modelNameList<-list()
    #plotList<-list()
    for(win in c(winnow)){
      winnow<-ifelse(win=="TRUE","T-","F-")
      for (nBoost in c(boost)) {
        cName<-paste("DecisionTree:",winnow,",",nBoost)
        storeOutput<-decisionTreeModel(trainDataset,testDataset,boost=nBoost,plot=FALSE,winnow=win)
        #removing unnecessary columns
        storeOutput<-storeOutput[names(storeOutput) %in% c("y_predicted_list", "y_expected_list") == FALSE]
        if(counter==1){
          tuningDT<-data.frame(DT_Tuning=unlist(storeOutput))
        }else{
          tuningDT<-cbind(tuningDT,data.frame(model=unlist(storeOutput)))
        }
       colnames(tuningDT)[counter] <- counter
       modelNameList <- c(modelNameList,list(cName)) 
       counter<-counter+1
       #extract the measures for plotting confusion matrix
       extractData <- as.data.frame(t(storeOutput))
       TP<-as.numeric(extractData$TP)
       FN<-as.numeric(extractData$FN)
       FP<-as.numeric(extractData$FP)
       TN<-as.numeric(extractData$TN)
       # Confusion Matrix plots for all optimized models
       #F_plot_confusion_matrix(TP=TP,FN=FN,FP=FP,TN=TN,title=cName)
      }
    }
    #interchanging columns and rows
    finalView <- as.data.frame(t(tuningDT))
    #adding column name
    finalView<-finalView[names(finalView) %in% c("TP","FN","TN","FP", "Precision", "Recall","F1") == TRUE]
    finalView$model<-modelNameList
    #bringing last column to first
    finalView <- finalView %>%
      select(model, everything())
    #Printing out final Results in view
    F_print_view(finalView,title="Decision Tree Model Comparison")
  }
}
#End of F_optimize_tree

#-------------------------------------------------------------------------------
#FUNCTION: Optimization of Random Forest
#-------------------------------------------------------------------------------

# *********************Optimizing Random Forest*******************************
# Since,we do not know the number of forest in which our dataset performs best,
# We are going to try different number of random forest to find the best one.
#
# INPUT   :
#             Data Frame        - trainDataset
#             Data Frame        - testDataset
#             LIST              - number_of_trees
#
# ****************************************************************************

F_gridsearch_random_forest<-function(trainDataset=trainDataset,
                                   testDataset=testDataset,
                                   number_of_trees=number_of_trees,
                                   plot=FALSE){
    
    # Load Dataset
    tuningRF<-NULL
    counter<-1
    modelNameList<-list()
    for (ntree in c(number_of_trees)) {
      storeOutput<-NULL
      cName<-paste("RF",ntree)
      storeOutput<-randomForestModel(train=trainDataset, test=testDataset,ntree,plot=plot)
      #removing unnecessary columns
      storeOutput<-storeOutput[names(storeOutput) %in% c("y_predicted_list", "y_expected_list") == FALSE]
      if(counter==1){
        tuningRF<-data.frame(RF_Tuning=unlist(storeOutput))
      }else{
        tuningRF<-cbind(tuningRF,data.frame(RF_Tuning=unlist(storeOutput)))
      }
      colnames(tuningRF)[counter] <- counter
      modelNameList <- c(modelNameList,list(cName)) 
      counter<-counter+1
      #extract the measures for plotting confusion matrix
      extractData <- as.data.frame(t(storeOutput))
      TP<-as.numeric(extractData$TP)
      FN<-as.numeric(extractData$FN)
      FP<-as.numeric(extractData$FP)
      TN<-as.numeric(extractData$TN)
      # Confusion Matrix plots for all optimized models
      #F_plot_confusion_matrix(TP=TP,FN=FN,FP=FP,TN=TN,title=cName)
    }
    #interchanging columns and rows
    finalView<- as.data.frame(t(tuningRF))
    #adding column name
    finalView<-finalView[names(finalView) %in% c("TP","FN","TN","FP", "Precision", "Recall","F1") == TRUE]
    finalView$model<-modelNameList
    #bringing last column to first
    finalView <- finalView %>%
      select(model, everything())
    #Printing out final Results in view
    F_print_view(finalView,title="Random Forest Model Comparison")
  }
#End of F_gridsearch_random_forest  


#-------------------------------------------------------------------------------
#FUNCTION: Decision Tree Model
#-------------------------------------------------------------------------------
# ************************************************
# decisionTreeModel() :
# The function contains some code and ideas from the lab works provided by Professor Nick.
# Create C5 Decision Tree on pre-processed dataset
#
# INPUT   :
#             Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             int            - boost       - number of trees to boost
#             boolean        - plot        - TRUE = plot charts
#             boolean        - compareFinalModel    - confirming if this is the final model
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
decisionTreeModel<-function(train,test,boost=1,plot=TRUE,winnow = FALSE,CompareFinalModel=FALSE){
  set.seed(123)
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  # train data: vector with the expected output
  train_expected<-train[,positionClassOutput]
  # ************************************************
  # Create a standard Decision Tree using the C5.0 algorithm
  # Uses library C5.0
  # Outputs the tree in the format of rules
  #Title of the Tree
  treeTitle<-c("Decision Tree")
  if (boost>1)
    treeTitle<-paste(treeTitle,"boost=",boost)
  print(treeTitle)
  #Title of the Tree
  #Training DT c5.0
  tree<-C50::C5.0(x=train_inputs,
                  factor(train_expected),
                  rules=TRUE,
                  trials=boost,
                  control = C5.0Control(winnow = winnow))
  #Using the decision tree and testing out with test dataset
  #to determine best classification threshold & calculate metrics
  measures<-getTreeClassifications(myTree = tree,
                                   testDataset = test,
                                   title=treeTitle,
                                   plot=plot)
  #If plot is true, printing summary of tree
  if (plot==TRUE){
    #Printing summary of the tree
    print(summary(tree))
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]
    print(formattable::formattable(importance))
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main=treeTitle)
    # Get importance of the input fields
    
    # ************************************************
    # We can visualise the tree
    
    #Function to output the tree as rules to a file
    dftreerules<-NDT5RuleOutput(tree)
    print(formattable::formattable(dftreerules))
  }
  #If this is the final comparing model
  if(CompareFinalModel==TRUE){
    #Increasing global counter value by 1
    ModelCounter<<-ModelCounter+1
    # Creates C5.0 decision tree & plot as a tree structure
    print("Plot decision tree to file called tree.pdf")
    suppressWarnings(
    printingDecisionTreeAsPDF(train_inputs,train_expected,boost,title=treeTitle)
    )
    dev.off()

    #plotting confusion matrix
    F_plot_confusion_matrix(TP=measures[["TP"]],FN=measures[["FN"]],FP=measures[["FP"]],TN=measures[["TN"]],title=treeTitle)
  }
  return(measures)
} 
#End of decisionTreeModel

#-------------------------------------------------------------------------------
#FUNCTION: Random Forest Model
#-------------------------------------------------------------------------------
# ************************************************
# randomForestModel() :
# The function contains some code and ideas from the lab works provided by Professor Nick.
# Create Random Forest on pre-processed dataset
#
# INPUT   :
#         :   Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             boolean        - plot        - TRUE = output charts/results
#             boolean        - compareFinalModel    - confirming if this is the final model
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
randomForestModel<-function(train,test,plot=FALSE,ntree=1000,CompareFinalModel=FALSE){
  
  set.seed(123)
  
  myTitle<-(paste("Random Forest:",ntree,"trees"))
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  #training random forest
  rf<-randomForest::randomForest(train_inputs,
                                 factor(train_expected),
                                 ntree=ntree ,
                                 importance=TRUE,
                                 mtry=sqrt(ncol(train_inputs)))
  #testing the trained model
  measures<-getTreeClassifications(myTree = rf,
                                   testDataset = test,
                                   title=myTitle,
                                   plot=plot)
  #when plot= TRUe, plotting thhe outputs
  if (plot==TRUE){
    # Get importance of the input fields
    importance<-randomForest::importance(rf,scale=TRUE,type=1)
    importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
    
    colnames(importance)<-"Strength"
    barplot(t(importance),las=2, border = 0,
            cex.names =0.7,
            main=myTitle)
    print(formattable::formattable(data.frame(importance)))
  }
  #If this is the final comparing model
  if(CompareFinalModel==TRUE){
    #Increasing global counter value by 1
    ModelCounter<<-ModelCounter+1
    #plotting confusion matrix
    F_plot_confusion_matrix(TP=measures[["TP"]],FN=measures[["FN"]],FP=measures[["FP"]],TN=measures[["TN"]],title=myTitle)
  }
  return(measures)
} #endof randomForestModel()

# ************************************************
# getTreeClassifications() :
#The function contains some code and ideas from the lab works provided by Professor Nick.
# Put in test dataset and get out class predictions of the decision tree
# Calculating threshold,measures and printing plots 
#
# INPUT   :   object         - myTree        - tree
#         :   Data Frame     - testDataset   - testing dataset
#         :   string         - title        - title for the model/plots
#         :   int            - classLabel   - label for positive calss
#         :   boolean        - plot         - confirmation for plotting the models
#
# OUTPUT  :   List       - Named evaluation measures
#
# ************************************************
getTreeClassifications<-function(myTree,
                                 testDataset,
                                 title,
                                 classLabel=1,
                                 plot=TRUE){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (No Attrition) and column 2 is for class 1 (Attrition)
  
  testPredictedClassProbs<-predict(myTree,test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)
  
  # Get the probabilities for classifying the Attrition
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)
  
  if (plot==TRUE){
    #removing the predicted and expected values as we do not need them
    measuresPrint<-measures[names(measures) %in% c("y_predicted_list", "y_expected_list") == FALSE]
    NprintMeasures(results=measuresPrint,title=title)
  }
  return(measures)
  
  
} #endof getTreeClassifications()

