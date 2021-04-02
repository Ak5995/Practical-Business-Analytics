# *********************Are we data mungers?*******************************
# Project: Identifying attrition and reasons behind it
#
# ****************************************************************************

# ****************************Setting up R environment**************************
#Global Environment is cleaned
rm(list=ls())
if(!is.null(dev.list())) dev.off()
# Clearing the console
cat("\014")
#Setting same starting number for random numbers 
set.seed(123)
#removing unused memory when  an object is not in use
gc()
#Clearing plots and graphs
if(!is.null(dev.list())) dev.off()
graphics.off()
# Clearing all warnings
assign("last.warning", NULL, envir = baseenv())
# Printing current working directory
print(paste("Current Directory: ",getwd()))
# ****************************Setting up R environment**************************

#CONSTANTS Begin --------------------------------------------------------------------------------------------

DATASET_FILENAME  <- "hrDataset.csv"          # Name of input dataset file
OUTPUT_FIELD      <- "Attrition"             # Field name of the output class to predict

MYLIBRARIES <- c("outliers",
                 "corrplot",
                 "formattable",
                 "stats",
                 "caret",
                 "e1071",
                 "smotefamily",
                 "rpart",
                 "rpart.plot",
                 "PerformanceAnalytics",
                 "stringr",
                 "partykit",
                 "C50",
                 "mlbench",
                 "randomForest",
                 "ROSE",
                 "DMwR",
                 "tidyverse",
                 "ggplot2",
                 "ROCR",
                 "reactable",
                 "PRROC",
                 "data.table",
                 "mltools",
                 "jmotif",
                 "yardstick",
                 "grid",
                 "gridExtra",
                 "pROC",
                 "GA",
                 "dplyr",
                 "htmltools",
                 "factoextra",
                 "ggcorrplot",
                 "repr"
)
# install.packages("ggplot")
# install.packages("tidyverse")
# install.packages("repr")
# install.packages("ggcorrplot")
# install.packages("gridExtra")
# install.packages("corr")
# install.packages('BiocManager')

#htmltools- for title to reactable table
#dplyr- for confusion matrix plotting with ggplot
#reactable- For good table view
#mlbench- for performance tuning of C50 algorithm
#contour- for plotting roc courve with colors
#ROSE- for over and under sampling of imbalanced dataset

#DISCREET_BINS     <- 5                    # Number of empty bins to determine discreet
#MAX_LITERALS      <- 55                   # Maximum number of hotcoding new fields

KFOLDS          <- 5                      # No. of K folds


# These are the supervised model constants
PDF_FILENAME      <- "tree.pdf"           # Name of PDF with graphical tree diagram
NODE_LEVEL        <- 1                    # The number is the node level of the tree to print
# BOOST             <- 22                   # Number of boosting iterations. 1=single model
ModelCounter      <-0                     #Creating global counter for no of models for final comparison
svmModelCounter   <-0                     #Creating global counter for no of svm models for SVM nodel comparison

#loading libraries using packman
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Loading Necessary R files
source("labs_functions.R")
source("svm_model_functions.R")
source("DT_RF_model_functions.R")
source("preprocessing_functions.R")
source("visualization_code.R")
source("model_comparison_functions.R")
#--------------------------------------------------------------------------------------------

#Preprocessing steps applied to original dataset
# ************************************************
# F_preProcessing() :
# 
#
# INPUT:   vector - Data frame - Original Dataset
#
# OUTPUT : List - Data frames
# ************************************************

F_preprocessing<-function(dataset){
  datasetList<-list()
  #storing the dataset
  print("Starting Pre-Processing of Dataset")
  Initial_hr_data <-dataset
  names(Initial_hr_data)[1] <- "Age"
  #1 check NA/no NA value in all column
  F_check_NA(Initial_hr_data)
  
  #2 Drop unwanted columns
  drops <- c("Over18","EmployeeCount","StandardHours","EmployeeNumber")
  dropded_ds <- F_remove_columns_dataframe(data = Initial_hr_data,
                                           drops = drops)
  
  #3 Checking data balance
  F_check_balance(data = dropded_ds, predicted_class = "Attrition")
  
  #4 Seperate into categorical and numerical data
  categorical_data <- F_seperate_numerical_categorical(data = dropded_ds)$categorical
  numerical_data <- F_seperate_numerical_categorical(data = dropded_ds)$numerical
  
  #5 One-hot encdoing
  one_hot_data <- F_one_hot_encoding_dataframe(categorical_data = categorical_data)
  
  #6 Removing the outliers
  removed_outliers_data <- F_remove_outliers_dataframe(numerical_data = numerical_data)
  #Upgraded Method-not used
  #*removed_outliers_data <- F_remove_outliers_dataframe_new(numerical_data = numerical_data)
  
  #7 Sample function to demonstrate use of Standardization
  standardized_data <- F_standardization_dataframe(data = removed_outliers_data)
  
  
  #8 Combine the processed categorical data and numerical data
  datasetList[["combined_data_svm"]] <- F_combine_dataframe(categorical_data = one_hot_data, 
                                                            numerical_data = standardized_data, 
                                                            predicted_class = OUTPUT_FIELD)
  
  datasetList[["combined_data_dt"]] <- F_combine_dataframe(categorical_data = categorical_data, 
                                                           numerical_data = removed_outliers_data, 
                                                           predicted_class = OUTPUT_FIELD)
  
  # Not Implemented
  # famd<-F_FAMD(datasetList[["combined_data_dt"]],predicted_class = OUTPUT_FIELD)
  print("Dataset PreProcessed.")
  return(datasetList)
}


main<-function(){
  #Initializing df/list
  allResults<-NULL
  y_pred_list <- list()
  y_test_list <- list()
  names_PR_curve <- list()
  #*ModelCounter is a global counter to store the results of all model dynamically in a list
  
  
  # *********************Pre-Processing Dataset*********************************
  # Load Description
  #
  # ****************************************************************************
  #Load the dataset
  loadedDataset <- read.csv(DATASET_FILENAME)
  
  # 
  #Preprocessing the loaded raw dataset
  processedDatasets<-F_preprocessing(loadedDataset)
  
  #seperating the returned list of dataset
  combined_data_dt<<-processedDatasets[["combined_data_dt"]]
  combined_data_svm<<-processedDatasets[["combined_data_svm"]]
  
  trainIndex_dt <- createDataPartition(combined_data_dt[[OUTPUT_FIELD]], p = .7,
                                       list = FALSE,
                                       times = 1)
  
  trainDataset <- combined_data_dt[trainIndex_dt,]
  testDataset  <- combined_data_dt[-trainIndex_dt,]


  #balancing new dataset : balancing of data by both method is good
  overSampled_dataset<-F_oversampling_dataframe(trainDataset,OUTPUT_FIELD,method="over")
  bothSampled_dataset<-F_oversampling_dataframe(trainDataset,OUTPUT_FIELD,method="both")


  # *********************Optimizing Decision Tree*******************************
  # Since,we do not know the number of boosted tree in which our dataset performs best,
  # We are going to try different parameters in DT boosting to find the best one.
  #
  #  Tuning Parameters
  #             LOGICAL           -winnow   #TRUE or FALSE
  #             LIST              -trials   #no of boosting. Eg: c(1,15,20)
  #  INPUT   :
  #             Data Frame        - overSampled_dataset
  #             Data Frame        - testDataset
  #             LIST              - number_of_trees
  #
  # ****************************************************************************
  #oversampled dataset and OverUnderSampled dataset were used
  #* Note:Different boosting iterations were used during model testing.
  #* boost=c(1,10,15,22,30)
  #* For faster running of system less boosting values are set
  F_optimize_tree(trainDataset=overSampled_dataset,
                  testDataset=testDataset,
                  boost=c(1,22,30),
                  winnow=c(TRUE,FALSE)
                  )
  ##Conclusion
  ##oversampled dataset was decided to be used
  ##DT-FALSE-22 was decided to be used for better overall results



  # *********************Optimizing Random Forest*******************************
  # Since,we do not know the number of forest in which our dataset performs best,
  # We are going to try different number of random forest to find the best one.
  #
  # INPUT   :
  #             Data Frame        - overSampled_dataset
  #             Data Frame        - testDataset
  #             LIST              - number_of_trees
  #
  # ****************************************************************************
  #* Note:Different boosting iterations were used during model testing.
  #* number_of_trees=c(100,400,750,920,1000,1400)
  #* For faster running of system less boosting values are set

  F_gridsearch_random_forest(trainDataset=overSampled_dataset,
                      testDataset=testDataset,
                      number_of_trees=c(100,2))
  ##RF-920 was decided to be used for RF modeling

  #
  # # *********************MODELS*******************************
  #Decision Tree with over sampled dataset

  measures<-decisionTreeModel(overSampled_dataset,testDataset,boost=1,winnow=FALSE,plot=TRUE,CompareFinalModel=TRUE)
  #storing the y-predicted and y-expected values from the model for Precison Recall chart/ ROC chart
  y_pred_list[[ModelCounter]]<-unlist(measures["y_predicted_list"])
  y_test_list[[ModelCounter]]<-unlist(measures["y_expected_list"])
  names_PR_curve[ModelCounter]<-measures["model"]
  #removing the predicted and expected values as we do not need them
  measures<-measures[names(measures) %in% c("y_predicted_list", "y_expected_list") == FALSE]
  #storing the measures from model
  allResults<-data.frame(Decision_Tree_Oversampled=unlist(measures))


  #decision Tree with over sampled dataset and boosting=22, winnow=false

  measures<-decisionTreeModel(overSampled_dataset,testDataset,boost=22,winnow=FALSE,plot=TRUE,CompareFinalModel=TRUE)
  #storing the y-predicted and y-expected values from the model for Precison Recall chart/ ROC chart
  y_pred_list[[ModelCounter]]<-unlist(measures["y_predicted_list"])
  y_test_list[[ModelCounter]]<-unlist(measures["y_expected_list"])
  names_PR_curve[ModelCounter]<-measures["model"]
  #removing the predicted and expected values as we do not need them
  measures<-measures[names(measures) %in% c("y_predicted_list", "y_expected_list") == FALSE]
  #storing the measures from model

  allResults<-cbind(allResults,data.frame(DT_Boosted=unlist(measures)))


  # #Random Forest with 920 tree with over sampled dataset

  # #**************************************************************************
  measures<-randomForestModel(train=overSampled_dataset, test=testDataset,plot=TRUE,ntree = 920,CompareFinalModel=TRUE)
  #storing the y-predicted and y-expected values from the model for Precison Recall chart/ ROC chart
  y_pred_list[[ModelCounter]]<-unlist(measures["y_predicted_list"])
  y_test_list[[ModelCounter]]<-unlist(measures["y_expected_list"])
  names_PR_curve[ModelCounter]<-measures["model"]
  #removing the predicted and expected values as we do not need them
  measures<-measures[names(measures) %in% c("y_predicted_list", "y_expected_list") == FALSE]
  #storing the measures from model
  allResults<-cbind(allResults,data.frame(RForestModel=unlist(measures)))

  
  #SVM
  # ---------------------------------------------MODEL - START -----------------------------------------------------------
  
  # Step1: Compare the differences between original, oversampled, and SMOTE-ed data
  # Step2: Apply Stratified technique to split the dataset evenly
  # Step3: Apply K-fold cross validation with stratified techniques
  # Step4: Define an evaluation standard
  # Step5: First optimize the parameters(Cost, Gamma) with grid search
  # Step6: Further optimize the parameters with Genetic Algorithms
  # Step7: Comparison of models
  
  #Definition:
  #FP = Wrongly classified the employee who won't be attrited
  #FN = failed to classify the employee who is going to leave
  
  #Put the preprocessed data into a constant
  #The dataset is set to global for GA Optimization
  PREPROCESSED_DATA <<- combined_data_svm
  
  #==================================== PART1: GRID SEARCH FOR PARAMETERS - START =======================================
  
  #GRIDSEARCH is used to find the approximate values of the optimal cost and gamma
  
  #Note: The values of cost_step_size is set to low in order to run the whole R project faster
  # During testing the following values were used to optimize
  #*Parameters for grid searching
  cost_range_min = 1
  cost_range_max = 1001
  cost_step_size = 1000
  gamma_range_min = 0.00001
  gamma_range_max = 0.00201
  gamma_step_size = 0.002

  #Call the function for grid searching
  F_svm_gridsearch_plot(preprocessed_data = PREPROCESSED_DATA,
                        sampling_method = "oversampling",
                        predicted_class = OUTPUT_FIELD,
                        cost_range_min = cost_range_min, #vector of 2, cost/gamma: real number
                        cost_range_max = cost_range_max, #vector of 2, cost/gamma: real number
                        cost_step_size = cost_step_size,
                        gamma_range_min = gamma_range_min, #vector of 2, cost/gamma: real number
                        gamma_range_max = gamma_range_max, #vector of 2, cost/gamma: real number
                        gamma_step_size = gamma_step_size) #vector of 2, cost/gamma: real number
  
  #==================================== END - GRID SEARCH FOR PARAMETERS - END =========================================
  
  #==================================== PART2: GENETIC ALGORITHM FOR PARAMETERS - START =================================
  
  #Note: The values of population_size and max_iterations are kept low for fast running of the whole R project
  # During testing the values of population_size were high as 100 and 40 for max iterations
  
  #*Lower and upper boundaries of the parameters are set
  para_value_min <- c(gamma = 0.00001, c = 300)
  para_value_max <- c(gamma = 0.0002, c = 700)

  #Run the genetic algorithm to search for optimal solution and plot the fitness values in all generations
  F_svm_ga_plot(para_value_min = para_value_min,
                para_value_max = para_value_max,
                population_size = 2, #population of at least 2 is needed
                max_iteration = 2 #iteration of at least 2 is needed
                )
  
  #==================================== END - GENETIC ALGORITHM FOR PARAMETERS - END ==================================
  
  #matching the indexing of svm dataset and dt/rf dataset
  trainIndex <- trainIndex_dt
  train <- PREPROCESSED_DATA[trainIndex,]
  test  <- PREPROCESSED_DATA[-trainIndex,]
  
  #Oversample the training data with SMOTE-technique
  oversampled_train <- F_oversampling_dataframe(training_data = train,
                                                predicted_class = OUTPUT_FIELD,
                                                method = "over")
  

  # #Train the SVM with optimized parameters (cost & gamma) chosen from above and turn them into a list containing all evaluated values
  F_SVM_Model_Selection(train=oversampled_train,test=test)
  #Final SVM model
  optimized_svm_list <- F_optimized_svm_list(training_data = oversampled_train,
                                             testing_data = test,
                                             predicted_class = OUTPUT_FIELD,
                                             optimized_cost = 516.13,
                                             optimized_gamma = 8.953556e-05,
                                             CompareFinalModel=TRUE)
  
  #storing the y-predicted and y-expected values from the model for Precision Recall Curve/ ROC Curve
  y_pred_list[[ModelCounter]]<-unlist(optimized_svm_list["y_predicted_list"])
  y_test_list[[ModelCounter]]<-unlist(optimized_svm_list["y_expected_list"])
  names_PR_curve[ModelCounter]<-"Support-Vector Machine"
  
  #optimized_svm_list[["model"]]<-names_PR_curve[ModelCounter]
  #removing the predicted and expected values as we do not need them
  measures<-optimized_svm_list[names(optimized_svm_list) %in% c("y_predicted_list", "y_expected_list") == FALSE]
  
  #storing the measures from model
  allResults<-cbind(allResults,data.frame(svm=unlist(measures)))
  
  # ##plotting precision recall and roc plots
  # #**************************************************************************
  F_plottingPR(y_pred_list,y_test_list,title=names_PR_curve)
  F_models_ROC_plot(y_pred_list,y_test_list,title=names_PR_curve)
  
  # *********************Displaying all Model Results*******************************
  # Printing out results of all model for easier comparison
  #********************************************************************************
  
  #*-----------------------------Error handling---------------------------------
  #*When checking out different section of code, model comparison is not done. 
  #*During this time,allResults is empty/null. So in order to prevent R from
  #* giving out error, allResults df is checked before priniting its content
  #*-----------------------------Error handling---------------------------------
  
  if(is_empty(allResults)){
    print("No models compared.")
  }else{
    print("Displaying Model Comparison Chart.")
    #interchanging columns and rows
    finalView <- as.data.frame(t(allResults))
    finalView<-finalView[names(finalView) %in% c("TP","FN","TN","FP", "Precision", "Recall","F1","model") == TRUE]
    finalView <- finalView %>%
      select(model, everything())
    #Printing out final Results in view
    F_print_view(finalView,title="All Model Comparison")
  }
  
  #visualization
  print("Plotting Visualization")
  visualisation(loadedDataset,OUTPUT_FIELD)
}

main()
print("Project Completed")
