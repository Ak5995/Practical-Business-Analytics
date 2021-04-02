# PART 1
# ---------------------------- Stratified K-fold, Oversampling, SMOTE : START ------------------------------

#ALL the functions are written in this forma: F_XXX_YYY()
#XXX: the function of the function
#YYY: what DATA TYPE the function is going to return


# ************************************************
# F_oversampling_dataframe() :
#
# 1) To perform oversampling using ovun.sample() from ROSE in CRAN
# 2) It will return a data frame
#
# INPUT1:  Data frame - training_data: Training data from train-test split of preprocessed data
# INPUT2:  String     - predicted_class: The name of predicted class
#
# OUTPUT : Data frame: Data frame over and under sampled
# ************************************************

F_oversampling_dataframe <- function(training_data, 
                                     predicted_class, 
                                     method = "both") #both / over / under are the options
{
  
  func <- as.formula(paste(predicted_class, "~ .", sep = " "))
  current_frame <<- sys.nframe()
  oversampled_train <- ovun.sample(get("func", sys.frame(current_frame)), data=get("training_data", sys.frame(current_frame)), p = 0.5, seed = 1, method = method)$data
  #print(sys.nframe())
  oversampled_train[[predicted_class]] <- factor(oversampled_train[[predicted_class]], levels = 0:1)
  
  oversampled_train_randomised <- oversampled_train[sample(1:nrow(oversampled_train)),]
  
  return(oversampled_train_randomised)
}

# ************************************************
# F_smote_dataframe() :
#
# To perform oversampling by SMOTE-technique using SMOTE() from smotefamily in CRAN
# It will return a data frame
#
# INPUT1:  Data frame - training_data: Training data from train-test split of preprocessed data
# INPUT2:  String     - predicted_class: The name of predicted class
# INPUT3:  Integer    - K: k number using in k-nearest neighbors for SMOTE-technique
#
# OUTPUT : Data frame: Data frame oversampled by SMOTE-technique
# ************************************************
F_smote_dataframe <- function(training_data, 
                              predicted_class, 
                              K = 5) {
  
  positionClassOutput<-which(names(training_data)==predicted_class)
  X_train <- training_data[-positionClassOutput]
  y_train <- training_data[,positionClassOutput]

  #Calling the SMOTE method from package "smotefamily"
  smote_train <- smotefamily::SMOTE(X_train, y_train, K = K)$data
  smote_train_randomised <- smote_train[sample(1:nrow(smote_train)),]
  colnames(smote_train_randomised)[which(colnames(smote_train_randomised) == "class")] <- predicted_class
  
  return(smote_train_randomised)
}

# ---------------------------- Stratified K-fold, Oversampling, SMOTE : END ------------------------------




# PART 2
# ---------------------------- Model Training and Optimization : START ------------------------------

# ************************************************
# F_svm_model() :
#
# 1) To train a SVM model using svm() from e1071 in CRAN
# 2) It will return a trained svm model object
#
# INPUT1:  data frame - training_data: Training data from train-test split of preprocessed data
# INPUT2:  String     - predicted_class: The name of predicted class
# INPUT3:  String     - kernel: Hyperparameter of SVM
# INPUT4:  Double     - degree: Hyperparameter of SVM
# INPUT5:  Double     - cost: Hyperparameter of SVM
# INPUT6:  Double     - gamma: Hyperparameter of SVM
#
# OUTPUT : Model object: Trained SVM model with parameters given
# ************************************************

F_svm_model <- function(training_data, 
                        predicted_class, 
                        kernel = "radial", 
                        degree = 1, 
                        cost = 1, 
                        gamma = 0.01) 
{
  
  position<-which(names(training_data)==predicted_class)
  x = training_data[-position]
  y = training_data[, position]
  model_dataset <- data.frame(x, y = as.factor(y))
  if (kernel == "polynomial") {
    func <- as.formula(paste("y", "~ .", sep = " "))
    svmfit = svm(func, 
                 data = model_dataset, 
                 kernel = kernel, 
                 degree = degree, 
                 cost = cost, 
                 gamma = gamma, 
                 scale = FALSE, 
                 probability = TRUE)
  } else {
    func <- as.formula(paste("y", "~ .", sep = " "))
    svmfit = svm(func, 
                 data = model_dataset, 
                 kernel = kernel, 
                 cost = cost, 
                 gamma = gamma, 
                 scale = FALSE, 
                 probability = TRUE)
  }
  
  return(svmfit)
}

# ************************************************
# F_svm_ga_fitness_numeric() :
#
# 1) To set up a fitness function for genetic algorithm
# 2) The evaluation is done using F_svm_evaluation_list()
#
# INPUT1:  Vector - tuning_parameter: A vector of cost and gamma
#
# OUTPUT : Double: negative value of Euclidean distance
# ************************************************

F_svm_ga_fitness_numeric <- function(tuning_parameter)
{
  
  # fetch SVM parameters
  gamma <- tuning_parameter[1]
  cost <- tuning_parameter[2]
  
  # Evaluation is done using 5-fold cross validation with smote-technique
  evaluation_list <- F_svm_evaluation_list(preprocessed_data = PREPROCESSED_DATA,
                                           predicted_class = OUTPUT_FIELD,
                                           svm_cost = cost,
                                           svm_kernel = "radial",
                                           svm_gamma = gamma)
  
  precision <- 100*evaluation_list$TP/(evaluation_list$TP+evaluation_list$FP)
  f1 <- 2*precision*evaluation_list$TPR/((precision+evaluation_list$TPR)*100)
  
  ## return negative Euclidean distance
  return (f1)
}

# ************************************************
# F_svm_ga_plot() :
#
# 1) To set up a fitness function for genetic algorithm
# 2) The evaluation is done using F_svm_evaluation_list()
#
# INPUT1:  Vector - para_value_min: vector of minimum cost and minimum gamma
# INPUT2:  Vector - para_value_max: vector of maximum cost and maximum gamma
#
# OUTPUT : /
# ************************************************



# ---------------------------- Optimization : END ------------------------------

F_svm_ga_plot <- function(para_value_min,
                          para_value_max,
                          population_size,
                          max_iteration,
                          PREPROCESSED_DATA) 
{
  cat("SVM Genetic algorithm started.\n")
  
  #Run the genetic algorithm to search for optimal solution
  GA <- ga( type = "real-valued",
            fitness = F_svm_ga_fitness_numeric,
            lower = para_value_min,
            upper = para_value_max,
            popSize = population_size,
            maxiter = max_iteration
            
  )
  plot(GA)
  print(summary(GA))
  cat("SVM Genetic algorithm ended.\n\n")
  
}

# PART 3
# ---------------------------- Model Evaluation : START ------------------------------

# ************************************************
# F_evaluation_matrix_list() :
#
# To obtain Area Under the Curve (AUC) from the model and testing data provided
#
# INPUT1:  Data frame - training_data: Training data from train-test split of preprocessed data
# INPUT2:  Data frame - testinging_data: Testing data from train-test split of preprocessed data
# INPUT3:  String     - predicted_class: The name of predicted class
#
# OUTPUT : List: Containing TPR, FPR and the confusion matrix
# ************************************************

F_evaluation_matrix_list <- function(trained_model, 
                                     testinging_data, 
                                     predicted_class) 
{
  
  y_pred <- predict(trained_model, newdata = testinging_data)
  # y_pred_prob <- attr(predict(trained_model, type="prob", newdata = testinging_data, probability = TRUE), "probabilities")[, 1]
  # 
  # gg <<- y_pred_prob
  # auc <- Nauroc(score=(as.numeric(y_pred)-1),bool=(as.numeric(testinging_data[[predicted_class]])-1))
  
  confusion<-table(factor(y_pred,levels=0:1),factor(testinging_data[[predicted_class]],levels=0:1))
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  
  conf_list <- list("TP"      = TP,
                    "FP"      = FP,
                    "FN"      = FN,
                    "TN"      = TN,
                    "FPR"     = 100.0*(FP/(FP+TN)),
                    "TPR"     = 100.0*(TP/(TP+FN))
                    
  )
  
  return(conf_list)
}

# ************************************************
# F_svm_evaluation_list() :
#
# 1) Stratified K-fold cross validation (KFCV) and sampling technique are performed 
# 2) It will return an evaluation list containing TPR, FPR and confusion matrix
#
# INPUT1:  Integer    - kfold: Choose numbers of fold in the stratified K-fold cross validation
# INPUT2:  String     - sampling_method: Choose sampling method from "original", "oversampling", or "SMOTE"
# INPUT3:  Data frame - preprocessed_data: Preprocessed data from preprocessing team
# INPUT4:  string     - predicted_class: The name of predicted class
# INPUT5:  Double     - svm_cost: Hyperparameter in the SVM
# INPUT6:  Double     - svm_kernel: Hyperparameter in the SVM
# INPUT6:  Double     - svm_degree: Hyperparameter in the SVM
# INPUT7:  Double     - svm_gamma: Hyperparameter in the SVM
#
# OUTPUT : List: Containing TPR, FPR and the confusion matrix
# ************************************************

F_svm_evaluation_list <- function(kfold = 5, #Choose numbers of fold in the stratified K-fold cross validation
                                  sampling_method = "oversampling", #Choose sampling method from "original", "oversampling", or "SMOTE"
                                  preprocessed_data,
                                  predicted_class,
                                  
                                  #Belows are parameters of SVM
                                  svm_cost = 1L, 
                                  svm_kernel = "radial", 
                                  svm_degree, 
                                  svm_gamma)
{
  
  #creating stratified k-fold indices by using "caret"
  kfold_indices <- createFolds(preprocessed_data[[predicted_class]], k = kfold, list = TRUE)
  
  #initialize an empty list to store all parameters
  total_list <- list("TPR" = c(),
                     "FPR" = c(),
                     "TP" = c(),
                     "FP" = c(),
                     "FN" = c(),
                     "TN" = c()
  )
  
  for (i in (1:kfold)) {
    
    #tran-test split the data
    train <- preprocessed_data[-kfold_indices[[i]],]
    test  <- preprocessed_data[kfold_indices[[i]],]
    
    #SVM model with different approaches
    switch(sampling_method,
           
           #original (without balancing)
           "original" = (sampled_dataset <- train),
           
           #Oversampling and randomizing the data
           "oversampling" = (sampled_dataset <- F_oversampling_dataframe(training_data = train, predicted_class = predicted_class)),
           
           #SMOTE and randomizing the data
           "SMOTE" = (sampled_dataset <- F_smote_dataframe(training_data = train, predicted_class = predicted_class, K = 5))
    )
    
    #train the SVM model
    svmfit <- F_svm_model(training_data = sampled_dataset, 
                          predicted_class = predicted_class, 
                          kernel = svm_kernel, 
                          degree = svm_degree, 
                          cost = svm_cost, 
                          gamma = svm_gamma)
    
    #predict and evaluate with training data
    train_evaluation_list <- F_evaluation_matrix_list(trained_model = svmfit, 
                                                      testinging_data = train, 
                                                      predicted_class = predicted_class)
    
    #predict and evaluate with testing data
    evaluation_list <- F_evaluation_matrix_list(trained_model = svmfit, 
                                                testinging_data = test, 
                                                predicted_class = predicted_class)
    
    #Add all the pred
    total_list$TPR <- append(total_list$TPR, evaluation_list$TPR)
    total_list$FPR <- append(total_list$FPR, evaluation_list$FPR)
    total_list$TP <- append(total_list$TP, evaluation_list$TP)
    total_list$FP <- append(total_list$FP, evaluation_list$FP)
    total_list$FN <- append(total_list$FN, evaluation_list$FN)
    total_list$TN <- append(total_list$TN, evaluation_list$TN)
    
  }
  
  #Calculate the mean of AUC and confusion matrix
  #mean_auc <- sum(total_list$AUC)/kfold
  #mean_train_auc <- sum(total_list$train_AUC)/kfold
  mean_tpr <- sum(total_list$TPR)/kfold
  mean_fpr <- sum(total_list$FPR)/kfold
  mean_confusion <- list("TP" = sum(total_list$TP)/kfold,
                         "FP" = sum(total_list$FP)/kfold,
                         "FN" = sum(total_list$FN)/kfold,
                         "TN" = sum(total_list$TN)/kfold)
  
  final_list <- list("TPR" = mean_tpr,
                     "FPR" = mean_fpr,
                     "Confusion_matrix"  = mean_confusion)
  
  #print(mean_y_prob)
  
  return(final_list)
}

# ************************************************
# F_svm_gridsearch_plot() :
#
# 1) Grid search is applied on the hyperparameters (cost & gamma) to search for the best model in the grid
# 2) Scatter plot is used to visulized the result
#
# INPUT1:  String     - sampling_method: "SMOTE" / "oversampling" / "original"
# INPUT2:  Data frame - preprocessed_data: Preprocessed data from preprocessing team
# INPUT3:  String     - predicted_class: The name of predicted class
# INPUT4:  string     - svm_parameter: "both" / "cost" / "gamma"
# INPUT5:  Double     - cost_range_min: Lower boundary of the cost range
# INPUT6:  Double     - cost_range_max: Upper boundary of the cost range
# INPUT7:  Double     - cost_step_size: Step size of the cost range
# INPUT8:  Double     - gamma_range_min: Lower boundary of the gamma range
# INPUT9:  Double     - gamma_range_max: Upper boundary of the gamma range
# INPUT9:  Double     - gamma_step_size: Step size of the gamma range
#
# OUTPUT : /
# ************************************************

F_svm_gridsearch_plot <- function(sampling_method = "oversampling", #Choose sampling method from "original", "oversampling", or "SMOTE"
                                  preprocessed_data,
                                  predicted_class,
                                  cost_range_min,
                                  cost_range_max,
                                  cost_step_size,
                                  gamma_range_min,
                                  gamma_range_max,
                                  gamma_step_size) 
{
  cat("SVM Grid search started.\n")
  plot_list <- data.frame()
  
  for (cost in seq(cost_range_min, cost_range_max, by = cost_step_size)) {
      for (gamma in seq(gamma_range_min, gamma_range_max, by = gamma_step_size)) {
             print(paste("Loading cost =", cost, ", gamma =", gamma))
             temp_list <- F_svm_evaluation_list(sampling_method = sampling_method,
                                                preprocessed_data = preprocessed_data,
                                                predicted_class = predicted_class,
                                                svm_cost = cost,
                                                svm_kernel = "radial",
                                                svm_gamma = gamma)
             
             plot_list <- rbind(plot_list,
                                data.frame(x1 = cost,
                                           x2 = gamma,
                                           fp = temp_list$FP,
                                           tp = temp_list$TP,
                                           tpr = temp_list$TPR))
           }
         }
    
  
  plot_list$precision <- 100*plot_list$tp/(plot_list$tp+plot_list$fp)
  plot_list$f1 <- 2*plot_list$precision*plot_list$tpr/((plot_list$precision+plot_list$tpr)*100)
  
  min_euclidean_var1 <- plot_list$x1[which.max(plot_list$f1)]
  min_euclidean_var2 <- plot_list$x2[which.max(plot_list$f1)]
  
  min_recall <- plot_list$tpr[which.min(plot_list$tpr)]
  max_recall <- plot_list$tpr[which.max(plot_list$tpr)]
  
  min_precision <- plot_list$precision[which.min(plot_list$precision)]
  max_precision <- plot_list$precision[which.max(plot_list$precision)]
  
  best_f1 <- plot_list$f1[which.max(plot_list$f1)]
  
  best_recall <- plot_list$tpr[which.max(plot_list$f1)]
  best_precision <- plot_list$precision[which.max(plot_list$f1)]
  
  xlim_max <- if(ceiling(max_recall+2)<=100) ceiling(max_recall+2) else 100
  xlim_min <- if(floor(min_recall-2)>=0) floor(min_recall-2) else 0
  
  ylim_max <- if(ceiling(max_precision+2)<=100) ceiling(max_precision+2) else 100
  ylim_min <- if(floor(min_precision-2)>=0) floor(min_precision-2) else 0
  
  
  plot(plot_list$tpr, plot_list$precision, type="p", lwd=3, col="blue",
       xlab="Recall (%)",
       ylab="Precision (%)",
       xlim=c(xlim_min, xlim_max),
       ylim=c(ylim_min, ylim_max)
  )
  
  abline(h=best_precision, col="red",lty=3,lwd=2)
  abline(v=best_recall, col="red",lty=3,lwd=2)
   
  title(main=paste("Precision-Recall relationship:","Varying cost from", cost_range_min, "to", cost_range_max, ", step =", cost_step_size,
                                    "\nand varying gamma from", gamma_range_min, "to", gamma_range_max, ", step =", gamma_step_size))
  
  annotate <- paste("cost", ": ", round(min_euclidean_var1, digits=4L),
                    "\ngamma", ": ", round(min_euclidean_var2, digits=6L),
                    "\nPrecision: ",round(best_precision, digits=2L),
                    "%\nRecall: ",round(best_recall, digits=2L),
                    "%\nF1-Score: ",round(best_f1, digits=2L))

  
  
  text(x=best_recall, y=best_precision, adj = c(1.2, 1.2),cex=1, col="red",annotate)
  
  cat("SVM Grid search ended.\n\n")
}


# ---------------------------- Model Evaluation : END ------------------------------



# PART 3
# ---------------------------- Model Comparison : START ------------------------------

# ************************************************
# F_optimized_svm_list() :
#
# 1) Train the SVM with optimized parameters
# 2) Calculate the probability of each row being classified as Attrition
# 3) Return into a list containing all the values and vectors for model comparison
#
# INPUT1:  Data frame - training_data: Original/oversampled training data
# INPUT2:  Data frame - testing_data: Testing data
# INPUT3:  String - predicted_class: The name of predicted class
# INPUT4:  Double - optimized_cost: The cost obtained in the optimization
# INPUT5:  Double  - optimized_gamma: The gamma obtained in the optimization
#
# OUTPUT : List: A list containing TP, TN, FP, FN, TPR, FPR, FNR, TNR, MCC, AUC, 
#                probabilities of each row being classified as Attrition,
#                and targeted testing output
# ************************************************

F_optimized_svm_list <- function(training_data,
                                 testing_data,
                                 predicted_class = OUTPUT_FIELD,
                                 optimized_cost,
                                 optimized_gamma,
                                 CompareFinalModel=FALSE
                                 ) 
{
  set.seed(123)
  if(CompareFinalModel==FALSE){
    svmModelCounter<<-svmModelCounter+1
    title<-paste("SVM: cost-",optimized_cost,", gamma-",optimized_gamma)
    
  }else{
    title<-"Support Vector Machine"
  }
  
  #Train the SVM with optimized parameters (cost & gamma) chosen from above
  trained_svm <- F_svm_model(training_data,
                             predicted_class,
                             kernel = "radial",
                             cost = optimized_cost,
                             gamma = optimized_gamma)
  
  #Calculating the probability of each row being classified as Attrition
  y_pred <- predict(trained_svm, type="prob", newdata = testing_data, probability = TRUE)
  y_prob_svm <- attr(y_pred, "probabilities")[, 1]
  
  storeSVM<-NdetermineThreshold(y_prob_svm, testing_data[, predicted_class],title=title,plot=ifelse(CompareFinalModel==TRUE,TRUE,FALSE))
  
  #confusion<-table(factor(y_pred,levels=0:1),factor(testing_data[, predicted_class],levels=0:1))
  
  # This "converts" the above into our preferred format
  # 
  # TP<-as.double(confusion[2,2])
  # FN<-as.double(confusion[1,2])
  # FP<-as.double(confusion[2,1])
  # TN<-as.double(confusion[1,1])
  # 
  # total_list <- NcalcMeasures(TP,FN,FP,TN)
  storeSVM[["y_predicted_list"]]<-y_prob_svm
  storeSVM[["y_expected_list"]]<-testing_data[, predicted_class]
  storeSVM[["model"]] <- title
  
  if(CompareFinalModel==TRUE){
    
    #Getting measures to display and compare with other models
    #storeValues<<-NdetermineThreshold(total_list[["y_predicted_list"]], total_list[["y_expected_list"]],title=title)
    
    #Printing Confusion Matrix
    F_plot_confusion_matrix(TP=storeSVM[["TP"]],FN=storeSVM[["FN"]],FP=storeSVM[["FP"]],TN=storeSVM[["TN"]],title=title)
    #Increasing global counter value by 1
    ModelCounter<<-ModelCounter+1
    #Adding columns to match the same no of columns
  }
  return(storeSVM)
}


# ************************************************
# F_S4_to_dataframe() :
#
# Convert S4 object into a data frame.
#
# INPUT:  S4 Object - s4obj: An object of S4 class
#
# OUTPUT : Data frame: An object of data frame data type
# ************************************************

F_S4_to_dataframe <- function(s4obj) {
  nms <- slotNames(s4obj)
  
  lst <- lapply(nms, function(nm) slot(s4obj, nm))
  
  return(as.data.frame(setNames(lst, nms)))
}

# ************************************************
# F_models_ROC_plot() :
#
# It will plot the ROCs of different models in the same graph with different colors and labels
#
# INPUT1:  list - y_prob_list: a list of vectors containing the predicted probabilities from different models
# INPUT2:  list - y_test_list: a list of vectors containing the true class labels of different models
#
# OUTPUT : /
# ************************************************

F_models_ROC_plot <- function(y_prob_list,
                              y_test_list,
                              title=title) 
{
  cat("Models ROC Comparison started.\n")
  # trained_model_1 <- trained_model_list[[1]]
  y_test_1 <- y_test_list[[1]]
  # y_prob_1 <- attr(predict(trained_model_1, type="prob", newdata = testing_data_1, probability = TRUE), "probabilities")[, 1]
  
  y_prob_1 <- y_prob_list[[1]]
  
  y_prob_rocr_1 <- prediction(y_prob_1, y_test_1)
  y_performance_1 <- performance(y_prob_rocr_1, "tpr","fpr")
  y_perf_1_df <- F_S4_to_dataframe(y_performance_1)
  
  colnames(y_perf_1_df)[4:6] <- c("fpr", "tpr", "alpha")
  
  y_perf_1_df$distance<-sqrt(((100- y_perf_1_df$tpr*100)^2)+((y_perf_1_df$fpr*100)^2))
  
  best_tpr_1 <- y_perf_1_df$tpr[which.min(y_perf_1_df$distance)]
  best_fpr_1 <- y_perf_1_df$fpr[which.min(y_perf_1_df$distance)]
  
  plot(y_performance_1, col=2, main="ROC curves of different models with different thresholds")
  abline(h=best_tpr_1,col=1,lty=3,lwd=2)
  abline(v=best_fpr_1,col=1,lty=3,lwd=2)
  
  print(paste("Model 1's TPR: ", round(best_tpr_1*100, digits=2L),
              "% | Model 1's FPR: ", round(best_fpr_1*100, digits=2L),
              "% | Model 1's Distance: ", round(min(y_perf_1_df$distance), digits=2L), sep = ""))
  
  # annotate_1<-paste("Model 1's TPR: ", round(best_tpr_1*100, digits=2L),
  #                   "%\nModel 1's FPR: ", round(best_fpr_1*100, digits=2L),
  #                   "%\nModel 1's Distance: ", round(min(y_perf_1_df$distance), digits=2L))
  
  # text(x=best_fpr_1, y=best_tpr_1, adj = c(-0.2,1.2),cex=1, col=1,annotate_1)
  legend(0.6, 0.6, c(title[1]))
  
  if (length(y_prob_list) >= 2) {
    
    for (i in 1:(length(y_prob_list)-1)) {
      
      # trained_model = trained_model_list[[i+1]]
      y_test = y_test_list[[i+1]]
      # 
      # y_prob <- attr(predict(trained_model, type="prob", newdata = testing_data, probability = TRUE), "probabilities")[, 1]
      y_prob <- y_prob_list[[i+1]]
      
      y_prob_rocr <- prediction(y_prob, y_test)
      
      y_performance <- performance(y_prob_rocr, "tpr","fpr")
      y_perf_df <- F_S4_to_dataframe(y_performance)
      global <<- y_perf_df
      
      colnames(y_perf_df)[4:6] <- c("fpr", "tpr", "alpha")
      
      y_perf_df$distance<-sqrt(((100- y_perf_df$tpr*100)^2)+((y_perf_df$fpr*100)^2))
      
      best_tpr <- y_perf_df$tpr[which.min(y_perf_df$distance)]
      best_fpr <- y_perf_df$fpr[which.min(y_perf_df$distance)]
      
      plot(y_performance, col=(i*2)+2, add=TRUE)
      abline(h=best_tpr,col=(i*2)+1,lty=3,lwd=2)
      abline(v=best_fpr,col=(i*2)+1,lty=3,lwd=2)
      
      print(paste("Model ", i+1, "'s TPR: ", round(best_tpr*100, digits=2L),
                  "% | Model ", i+1, "'s FPR: ", round(best_fpr*100, digits=2L),
                  "% | Model ", i+1, "'s Distance: ", round(min(y_perf_df$distance), digits=2L), sep = ""))
      
      # annotate<-paste("Model", i+1, "'s TPR: ", round(best_tpr*100, digits=2L),
      #                 "%\nModel", i+1, "'s FPR: ", round(best_fpr*100, digits=2L),
      #                 "%\nModel", i+1, "'s Distance: ", round(min(y_perf_df$distance), digits=2L))
      
      # text(x=best_fpr, y=best_tpr, adj = c(-0.2,1.2),cex=1, col=(i*2)+1,annotate)
      
      vec = c()
      for (i in 1:(length(y_prob_list))) {
        vec[i] <- paste(title[i])
      } 
      
      legend(0.6, 0.6, vec, 2*(1:length(y_prob_list)))
    }
    
  }
  cat("Models comparison ended.\n\n")
}


F_models_ROC_plot2 <- function(predicted=y_prob_list,
                              expected=y_test_list,
                              title=title) 
{
  lengthOfList<-length(predicted)
  for (i in 1:lengthOfList ) {
    if(i==1){
      cat("Models ROC Comparison started.\n")
      # trained_model_1 <- trained_model_list[[1]]
      y_test_1 <- expected[[i]]
      y_prob_1 <- predicted[[i]]
      
      y_prob_rocr_1 <- prediction(y_prob_1, y_test_1)
      y_performance_1 <- performance(y_prob_rocr_1, "tpr","fpr")
      y_perf_1_df <- F_S4_to_dataframe(y_performance_1)
      
      colnames(y_perf_1_df)[4:6] <- c("fpr", "tpr", "alpha")
      
      y_perf_1_df$distance<-sqrt(((100- y_perf_df$tpr*100)^2)+((y_perf_1_df$fpr*100)^2))
      
      best_tpr_1 <- y_perf_1_df$tpr[which.min(y_perf_1_df$distance)]
      best_fpr_1 <- y_perf_1_df$fpr[which.min(y_perf_1_df$distance)]
      
      plot(y_performance_1, col=2, main="ROC curves of different models with different thresholds")
      abline(h=best_tpr_1,col=1,lty=3,lwd=2)
      abline(v=best_fpr_1,col=1,lty=3,lwd=2)
      
      print(paste("Model 1's TPR: ", round(best_tpr_1*100, digits=2L),
                  "% | Model 1's FPR: ", round(best_fpr_1*100, digits=2L),
                  "% | Model 1's Distance: ", round(min(y_perf_1_df$distance), digits=2L), sep = ""))
      
      # annotate_1<-paste("Model 1's TPR: ", round(best_tpr_1*100, digits=2L),
      #                   "%\nModel 1's FPR: ", round(best_fpr_1*100, digits=2L),
      #                   "%\nModel 1's Distance: ", round(min(y_perf_1_df$distance), digits=2L))
      
      # text(x=best_fpr_1, y=best_tpr_1, adj = c(-0.2,1.2),cex=1, col=1,annotate_1)
      legend(0.6, 0.6, c(title[1]))
    }else{
      # trained_model = trained_model_list[[i+1]]
      y_test <- expected[[i]]
      y_prob <- predicted[[i]]
      
      y_prob_rocr <- prediction(y_prob, y_test)
      
      y_performance <- performance(y_prob_rocr, "tpr","fpr")
      y_perf_df <- F_S4_to_dataframe(y_performance)
      global <<- y_perf_df
      
      colnames(y_perf_df)[4:6] <- c("fpr", "tpr", "alpha")
      
      y_perf_df$distance<-sqrt(((100- y_perf_df$tpr*100)^2)+((y_perf_df$fpr*100)^2))
      
      best_tpr <- y_perf_df$tpr[which.min(y_perf_df$distance)]
      best_fpr <- y_perf_df$fpr[which.min(y_perf_df$distance)]
      
      plot(y_performance, col=(i*2)+2, add=TRUE)
      abline(h=best_tpr,col=(i*2)+1,lty=3,lwd=2)
      abline(v=best_fpr,col=(i*2)+1,lty=3,lwd=2)
      
      print(paste("Model ", i+1, "'s TPR: ", round(best_tpr*100, digits=2L),
                  "% | Model ", i+1, "'s FPR: ", round(best_fpr*100, digits=2L),
                  "% | Model ", i+1, "'s Distance: ", round(min(y_perf_df$distance), digits=2L), sep = ""))
      
      # annotate<-paste("Model", i+1, "'s TPR: ", round(best_tpr*100, digits=2L),
      #                 "%\nModel", i+1, "'s FPR: ", round(best_fpr*100, digits=2L),
      #                 "%\nModel", i+1, "'s Distance: ", round(min(y_perf_df$distance), digits=2L))
      
      # text(x=best_fpr, y=best_tpr, adj = c(-0.2,1.2),cex=1, col=(i*2)+1,annotate)
      
      vec = c()
      for (i in 1:(length(y_prob_list))) {
        vec[i] <- paste(title[i])
      } 
      
      legend(0.6, 0.6, vec, 2*(1:length(y_prob_list)))
    }
  }
  
  
}
# ************************************************
# F_plot_confusion_matrix() :
#
# 
#
# INPUT1:  Integer - TP: True positive
# INPUT2:  Integer     - FN: False negative
# INPUT3:  Integer     - FP: False postive
# INPUT4:  Integer     - TN: True negative
# INPUT5:  String     - title: title of plot
#
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
  plotOUtput<-ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad)) + geom_tile() 
  + geom_text(aes(label = Freq), vjust = 0.5, fontface  = "bold", alpha = 1) 
  + scale_fill_manual(values = c(CorrectPrediction = "#58D68D", IncorrectPrediction = "#EC7063")) 
  + theme_bw() +ggtitle(title)+ xlim(rev(levels(table$Reference)))
  
  #Adding theme to the text of the plot
  plotOUtput<-plotOUtput + theme(
    plot.title = element_text(color="#2D5445", size=18, face="bold.italic"),
    axis.title.x = element_text(color="#701026", size=15, face="bold"),
    axis.title.y = element_text(color="#993333", size=15, face="bold")
  )
  print(plotOUtput)
  return(plotOUtput)
}

# ---------------------------- Model Comparison : END ------------------------------


# PART 4
# ---------------------------- Model Selection : START ------------------------------

# ************************************************
# F_SVM_Model_Selection() :
#
# Comparison of SVM with different parameters
#
# INPUT1:  dataframe - train: dataframe of training data
# INPUT2:  vector - test: vector of testing  data
#
# ************************************************
F_SVM_Model_Selection<-function(train,test){
  set.seed(123)
  print("Comparing SVM Model with optimised and non-optimised parameters")
  storeResults<-NULL
  svmNames<-list()
  #SVM with optmized parameters
  storeMeasures <- F_optimized_svm_list(training_data = train,
                                        testing_data = test,
                                        predicted_class = OUTPUT_FIELD,
                                        optimized_cost = 441.6,
                                        optimized_gamma = 0.000027)
  #getting model name
  svmNames[[svmModelCounter]]<-storeMeasures[["model"]]
  #removing unwanted measures
  storeMeasures<-storeMeasures[names(storeMeasures) %in% c("y_predicted_list", "y_expected_list") == FALSE]
  storeResults<-data.frame(Model1=unlist(storeMeasures))
  
  #SVM with non-optmized parameters
  storeMeasures <- F_optimized_svm_list(training_data = train,
                                        testing_data = test,
                                        predicted_class = OUTPUT_FIELD,
                                        optimized_cost = 1500,
                                        optimized_gamma = 0.08)
  #getting model name
  svmNames[[svmModelCounter]]<-storeMeasures[["model"]]
  #removing unwanted measures
  storeMeasures<-storeMeasures[names(storeMeasures) %in% c("y_predicted_list", "y_expected_list") == FALSE]
  storeResults<-cbind(storeResults,data.frame(Model2=unlist(storeMeasures)))
  
  #SVM with non-optmized parameters
  storeMeasures <- F_optimized_svm_list(training_data = train,
                                        testing_data = test,
                                        predicted_class = OUTPUT_FIELD,
                                        optimized_cost = 700,
                                        optimized_gamma = 0.05)
  #getting model name
  svmNames[[svmModelCounter]]<-storeMeasures[["model"]]
  #removing unwanted measures
  storeMeasures<-storeMeasures[names(storeMeasures) %in% c("y_predicted_list", "y_expected_list") == FALSE]
  storeResults<-cbind(storeResults,data.frame(Model3=unlist(storeMeasures)))
  
  #SVM with non-optmized parameters
  storeMeasures <- F_optimized_svm_list(training_data = train,
                                        testing_data = test,
                                        predicted_class = OUTPUT_FIELD,
                                        optimized_cost = 100,0.5)
  #getting model name
  svmNames[[svmModelCounter]]<-storeMeasures[["model"]]
  #removing unwanted measures
  storeMeasures<-storeMeasures[names(storeMeasures) %in% c("y_predicted_list", "y_expected_list") == FALSE]
  storeResults<-cbind(storeResults,data.frame(Model4=unlist(storeMeasures)))
  
  
  #Displaying all results
  #**************************************************************************
  
  #Displaying Model results if storeResults is not empty
  if(is_empty(storeResults)){
    print("No models compared.")
  }else{
    print("Displaying SVM Model Comparison Chart.")
    #interchanging columns and rows
    finalView <- as.data.frame(t(storeResults))
    finalView$model<-svmNames
    #removing unwanted columns from final results
    finalView<-finalView[names(finalView) %in% c("TP","FN","TN","FP", "Precision", "Recall","F1","model") == TRUE]
    
    finalView <- finalView %>%
      select(model, everything())
    #Printing out final Results in view
    F_print_view(finalView,title="SVM Model Comparison")
  }
}
