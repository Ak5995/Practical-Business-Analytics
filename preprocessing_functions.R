

# ************************************************
# F_check_NA() :
# 
#
# INPUT:   vector - input - CSV File
#
# OUTPUT : vector - Data frame
# ************************************************

F_check_NA <- function(data) {
  
  no_NA <- 0
  yes_NA <- 0
  
  for (name in names(data)) {
    if (sum(is.na(data[[name]])) != 0) {
      yes_NA <- yes_NA+1
    } else {
      no_NA <- no_NA+1
    }
  }
  
  print(paste("Proprocessing step 1: There are", yes_NA, "columns with N/A value, and there are", no_NA, "without N/A value.", sep = " "))
  
}

# ************************************************
# F_seperate_numerical_categorical() :
# 
#
# INPUT:   vector - input - CSV File
#
# OUTPUT : vector - Data frame
# ************************************************

F_seperate_numerical_categorical <- function(data) {
  
  numerical_vec <- c()
  categorical_vec <- c()
  i <- 1
  j <- 1
  
  for (name in names(data)) {
    if (is.numeric(data[[name]]) == TRUE) {
      numerical_vec[i] <- name
      i <- i+1
    }
    
    if (is.numeric(data[[name]]) == FALSE){
      categorical_vec[j] <- name
      j <- j+1
    }
    
  }
  
  print("Proprocessing step 4: The dataset has been separated into numerical and categorical data.")
  
  return(list("numerical" = data[numerical_vec],
              "categorical" = data[categorical_vec]))
}

# ************************************************
# F_check_balance() :
# 
#
# INPUT:   vector - input - CSV File
#
# OUTPUT : vector - Data frame
# ************************************************

F_check_balance <- function(data, predicted_class) {
  
  print("Proprocessing step 3: Check if it is a balanced dataset.")
  
  if (table(data[[predicted_class]])[1] >= table(data[[predicted_class]])[2]) {
    if (between(table(data[[predicted_class]])[1], table(data[[predicted_class]])[2]*0.95, table(data[[predicted_class]])[2]*1.05)){
      print("  The data is balanced.")
    } else {print("  The data is unbalanced.")}
  } else {
    if (between(table(data[[predicted_class]])[2], table(data[[predicted_class]])[1]*0.95, table(data[[predicted_class]])[1]*1.05)){
      print("  The data is balanced.")
    } else {print("  The data is unbalanced.")}
  }
  
}

# ************************************************
# Removecolumns() :
#
# To remove unwanted columns from dataset
#
# INPUT1:   vector - input - datset
# INPUT2:   list - input - columnname
#
# OUTPUT : vector - Data frame
# ************************************************
F_remove_columns_dataframe <- function(data,
                                       drops){
  
  print("Proprocessing step 2: Redundant columns are removed.")
  Dropped_data<-data[ , !(names(data) %in% drops)]
  
  return (Dropped_data)
}
# ************************************************

# ************************************************
# F_standardization_dataframe() :
#
# To standardize data in the range 0 to 1
#
# INPUT1:   vector - input - datsetcolumn

#
# OUTPUT : vector - Data frame
# ************************************************
F_standardization_dataframe <- function(data){
  
  print("Proprocessing step 7: Standardization is applied to all continuous data.")
  for (name in names(data)) {
    data[[name]]<-scale(data[[name]])[, 1]
  }
  
  return (data)
}

# ************************************************

# ************************************************
# Categorical() :
#
# To transform into categorical variables 0 and 1
#
# INPUT1:   vector - input - datsetcolumn

#
# OUTPUT : vector - Data frame
# ************************************************
F_one_hot_encoding_dataframe <- function(categorical_data){
  
  print("Proprocessing step 5: One-hot encoding is applied to all categorical data.")
  dmy <- dummyVars(" ~ .", data = categorical_data)
  one_hot_data <- data.frame(predict(dmy, newdata = categorical_data))
  
  return (one_hot_data)
}

# ************************************************

# F_remove_outliers_dataframe() :
#
# To remove outliers
#
# INPUT1:   vector - input - datset

#
# OUTPUT : vector - Data frame
# ************************************************

F_remove_outliers_dataframe <- function(numerical_data) {
  
  print("Proprocessing step 6: Remove the outliers in numerical data.")
  for (name in names(numerical_data)) {
    #boxplot(numerical_data[[name]], main = paste("Box plot of", name))
    if (length(boxplot.stats(numerical_data[[name]])$out) > 50) {
      quantiles <- quantile(numerical_data[[name]], c(.05, .95))
      numerical_data[[name]][numerical_data[[name]] < quantiles[1]] <- quantiles[1]
      numerical_data[[name]][numerical_data[[name]] > quantiles[2]] <- quantiles[2]
      print(paste("  Outliers of", name, "has been removed."))
    } else {
      print(paste("  No outlier presents in", name))
    }
  }

  return(numerical_data)
}

# ************************************************

# ************************************************

# F_remove_outliers_dataframe_new() :
#
# To remove outliers
#
# INPUT1:   vector - input - datset

#
# OUTPUT : vector - Data frame
# ************************************************

F_remove_outliers_dataframe_new <- function(numerical_data) {
  
  print("Proprocessing step 6: Remove the outliers in numerical data.")
  for (name in names(numerical_data)) {
    
      quantiles <- quantile(numerical_data[[name]], c(.05, .95))
      numerical_data[[name]][numerical_data[[name]] < quantiles[1]] <- quantiles[1]
      print(paste("Values less than 5 percentile replaced in", name))
      numerical_data[[name]][numerical_data[[name]] > quantiles[2]] <- quantiles[2]
      print(paste("Values greater than 95 percentile replaced in", name))
  }
  
  return(numerical_data)
}

# ************************************************


# F_combine_dataframe() :
#
# To remove outliers
#
# INPUT1:   vector - input - datset

#
# OUTPUT : vector - Data frame
# ************************************************
F_combine_dataframe <- function(categorical_data, numerical_data, predicted_class) {
  
  combined_data <- cbind(categorical_data, numerical_data)
  
  drop <- paste(predicted_class, "No", sep="")
  combined_data <- combined_data[ , !(names(combined_data) %in% drop)]
  
  for (name in names(combined_data)) {
    if (name == predicted_class) {
      combined_data[[predicted_class]] <- as.numeric(as.factor(combined_data[[predicted_class]]))-1
    }
  }
  
  names(combined_data)[names(combined_data) == paste(predicted_class, "Yes", sep="")] <- predicted_class
  
  return(combined_data)
}

# F_FAMD() :
#
# Dimensionality Reduction
#
# INPUT1:   vector - input - datset

#
# OUTPUT : vector - Data frame
# ************************************************
F_FAMD <- function(dataset, predicted_class) {


  drop <- paste(predicted_class, "No", sep="")
  combined_data <- dataset[ , !(names(dataset) %in% drop)]

  res.famd <- FAMD(combined_data,ncp = 31, graph = FALSE)
  eig.val <- res.famd$eig

  library("factoextra")
  eig.val <- get_eigenvalue(res.famd)
  eig.val<- as.data.frame(eig.val)

  rowfirst<-rownames(eig.val)
  row_first<-rowfirst[1]
  row_first

  row<-rownames(eig.val)[eig.val$cumulative.variance.percent >= 90]
  row_last<-row[1]

  var <- get_famd_ind(res.famd)
  var$coord<-data.frame(var$coord)
  result_data<-var$coord[, 1:30]

  return(result_data)
}

