visualisation<-function(loadedDataset,OUTPUT_FIELD){
set.seed(123)
# loading the dataset
HRattrition <- loadedDataset
names(HRattrition)[1] <- "Age"

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### PARAMETERS USED IN GGPLOT SYNTAX ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# factor is used to categorize data
# aes - Function used to describe the aesthetics of the plot
# scales - Function used to assign dollar value to y-axis(monthly income)
# scale_y_continuous - Function used to denote the values in y-axis
# scale_x_continuous - Function used to denote the values in x-axis
# labs() - Function is used to describe labels
# plot.title() - Function used to choose the location of the title
# geom_box() - Function used to plot a box graph
# geom_jitter() - Function used to show all the data points
# geom_vline() - Function used to draw a vertical line through the plot
# labs() - Function used to add names to axes and the title
# theme_classic() - Kind of theme used throughout the visualisation
# annotate() - Function used to add notes and position to the label


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###  START OF USER DEFINED FUNCTIONS ###
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### FUNCTION FOR PLOTTING DENSITY GRAPHS ###
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# **************************************************************************************************
# F_Plot_Density() :
#
# Function to plot the geom_density for Employee numerical data
#
# INPUT   :   DATASET_NAME  - Name of the dataset       
#         :   X_COLUMN      - Name of the column 
#         :   FILLCOLOR     - Color to be filled in the density plot
#         :   MAIN_TITLE    - Plot title
#         :   X_TITLE       - Plot's x-axis title
#
# OUTPUT  :   Density plot    
#
# **************************************************************************************************

F_Plot_Density<-function(DATASET_NAME, X_COLUMN, FILLCOLOR, MAIN_TITLE, X_TITLE){  
  
  ggplot(data= DATASET_NAME, mapping= aes(x = {{X_COLUMN}})) +
    geom_density(fill = FILLCOLOR,color= "grey") + 
    theme_classic() +
    labs(title = MAIN_TITLE, x = X_TITLE) +
    theme(plot.title = element_text(hjust=0.5, size=13, color="black"))
} #End of density plot function

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### FUNCTION FOR PLOTTING BAR GRAPHS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# **************************************************************************************************
# F_Plot_Bar() :
#
# Function to plot the geom_bar for Employee numerical data
#
# INPUT   :   DATASET_NAME  - Name of the dataset       
#         :   X_COLUMN      - Name of the attribute to be plotted 
#         :   FILLCOLOR     - Fill the plot with color of choice
#         :   MAIN_TITLE    - Plot title
#         :   X_TITLE       - Name of x-axis
#         :   Y_TITLE       - Name of y-axis
#
# OUTPUT  :   Bar plot    
#
# **************************************************************************************************

F_Plot_Bar<-function(DATASET_NAME, X_COLUMN, FILLCOLOR, MAIN_TITLE, X_TITLE, Y_TITLE){  
  
  ggplot(data= DATASET_NAME, mapping= aes(x = {{X_COLUMN}})) +
    geom_bar(fill = FILLCOLOR,color= "grey") + 
    theme_classic() +
    labs(title = MAIN_TITLE, x = X_TITLE,y= Y_TITLE) +
    theme(plot.title = element_text(hjust=0.5, size=13, color="black"))
} #End of bar graph function

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### FUNCTION FOR PLOTTING HISTOGRAMS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# **************************************************************************************************
# F_Plot_Histogram() :
#
# Function to plot the geom_histogram for Employee numerical data
#
# INPUT   :   DATASET_NAME  - Name of the dataset       
#         :   X_COLUMN      - Name of the column 
#         :   Y_COLUMN      - Name of the column based on which the plot data is segregated
#         :   BIN_VALUE     - Bin value for the histogram
#         :   MAIN_TITLE    - Plot title
#         :   X_TITLE       - Plot's x-axis title
#
# OUTPUT  :   Histogram plot    
#
# **************************************************************************************************

F_Plot_Histogram<-function(DATASET_NAME, X_COLUMN, Y_COLUMN, BIN_VALUE, MAIN_TITLE, X_TITLE){
  
  ggplot(DATASET_NAME, aes(x = {{X_COLUMN}}, fill = {{Y_COLUMN}})) +
    geom_histogram(color = "black", bins = BIN_VALUE)  + 
    facet_grid(cols=vars({{Y_COLUMN}})) +
    labs(title = MAIN_TITLE, x = X_TITLE) + theme_classic() +
    theme(plot.title = element_text(hjust=0.5, size=13, color="black"))
  
} #End of Histogram function

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### END OF USER DEFINED FUNCATIONS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### START OF VISUALISATION GRAPHS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Plotting the overall percentage of attrition in the dataset

attrition_percent <- HRattrition %>% group_by(Attrition) %>% summarise(n=n()) %>% mutate(percent=round(prop.table(n),2) * 100) %>%
  ggplot(aes(x=Attrition, y=percent, fill=Attrition)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = paste(percent, "%")),vjust = -0.5, position = position_dodge(0.9)) +
  theme_classic() + scale_fill_manual(values=c("olivedrab3", "tomato")) +
  labs(title = "Attrition Percentage") + theme(plot.title = element_text(hjust=0.5, size=16, color="black"))

# Printing the plot for attrition percentage in the dataset

attrition_percent

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### PLOTTING THE DISTRIBUTION OF DATA ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE PERSONAL - NUMERICAL DATA EXPLORATION ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# geom_density()- used to plot a density graph

# 1. Age
Overall_age_distribution <- F_Plot_Density(HRattrition,Age,"skyblue","Age Distribution","Age")

# 2. Distance from home
Distance_from_home <- F_Plot_Density(HRattrition,DistanceFromHome,"skyblue","Distance to work from employees home","Distance") +
  scale_x_continuous(breaks=seq(0,30,5))

# 3. Number of companies worked
Number_of_companies_worked <- F_Plot_Density(HRattrition,NumCompaniesWorked,"skyblue","Number of companies previous worked","Number of companies") +
  scale_x_continuous(breaks=seq(0,9,1))

# 4. Total working years
Total_working_Years <- F_Plot_Density(HRattrition,TotalWorkingYears,"skyblue","Years of experience","Total working years") +
  scale_x_continuous(breaks=seq(0,40,5))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE PERSONAL - CATEGORICAL DATA EXPLORATION ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# geom_bar()- used to plot a bar graph

# 1. Gender
Gender_distribution <- F_Plot_Bar(HRattrition,Gender,"skyblue","Gender distribution","Gender","Number of employees") +
  scale_y_continuous(breaks=seq(0,800,200))

# 2. Education level
Education_level <- F_Plot_Bar(HRattrition, Education, "skyblue", "Level of education", "Education level", "Number of employees") +
  scale_y_continuous(breaks=seq(0,600,100))

# 3. Education field
Education_field <- F_Plot_Bar(HRattrition,EducationField,"skyblue", "Field of study", "Education Field", "Number of employees" ) +
  scale_y_continuous(breaks=seq(0,600,100))

# 4. Marital status
Marital_status <- F_Plot_Bar(HRattrition,MaritalStatus, "skyblue", "Demographics of marital status", "Marital Status", "Number of employees") +
  scale_y_continuous(breaks=seq(0,700,100))

# 5. Relationship satisfaction
Relationship_satisfaction <- F_Plot_Bar(HRattrition,RelationshipSatisfaction, "skyblue", "Relationship satisfaction", "Satisfaction level", "Number of employees") +
  scale_y_continuous(breaks=seq(0,500,100))

# 6. Work-life balance
Worklife_balance <- F_Plot_Bar(HRattrition, WorkLifeBalance, "skyblue", "Work-Life balance", "Balance level", "Number of employees") +
  scale_y_continuous(breaks=seq(0,900,150))

# plotting all the graphs in a grid using the grid.arrange() function
grid.arrange(Gender_distribution, Education_level, Education_field, Marital_status, Relationship_satisfaction, Worklife_balance,nrow =3, 
             top= textGrob("Distribution of  Personal Attributes",gp=gpar(fontsize=16),hjust = 2.5,x = 1))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE BILLING - NUMERICAL DATA EXPLORATION ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Hourly rate
Hourly_rate <- F_Plot_Density(HRattrition, HourlyRate, "skyblue", "Distribution of Hourly rate data", "Hourly rate") +
  scale_x_continuous(breaks = seq(0,100,20),label= scales::dollar)

# 2. Daily rate
Daily_rate <- F_Plot_Density(HRattrition, DailyRate, "skyblue", "Distribution of Daily rate data", "Daily rate") +
  scale_x_continuous(breaks = seq(0,1500,300),label= scales::dollar)


# 3. Monthly rate
Monthly_rate <- F_Plot_Density(HRattrition, MonthlyRate, "skyblue", "Distribution of Monthly rate data", "Monthly rate") +
  scale_x_continuous(breaks = seq(0,30000,6000),label= scales::dollar)

# plotting all the graphs in a grid using the grid.arrange() function
grid.arrange(Overall_age_distribution,Distance_from_home,Number_of_companies_worked,Total_working_Years,Hourly_rate, Daily_rate, Monthly_rate,
             nrow = 3, top= textGrob("Distribution of Personal Attributes",gp=gpar(fontsize=16),hjust = 2.5,x = 1))


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE WORK - NUMERICAL DATA EXPLORATION ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Monthly income
Income_distribution <- F_Plot_Histogram(HRattrition, MonthlyIncome, Gender, 5, " Distribution of Monthly income data", "Income") +
  scale_fill_manual(values=c("skyblue","tomato"))

# 2. Perentage salary hike 
Salary_hike <- F_Plot_Histogram(HRattrition, PercentSalaryHike, Gender, 5, "Distribution of Percentage salary hike data","Salary hike") +
  scale_fill_manual(values=c("paleturquoise", "salmon"))

# 3. Years at company
Yearsof_experience <- F_Plot_Histogram(HRattrition, YearsAtCompany, Gender, 10, "Distribution of Years of experience data", "Years of experience") +
  scale_fill_manual(values=c("lightsteelblue", "plum"))

# 4. Years in current role
Yearsin_currentrole <- F_Plot_Histogram(HRattrition, YearsInCurrentRole, Gender, 5, "Distribution of Years in current role data", "Years in current role") +
  scale_fill_manual(values=c("tan1", "rosybrown"))

# 5. Years since last promotion
Yearssince_lastpromotion <- F_Plot_Histogram(HRattrition, YearsSinceLastPromotion, Gender, 5, "Distribution of Years since last promotion data", "Years since last promotion") +
  scale_fill_manual(values=c("slateblue1", "peachpuff"))

# 6. Years with current manager
Yearsunder_currentmgr <- F_Plot_Histogram(HRattrition, YearsWithCurrManager, Gender, 5, "Distribution of Years with current manager data", "Years with current manager") +
  scale_fill_manual(values=c("lightgoldenrod", "sienna"))

# plotting all the graphs in a grid using the grid.arrange() function
grid.arrange(Income_distribution, Salary_hike, Yearsof_experience, Yearsin_currentrole, Yearssince_lastpromotion, Yearsunder_currentmgr,
             nrow =3, top= textGrob("Distribution of Work Attributes",gp=gpar(fontsize=16),hjust = 2.5,x = 1))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE WORK - CATEGORICAL DATA EXPLORATION ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Business travel 
Businesstravel_dist <- F_Plot_Bar(HRattrition, BusinessTravel, "skyblue", "Business Travel Data", "Business Travel", "Number of employees") +
  scale_y_continuous(breaks=seq(0,1000,100))

# 2. Overtime 
Overtime_dist <- F_Plot_Bar(HRattrition, OverTime, "skyblue", "Overtime Data", "Overtime", "Number of employees") +
  scale_y_continuous(breaks=seq(0,2000,100))

# 3. Department 
Department_dist <- F_Plot_Bar(HRattrition, Department, "skyblue", "Department Data", "Department", "Number of employees") +
  scale_y_continuous(breaks=seq(0,2000,100))

# 4. Job role 
Jobrole_dist <- F_Plot_Bar(HRattrition, JobRole, "skyblue", "Job Role Data", "Job Role", "Number of employees") +
  scale_y_continuous(breaks=seq(0,1000,100)) + coord_flip()

# 5. Environment satisfaction 
Environment_satisfaction_dist <- F_Plot_Bar(HRattrition, EnvironmentSatisfaction, "skyblue", "Environment Satisfaction Data", "Environment Satisfaction", "Number of employees") +
  scale_y_continuous(breaks=seq(0,2000,100))

# 6. Job involvement 
Job_involvement_dist <- F_Plot_Bar(HRattrition, JobInvolvement, "skyblue", "Job Involvement Data", "Job Involvement", "Number of employees") +
  scale_y_continuous(breaks=seq(0,1000,100))

# 7. Job satisfaction
Job_satisfaction_dist <- F_Plot_Bar(HRattrition, JobSatisfaction, "skyblue", "Job Satisfaction Data", "Job Satisfaction", "Number of employees") +
  scale_y_continuous(breaks=seq(0,500,50))

# 8. Performance rating
Performance_rating_dist <- F_Plot_Bar(HRattrition, PerformanceRating, "skyblue", "Performance Rating Data", "Performance rating", "Number of employees") +
  scale_y_continuous(breaks=seq(0,1400,200)) + scale_x_continuous(breaks=seq(0,5,1))

# plotting all the graphs in a grid using the grid.arrange() function
grid.arrange(Businesstravel_dist, Overtime_dist, Department_dist, Jobrole_dist, Environment_satisfaction_dist, Job_involvement_dist,
             Job_satisfaction_dist, Performance_rating_dist, nrow = 4, top= textGrob("Distribution of Work Attributes",gp=gpar(fontsize=16),hjust = 2.5,x = 1))


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### END OF DATA DISTRIBUTION PLOTS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### PLOTTING THE CORRELATION MATRIX AND ITS FEATURES ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Do not have to scale, as the values as correlation coefficient is independent of scale
# Choosing all numeric values

isanumber <- select_if(HRattrition,is.numeric)

# correlation matrix of square shape

corr <- round(cor(isanumber),2)
ggcorrplot(corr,type = "lower",lab=TRUE,lab_size = 3, colors= c("tan","white","palevioletred1"),method="square", 
           theme_classic())

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### CORRELATION BETWEEN ATTRIBUTES ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Bivariate graphs

# 1. Job level vs Monthly income
ggplot(data=HRattrition,mapping=aes(x=factor(JobLevel),y=MonthlyIncome)) +
  geom_boxplot(fill="olivedrab3",show.legend = TRUE) + 
  theme_classic() +  
  theme(plot.title=element_text(hjust=0.5,size=16)) +
  scale_y_continuous(breaks = seq(0,20000,2500),label= scales::dollar) +
  labs(title="Relationship between Job level and Monthly Income", x="Job level",y="Monthly Income")

# 2. Total working years vs Job level
ggplot(data = HRattrition,mapping = aes(x=factor(JobLevel),y=TotalWorkingYears)) +
  geom_boxplot(fill="olivedrab3") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.5,size=16)) +
  scale_y_continuous(breaks = seq(0,45,5)) +
  labs(title="Relationship between Job level and Total years worked", x="Job level",y="Number of years worked")

# 3. Percentage salary hike vs Performance rating
RatingVIncome <-  HRattrition%>% select(PerformanceRating, PercentSalaryHike)
ggplot(data= RatingVIncome, mapping= aes(x= factor(PerformanceRating), y= PercentSalaryHike)) + 
  geom_boxplot(fill="olivedrab3") + 
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.5,size=16)) +
  labs(title="Relationship between Salary hike and Performance rating", x="Performance Rating",y="Percentage salary hike")

# 4. Total working years vs Monthly Income
# geom_point() is used to plot scatter plots
# position="jitter" Add random noise to X and Y position of each element to avoid over plotting
# loess method-is is a method for fitting a smooth curve between two variables

ggplot(data=HRattrition,mapping = aes(x=TotalWorkingYears,y=MonthlyIncome)) +
  geom_point(color="tomato", size=1, alpha=0.5,position = "jitter",) + 
  geom_smooth(method="loess",color="grey",se=FALSE,size=1) +
  scale_x_continuous(breaks = seq(0,45,5),limits = c(0,45)) + # x-axis mapping
  scale_y_continuous(breaks = seq(0,30000,5000),label= scales::dollar) + # add dollar sign to the y-axis
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5,size=16)) +
  labs(x="Total years worked",y="Monthly income",title = "Relationship between Years worked and Monthly income")

# 5. Years at company vs Years in current role
# method="lm"-is used to fit a linear model line between two variables

ggplot(data=HRattrition,mapping = aes(x=YearsAtCompany,y=YearsInCurrentRole)) +
  geom_point(color="tomato", size=1, alpha=0.5,position = "jitter") +
  geom_smooth(method="lm",color="grey",size=1) +
  scale_x_continuous(breaks = seq(0,40,4),limits = c(0,45)) + # x-axis mapping
  scale_y_continuous(breaks = seq(0,20,2)) + # add dollar sign to the y-axis
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5,size=16)) +
  labs(x="Years at company",y="Years in current role",title = "Relationship between Total years worked at the company and Years in current role")

# 6. Years with current manager vs Years in current role
ggplot(data=HRattrition, mapping=aes(x=YearsWithCurrManager, y=YearsInCurrentRole)) +
  geom_point(colour = "tomato",size=1,alpha=0.5,position = "jitter") + 
  geom_smooth(method="lm",color="gray",size=1) + 
  theme_classic() + 
  theme( plot.title=element_text(hjust=0.5)) + 
  labs(title="Relationship between Years with the current mananger and Existing role", x="Years with Current Manager",y="Years in current role")

# 7. Salaries based on job sector categorized by gender
# aes() function is used to describe the aesthetics of the plot
# alpha=transparency
# facewrap() function is used for multi panel plots

ggplot(data=HRattrition, mapping=aes(x=EmployeeNumber,y=MonthlyIncome,color= Gender)) + 
  geom_smooth(method="lm",size = 0.5,se=FALSE) +
  geom_point(alpha=0.6, size= 1) +
  scale_x_continuous(breaks=seq(0,2000,500)) +
  scale_y_continuous(breaks=seq(0,20000,4000),label=scales::dollar) +
  scale_color_manual(values = c("olivedrab3","tomato")) +
  facet_wrap(~JobRole) +
  theme_classic() +
  labs(title="Salaries based on Job roles",subtitle = "categorized by gender",x= "Employee",Y= "Monthly Income") 


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### PLOTTING ATTRIBUTES AGAINST ATTRITION ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE PERSONAL ATTRIBUTES - DATA ANALYSIS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Age vs Attrition
# labs() function is used to assign a name to the axes and title
# scale_fill_manual() is used to fill the color of the graphs
# geom_density() is used to plot a kernel density plot

age_attr <- ggplot(data=HRattrition,mapping=aes(x=Age,fill=Attrition)) +
  geom_density(alpha=0.4, show.legend=TRUE) +
  theme_classic() +
  labs(x="Overall age",title="Relationship between Attrition and Age",y="Attrition density") +
  theme(plot.title=element_text(hjust=0.5,size=16)) +
  scale_x_continuous(breaks=seq(0,60,5)) +
  scale_fill_manual(values =c("olivedrab3","tomato"))

# 2. Marital status vs Attrition
marital_attr <- ggplot(HRattrition, aes(MaritalStatus, fill = Attrition )) +
  geom_bar(position = position_dodge()) + theme_classic() + 
  scale_fill_manual(values=c("olivedrab3", "tomato")) +
  labs(title = "Relationship between Attrition and Marital status", x="", y="Number of employees") + 
  theme(plot.title = element_text(hjust=0.5, size=16, color="black"))

# 3. Gender vs Attrition
gender_attr <- ggplot(HRattrition, aes(x = Gender, fill = Attrition)) + 
  geom_bar(position = position_dodge()) + theme_classic() + 
  scale_fill_manual(values=c("olivedrab3", "tomato"))  + 
  labs(title = "Relationship between Attrition and Gender", x="", y="Number of employees") + 
  theme(plot.title = element_text(hjust=0.5, size=16, color="black"))


# 4. Number of companies worked vs Attrition
# tally() summarizes the data by category
# stat=identity does not calculate counts
# geom_bar() is used to plot bar graphs

numofpreviouscompwkd <- HRattrition %>% group_by(Attrition,NumCompaniesWorked) %>% tally(sort=TRUE) 
numofpreviouscompwkd_attr <- ggplot(numofpreviouscompwkd, aes(NumCompaniesWorked, n, fill=Attrition,label=n)) +
  geom_bar(stat="identity",color="grey",position="dodge") +
  scale_fill_manual(values =c("olivedrab3","tomato")) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5,size=16)) +
  labs(x="Number of companies changed",y="Number of employees",title="Relationship between Attrition and Number of companies worked") +
  scale_x_continuous(breaks=seq(0,9,1))

# plotting all the graphs in a grid using the grid.arrange() function
grid.arrange(age_attr, marital_attr, gender_attr, numofpreviouscompwkd_attr,
             nrow =2, top= textGrob("Attrition vs Personal Attributes",gp=gpar(fontsize=16),hjust = 2.5,x = 1))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE EDUCATION - DATA ANAYLSIS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Education vs Attrition
# categorizing levels of education

Education_attr <- HRattrition %>%  select(Attrition, Education) %>% 
  mutate(Education = as.factor(if_else(Education == 1,"Below College",
                                       if_else(Education == 2,"College", 
                                               if_else(Education == 3,"Bachelor's", 
                                                       if_else(Education == 4,"Master's", "Doctor's")))))) %>%
  ggplot(aes(x=Attrition, y=Education, color=Attrition)) +
  geom_jitter(alpha = 0.7, size = 1) + theme_classic() + 
  labs(title = "Relationship between Attrition and Education", y='') + 
  theme(plot.title = element_text(hjust=0.5, color="black", size=16))

# 2. Education field vs Attrition
Educationfield_attr <- ggplot(HRattrition, aes(EducationField,fill = Attrition )) +
  geom_bar(position = position_dodge()) + theme_classic() + 
  scale_fill_manual(values=c("olivedrab3", "tomato"))  + 
  labs(title = "Relationship between Attrition and Education field", x='', y='Number of employees') + 
  theme(plot.title = element_text(hjust=0.5, color="black", size=16)) 

# plotting all the graphs in a grid using the grid.arrange() function
grid.arrange(Education_attr,Educationfield_attr, top= textGrob("Attrition vs Educational Background",gp=gpar(fontsize=16),hjust = 2.5,x = 1))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE TRAVEL - DATA ANALYSIS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Business travel vs Attrition
businesstravel <- HRattrition %>% group_by(BusinessTravel, Attrition) %>% tally(sort=TRUE) 
businesstravel_attr <- ggplot(businesstravel,aes(BusinessTravel, n, fill=Attrition)) +
  geom_bar(stat="identity", color="grey", position="dodge") + theme_classic() +
  coord_flip()+ scale_fill_manual(values=c("olivedrab3", "tomato")) +  
  theme(plot.title=element_text(hjust=0.5, size=16)) + scale_y_continuous(breaks=seq(0,1000,100)) +
  labs(title="Relationship between Attrition and Travel", x="", y="Number of employees")

# 2. Distance from home vs Attrition
distfromhome_attr <- ggplot(HRattrition,aes(DistanceFromHome,fill=Attrition)) +
  geom_density(alpha=0.4,show.legend=TRUE, color="grey") + theme_classic() +
  theme(plot.title=element_text(hjust=0.5,size=16)) +
  scale_fill_manual(values=c("olivedrab3","tomato")) +
  labs(x="Distance from home",y="Attrition density",title="Relationship between Attrition and Distance from home")

# plotting all the graphs in a grid using the grid.arrange() function
grid.arrange(businesstravel_attr,distfromhome_attr, top= textGrob("Attrition vs Travel",gp=gpar(fontsize=16),hjust =  3.5,x = 1))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE TENURE - DATA ANALYSIS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Total working years vs Attrition
numworkingyrs_attr  <- ggplot(HRattrition,aes(TotalWorkingYears,fill=Attrition)) +
  geom_density(alpha=0.4,show.legend=TRUE) +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5,size=16)) +
  labs(x="Total years worked",y="Attrition density",title="Relationship between Attrition and Total years of experience") +
  scale_x_continuous(breaks=seq(0,50,5)) +
  scale_fill_manual(values =c("olivedrab3","tomato"))

# 2. Years in the current company vs Attrition
Yrsatcomp_attr <- ggplot(HRattrition, aes(YearsAtCompany, Attrition)) +
  geom_jitter(aes(colour = Attrition), width = 0.4, height = 0.4) + 
  theme_classic() + 
  scale_fill_manual(values=c("olivedrab3", "tomato")) +
  labs(title = "Relationship between Attrition and Years at the company") + 
  theme(plot.title = element_text(hjust=0.5, color="black",size=16))

# 3. Years in the current role vs Attrition
Yrsatcurrrole_attr <- ggplot(HRattrition, aes(x= YearsInCurrentRole, fill=Attrition)) + 
  geom_bar(position="dodge", color="grey") +
  theme_classic() + 
  scale_fill_manual(values=c("olivedrab3", "tomato")) + 
  scale_x_continuous(breaks=seq(0,18,3)) + scale_y_continuous(breaks=seq(0,300,20)) +
  labs(title = "Relationship between Attrition and Years in current role", x="Years in current role", y="Number of employees") + 
  theme(plot.title = element_text(hjust=0.5, size=16))

# 4. Years with current manager vs Attrition
Yrscurrmanager_attr <- ggplot(HRattrition, aes(x= YearsWithCurrManager, fill = Attrition)) + 
  geom_bar(position="dodge", color="grey") +
  theme_classic() + 
  scale_fill_manual(values=c("olivedrab3", "tomato")) + 
  scale_x_continuous(breaks=seq(0,20,2)) + 
  scale_y_continuous(breaks=seq(0,300,20)) +
  labs(title = "Relationship between Attrition and Years with current manager", x="Years with current manager", y="Number of employees") + 
  theme(plot.title = element_text(hjust=0.5, size=16, color="black")) 

# 5. Years since last promotion vs Attrition
Yrslstpromotion_attr <- ggplot(HRattrition, aes(x= YearsSinceLastPromotion, fill=Attrition)) + 
  geom_bar(position="dodge", color="grey") +
  theme_classic() + 
  scale_fill_manual(values=c("olivedrab3", "tomato")) + 
  scale_x_continuous(breaks=seq(0,20,2)) + scale_y_continuous(breaks=seq(0,500,50)) +
  labs(title = "Relationship between Attrition and Years since last promotion",x="Years since promotion", y="Number of employees") + 
  theme(plot.title = element_text(hjust=0.5, size=16, color="black"))

# plotting all the graphs in a grid using the grid.arrange() function
grid.arrange(numworkingyrs_attr,Yrsatcomp_attr, Yrsatcurrrole_attr, Yrscurrmanager_attr, Yrslstpromotion_attr,
             top= textGrob("Attrition vs Tenure",gp=gpar(fontsize=16),hjust = 4,x = 1))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE BILLING - DATA ANALYSIS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Monthly income vs Attrition
monthlyincome_attr <- ggplot(HRattrition, aes(x=Attrition, y=MonthlyIncome, fill=Attrition)) + 
  geom_boxplot() + theme_classic() + 
  scale_fill_manual(values=c("olivedrab3", "tomato")) + 
  labs(title = "Relationship between Attrition and Monthly income") + 
  scale_y_continuous(label=scales::dollar)+
  theme(plot.title = element_text(hjust=0.5,size=16, color="black"))

# 2. Hourly rate vs Attrition
hourlyrate_attr <- ggplot(HRattrition, aes(x=Attrition, y=HourlyRate, fill=Attrition)) +
  geom_boxplot() + theme_classic() +
  scale_y_continuous(label=scales::dollar)+
  scale_fill_manual(values=c("olivedrab3", "tomato")) +
  labs(title = "Relationship between Attrition and Hourly rate") + theme(plot.title = element_text(hjust=0.5, color="black", size=16))

# 3. Daily Rate vs Attrition
dailyrate_attr <- ggplot(HRattrition, aes(x=Attrition, y=DailyRate, fill=Attrition)) +
  geom_boxplot() + theme_classic() +
  scale_y_continuous(label=scales::dollar)+
  scale_fill_manual(values=c("olivedrab3", "tomato")) +
  labs(title = "Relationship between Attrition and Daily rate") + theme(plot.title = element_text(hjust=0.5, color="black", size=16))

# 4. Monthly Rate vs Attrition
monthlyrate_attr <- ggplot(HRattrition, aes(x=Attrition, y=MonthlyRate, fill=Attrition)) +
  geom_boxplot() + theme_classic() +
  scale_y_continuous(label=scales::dollar)+
  scale_fill_manual(values=c("olivedrab3", "tomato")) +
  labs(title = "Relationship between Attrition and Monthly rate") + theme(plot.title = element_text(hjust=0.5, color="black", size=16))

# 5. Stock options VS Attrition 
# factor function takes different values and converts into categorical variables

stockoptions <- HRattrition %>% group_by(StockOptionLevel, Attrition) %>% tally(sort=TRUE)
stockoptions_attr <- ggplot(stockoptions,aes(StockOptionLevel, n, fill=Attrition)) +
  geom_bar(stat="identity", color="grey", position="dodge") +
  theme_classic() + scale_fill_manual(values=c("olivedrab3", "tomato")) +  
  theme(plot.title=element_text(hjust=0.5, size=16)) +
  scale_y_continuous(breaks=seq(0,550,50)) +
  labs(title="Relationship between Attrition and Stock options", x="Stock Option Levels", y="Number of employees")

# plotting all the graphs in a grid using the grid.arrange() function
grid.arrange(hourlyrate_attr, dailyrate_attr, monthlyrate_attr, nrow = 3,
             top= textGrob("Attrition vs Rates",gp=gpar(fontsize=16),hjust = 4,x = 1))

grid.arrange(monthlyincome_attr, stockoptions_attr,top= textGrob("Attrition vs Income data",gp=gpar(fontsize=16),hjust = 3.5,x = 1))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE JOB - DATA ANALYSIS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Department vs Attrition
deptattrition <- HRattrition %>% group_by(Department, Attrition) %>% tally(sort=TRUE)
dept_attr <- ggplot(deptattrition,aes(Department, n, fill=Attrition)) +
  geom_bar(stat="identity", color="grey", position="dodge") +
  theme_classic() + scale_fill_manual(values=c("olivedrab3", "tomato")) +  
  theme(plot.title=element_text(hjust=0.5, size=16)) +
  scale_y_continuous(breaks=seq(0,800,50)) +
  labs(title="Relationship between Attrition and Department", x="department", y="Number of employees")

# 2. Job role vs Attrition
# geom_jitter() is used to plot a scatter graph

jobrole_attr <- ggplot(HRattrition,aes(x=Attrition, y=JobRole, color=Attrition)) +
  geom_jitter(alpha = 0.4, size =2) + 
  theme_classic() + scale_fill_manual(values=c("olivedrab3", "tomato")) +
  labs(title = "Relationship between Attrition and Job role",x="Attrition",y="Job Role") + 
  theme(plot.title = element_text(hjust=0.5,size=16))

# 3. Job level vs Attrition
joblevel_attr <- ggplot(data= HRattrition,mapping= aes(y=Attrition, x=JobLevel, fill=Attrition)) +
  geom_boxplot(color="blue") + 
  geom_jitter(color="slategrey",alpha=0.4,size=0.1) +
  scale_fill_manual(values=c("olivedrab3", "tomato")) +
  theme_classic() + theme(plot.title=element_text(hjust=0.5,size=16)) +
  labs(title="Relationship between Attrition and Job level", x="Job level",y="Attrition")

# plotting all the graphs in a grid using the grid.arrange() function
grid.arrange(dept_attr, jobrole_attr, joblevel_attr,  
             top= textGrob("Attrition vs Job Factors",gp=gpar(fontsize=16),hjust = 3.5,x = 1))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE PERFORMANCE - DATA ANALYSIS  ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Percentage salary hike vs Attrition
percentagehike <- HRattrition %>% group_by(PercentSalaryHike, Attrition) %>% tally(sort=TRUE)
Percentagehike_attr <- ggplot(percentagehike, aes(PercentSalaryHike, n, fill=Attrition)) +
  geom_bar(stat="identity", color="grey", position="dodge") + 
  theme_classic() + scale_x_continuous(breaks=seq(0,26,2))+
  scale_y_continuous(breaks=seq(0,200,40))+
  scale_fill_manual(values=c("olivedrab3", "tomato")) +
  labs(title = "Relationship between Attrition and Percentage Salary Hike",x="Percentage salary hike",y="Number of employees") +
  theme(plot.title = element_text(hjust=0.5, size=16, color="black"))

# 2. Performance rating vs Attrition
performancerating <- HRattrition %>% group_by(PerformanceRating, Attrition) %>% tally(sort=TRUE)
performancerating_attr <- ggplot(performancerating,aes(PerformanceRating, n, fill=Attrition))+
  geom_bar(stat="identity", color="grey", position="dodge")+ theme_classic()+
  scale_x_continuous(breaks=seq(0,4,1))+
  scale_y_continuous(breaks=seq(0,1000,100))+
  coord_flip()+ scale_fill_manual(values=c("olivedrab3", "tomato")) +  
  theme(plot.title=element_text(hjust=0.5, size=16))+ 
  labs(title="Relationship between Attrition and Performance rating", x="Performance rating", y="Number of employees")

# plotting all the graphs in a grid using the grid.arrange() function
grid.arrange(Percentagehike_attr, performancerating_attr,  top= textGrob("Attrition vs Performance data",gp=gpar(fontsize=16),hjust = 3,x = 1))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### EMPLOYEE ENGAGEMENT - DATA ANALYSIS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. Environment satisfaction vs Attrition
# coord_flip() - flips the co-ordinates

environmentsatisfaction <- HRattrition %>% group_by(EnvironmentSatisfaction, Attrition) %>% tally(sort=TRUE)
envsatisfaction_attr <- ggplot(environmentsatisfaction,aes(EnvironmentSatisfaction, n, fill=Attrition)) +
  geom_bar(stat="identity", color="grey", position="dodge") +
  theme_classic() + coord_flip() +
  scale_fill_manual(values=c("olivedrab3", "tomato")) +  
  theme(plot.title=element_text(hjust=0.5, size=16)) +
  scale_y_continuous(breaks=seq(0,550,50)) +
  labs(title="Relationship between Attrition and Environment satisfaction", x="Environment satisfaction", y="Number of employees")

# 2. Work life balance vs Attrition
#categorizing levels of work life balance
Worklife_attr <- HRattrition %>% select(Attrition, WorkLifeBalance) %>% 
  mutate(WorkLifeBalance = as.factor(if_else(WorkLifeBalance == 1,"Bad",
                                             if_else(WorkLifeBalance == 2,"Good", 
                                                     if_else(WorkLifeBalance == 3,"Better", "Best"))))) %>%
  ggplot(aes(x=WorkLifeBalance,group=Attrition)) +
  geom_bar(stat="count",aes(y=..prop..,fill=Attrition)) +
  labs(x="WorkLife Balance",y="Percentage",title="Relationship between Attrition and WorkLife Balance") +
  facet_wrap(~Attrition) + scale_y_continuous(labels=scales::percent) + theme_classic() +
  theme(legend.position="none",plot.title=element_text(hjust=0.5,size=16)) +
  geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)

# 3. Relationship satisfaction vs Attrition
# categorizing relationship satisfaction level

Relationshipsatis_attr <- HRattrition %>%  select(Attrition, RelationshipSatisfaction) %>%  
  mutate(RelationshipSatisfaction = as.factor(if_else(RelationshipSatisfaction == 1,"Low",
                                                      if_else(RelationshipSatisfaction == 2,"Medium", 
                                                              if_else(RelationshipSatisfaction == 3,"High", "Very High"))))) %>%
  ggplot(aes(x=RelationshipSatisfaction,group=Attrition)) +
  geom_bar(stat="count",aes(y=..prop..,fill=Attrition)) +
  labs(x="Relationship Satisfaction",y="Percentage",title="Relationship between Attrition and Relationship Satisfaction") +
  facet_wrap(~Attrition) + scale_y_continuous(labels=scales::percent) + theme_classic() +
  theme(legend.position="none",plot.title=element_text(hjust=0.5,size=16)) +
  geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)

# 4. Job satisfaction vs Attrition
jobsatisfaction_attr <- HRattrition %>%  select(Attrition, JobSatisfaction) %>%  
  mutate(JobSatisfaction = as.factor(if_else(JobSatisfaction == 1,"Low",
                                             if_else(JobSatisfaction == 2,"Medium", 
                                                     if_else(JobSatisfaction == 3,"High", "Very High"))))) %>%
  ggplot(aes(x=JobSatisfaction,group=Attrition)) +
  geom_bar(stat="count",aes(y=..prop..,fill=Attrition)) +
  labs(x="Job Satisfaction",y="Percentage",title="Relationship between Attrition and Job Satisfaction") +
  facet_wrap(~Attrition) + scale_y_continuous(labels=scales::percent) + theme_classic() +
  theme(legend.position="none",plot.title=element_text(hjust=0.5,size=16)) +
  geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)

# 5. Overtime vs Attrition
overtime_attr <- ggplot(HRattrition,aes(x=OverTime,group=Attrition)) +
  geom_bar(stat="count",aes(y=..prop..,fill=factor(..x..))) +
  labs(x="Overtime",y="Percentage",title="Relationship between Attrition and Overtime") +
  facet_wrap(~Attrition) + scale_y_continuous(labels=scales::percent) + theme_classic() +
  theme(legend.position="none",plot.title=element_text(hjust=0.5,size=16)) +
  geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)

# 6. Job involvement vs Attrition
jobinvolvement <- HRattrition %>% group_by(JobInvolvement, Attrition) %>% tally(sort=TRUE)
jobinvol_attr <- ggplot(jobinvolvement,aes(JobInvolvement, n, fill=Attrition)) +
  geom_bar(stat="identity", color="grey", position="dodge") +
  theme_classic() + coord_flip() +
  scale_fill_manual(values=c("olivedrab3", "tomato")) +  
  theme(plot.title=element_text(hjust=0.5, size=16)) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  labs(title="Relationship between Attrition and Job involvement", x="Jobinvolvemt level", y="Number of employees")

# plotting all the graphs in a grid using the grid.arrange() function
grid.arrange(envsatisfaction_attr, Worklife_attr, Relationshipsatis_attr, jobsatisfaction_attr, overtime_attr, jobinvol_attr,
             top= textGrob("Attrition vs Engagement data",gp=gpar(fontsize=16),hjust = 2.5,x = 1))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### END OF VISUALISATION GRAPHS ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

}
