#####################################################################################################################
##                                      BOSTON HOUSING DATA ANALYSIS                                               ##
#####################################################################################################################
## CODE DEVELOPED BY ADAM TASHMAN FOR UCSB PSTAT DATA SCIENCE TRACK                                                ##
##                                                                                                                 ##
## DATA SOURCE: UCI MACHINE LEARNING REPOSITORY                                                                    ##
## https://archive.ics.uci.edu/ml/                                                                                 ##
##                                                                                                                 ##
#####################################################################################################################
# Load libraries
library(RCurl)

#####################################################################################################################
## Data Loading

# specify the URL for the data
urlfile <-'https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data'

# download the file
dl <- getURL(urlfile, ssl.verifypeer=FALSE)

# treat the text data as a stream so we can read from it
conn <- textConnection(dl)

# parse the downloaded data as UTF-16
bos <- read.table(conn, header = FALSE, fileEncoding="UTF-16", stringsAsFactors = FALSE)

# set column names (sourced from https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names/)
colnames(bos) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")

# 1. CRIM      per capita crime rate by town
# 2. ZN        proportion of residential land zoned for lots over 
# 25,000 sq.ft.
# 3. INDUS     proportion of non-retail business acres per town
# 4. CHAS      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
# 5. NOX       nitric oxides concentration (parts per 10 million)
# 6. RM        average number of rooms per dwelling
# 7. AGE       proportion of owner-occupied units built prior to 1940
# 8. DIS       weighted distances to five Boston employment centres
# 9. RAD       index of accessibility to radial highways
# 10. TAX      full-value property-tax rate per $10,000
# 11. PTRATIO  pupil-teacher ratio by town
# 12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# 13. LSTAT    % lower status of the population
# 14. MEDV     Median value of owner-occupied homes in $1000's

#####################################################################################################################
##  Exploratory Data Analysis

response <- "MEDV"
features <- colnames(bos)[which(!colnames(bos) %in% response)]
  
# view first few rows
head(bos)

summary(bos)

cor(bos)

# histogram of response variable
hist(d$MEDV, nclass = 25, col = 'blue')

# add more plotting


#####################################################################################################################
# Fit some ols regression models

m1 <- lm(MEDV ~ CRIM + ZN, data = bos); summary(m1)
m2 <- lm(paste0(response, "~", paste0(features, collapse = "+")), data = bos); summary(m2)

