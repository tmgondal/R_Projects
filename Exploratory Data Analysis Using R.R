


#Import Data
dataset <- read.csv(file="D:\\stirling\\but\\a3\\L_E_D.csv")
#Print Data
dataset

# Print column names
column_names <- names(dataset)
print(column_names)

# Print column data types
column_types <- sapply(dataset, class)
print(column_types)

