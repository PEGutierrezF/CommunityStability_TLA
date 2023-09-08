



# ---------------------------------------------
# Summarize Carapa-60 Abundance
# 07 Sep 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# Define the file path with double quotes and forward slashes
file_path <- "raw_data/Carapa 60_1997-2016.xlsx"

# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets(file_path)

# Initialize an empty list to store the data frames
sheet_data <- list()

# Initialize empty data frames to store the extracted columns
extracted_dataframes <- list()

# Define the columns you want to extract
columns_to_extract <- c("taxa", "C1_abun", "C2_abun", "C3_abun")









# Loop through each sheet and extract the specified columns
for (sheet_name in sheet_names) {
  # Read the sheet into a data frame
  sheet <- read_excel(file_path, sheet = sheet_name)
  
  # Check if the specified columns exist in the sheet
  if (all(columns_to_extract %in% colnames(sheet))) {
    # Define the number of rows to remove from the end
    rows_to_remove <- 7
    
    # Extract the specified columns if they exist
    extracted_data <- sheet[, columns_to_extract, drop = FALSE]
    
    # Remove the last 7 rows from the extracted data
    extracted_data <- head(extracted_data, -rows_to_remove)
    
    # Store the extracted data frame in the list
    sheet_data[[sheet_name]] <- extracted_data
  } else {
    # If one or more columns don't exist, you can handle it as needed.
    # For example, you can skip this sheet or add a placeholder data frame.
    sheet_data[[sheet_name]] <- data.frame()
  }
  
  # Print the column names in the current sheet
  cat("Sheet Name:", sheet_name, "\n")
  cat("Column Names:", colnames(sheet), "\n")
}

Nov97 <- sheet_data[["Nov97"]]


