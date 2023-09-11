



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
    
    # Exclude and remove rows with 3 values or NAs or 0s
    extracted_data <- extracted_data %>%
      filter(!rowSums(is.na(.)) >= 3, !rowSums(. == 0) >= 3)
    
    # Remove the first row from the extracted data
    extracted_data <- extracted_data[-1, ]
    
    # Store the extracted and cleaned data frame in the list
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


Jan97 <- sheet_data[["Jan97"]]
# Add the "year" column with rows as 1997
Jan97$year <- 1997
Jan97 <- pivot_longer(Jan97,
                             names_to = "rep",
                             cols = starts_with("C"),
                             values_to = "abundance")
Jan97 <- Jan97 %>%
  mutate(rep = case_when(
    rep == "C1_abun" ~ "C1",
    rep == "C2_abun" ~ "C2",
    rep == "C3_abun" ~ "C3",
    TRUE ~ rep  # Keep other values as they are
  ))

Jan97 <- Jan97 %>% filter(abundance!= "0")
Jan97$abundance <- as.numeric(Jan97$abundance)
tail(Jan97)


# -------------------------------------------------------------------------
Feb97 <- sheet_data[["Feb97"]]
# Add the "year" column with rows as 1997
Feb97$year <- 1998
Feb97 <- pivot_longer(Feb97,
                      names_to = "rep",
                      cols = starts_with("C"),
                      values_to = "abundance")

Feb97 <- Feb97 %>%
  mutate(rep = case_when(
    rep == "C1_abun" ~ "C1",
    rep == "C2_abun" ~ "C2",
    rep == "C3_abun" ~ "C3",
    TRUE ~ rep  # Keep other values as they are
  ))


Feb97 <- Feb97 %>% filter(abundance!= "0")
Feb97$abundance <- as.numeric(Feb97$abundance)
head(Feb97)


# -------------------------------------------------------------------------
Mar97 <- sheet_data[["Mar97"]]
# Add the "year" column with rows as 1997
Mar97$year <- 1998
Mar97 <- pivot_longer(Mar97,
                      names_to = "rep",
                      cols = starts_with("C"),
                      values_to = "abundance")

Mar97 <- Mar97 %>%
  mutate(rep = case_when(
    rep == "C1_abun" ~ "C1",
    rep == "C2_abun" ~ "C2",
    rep == "C3_abun" ~ "C3",
    TRUE ~ rep  # Keep other values as they are
  ))


Mar97 <- Mar97 %>% filter(abundance!= "0")
Mar97$abundance <- as.numeric(Mar97$abundance)
head(Mar97)



# Assuming you have data frames Feb97 and Jan97
merged_df <- bind_rows(Jan97,Feb97,Mar97)

laselva <- turnover(df = merged_df, 
                         time.var = "year", 
                         species.var = "taxa", 
                         abundance.var = "abundance", 
                         replicate.var = "rep")
ggplot(laselva, aes(x=year, y=total, color=rep)) + 
  geom_line(size = 2) + theme_bw()



laselva_agg <- turnover(df = merged_df, 
                         time.var = "year", 
                         species.var = "taxa", 
                         abundance.var = "abundance", 
                         replicate.var = "rep")

laselva_app <- turnover(df = merged_df, 
                           time.var = "year",
                           species.var = "taxa",
                           abundance.var = "abundance",
                           replicate.var = "rep",
                           metric = "appearance")

laselva_appearance <- turnover(df = merged_df, 
                               time.var = "year",
                               species.var = "taxa",
                               abundance.var = "abundance",
                               replicate.var = "rep",
                               metric = "disappearance")


ggplot(laselva_app, aes(x=year, y=turnover, color=rep)) + 
geom_line(size = 2) + theme_bw() + facet_wrap(~metric)

https://rdrr.io/cran/codyn/f/vignettes/Temporal_Diversity_Indices.Rmd

