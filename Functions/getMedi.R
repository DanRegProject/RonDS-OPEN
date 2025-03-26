library(dplyr)
library(lubridate)

# Function to process medications based on a list and conditions
getMedi <- function(outlib, medlist, basedata=NULL, basepop=NULL, fromyear=1994) {
  # Ensure unique and lowercase list of medications
  medlist <- unique(tolower(unlist(strsplit(medlist, " "))))
  
  # Loop over each medication in the list
  for (medi in medlist) {
    # Extract relevant data for each medication
    findingMedi(paste0(outlib, ".LMDB", medi, ".ALL"), medi, get(paste0("ATC", medi)), basedata=basedata, fromyear=fromyear)
  
    # If a population dataset is specified, integrate medication info into it
    if (!is.null(basepop)) {
      combined_data <- bind_rows(lapply(medlist, function(medi) {
        # Access the medication dataset and select relevant columns
        outdata <- get(paste0(outlib, ".LMDB", medi, ".ALL"))
        outdata %>% select(pnr, eksd, drug=medi, rec_in, rec_out)
      }))
      
      # Assign combined data to output variable and sort appropriately
      assign(paste0(outlib, ".", basepop), combined_data)
      combined_data <- combined_data %>% arrange(pnr, eksd, drug, rec_out)
    }
  }
}

# Function to find medication data based on ATC codes and other criteria
findingMedi <- function(outdata, drug, atc, basedata=NULL, fromyear=NULL) {
  # Split ATC codes into a list for processing
  atc_list <- unlist(strsplit(atc, " "))
  
  # Filter to get distinct valid medication records
  lmdb_data <- raw_lms_laegemiddeloplysninger %>%
    filter(!is.na(volume) | !is.na(packsize)) %>%
    arrange(vnr) %>%
    distinct(vnr)
  
  # Determine the latest year available for dataset
  lastyr <- year(Sys.Date())
  while (!exists(paste0("raw.lms_epikur", lastyr)) && lastyr > 1990) {
    lastyr <- lastyr - 1
  }
  
  # Adjust year variables if the setup uses old year settings
  if (lastyr == 1990) {
    fromyear <- 0
    lastyr <- 0
  }
  
  # Loop through available years and process datasets accordingly
  for (y in fromyear:lastyr) {
    yy <- ifelse(y == 0, "", y)
    lms_epikur_yy <- get(paste0("raw.lms_epikur", yy))
    
    # Perform an inner join with population data if provided
    if (!is.null(basedata)) {
      basedata_df <- get(basedata)
      merged_data <- inner_join(basedata_df, lms_epikur_yy, by = c("pnr" = "cpr_encrypted"))
    } else {
      merged_data <- lms_epikur_yy
    }
    
    # Filter data according to ATC codes and select specific columns
    merged_data <- merged_data %>%
      filter(atc %in% atc_list) %>%
      select(pnr, eksd, atc, apk, packsize, volume, voltypetxt, strnum, strunit,
             rec_in=ifelse(!is.null(get("rec_in")), rec_in, mdy(01,01,1994)),
             rec_out=ifelse(!is.null(get("rec_out")), rec_out, mdy(12,31,2099))) %>%
      arrange(pnr, drug=atc, eksd)
    
    # Assign processed data to the output variable
    assign(outdata, merged_data)
    
    # Calculate and display execution time for tracking performance
    exec_time <- Sys.time() - start_meditime
    print(paste0("Execution time for FindingMedi: ", exec_time))
  }
}
