#Load libraries

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(stringr)
library(DT)
library(shinyjs)
library(readxl)
library(writexl)
library(data.table)
library(cowplot)
library(rebus)
library(kableExtra)
library(readr)

## *Side note: Change library path to make sure that i can use old packages*: 
## Current library path: "C:/Users/la000062/OneDrive - Defra/Migrated Data/R/win-library/4.0" "C:/Program Files/R/R-4.2.2/library"
## Changed to old library path: "C:/Users/la000062/OneDrive - Defra/Migrated Data/R/win-library/4.0"
## .libPaths()
## alternative: tell R to look for your library folder in a specific location using lib.loc

#Step 1

## Import the "import" detail data
#Specify the formatting as numbers, then characters

imp_list <- list.files(path = "Raw data/Imports/", pattern = ".txt", full.names = TRUE)

readin <- function(df) {
  x <- read_delim(df,delim="\t", escape_double = FALSE, col_names = FALSE,
                  show_col_types = TRUE, trim_ws= TRUE,
                  col_types = "nnccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")
  return(x)
}

#Apply that function to all the files in the read in list
impdatalist <- lapply(imp_list,readin)

#Join them into one dataframe
imports <- do.call("rbind",impdatalist)%>%
  mutate(flow = "imports")

#Check the class
sapply(imports,class)

#Pivot longer so each code per business has a unique entry
imports <- pivot_longer(imports, cols = 10:59, names_to ="cols", values_to = "comcode")%>%
  filter(!is.na(comcode))%>%
  select(-cols)

#Step 2

## Import the "export" detail data

exp_list <- list.files(path = "Raw data/Exports/", pattern = ".txt", full.names = TRUE)

expdatalist <- lapply(exp_list,readin)

exports <- do.call("rbind",expdatalist)%>%
  mutate(flow = "exports")

sapply(exports,class)

#Pivot longer so each code per business has a unique entry
exports <- pivot_longer(exports, cols = 10:59, names_to ="cols", values_to = "comcode")%>%
  filter(!is.na(comcode))%>%
  select(-cols)

##Step 3 - merge "imports" and "exports" together and format

#Join them, rename columns and drop those beyond HS1-23
Business_data <- do.call("rbind", list(imports, exports))%>%
  rename(month = X1, moncomplines = X2,
         compname = X3, address1 = X4,
         address2 = X5, address3= X6,
         address4= X7, address5= X8,
         postcode = X9)%>%
  mutate(HS2= substr(comcode,1 ,2))%>%
  filter(HS2 < 24)%>%
  filter(!is.na(comcode))

#Create an address field by bringing together the other 5 into one column.
Business_data <- Business_data %>%
  unite(., col = "address", address1, address2, address3, address4, address5, na.rm= TRUE,sep = ", ")  

##Step 4 - drop "moncomplines" adn condense the different months into one row

Business_data <- Business_data %>%
  select(month,HS2,comcode,compname,address,postcode,flow)

Business_data<- Business_data %>%
  group_by(HS2, comcode, compname, address, postcode, flow)%>%
  summarise(vlist = paste0(month, collapse = ", "))%>%
  ungroup()

Business_data<- Business_data %>%
  rename("Period"="vlist")

##Step 5 - Import postcode data and merge with the business data

## Import postcode coordinates (note files are in RDS format)

Postcode_data <- readRDS("Postcode_data.RDS")

## Change postcode name in column 6

colnames(Postcode_data)[6] <- "postcode"

## Bind the two datasets together to create dataframe for maps

Business_map <- left_join(Business_data,Postcode_data, by = "postcode")

##Converting any non-ACSII characters

Business_map$compname <- str_replace_all(Business_map$compname,"[^[:ascii:]]", "")

## load sector data

sector_data <- read_csv("BasicCompany/BasicCompanyDataAsOneFile-2024-02-07.csv")


## keep specific columns  in sector data

sector_data <- sector_data %>%
  select("CompanyName","RegAddress.AddressLine1","RegAddress.AddressLine2","RegAddress.PostTown","RegAddress.County","RegAddress.Country","RegAddress.PostCode","CountryOfOrigin","SICCode.SicText_1","SICCode.SicText_2","SICCode.SicText_3","SICCode.SicText_4")

## merge the address columns and the SIC codes

sector_data <- sector_data %>%
  unite(., col = "address", RegAddress.AddressLine1,RegAddress.AddressLine2,RegAddress.PostTown,RegAddress.County,RegAddress.Country, na.rm= TRUE,sep = ", ")  

## and the SIC codes

sector_data <- sector_data %>%
  unite(., col = "SICCode",SICCode.SicText_1,SICCode.SicText_2,SICCode.SicText_3,SICCode.SicText_4, na.rm= TRUE,sep = ", ")  

## Change Postcode name

sector_data <- sector_data %>%
  rename(c("postcode"="RegAddress.PostCode","compname"="CompanyName"))

# Select only the necessary columns from sector_data
sector_data_selected <- sector_data %>%
  select(postcode, SICCode, address) %>%
  distinct(postcode,address,.keep_all = TRUE) 

# Perform the left join with Business_map
Business_final <- left_join(Business_map, sector_data_selected, by = c("postcode","address"))

## Move the SICCode next to the postcode
Business_final <- Business_final %>%
  relocate(SICCode, .after = postcode) 

# Isolate rows with NA in SICCode
na_rows <- Business_final %>%
  filter(is.na(SICCode))

# Select necessary columns for the second join, ensuring uniqueness by business name
sector_data_by_name <- sector_data %>%
  select(compname, SICCode, postcode) %>%
  distinct(compname, postcode, .keep_all = TRUE)

# Perform a left join on the NA rows using business_name

na_rows_filled <- na_rows %>%
  left_join(sector_data_by_name, by = c("compname", "postcode"), suffix = c("", "_from_name")) %>%
  # Replace SICCode with SICCode_from_name where it is available
  mutate(SICCode = coalesce(SICCode_from_name, SICCode)) %>%
  # Remove the SICCode_from_name column now that it is no longer needed
  select(-SICCode_from_name)

# Combine the filled NA rows back with the non-NA rows from the original join
Business_final <- Business_final %>%
  filter(!is.na(SICCode)) %>%  # Keep rows with filled SICCode from first join
  bind_rows(na_rows_filled)

# writexl::write_xlsx(Business_map,paste0("Business Map", Sys.Date(),".xlsx"))

saveRDS(Business_final,paste0("Business Map", Sys.Date(),".RDS"))


