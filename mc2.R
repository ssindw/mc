library(readxl) 
library(rvest)
library(XML)
library(stringr)

#Get the xls from the zipped file
sec_dataframe <- function() {

        # Scrape the webpage for for the zipped file links        
        pg <- read_html("https://www.sec.gov/help/foiadocsinvafoiahtm.html")
        fileLinks <- pg %>% html_nodes("a") %>% html_attr("href")
        fileLinks <- as.character(fileLinks)
        zipLinks <- fileLinks[substring(fileLinks, nchar(fileLinks)- 3, 
                                                        nchar(fileLinks)) == ".zip"]
        zipLinks <- zipLinks[!is.na(zipLinks)]
        #-----------------------------------------------------------------------
        
        # Return the dataframe of the following structure 
        # File_URL|    Date        |      type
        df <- data.frame(file_URL = as.character(),
                         date = as.character(),
                         type = as.character())
        
        domain <- "https://www.sec.gov"
        
        for(i in 1:length(zipLinks)){
        
                file_URL <- paste(domain,zipLinks[i], sep="")   
                
                loc <- str_locate_all(pattern ='/ia', zipLinks[i])
                date <- substr(zipLinks[i],loc[[1]][1,2]+1, nchar(zipLinks[i])-4)

                loc2 <- str_locate_all(pattern ='/ia', zipLinks[i])            
                type <- ifelse(substr(zipLinks[i], loc2[[1]][1,1]-6, loc2[[1]][1,1]-1)
                               =="exempt", "exempt","non-exempt")
                localDf <- data.frame(file_URL,date,type)
                df <- rbind(df, localDf)
        }
        #-----------------------------------------------------------------------
        df              
}

# Create the data frame
get_sec_zip_by_period <- function(){

        # Extract the excel from the given zipped file
        Sys.setlocale("LC_ALL", "C")
        loc.url <- "https://www.sec.gov/foia/iareports/ia040317.zip"
        td <- tempdir() 
        tf <- tempfile(tmpdir=td, fileext=".zip") 
        download.file(loc.url, tf) 
        fname <- unzip(tf, list=TRUE)$Name[1] 
        unzip(tf, files=fname, exdir=td, overwrite=TRUE) 
        fpath <- file.path(td, fname)
        data <- as.data.frame(read_excel(fpath, sheet = 1))
}

## Run the functions...
files_df <- sec_dataframe()
dat <- get_sec_zip_by_period()


# Analysis----------------------------------------------------------------------
# Determine Top 15 Asset Managers by AUM (Assets Under Management) - Calculate 
# distribution of Assets Under Management, Employees and Managers by State
# From https://www.sec.gov/about/forms/formadv-part1a.pdf
# state - Column -> 3C-State
# Employees = 5A
library(dplyr)

dat$`5A`[is.na(dat$`5A`)] <- 0
dat$`3C-State`[is.na(dat$`3C-State`)] <- "Not US"
analysis <- dat %>%
                group_by(`3C-State`) %>%
                summarise(tot_emp = sum(as.numeric('5A')))


# JSON Blackstone--------------------------------------------------------------------
library(rjson)
jsonData = fromJSON(file="https://doppler.finra.org/doppler-lookup/api/v1/search/firms?hl=true&nrows=99000&query=Blackstone&r=2500&wt=json")

jasonDF <- data.frame(bc_firm_name = as.character(),
                      bc_source_id = as.integer())

length(jsonData[3][[1]]$BROKER_CHECK_FIRM$results)

for(i in 1: length(jsonData[3][[1]]$BROKER_CHECK_FIRM$results)){
                if(jsonData[3][[1]]$BROKER_CHECK_FIRM$results[[i]]$fields$score > 0.4){
                bc_firm_name <- jsonData[3][[1]]$BROKER_CHECK_FIRM$results[[i]]$fields$bc_firm_name
                bc_source_id <- jsonData[3][[1]]$BROKER_CHECK_FIRM$results[[i]]$fields$bc_source_id
                jasonDF <- rbind(jasonDF, data.frame(bc_firm_name, bc_source_id)) 
        }
}        

 



