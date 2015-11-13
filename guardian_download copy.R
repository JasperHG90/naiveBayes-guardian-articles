library(httr)
library(rjson)
library(lubridate)
sections <- c("sport", "world_news", "politics", "travel")
# Fields
# Create url for each desk
urls <- lapply(sections, function(x) {
  paste0("http://content.guardianapis.com/search?section=", x, "&order-by=newest&show-fields=body&page-size=50&api-key=t85ewprkpps2mx7pua8uhyf2")
})

# Write a function to get results
getRes <- function(url, pageNum) {
  # If pagenum is > 1, then add
  if(pageNum > 1) {
    url <- paste0(url, "&page=", pageNum)
  }
  # Query
  res <- content(GET(url), "parsed")
  # Control
  if(length(res) == 0) {
    warning("Empty result. Returning NULL.")
    return(NULL)
  }
  # Simmer down
  res <- res[["response"]]$results
  # Return
  return(res)
}

# Main function
mainCall <- function(url, pages = 30) {
  # master
  master <- vector("list", pages)
  # Loop
  for(page in 1:pages) {
    temp <- getRes(url, page)
    # add
    master[[page]] <- temp
    # zZzZzZzZ
    #Sys.sleep(0.5)
  }
  # Return
  return(master)
}

# For each url, call Main
data <- lapply(urls, function(x) {
  
  '
  Main
  '
  
  print(paste0("Section ", x))
  # Set limit (3000 articles)
  upperLim <- 1500
  limitPP <- 50 
  # Number of calls needed
  calls <- upperLim / (limitPP)
  # Run main
  main <- mainCall(x, calls)
  # Get name
  nam <- gsub("http://content.guardianapis.com/search?section=",
       "",
       unlist(strsplit(urls[[1]], "&order", fixed=T))[1],
       fixed=T)
  # Write to disk
  save(main, file = paste0(nam,"_guardian.Rdata"))
})

# Load all
folder <- paste0(getwd(), "/guardian_data/")
files <- paste0(folder, list.files(folder))
# Vector
master <- vector("list", length(files))
# Loop
for(x in 1:length(files)){
  load(file=files[x])
  master[[x]] <- main
}

# To data frame
library(plyr)
library(data.table)
# Function to turn json into df
toDF <- function(json) {
  # return in df format
  res <- lapply(json, function(x){
    # lapply
    res <- lapply(x, function(b) {
      # lapply
      tmp <- lapply(b, function(y) {
        striphtml <- function(htmlString) {
          return(gsub("<.*?>", "", htmlString))
        }
        # Return fields
        y$fields <- striphtml(y$fields$body)
        temp <- lapply(y, Filter, f = Negate(is.null))
        temp <- list("url" = ifelse(length(temp$webUrl) > 0, temp$webUrl, NA),
                     "headline" = ifelse(length(temp$webTitle) > 0, temp$webTitle, NA),
                     "body" = ifelse(length(temp$fields) > 0, temp$fields, NA),
                     "section" = ifelse(length(temp$sectionName) > 0, temp$sectionName, NA))
        # To ASCII
        temp$body <- iconv(temp$body, "latin1", "ASCII", sub="")
        # Return
        return(temp)
      })
      # Bind
      return(rbindlist(tmp, fill=T))
    })
    return(rbindlist(res, fill=T))
  })
  return(rbindlist(res, fill=T))
}

# Convert variables
res$section <- as.factor(res$section)

# Object name
final.data <- res

# Save as Rdata
save(final.data, file = "guardian_final.Rdata")
