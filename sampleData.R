# Clean wd
rm(list=ls())
# Load final data
load(file="/users/jasper/documents/github.projects/blog_naiveBayes/guardian_final.Rdata")
# To df
final.data <- as.data.frame(final.data)
final.data <- unique(final.data)
# Rearrange
final.data <- final.data[,c(1,4,2,3)]
# Function to fix whitespace
whiteSpaceFix <- function(string) {
  # Strip punctuation
  temp <- gsub("[[:punct:]]", "", string)
  # Take sentence apart
  temp <- unlist(strsplit(temp, " "))
  # Control statement. If the result of the above is an empty character, then return NULL
  if(length(temp) == 0) {
    # Print message
    print("Empty character. Moving on . . . ")
    # Return empty character
    return("")
  } else{
    # Take out whitespaces
    temp <- temp[sapply(temp, function(b) b != "")]
    # Reconstruct and take out punctuation + newlines etc.
    checkF <- function(z) grepl("[[:punct:]]", z) | grepl("[\r\n\t]", z)
    temp <- temp[!checkF(temp) == TRUE]
    # Paste & collapse
    paste0(temp, collapse = " ") 
  }
}
# Set character encoding
final.data$body <- iconv(final.data$body, "latin1", "ASCII", sub="")
final.data$headline <- iconv(final.data$headline, "latin1", "ASCII", sub="")
# Take out NA's
final.data <- final.data[!is.na(final.data$body), ]
final.data <- final.data[!is.na(final.data$headline), ]
final.data <- final.data[!nchar(final.data$headline) < 2, ]
final.data <- final.data[!nchar(final.data$body) < 2, ]
# whitespaces
final.data$body <- sapply(final.data$body, whiteSpaceFix)
final.data$headline <- sapply(final.data$headline, whiteSpaceFix)
# Shuffle rows
set.seed(215)
final.data <- final.data[sample(nrow(final.data)),]
# Split data
library(caret)
set.seed(9)
index <- createDataPartition(final.data$section, p=0.9, list=F)
# Training data
samp <- final.data[index,]
summary(samp$section)
# Write to file
write.table(samp, file = "/users/jasper/documents/github.projects/blog_naiveBayes/train.txt", sep="\t", row.names=F, col.names = F, fileEncoding = 'UTF-8')

# Test data
sampT <- final.data[-index,]
write.table(sampT, file = "/users/jasper/documents/github.projects/blog_naiveBayes/test.txt", sep="\t", row.names=F, col.names = F)

