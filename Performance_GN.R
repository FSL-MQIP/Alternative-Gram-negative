###########
#File: Performance_GN.R
#
#Written by: Sarah I. Murphy
#
#Edited: October 15, 2019
#
#This analysis treats each individual sample as unique (regardless of shelf life day)
###########

#load packages
library(readr)
library(dplyr)

##load data
df10 <- read_csv("raw_data/D10_VSL464.csv")
df14 <- read_csv("raw_data/D14_VSL464.csv")
df17 <- read_csv("raw_data/D17_VSL464.csv")
df21 <- read_csv("raw_data/D21_VSL464.csv")

#join data
df <- rbind(df10,df14,df17,df21)

##adjust values so they are in 0,1 format where 0 is negative and 1 is positive
df[df=="--"] <- 0
df[df=="-"] <- 0
df[is.na(df)] <- 0
df[df=="++"] <- 1
df[df=="-+"] <- 1
df[df=="+-"] <- 1
df[df=="+"] <- 1

rm(df10,df14,df17,df21)


#remove where all other tests are NT; this also effectively removed any row without CVTA
dfc<-df[!(c(df$COLI_COLI_24 =="NT" & 
              df$COLI_NON_24 == "NT" &
              df$COLI_COLI_48 == "NT" &
              df$COLI_NON_48 == "NT" &
              df$EB_COLI_24 == "NT" &
              df$EB_NON_24 == "NT" &
              df$EB_COLI_48 == "NT" &
              df$EB_NON_48 == "NT")),]

## convert all classes to factor
columns <- (colnames(dfc))
dfc[columns] <- lapply(dfc[columns], factor) 

## put the performance measures for each test in tables and save .csv
library(caret)

#
cms_full <- NULL
for( column in names(dfc[4:11]) ) {
  print(column)
  
  {
    cm <- confusionMatrix(dfc[[column]], reference = dfc$CVTA, positive = "1")
    cms <- data.frame(cbind(t(cm$overall),t(cm$byClass))) 
    cms["test"] <- column
    
  } 
  cms_full <- rbind(cms,cms_full)
}

rm(cm,cms) #clean up environment