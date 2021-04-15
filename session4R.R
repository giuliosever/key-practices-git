# ------- Header ------------------------------------------------------------------####
# Author:      Giulio Severijnen
# Date:        13-4-2021
# Description: Script used to prepare the dataset of the production experiment. 
#              As input, it takes the acoustic measurements from Praat.
#              The script then cleans the data, transforms the raw measurements and saves an output file
# Project:     Production experiment

#---------------------------------------------------------------------------------#### 


# ------- Logbook ----------------------------------------------------------------####
# To do:
# Vowels:  Change labels in CSV
# Add CSV with pitch settings

#---------------------------------------------------------------------------------####




#--------- Load packages ---------------------------------------------####

# Load the required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(here)

# -----------------------------------------------------------------------------------####

#------- Load data -----------------------------------------------------------####

# This reads in the datafiles
# First, we load in the measurements taken over the entire syllable --> dataSyll

resultsDir <- here::here()
fileList <- list.files(pattern = ".txt")

require(data.table)  # for the fread() function which allows to read in the final column containing full sentences

check = 0
for (file in fileList){
  
  # if the loop is at the first file, create the dataset
  # else append to it
  
  
  if (check == 0){
    print(file)
    dataSyll <- fread(file.path(path = resultsdir, file), header=TRUE)
    
    check = check + 1
    #curppid <- as.numeric(strsplit(file, "/")[[1]][1])
    #d$ppid <- curppid
    
  } else {
    print(file)
    tmp <- fread(file.path(path = resultsDir, file), header=TRUE)
    
    check = check + 1
    #curppid <- as.numeric(strsplit(file, "/")[[1]][1])
    #tmp$ppid <- curppid
    
    dataSyll <- rbind(dataSyll, tmp, fill = TRUE)
    rm(tmp)
    
  }
}

# Second, we load in the measurement taken from the vowel in each syllable --> dataVowel
# The vowel data is txt files in a different directory
resultsDir <- paste(resultsDir, "/Voweldata", sep = "")
fileList <- list.files(pattern = ".txt")

require(data.table)  # for the fread() function which allows to read in the final column containing full sentences

check = 0
for (file in fileList){
  
  # if the loop is at the first file, create the dataset
  # else append to it
  
  
  if (check == 0){
    print(file)
    dataVowel <- fread(file.path(path = resultsDir, file), header=TRUE)
    
    check = check + 1
    #curppid <- as.numeric(strsplit(file, "/")[[1]][1])
    #d$ppid <- curppid
    
  } else {
    print(file)
    tmp <- fread(file.path(path = resultsdir, file), header=TRUE)
    
    check = check + 1
    #curppid <- as.numeric(strsplit(file, "/")[[1]][1])
    #tmp$ppid <- curppid
    
    dataVowel<- rbind(dataVowel, tmp, fill = TRUE)
    rm(tmp)
    
  }
}


head(dataSyll)
tail(dataSyll)

head(dataVowel)
tail(dataVowel)

#----------------------------------------------------------------------####




#-------  Clean data --------------------------------------------------####
# The variables in the dataframe aren't all loaded in the correct format, change this: 
dataSyll <- dataSyll %>%
  mutate(f0 = as.numeric(as.character(f0)),
         f0Max = as.numeric(as.character(foMax)),
         f0Min = as.numeric(as.character(foMin)),
         f0Onset = as.numeric(as.character(f0Onset)),
         f0Offset = as.numeric(as.character(f0Offset)),
         wavfile = as.character(wavfile),
         syll = as.factor(syll),
         cat = as.factor(cat),
         cond = as.factor(cond))


# F0 measurements that cannot be estimated in Praat contain 999 as output.
# To not have these influence our stats, change to NA.
dataSyll[dataSyll == 999] <- NA

# Now we want to add information about the target word, stress pattern, and the repetition in the experiment to the dataframe.
# This information is in the wavfile so we will extract it from there.

# First, add columns of interest to the dataframe
dataSyll$word <- 0
dataSyll$stress <- 0
dataSyll$rep <- 0

# This piece of code loops through the dataframe and stores each part of the wavfile in different vectors. 
# Afterwards, eadh vector is assigned to the corresponding column.
dataSyll$wavfile <- as.character(dataSyll$wavfile)
nrows <- dim(dataSyll)[1]


# First, we want to split the strings based on the "_"
aux <- strsplit(dataSyll$wavfile,"_")

tmp <- NULL
newtmp <- NULL
oldtmp <- NULL
tmp2 <- NULL
newtmp2 <- NULL
oldtmp2 <- NULL
tmp3 <- NULL
newtmp3 <- NULL
oldtmp3 <- NULL

# aux contains a list with subparts of the string. Make sure you select the correct part [1], [2], or [3] etc. to be stored in the correct tmp variable
for (i in 1:nrows ) {
  tmp = aux[[i]][1]
  newtmp = c(oldtmp, tmp)
  oldtmp = newtmp
  
  tmp2 = aux[[i]][2]
  newtmp2 = c(oldtmp2, tmp2)
  oldtmp2 = newtmp2
  
  tmp3 = aux[[i]][4]
  newtmp3 = c(oldtmp3, tmp3)
  oldtmp3 = newtmp3
  
}

dataSyll$word <- substr(newtmp, 5, nchar(newtmp))
dataSyll$stress <- newtmp2
dataSyll$rep <- as.factor(substr(newtmp3, 4, 4))


# Now, we want to do clean the voweldata separately


dataVowel <- dataVowel %>%
  mutate(syll = as.factor(syll),
         ppt = as.integer(ppt))


# The voweldata has different condition labels, change these so they are compatible with the other dataframe
dataVowel[dataVowel$cond == "_iso"]$cond <- "iso"
dataVowel[dataVowel$cond == "_acc"]$cond <- "acc"
dataVowel[dataVowel$cond == "nacc"]$cond <- "nonacc"

# Similar to the original data, add information about word, stress and repetition
# Add columns of interest

dataVowel$word <- 0
dataVowel$stress <- 0
dataVowel$rep <- 0

dataVowel$wavfile <- as.character(dataVowel$wavfile)
nrows <- dim(dataVowel)[1]
aux <- strsplit(dataVowel$wavfile,"_")
tmp <- NULL
newtmp <- NULL
oldtmp <- NULL
tmp2 <- NULL
newtmp2 <- NULL
oldtmp2 <- NULL
tmp3 <- NULL
newtmp3 <- NULL
oldtmp3 <- NULL


for (i in 1:nrows ) {
  tmp = aux[[i]][1]
  newtmp = c(oldtmp, tmp)
  oldtmp = newtmp
  
  tmp2 = aux[[i]][2]
  newtmp2 = c(oldtmp2, tmp2)
  oldtmp2 = newtmp2
  
  tmp3 = aux[[i]][4]
  newtmp3 = c(oldtmp3, tmp3)
  oldtmp3 = newtmp3
  
}

dataVowel$word <- substr(newtmp, 5, nchar(newtmp))
dataVowel$stress <- newtmp2
dataVowel$rep <- as.factor(substr(newtmp3, 4, 4))


# Merge the data with voweldata

# We don't need two variables indicating wavfile
dataVowel[,c("wavfile")] <- NULL

# The following variables have to originate from data_vowel so remove them from data
dataSyll[,c("slope", "f1", "f2", "f3", "f4")] <- NULL


dataComplete <- merge(dataSyll, dataVowel)


# Remove the columns that I don't need for the analyses
dataComplete <- dataComplete%>%
  select(-c("b1", "b2", "b3", "b4", "slope", "f0Onset", "f0Offset"))

#----------------------------------------------------------------------------####
