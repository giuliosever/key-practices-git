<<<<<<< HEAD
# ------- Header ------------------------------------------------------------------####
# Author:      Giulio Severijnen
# Date:        13-4-2021
# Description: Script used to prepare the dataset of the production experiment. 
#              As input, it takes the acoustic measurements from Praat.
#              The script then cleans the data, transforms the raw measurements and saves an output file
# Project:     Production experiment

#---------------------------------------------------------------------------------#### 


# ------- Logbook ----------------------------------------------------------------####
=======
# Script written by Giulio Severijnen, January 2021

>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
# To do:
# Vowels:  Change labels in CSV
# Add CSV with pitch settings

<<<<<<< HEAD
#---------------------------------------------------------------------------------####




#--------- Load packages ---------------------------------------------####

# Load the required packages
=======

# Data preparation Suprasegmental production experiment
# As input, each individual acoustic txt. file is taken. These are cleaned, additional columns are added and output files are saved


##############################################
#--------- Initialize values, packages ---------------------------------------------####

# Packages
>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)
<<<<<<< HEAD
library(here)

# -----------------------------------------------------------------------------------####

#------- Load data -----------------------------------------------------------####

# This reads in the datafiles
# First, we load in the measurements taken over the entire syllable --> dataSyll

resultsDir <- here::here()
fileList <- list.files(pattern = ".txt")
=======

# Functions
#-----------------------------------------------------------------------------------####
# This reads in the datafiles
setwd("\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/Outputfiles/")
resultsdir <- getwd()
filelist <- list.files(pattern = ".txt")
>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36

require(data.table)  # for the fread() function which allows to read in the final column containing full sentences

check = 0
<<<<<<< HEAD
for (file in fileList){
=======
for (file in filelist){
>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
  
  # if the loop is at the first file, create the dataset
  # else append to it
  
  
  if (check == 0){
    print(file)
<<<<<<< HEAD
    dataSyll <- fread(file.path(path = resultsdir, file), header=TRUE)
=======
    data <- fread(file.path(path = resultsdir, file), header=TRUE)
>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
    
    check = check + 1
    #curppid <- as.numeric(strsplit(file, "/")[[1]][1])
    #d$ppid <- curppid
    
  } else {
    print(file)
<<<<<<< HEAD
    tmp <- fread(file.path(path = resultsDir, file), header=TRUE)
=======
    tmp <- fread(file.path(path = resultsdir, file), header=TRUE)
>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
    
    check = check + 1
    #curppid <- as.numeric(strsplit(file, "/")[[1]][1])
    #tmp$ppid <- curppid
    
<<<<<<< HEAD
    dataSyll <- rbind(dataSyll, tmp, fill = TRUE)
=======
    data <- rbind(data, tmp, fill = TRUE)
>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
    rm(tmp)
    
  }
}

<<<<<<< HEAD
# Second, we load in the measurement taken from the vowel in each syllable --> dataVowel
# The vowel data is txt files in a different directory
resultsDir <- paste(resultsDir, "/Voweldata", sep = "")
fileList <- list.files(pattern = ".txt")
=======
### Now read in the vowel data
setwd("\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/Outputfiles/Voweldata/")
resultsdir <- getwd()
filelist <- list.files(pattern = ".txt")
>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36

require(data.table)  # for the fread() function which allows to read in the final column containing full sentences

check = 0
<<<<<<< HEAD
for (file in fileList){
=======
for (file in filelist){
>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
  
  # if the loop is at the first file, create the dataset
  # else append to it
  
  
  if (check == 0){
    print(file)
<<<<<<< HEAD
    dataVowel <- fread(file.path(path = resultsDir, file), header=TRUE)
=======
    data_vowel <- fread(file.path(path = resultsdir, file), header=TRUE)
>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
    
    check = check + 1
    #curppid <- as.numeric(strsplit(file, "/")[[1]][1])
    #d$ppid <- curppid
    
  } else {
    print(file)
    tmp <- fread(file.path(path = resultsdir, file), header=TRUE)
    
    check = check + 1
    #curppid <- as.numeric(strsplit(file, "/")[[1]][1])
    #tmp$ppid <- curppid
    
<<<<<<< HEAD
    dataVowel<- rbind(dataVowel, tmp, fill = TRUE)
=======
    data_vowel<- rbind(data_vowel, tmp, fill = TRUE)
>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
    rm(tmp)
    
  }
}


<<<<<<< HEAD
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

=======
head(data)



# Clean data
data$f0 <- as.numeric(as.character(data$f0))
data$f0Max <-  as.numeric(as.character(data$f0Max))
data$f0Min <-  as.numeric(as.character(data$f0Min))
data$f0Onset <-  as.numeric(as.character(data$f0Onset))
data$f0Offset <-  as.numeric(as.character(data$f0Offset))
data$wavfile <- as.character(data$wavfile)
data$syll <- as.factor(data$syll)
data$cat <- as.factor(data$cat)
data$cond <- as.factor(data$cond)

data[data == 999] <- NA

# Based on the wavfile, extract the word, stress pattern and repetition

#### Add columns of interest

data$word <- 0
data$stress <- 0
data$rep <- 0

data$wavfile <- as.character(data$wavfile)
nrows <- dim(data)[1]
aux <- strsplit(data$wavfile,"_")
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

data$word <- substr(newtmp, 5, nchar(newtmp))
data$stress <- newtmp2
data$rep <- as.factor(substr(newtmp3, 4, 4))

# Clean data_vowel
# Clean data
data_vowel$syll <- as.factor(data_vowel$syll)

#### Add columns of interest

data_vowel$word <- 0
data_vowel$stress <- 0
data_vowel$rep <- 0

data_vowel$wavfile <- as.character(data_vowel$wavfile)
nrows <- dim(data_vowel)[1]
aux <- strsplit(data_vowel$wavfile,"_")
>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
tmp <- NULL
newtmp <- NULL
oldtmp <- NULL
tmp2 <- NULL
newtmp2 <- NULL
oldtmp2 <- NULL
tmp3 <- NULL
newtmp3 <- NULL
oldtmp3 <- NULL

<<<<<<< HEAD
# aux contains a list with subparts of the string. Make sure you select the correct part [1], [2], or [3] etc. to be stored in the correct tmp variable
=======

>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
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

<<<<<<< HEAD
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
=======
data_vowel$word <- substr(newtmp, 5, nchar(newtmp))
data_vowel$stress <- newtmp2
data_vowel$rep <- as.factor(substr(newtmp3, 4, 4))
data_vowel$ppt <- as.integer(data_vowel$ppt)


# Now merge them
data_vowel[data_vowel$cond == "_iso"]$cond <- "iso"
data_vowel[data_vowel$cond == "_acc"]$cond <- "acc"
data_vowel[data_vowel$cond == "nacc"]$cond <- "nonacc"
data_vowel[,c("wavfile")] <- NULL

data[,c("slope", "f1", "f2", "f3", "f4")] <- NULL

data <- merge(data, data_vowel)


# Remove the columns that I don't need for the analyses
data <- data%>%
  select(-c("b1", "b2", "b3", "b4", "slope", "f0Onset", "f0Offset"))

##########################################
### Spectral balance
##########################################
# Input Tabuated vowel spectra and do a lm on the spectrum

setwd("\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/Outputfiles/VowelTables")
filelist <- list.files(pattern = ".Table")

>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
tmp <- NULL
newtmp <- NULL
oldtmp <- NULL
tmp2 <- NULL
newtmp2 <- NULL
oldtmp2 <- NULL
tmp3 <- NULL
newtmp3 <- NULL
oldtmp3 <- NULL
<<<<<<< HEAD


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
=======
tmp4 <- NULL
newtmp4 <- NULL
oldtmp4 <- NULL
tmp5 <- NULL
newtmp5 <- NULL
oldtmp5 <- NULL
tmp5 <- NULL
newtmp5 <- NULL
oldtmp5 <- NULL
tmp6 <- NULL
newtmp6 <- NULL
oldtmp6 <- NULL
tmp7 <- NULL
newtmp7 <- NULL
oldtmp7 <- NULL
tmp8 <- NULL
newtmp8 <- NULL
oldtmp8 <- NULL


for (i in 1:length(filelist)){
  wav <- filelist[i]
  aux <- strsplit(wav,"_")
  
  
  tmp2 = aux[[1]][1]
  newtmp2 = c(oldtmp2, tmp2)
  oldtmp2 = newtmp2
  
  tmp3 = aux[[1]][2]
  newtmp3 = c(oldtmp3, tmp3)
  oldtmp3 = newtmp3
  
  tmp4 = aux[[1]][3]
  newtmp4 = c(oldtmp4, tmp4)
  oldtmp4 = newtmp4
  
  tmp5 = aux[[1]][4]
  newtmp5 = c(oldtmp5, tmp5)
  oldtmp5 = newtmp5
  
  tmp6 = aux[[1]][5]
  newtmp6 = c(oldtmp6, tmp6)
  oldtmp6 = newtmp6
  
  vowel <- read.table(filelist[i], T)
  colnames(vowel) = c("frequency","power")
  vowel <- vowel[vowel$frequency <= 4000 & vowel$frequency > 0,]
  vowel$frequency_sem <- (12*log(vowel$frequency/50)/log(2)) # This does a semitone conversion
  
  # LM on raw measures
  myTilt.lm = lm(power~frequency, vowel)
  myObj = summary(myTilt.lm)
  tmp7 = myObj$coefficients[2,1]
  newtmp7 <- c(oldtmp7, tmp7)
  oldtmp7 <- newtmp7
  
  # LM on semitone measures
  myTilt_sem.lm = lm(power~frequency_sem, vowel)
  myObj_sem = summary(myTilt_sem.lm)
  tmp8 = myObj_sem$coefficients[2,1]
  newtmp8 <- c(oldtmp8, tmp8)
  oldtmp8 <- newtmp8
}

# Place in dataframe
slopes <- data.frame(ppt = substr(newtmp2, 1, 4), word = substr(newtmp2, 5, nchar(newtmp2)), cond = newtmp4, syll = substr(newtmp6, 1,1), stress = newtmp3, rep = substr(newtmp5, 4, 4), slope = newtmp7, slope_sem = newtmp8)
#slopes$cat <- ifelse((slopes$syll==1 & slopes$stress == "sw") | (slopes$syll==2 & slopes$stress == "ws"),"stressed","unstressed")

slopes <- slopes%>%
  mutate(ppt = as.integer(ppt),
         word = as.character(word),
         stress = as.factor(stress),
         rep = as.factor(rep))
data <- merge(data, slopes, by = c("ppt", "word", "cond", "syll", "stress", "rep"))

################################################
### Add vowel info
################################################

# First, create new variable for stressed vs. unstressed
data$cat <- ifelse((data$syll==1 & data$stress == "sw") | (data$syll==2 & data$stress == "ws"),"stressed","unstressed")
  
# Add column of minimal/temporary pairs
list_of_minimalpairs =list("aanvaart", "aanvaardt", "misbruik", "plato", "plateau", "voornaam", "canon", "kanon", "servisch", "servies")
data$type <- ifelse(data$word %in% list_of_minimalpairs, "minimal", "temp")

# Now, move on to the vowel info
# Add syllable of interest
sylloint <- read.csv("\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/Syll_of_interest.csv", header = TRUE)

#remapping <- c(A = "ɑ", a = "a", E = "ɛ", e = "e", i = "i", I = "ɪ", O = "ɔ", o = "o", u = "u", y = "y")# This puts in the IPA symbols and gives NA to diphtongs
#sylloint$vowel <- remapping[as.character(sylloint$vowel)]

sylloint$vowel <- as.character(sylloint$vowel)

sylloint <- sylloint%>%
  select("word", "syll_o_int", "vowel")%>%
  mutate(word = as.character(word))
data <- merge(data_vowels, sylloint, by = "word", all = TRUE)
#data[,c("vowel", "syll_o_int")]<- NULL
## Now add the vowels to minimal pairs
data <- data %>%
  mutate(vowel = case_when(word == "aanvaardt" & syll == 1 ~ "a",
                           word == "aanvaardt" & syll == 2 ~ "a",
                           word == "aanvaart" & syll == 1 ~ "a",
                           word == "aanvaart" & syll == 2 ~ "a",
                           word == "canon" & syll == 1 ~ "A",
                           word == "canon" & syll == 2 ~ "O",
                           word == "kanon" & syll == 1 ~ "A",
                           word == "kanon" & syll == 2 ~ "O",
                           word == "misbruik" & syll == 1 ~ "I",
                           word == "misbruik" & syll == 2 ~ "9y",
                           word == "plateau" & syll == 1 ~ "a",
                           word == "plateau" & syll == 2 ~ "o",
                           word == "plato" & syll == 1 ~ "a",
                           word == "plato" & syll == 2 ~ "o",
                           word == "servies" & syll == 1 ~ "E",
                           word == "servies" & syll == 2 ~ "i",
                           word == "servisch" & syll == 1 ~ "E",
                           word == "servisch" & syll == 2 ~ "i",
                           word == "voornaam" & syll == 1 ~ "O",
                           word == "servies" & syll == 2 ~ "a",
                           TRUE ~ vowel
                           ))


# Add gender to the dataframe
demographics <- read.csv("\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/Demographics.csv", header = TRUE)
demographics$ppt <- as.numeric(demographics$ppt)

data <- merge(data, demographics, by = "ppt")
data$ppt <- as.integer(data$ppt)

data_cong <- data[data$type == "temp" & data$syll == data$syll_o_int,]
data_cong <- rbind(data[data$type == "minimal",], data_cong)

backup <- data_cong


######################################
### Clean outliers in F0
######################################

# Cleaning outliers has the following procedure based on advice from Laurel:
#   1. First, do an initial round of trimming. In the end, we will calculate outliers based on 3sd. Too large errors will affect this 
#   Therefore, this initial round will remove octave jumps
#   2. Give all outliers based on 3*sd (advice by Laurel)
#   3. Read new acoustics for these by setting new pitch settings
#   4. Remove any remaning outliers

# Give each row an identifier to remove it afterwards
data_cong$id <- 1:nrow(data_cong)


# For each participant, give the wavfile that has an outlier and check in the recording what's going on?
# First, visualize the outliers w. boxplots
ggplot(data_cong,  aes(x = cat, y = f0)) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()+
  facet_wrap(~ppt, nrow = 2)


####################################
### 1. Remove octave jumps
####################################

# Octave jumps occur at (at least) one octave above. The F0 is then doubled.
# If the minimum F0 is 100 Hz, octave jump is at least at 200. Since this is not that precise (it's not always exactly 200), do the following:

jumps <- data_cong[data_cong$f0Max >= data_cong$f0Min * 2]

# This gives instances that have at least double the frequency

# Plot F0 for each participant without these jumps, this should give an indication of the frequency range
data_noJumps <- subset(data_cong, !(data_cong$id %in% jumps$id))

######################################################################################
## Is it ok to do this based on IQR? 

#r <-smean.sdl(data_cong$f0, mult = 3)
#ggplot(d, aes(x, y)) + stat_summary(fun.data = f, geom="boxplot")

#ggplot(data_cong,  aes(x = cat, y = f0)) +
#  geom_boxplot(fill = "#0c4c8a") +
#  theme_minimal()+
#  facet_wrap(~ppt, nrow = 2)

######################################################################################



# Write jumps to external CSV with new Pitch settings for each ppts
cleanOctave <- jumps[, c("ppt", "wavfile", "cond", "syll", "f0", "f0Max", "f0Min", "id", "gender")]
cleanOctave$folder <- substr(cleanOctave$wavfile, 1, 4)

cleanOctave$lowPitch <- 0
cleanOctave$highPitch <- 0

pitchSettings <- data
# New pitch settings p participant. Do this based on boxplots not divided by cat
cleanOctave[cleanOctave$ppt == 1,]$lowPitch <- 100
cleanOctave[cleanOctave$ppt == 1,]$highPitch <- 150
cleanOctave[cleanOctave$ppt == 3,]$lowPitch <- 150
cleanOctave[cleanOctave$ppt == 3,]$highPitch <- 250
cleanOctave[cleanOctave$ppt == 4,]$lowPitch <- 150
cleanOctave[cleanOctave$ppt == 4,]$highPitch <- 250
cleanOctave[cleanOctave$ppt == 5,]$lowPitch <- 150
cleanOctave[cleanOctave$ppt == 5,]$highPitch <- 300
cleanOctave[cleanOctave$ppt == 6,]$lowPitch <- 200
cleanOctave[cleanOctave$ppt == 6,]$highPitch <- 300
cleanOctave[cleanOctave$ppt == 7,]$lowPitch <- 150
cleanOctave[cleanOctave$ppt == 7,]$highPitch <- 250
cleanOctave[cleanOctave$ppt == 8,]$lowPitch <- 150
cleanOctave[cleanOctave$ppt == 8,]$highPitch <- 250
cleanOctave[cleanOctave$ppt == 9,]$lowPitch <- 150
cleanOctave[cleanOctave$ppt == 9,]$highPitch <- 250
cleanOctave[cleanOctave$ppt == 10,]$lowPitch <- 150
cleanOctave[cleanOctave$ppt == 10,]$highPitch <- 250
cleanOctave[cleanOctave$ppt == 11,]$lowPitch <- 75
cleanOctave[cleanOctave$ppt == 11,]$highPitch <- 150
cleanOctave[cleanOctave$ppt == 12,]$lowPitch <- 100
cleanOctave[cleanOctave$ppt == 12,]$highPitch <- 200
cleanOctave[cleanOctave$ppt == 13,]$lowPitch <- 150
cleanOctave[cleanOctave$ppt == 13,]$highPitch <- 250
cleanOctave[cleanOctave$ppt == 14,]$lowPitch <- 75
cleanOctave[cleanOctave$ppt == 14,]$highPitch <- 150
cleanOctave[cleanOctave$ppt == 15,]$lowPitch <- 150
cleanOctave[cleanOctave$ppt == 15,]$highPitch <- 250
cleanOctave[cleanOctave$ppt == 16,]$lowPitch <- 75
cleanOctave[cleanOctave$ppt == 16,]$highPitch <- 150
cleanOctave[cleanOctave$ppt == 17,]$lowPitch <- 150
cleanOctave[cleanOctave$ppt == 17,]$highPitch <- 250
cleanOctave[cleanOctave$ppt == 18,]$lowPitch <- 150
cleanOctave[cleanOctave$ppt == 18,]$highPitch <- 250
cleanOctave[cleanOctave$ppt == 19,]$lowPitch <- 100
cleanOctave[cleanOctave$ppt == 19,]$highPitch <- 250
cleanOctave[cleanOctave$ppt == 20,]$lowPitch <- 150
cleanOctave[cleanOctave$ppt == 20,]$highPitch <- 250

write.csv(cleanOctave, "\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/Clean_octave.csv", row.names = FALSE, quote = FALSE)


###################################
### 2. Check for new outliers
###################################

# That was the first round, now read acoustics and merge with existing dataset

cleaned <- read.table("\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/Cleaned_acoustics.txt", sep = "\t", header = TRUE)
cleaned[cleaned == 999] <- NA

cleaned <- cleaned%>%
  mutate(wavfile = as.character(wavfile),
         syll = as.factor(syll),
         ppt = as.integer(ppt))%>%
  select(-c("slope", "f1", "f2", "f3", "f4"))

cleanOctave <- cleanOctave %>%
  mutate(ppt = as.integer(ppt))%>%
  select(-folder)

tmp <- cleanOctave %>%
  select(-c("ppt", "f0", "f0Max", "f0Min", "lowPitch", "highPitch"))
  
tmp <- merge(tmp, cleaned, by = c("wavfile", "cond", "syll"))
tmp <- unique(tmp)

# Merge with data_cong
data_cong2 <- data_cong #keep data_cong2 as back-up
data_cong[match(tmp$id, data_cong$id), c("f0", "f0Min", "f0Max", "f0Onset", "f0Offset")] <- tmp[,c("f0", "f0Min", "f0Max", "f0Onset", "f0Offset")]




# Then, calculate new outliers based on mean +- 3sd
# Do this for, f0, f0Max and f0Min. I checked the data for F0max and F0 min separately. These also give quite some pitch jumps
# These are for example in consonants (b,d,s) but also within a single vowel. Try to re-read those too.
data_cong3<- data_cong %>% 
  group_by(ppt) %>% 
  summarise(threeSD = 3*sd(f0, na.rm = TRUE) + mean(f0, na.rm = TRUE),minusthreeSD = mean(f0, na.rm = TRUE) - (3*sd(f0, na.rm = TRUE)), 
  threeSDMax = 3*sd(f0Max, na.rm = TRUE) + mean(f0Max, na.rm = TRUE),minusthreeSDMax = mean(f0Max, na.rm = TRUE) - (3*sd(f0Max, na.rm = TRUE)),
  threeSDMin = 3*sd(f0Min, na.rm = TRUE) + mean(f0Min, na.rm = TRUE),minusthreeSDMin = mean(f0Min, na.rm = TRUE) - (3*sd(f0Min, na.rm = TRUE)))

data_cong <- merge(data_cong,data_cong3)

possibleOutliers <- data_cong %>% filter(f0Max < minusthreeSDMax | f0Max > threeSDMax | f0Min < minusthreeSDMin | f0Min > threeSDMin) %>%
  unique()

# Write outliers to external CSV with new Pitch settings for each ppts
possibleOutliers <- possibleOutliers[, c("ppt", "wavfile", "cond", "syll", "f0", "f0Max", "f0Min", "id", "gender")]
possibleOutliers$folder <- substr(possibleOutliers$wavfile, 1, 4)

possibleOutliers$lowPitch <- 0
possibleOutliers$highPitch <- 0

possibleOutliers[possibleOutliers$ppt == 1,]$lowPitch <- 100
possibleOutliers[possibleOutliers$ppt == 1,]$highPitch <- 150
possibleOutliers[possibleOutliers$ppt == 3,]$lowPitch <- 150
possibleOutliers[possibleOutliers$ppt == 3,]$highPitch <- 250
possibleOutliers[possibleOutliers$ppt == 4,]$lowPitch <- 150
possibleOutliers[possibleOutliers$ppt == 4,]$highPitch <- 250
possibleOutliers[possibleOutliers$ppt == 5,]$lowPitch <- 150
possibleOutliers[possibleOutliers$ppt == 5,]$highPitch <- 300
possibleOutliers[possibleOutliers$ppt == 6,]$lowPitch <- 200
possibleOutliers[possibleOutliers$ppt == 6,]$highPitch <- 300
possibleOutliers[possibleOutliers$ppt == 7,]$lowPitch <- 150
possibleOutliers[possibleOutliers$ppt == 7,]$highPitch <- 250
possibleOutliers[possibleOutliers$ppt == 8,]$lowPitch <- 150
possibleOutliers[possibleOutliers$ppt == 8,]$highPitch <- 250
possibleOutliers[possibleOutliers$ppt == 9,]$lowPitch <- 150
possibleOutliers[possibleOutliers$ppt == 9,]$highPitch <- 250
possibleOutliers[possibleOutliers$ppt == 10,]$lowPitch <- 150
possibleOutliers[possibleOutliers$ppt == 10,]$highPitch <- 250
possibleOutliers[possibleOutliers$ppt == 11,]$lowPitch <- 75
possibleOutliers[possibleOutliers$ppt == 11,]$highPitch <- 150
possibleOutliers[possibleOutliers$ppt == 12,]$lowPitch <- 100
possibleOutliers[possibleOutliers$ppt == 12,]$highPitch <- 200
possibleOutliers[possibleOutliers$ppt == 13,]$lowPitch <- 100
possibleOutliers[possibleOutliers$ppt == 13,]$highPitch <- 250
possibleOutliers[possibleOutliers$ppt == 14,]$lowPitch <- 75
possibleOutliers[possibleOutliers$ppt == 14,]$highPitch <- 150
possibleOutliers[possibleOutliers$ppt == 15,]$lowPitch <- 150
possibleOutliers[possibleOutliers$ppt == 15,]$highPitch <- 250
possibleOutliers[possibleOutliers$ppt == 16,]$lowPitch <- 75
possibleOutliers[possibleOutliers$ppt == 16,]$highPitch <- 150
possibleOutliers[possibleOutliers$ppt == 17,]$lowPitch <- 150
possibleOutliers[possibleOutliers$ppt == 17,]$highPitch <- 250
possibleOutliers[possibleOutliers$ppt == 18,]$lowPitch <- 150
possibleOutliers[possibleOutliers$ppt == 18,]$highPitch <- 250
possibleOutliers[possibleOutliers$ppt == 19,]$lowPitch <- 100
possibleOutliers[possibleOutliers$ppt == 19,]$highPitch <- 250
possibleOutliers[possibleOutliers$ppt == 20,]$lowPitch <- 150
possibleOutliers[possibleOutliers$ppt == 20,]$highPitch <- 250


write.csv(possibleOutliers, "\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/Clean_outliers.csv", row.names = FALSE, quote = FALSE)

# Remove dataframes that are not needed anymore
cleaned <- NULL
tmp <- NULL


###################################
### 3. Clean outliers
####################################

cleaned <- read.table("\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/Cleaned_acoustics2.txt", sep = "\t", header = TRUE)
cleaned[cleaned == 999] <- NA

cleaned <- cleaned %>%
  mutate(wavfile = as.character(wavfile),
         syll = as.factor(syll),
         ppt = as.integer(ppt))%>%
  select(-c("slope", "f1", "f2", "f3", "f4"))


tmp <- possibleOutliers %>%
  mutate(ppt = as.integer(ppt),
         syll = as.factor(syll))%>%
  select(-c("folder", "ppt", "f0", "f0Max", "f0Min", "lowPitch", "highPitch"))


tmp <- merge(tmp, cleaned, by = c("wavfile", "cond", "syll"))
tmp <- unique(tmp)

# Merge with data_cong
data_cong[match(tmp$id, data_cong$id), c("f0", "f0Min", "f0Max", "f0Onset", "f0Offset")] <- tmp[,c("f0", "f0Min", "f0Max", "f0Onset", "f0Offset")]


########################################
### 4. Remove remaining outliers
########################################

# Remove remaining outliers based on previous SDs

#data_cong4<- data_cong %>% 
#  group_by(ppt) %>% 
#  summarise(threeSD = 3*sd(f0, na.rm = TRUE) + mean(f0, na.rm = TRUE),minusthreeSD = mean(f0, na.rm = TRUE) - (3*sd(f0, na.rm = TRUE)),
#            threeSDMax = 3*sd(f0Max, na.rm = TRUE) + mean(f0Max, na.rm = TRUE),minusthreeSDMax = mean(f0Max, na.rm = TRUE) - (3*sd(f0Max, na.rm = TRUE)),
#            threeSDMin = 3*sd(f0Min, na.rm = TRUE) + mean(f0Min, na.rm = TRUE),minusthreeSDMin = mean(f0Min, na.rm = TRUE) - (3*sd(f0Min, na.rm = TRUE)))

#data_cong[, c("minusthreeSD", "threeSD", "minusthreeSDMax", "threeSDMax", "minusthreeSDMin", "threeSDMin")]<- NULL
#data_cong <- merge(data_cong,data_cong4)
notFixed <- data_cong %>% filter(f0 < minusthreeSD | f0 > threeSD | f0Max < minusthreeSDMax | f0Max > threeSDMax | f0Min < minusthreeSDMin | f0Min > threeSDMin)


# Items that aren't fixed, are removed from the dataset
data_cong_notclean <- data_cong
data_cong <- subset(data_cong, !(data_cong$id %in% notFixed$id))



#################################################
### Convert the variables
#################################################

# For each variable, if it's still on a 'raw' scale, we will convert it to log.
# For the LMERs, that's fine. But, for the LDA we will also divide it by the talker mean to normalize the data

################################
###Duration 
#################################
## Convert ms to logscale. Then divide by the talker mean dur (in log) to set all at the same level.
data_cong$dur <- data_cong$dur * 1000 # convert to ms
data_cong$logdur <- log(data_cong$dur)

meanlogdur <- data_cong %>%
  group_by(ppt)%>%
  summarise_at(vars(logdur), list(meanlogdur = mean))

data_cong <-data_cong %>% 
  merge(meanlogdur, by = "ppt") %>%
  mutate(rel_logdur2 = logdur/meanlogdur)

#######################################
### Amplitude 
#######################################
#Already in log. Still, divide by mean amplitude
meanint <- data_cong %>%
  group_by(ppt)%>%
  summarise_at(vars(int), list(meanint = mean))

data_cong <- data_cong %>% 
  merge(meanint, by = "ppt") %>%
  mutate(rel_int = int/meanint)

###########################################
### F0 
###########################################
# Convert to semitones relative to the talker mean and to 1 Hz
meanf0 <- data_cong %>%
  group_by(ppt)%>%
  summarise_at(vars(f0), list(meanf0 = mean), na.rm = TRUE)


data_cong <- data_cong %>%
  merge(meanf0, by = "ppt") %>%
  mutate(rel_f0 = ifelse(is.na(f0), NA, (12*log(f0/meanf0)/log(2))))%>%
  mutate(rel_f0_50hz = ifelse(is.na(f0), NA, (12*log(f0/50)/log(2))))%>%
  mutate(rel_f0Max_50hz = ifelse(is.na(f0), NA, (12*log(f0Max/50)/log(2))))%>%
  mutate(rel_f0Min_50hz = ifelse(is.na(f0), NA, (12*log(f0Min/50)/log(2))))

# For F0 variation, compute slopes (F0max-F0min/Tmax-Tmin)
data_cong <- data_cong %>%
 # mutate(tmax = tmax*1000)%>%
  #mutate(tmin = tmin*1000)%>%
  mutate(F0_slope_sem = (rel_f0Max_50hz-rel_f0Min_50hz)/(tmax-tmin))%>%
  mutate(F0_slope = (f0Max-f0Min)/(tmax-tmin))

data_cong[data_cong$F0_slope == "Inf"]<- NA
tapply(data_cong$F0_slope_sem, list(data_cong$cat), mean, na.rm = TRUE)
tapply(data_cong$F0_slope, list(data_cong$cat), mean, na.rm = TRUE)
# Using semitones/raw F0 doesn't change that much. Cleaner to use semitones then.




##########################################
### Vowel quality
##########################################
# Compute the middle vowel point for each talker and the Euclidean distance for each vowel from that point

# 1. Calculate the centroid lcoation (Karlsson & van Doorn, 2012)
# 2. Calculate VFD
# 3. Plots


# First, subset the data containing only monophtongs

data_vowels <- data_cong%>%
  filter(vowel != "ei" & vowel != "au" & vowel != "9y")#%>%
   
vowelOut <- data_vowels %>%
  filter(f1>1000 & f2>2000)

data_vowels <- data_vowels[!data_vowels$id %in% vowelOut$id]
  

#####################################################
### Centroid location
#####################################################

# Before doing this, we need to make sure that every vowel has an equal number of observations!
# The "a" is from words that can only be open (aanvaart, piraat). Vowels that could be more closed (kanon, patat) are "A".
calculate_cent <- data_vowels %>%
  filter(vowel == "a" | vowel == "u" | vowel == "i")%>%
  group_by(vowel, ppt)%>%
  summarise(N = n())

# Take the lowest N in calculate_cent and pitck the first N of observations in the other vowels

vowelN = min(calculate_cent$N)

vowelA <- data_vowels %>%
  group_by(ppt)%>%
  filter(vowel == "a")%>%
  slice_head(n = vowelN)

vowelI<- data_vowels %>%
  group_by(ppt)%>%
  filter(vowel == "i")%>%
  slice_head(n = vowelN)

vowelU <- data_vowels %>%
  group_by(ppt)%>%
  filter(vowel == "u")%>%
  slice_head(n = vowelN)


tmp_vowels <- rbind(vowelA,vowelI, vowelU)

# For F1 this is: (1/n) * sum(f1)
cent_loc <- tmp_vowels %>%
  filter(vowel == "a" | vowel == "u" | vowel == "i")%>%
  group_by(ppt)%>%
  summarise(F1_cent = sum(f1) * (1/n()),N = n())
  
tmp_vowels <- merge(tmp_vowels, cent_loc)

# For F2 this is: (1/n) * sum(f2[f1<F1_cent])
cent_loc2 <- tmp_vowels %>%
  filter(vowel == "a" | vowel == "u" | vowel == "i")%>%
  filter(f1<F1_cent)%>%
  group_by(ppt)%>%
  summarise(F2_cent = sum(f2) * (1/n()), N = n())

cent_loc_tot <- merge(cent_loc, cent_loc2, by = "ppt")                          
cent_loc_tot[,c("N.x", "N.y")] <- NULL

data_vowels <- merge(data_vowels, cent_loc_tot)

#############################################
### Calculate VFD
#############################################


# Now I have on each row a F1_cent and a weighted F2_cent
# Next, I want to calculate the VFD for each vowel from this location

data_vowels <- data_vowels %>%
  mutate(VFD = sqrt(((f1 - F1_cent)^2) + ((f2 - F2_cent)^2)))

# These are comparable numbers to Karlsson and van Doorn so I think it works!

# Try some means
tapply(data_vowels$VFD, list(data_vowels$cat), mean)
tapply(data_vowels$VFD, list(data_vowels$cat, data_vowels$vowel), mean)

# Depending on the vowel, it does look like unstressed vowels have a shorter VFD, so closer to the reduced version

#########################################
### Plots
#########################################

ggplot(data_vowels, aes(x = f2, y = f1, color = vowel))+
  geom_point()+
  scale_x_reverse() + scale_y_reverse()+
  theme_classic()
  
# Make plot with means
vowelmeans <- data_vowels %>%
  group_by(vowel)%>%
  summarise(meanF1 = mean(f1),
            meanF2 = mean(f2))


ggplot(vowelmeans, aes(x = meanF2, y = meanF1, label = vowel))+
  geom_label()+
  scale_x_reverse() + scale_y_reverse()+
  theme_classic()
  

ggplot(data_vowels, aes(x = f2, y = f1, label = vowel))+
  scale_x_reverse() + scale_y_reverse()+
  geom_point()+
  facet_wrap(~ppt)+
  theme_classic()


# plot mean/ppt with center location
  
ggplot(data_vowels, aes(x = f2, y = f1, label = vowel))+
  geom_label(data = vowelmeans, aes(x = meanF2, y = meanF1))+
  scale_x_reverse() + scale_y_reverse()+
  geom_point(aes(x = F2_cent, y = F1_cent))+
  facet_wrap(~ppt)+
  theme_classic()
  
# You can see that the centre location is more or less in the middle for all ppts.
  
# Try differences between stressed and unstressed
vowelmeans2 <- data_vowels %>%
  group_by(vowel, cat)%>%
  summarise(meanF1 = mean(f1),
            meanF2 = mean(f2))


ggplot(data_vowels, aes(x = f2, y = f1, label = vowel))+
  geom_label(data = vowelmeans2, aes(x = meanF2, y = meanF1, color = cat))+
  scale_x_reverse() + scale_y_reverse()+
  geom_point(aes(x = F2_cent, y = F1_cent))+
  facet_wrap(~ppt)+
  theme_classic()

# It checks out! Unstressed vowels are more towards the centroid location

#############################################
### Finalization
#############################################
# Clean-up any remaining unneeded columns


# Write to txt file
write.table(data_cong, "\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/Production; prepared data.txt", sep = "\t", col.names = T,row = F)




#####################################
##### Rest code
#####################################



# Function that prints the wavfiles of the outliers for each participant and their F0 value.
findOUt <- function(data=NULL, PPn) {
  
  tmp <- data %>%
    filter(ppt == PPn & cat == "stressed")
  
  InterQ <- IQR(tmp$f0, na.rm = TRUE)
  q1 <- quantile(tmp$f0, na.rm = TRUE)[2]
  q3 <- quantile(tmp$f0, na.rm = TRUE)[4]
  
  indLow<- which(tmp$f0 < (q1-(1.5*InterQ)))
  indHigh <-which(tmp$f0 > (q3 + (1.5* InterQ)))
  
  folder <- substr(tmp[indLow[1],c(3)],1,4)
  tmp_clean <- cbind(folder,tmp[indLow, c("ppt", "wavfile", "cond", "syll", "f0", "f0Max", "f0Min", "id", "gender")])
  outliers <- rbind(outliers,tmp_clean)
  
  folder <- substr(tmp[indHigh[1],c(3)],1,4)
  tmp_clean <- cbind(folder,tmp[indHigh, c("ppt", "wavfile", "cond", "syll", "f0", "f0Max", "f0Min", "id", "gender")])
  outliers <- rbind(outliers,tmp_clean)
  
  
  tmp <- data_cong %>%
    filter(ppt == PPn & cat == "unstressed")
  
  InterQ <- IQR(tmp$f0, na.rm = TRUE)
  q1 <- quantile(tmp$f0, na.rm = TRUE)[2]
  q3 <- quantile(tmp$f0, na.rm = TRUE)[4]
  
  indLow<- which(tmp$f0 < (q1-(1.5*InterQ)))
  indHigh <-which(tmp$f0 > (q3 + (1.5* InterQ)))
  
  folder <- substr(tmp[indLow[1],c(3)],1,4)
  tmp_clean <- cbind(folder,tmp[indLow, c("ppt", "wavfile", "cond", "syll", "f0", "f0Max", "f0Min", "id", "gender")])
  outliers <- rbind(outliers,tmp_clean)
  
  folder <- substr(tmp[indHigh[1],c(3)],1,4)
  tmp_clean <- cbind(folder,tmp[indHigh, c("ppt", "wavfile", "cond", "syll", "f0", "f0Max", "f0Min", "id", "gender")])
  outliers <- rbind(outliers,tmp_clean)
  
  outliers <<- outliers
}


outliers <- data.frame(folder = character(),ppt = character(), wavfile = character(), cond = character(), syll = numeric(), f0 = numeric(), f0Max = numeric(), f0Min = numeric(), id = numeric(), gender = character())

for (pNumb in 1: 5){
  findOUt(data_cong, pNumb)  
}

outliers <- outliers %>% drop_na()



####### Vowels package

pros_vowels <- data_cong %>% 
  select("ppt", "vowel", "f1", "f2", "f3")%>%
  rename(speaker = ppt,
         F1 = f1,
         F2 = f2,
         F3 = f3) %>%
  mutate(speaker = as.factor(speaker),
         context = NA,
         g1.F1 = NA,
         gl.F2 = NA,
         gl.F3 = NA)
pros_vowels <- pros_vowels[,c(1,2,6,3,4,5,6:9)]

# Vowels package wants to load in the data using it's own function. So first save the df as tab-delimited, then re-read.
# In the end, not sure about this package. It's not very flexible... Maybe best to make the plot using ggplot after all.

write.table(pros_vowels, "\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/pros_vowels.txt", sep = "\t", col.names = T,row = F, quote = FALSE, fileEncoding = "UTF-8")

pros_vowels <- NULL
pros_vowels <- load.vowels("\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/pros_vowels.txt")
pros_vowels$speaker <- as.factor(pros_vowels$speaker)
######WHERE TO RENAME THE VOWELS? OR KEEP SAMPA
########### HOW TO INCLUDE THE DIFF CONDITIONS

vowelplot(pros_vowels, color = "vowel", labels = "vowel")
vowelplot(compute.means(pros_vowels), color = "vowel", labels = "vowel")




#################################################
### Try for F0max now
#################################################

### Allright, F0Max outliers are mostly due to weird F0 in consonants (b, d, s)
# These can be fixed in the same way as F0 mean

possibleOutliers <- data_cong %>% filter(f0Max < minusthreeSDMax | f0Max > threeSDMax)%>%# | f0Min < minusthreeSDMin | f0Min > threeSDMin) %>%
  unique()


cleaned <- NULL
tmp <- NULL


cleaned <- read.table("\\\\CNAS.RU.NL/U759237/Documents/Projects/Suprasegmental production/Analysis/R analyses/Cleaned_acoustics3.txt", sep = "\t", header = TRUE)
cleaned[cleaned == 999] <- NA

cleaned <- cleaned %>%
  mutate(wavfile = as.character(wavfile),
         syll = as.factor(syll),
         ppt = as.integer(ppt))%>%
  select(-c("slope", "f1", "f2", "f3", "f4"))


tmp <- possibleOutliers %>%
  mutate(ppt = as.integer(ppt),
         syll = as.factor(syll))%>%
  select(-c("folder", "ppt", "f0", "f0Max", "f0Min", "lowPitch", "highPitch"))


tmp <- merge(tmp, cleaned, by = c("wavfile", "cond", "syll"))
tmp <- unique(tmp)

# Merge with data_cong
data_cong[match(tmp$id, data_cong$id), c("f0", "f0Min", "f0Max", "f0Onset", "f0Offset")] <- tmp[,c("f0", "f0Min", "f0Max", "f0Onset", "f0Offset")]

##################################################
### F0Min
##################################################
possibleOutliers <- data_cong %>% filter(f0Min > threeSDMin) %>%
  unique()


f0Min < minusthreeSDMin

>>>>>>> 08f4cc0400136b2e1a4a290bb907d27bf9211e36
