###### Set WD ######
setwd("~/Dropbox/Grad Stat Classes/CC Research/CurrentData")

###### Libraries ######
library(ggplot2)
library(reshape2)
library(dplyr)
library(gtools)

###### Read in Data ######
data <- read.csv("~/Dropbox/Grad Stat Classes/CC Research/CurrentData/hsspri9507full.csv", 
                 header=TRUE, na.strings=c("","N","O","B","U"))

###### Setup the Data ######
# Different measures of Husband's report of Husband's hostility towards Wife
# Check that nothing is above 7 for questionaires
# Change the missing(9) and Do not apply(8) to NA if they occur
table(data[,"HHanW0"])


# Set of variables we want!
vwant <- c("HHanW0","HHcrW0","HHylW0","HHhtW0","HHarW0",
           "HWanH0","HWcrH0","HWylH0","HWhtH0","HWarH0",
           "WHanW0","WHcrW0","WHylW0","WHhtW0","WHarW0",
           "WWanH0","WWcrH0","WWylH0","WWhtH0","WWarH0",
           "HHseW0","HHfrW0","HHtrW0","HHotW0","HHthW0",
           "WWseH0","WWfrH0","WWtrH0","WWotH0","WWthH0",
           "HHanW2A","HHcrW2A","HHylW2A","HHhtW2A","HHarW2A",
           "HWanH2A","HWcrH2A","HWylH2A","HWhtH2A","HWarH2A",
           "WHanW2A","WHcrW2A","WHylW2A","WHhtW2A","WHarW2A",
           "WWanH2A","WWcrH2A","WWylH2A","WWhtH2A","WWarH2A",
           "HHseW2A","HHfrW2A","HHtrW2A","HHotW2A","HHthW2A",
           "WWseH2A","WWfrH2A","WWtrH2A","WWotH2A","WWthH2A")


# Check to see if variables are between 1-7
levelcheck <- function(x){
  for(i in 1:length(x)){
    if(max(data[,x[i]], na.rm=T) >7 | min(data[,x[i]], na.rm=T) < 1){print(x[i])}
    else{print("ok")}
  }
}
levelcheck(vwant)



# Re-level the variables
# For hostility:
# ORIGINAL
# Always 1
# Almost always 2
# Fairly often 3
# About half the time 4
# Not too often 5
# Almost never 6
# Never 7
#
# UPDATED
# Always 7
# Almost always 6
# Fairly often 5
# About half the time 4
# Not too often 3
# Almost never 2
# Never 1

# Function the recodes the levels
relev <- function(x){
  tmp <- as.factor(as.character(x))
  levels(tmp) <- rev(levels(tmp))
  tmp <- as.numeric(as.character(tmp))
  return(tmp)
}

# Releving the data vars
for(i in 1:length(vwant)){
  data[,vwant[i]] <- relev(data[,vwant[i]])
}


# Look @ grid of plots
hist(data[,"WWhtH0"])

wget  <- c("WWanH0","WWcrH0","WWylH0","WWhtH0","WWarH0")
tempw <- data[,wget]
tempw <- melt(tempw)
ggplot(tempw) + geom_histogram(aes(value)) + facet_wrap(~variable)

mget  <- c("HWanH0","HWcrH0","HWylH0","HWhtH0","HWarH0")
tempm <- data[,mget]
tempm <- melt(tempm)
ggplot(tempm) + geom_histogram(aes(value)) + facet_wrap(~variable)


tempc <- data[,c(wget, mget)]
tempc <- melt(tempc)
tempc$gender <- rep("H", nrow(tempc))
tempc[(tempc$variable=="WWanH0") | (tempc$variable=="WWcrH0") |
      (tempc$variable=="WWylH0") | (tempc$variable=="WWhtH0") |
      (tempc$variable=="WWarH0"),"gender"] <- "W"
tempc$var <- rep(NA, nrow(tempc))
tempc$var[grep("an", tempc$variable)] <- "Angry"
tempc$var[grep("cr", tempc$variable)] <- "Criticize"
tempc$var[grep("yl", tempc$variable)] <- "Yell"
tempc$var[grep("ht", tempc$variable)] <- "Hit"
tempc$var[grep("ar", tempc$variable)] <- "Argue"
tempc$var <- as.factor(tempc$var)
ggplot(tempc, aes(value, fill=gender)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + 
  facet_wrap(~var)





# Create the host variables

##### 1st Time Wave #####

# Compute Row Means for host varaibles for husband
data$HHhostW0 <- rowMeans(data[,c("HHanW0","HHcrW0","HHylW0","HHhtW0","HHarW0")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("HHanW0","HHcrW0","HHylW0","HHhtW0","HHarW0")]))>3)
    {tmp[i] <- i
    data$HHhostW0[i] <- NA}
}


# Compute Row Means for host varaibles for husband
data$HWhostH0 <- rowMeans(data[,c("HWanH0","HWcrH0","HWylH0","HWhtH0","HWarH0")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("HWanH0","HWcrH0","HWylH0","HWhtH0","HWarH0")]))>3)
  {tmp[i] <- i
   data$HWhostH0[i] <- NA}
}

# Compute Row Means for host varaibles for husband
data$HWhostH0 <- rowMeans(data[,c("HWanH0","HWcrH0","HWylH0","HWhtH0","HWarH0")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("HWanH0","HWcrH0","HWylH0","HWhtH0","HWarH0")]))>3)
  {tmp[i] <- i
  data$HWhostH0[i] <- NA}
}

# Compute Row Means for host varaibles for wife
data$WHhostW0 <- rowMeans(data[,c("WHanW0","WHcrW0","WHylW0","WHhtW0","WHarW0")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("WHanW0","WHcrW0","WHylW0","WHhtW0","WHarW0")]))>3)
  {tmp[i] <- i
   data$WHhostW0[i] <- NA}
}

# Compute Row Means for host varaibles for wife
data$WWhostH0 <- rowMeans(data[,c("WWanH0","WWcrH0","WWylH0","WWhtH0","WWarH0")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("WWanH0","WWcrH0","WWylH0","WWhtH0","WWarH0")]))>3)
  {tmp[i] <- i
   data$WWhostH0[i] <- NA}
}


# Compute Row Means for observed hostility varaible from Husband to Wife
data$OHhostW0 <- rowMeans(data[,c("OHhsW0","OHacW0","OHehW0","OHrhW0","OHanW0")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("OHhsW0","OHacW0","OHehW0","OHrhW0","OHanW0")]))>3)
  {tmp[i] <- i
  data$OHhostW0[i] <- NA}
}


# Compute Row Means for observed hostility varaible from Wife to Husband
data$OWhostH0 <- rowMeans(data[,c("OWhsH0","OWacH0","OWehH0","OWrhH0","OWanH0")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("OWhsH0","OWacH0","OWehH0","OWrhH0","OWanH0")]))>3)
  {tmp[i] <- i
  data$OWhostH0[i] <- NA}
}


# Compute Row Means for relationship instability varaible from Husband to Wife
data$HHinstW0 <- rowMeans(data[,c("HHseW0","HHfrW0","HHtrW0","HHotW0","HHthW0")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("HHseW0","HHfrW0","HHtrW0","HHotW0","HHthW0")]))>3)
  {tmp[i] <- i
  data$HHinstW0[i] <- NA}
}


# Compute Row Means for relationship instability varaible from Wife to Husband
data$WWinstH0 <- rowMeans(data[,c("WWseH0","WWfrH0","WWtrH0","WWotH0","WWthH0")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("WWseH0","WWfrH0","WWtrH0","WWotH0","WWthH0")]))>3)
  {tmp[i] <- i
  data$WWinstH0[i] <- NA}
}



###### 2nd Time Wave #####

# Compute Row Means for host varaibles for husband
data$HHhostW2A <- rowMeans(data[,c("HHanW2A","HHcrW2A","HHylW2A","HHhtW2A","HHarW2A")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("HHanW2A","HHcrW2A","HHylW2A","HHhtW2A","HHarW2A")]))>3)
  {tmp[i] <- i
  data$HHhostW2A[i] <- NA}
}


# Compute Row Means for host varaibles for husband
data$HWhostH2A <- rowMeans(data[,c("HWanH2A","HWcrH2A","HWylH2A","HWhtH2A","HWarH2A")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("HWanH2A","HWcrH2A","HWylH2A","HWhtH2A","HWarH2A")]))>3)
  {tmp[i] <- i
  data$HWhostH2A[i] <- NA}
}

# Compute Row Means for host varaibles for husband
data$HWhostH2A <- rowMeans(data[,c("HWanH2A","HWcrH2A","HWylH2A","HWhtH2A","HWarH2A")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("HWanH2A","HWcrH2A","HWylH2A","HWhtH2A","HWarH2A")]))>3)
  {tmp[i] <- i
  data$HWhostH2A[i] <- NA}
}

# Compute Row Means for host varaibles for wife
data$WHhostW2A <- rowMeans(data[,c("WHanW2A","WHcrW2A","WHylW2A","WHhtW2A","WHarW2A")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("WHanW2A","WHcrW2A","WHylW2A","WHhtW2A","WHarW2A")]))>3)
  {tmp[i] <- i
  data$WHhostW2A[i] <- NA}
}

# Compute Row Means for host varaibles for wife
data$WWhostH2A <- rowMeans(data[,c("WWanH2A","WWcrH2A","WWylH2A","WWhtH2A","WWarH2A")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("WWanH2A","WWcrH2A","WWylH2A","WWhtH2A","WWarH2A")]))>3)
  {tmp[i] <- i
  data$WWhostH2A[i] <- NA}
}

# Compute Row Means for observed hostility varaible from Husband to Wife
data$OHhostW2A <- rowMeans(data[,c("OHhsW2A","OHacW2A","OHehW2A","OHrhW2A","OHanW2A")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("OHhsW2A","OHacW2A","OHehW2A","OHrhW2A","OHanW2A")]))>3)
  {tmp[i] <- i
  data$OHhostW2A[i] <- NA}
}


# Compute Row Means for observed hostility varaible from Wife to Husband
data$OWhostH2A <- rowMeans(data[,c("OWhsH2A","OWacH2A","OWehH2A","OWrhH2A","OWanH2A")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("OWhsH2A","OWacH2A","OWehH2A","OWrhH2A","OWanH2A")]))>3)
  {tmp[i] <- i
  data$OWhostH2A[i] <- NA}
}


# Compute Row Means for relationship instability varaible from Husband to Wife
data$HHinstW2A <- rowMeans(data[,c("HHseW2A","HHfrW2A","HHtrW2A","HHotW2A","HHthW2A")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("HHseW2A","HHfrW2A","HHtrW2A","HHotW2A","HHthW2A")]))>3)
  {tmp[i] <- i
  data$HHinstW2A[i] <- NA}
}


# Compute Row Means for relationship instability varaible from Wife to Husband
data$WWinstH2A <- rowMeans(data[,c("WWseH2A","WWfrH2A","WWtrH2A","WWotH2A","WWthH2A")], na.rm=TRUE)
# Loop to check if each mean is an avg of 3 or more responses
tmp <- rep(NA, nrow(data))
for(i in 1:nrow(data)){
  if(sum(is.na(data[i,c("WWseH2A","WWfrH2A","WWtrH2A","WWotH2A","WWthH2A")]))>3)
  {tmp[i] <- i
  data$WWinstH2A[i] <- NA}
}




















# look at different combinations of the variables to see if something shows up
remove <- NULL
comb <- combn(c("HHhostW2A","HWhostH2A","WHhostW2A","WWhostH2A"), 2)
for(i in 1:ncol(comb)){
  if(substr(comb[1,i],1,1) == substr(comb[2,i],1,1)){remove[i] <- i}
}

comb <- comb[,-remove[!is.na(remove)]]
tmpd <- NULL
for(i in 1:ncol(comb)){
  tmp <- melt(data[,comb[,i]])
  tmp$combination <- i
  tmpd <- rbind(tmpd, tmp)
}
tmpd$combination <- as.factor(tmpd$combination)

ggplot(tmpd, aes(value, fill=variable)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + 
  facet_wrap(~combination)


#write.csv(data[,c("HHhostW0","HWhostH0","WHhostW0","WWhostH0","HHhostW2A","HWhostH2A","WHhostW2A","WWhostH2A","OHrqW0","OHrqW2A")],"ModData_4_4.csv",row.names=FALSE)
write.csv(data[,c("HHhostW0","HWhostH0","WHhostW0","WWhostH0","HHinstW0","WWinstH0","HHhostW2A","HWhostH2A","WHhostW2A","WWhostH2A","HHinstW2A","WWinstH2A","OHhostW0","OWhostH0","OHrqW0","OHrqW2A","OHhostW2A","OWhostH2A")],"ModData_6_9.csv",row.names=FALSE)
