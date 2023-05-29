# Hair comparison chooser

# Load the necessary library
library(tidyverse)

#Make a data frame of the hair pictures
# Read the HairResearchData.csv file and select relevant columns
hairPictures <- read.csv('HairResearchData.csv') %>%
  select(Date, Month, Day, Treatment.Variable, Treatment.Day, Actual.Treatment, Treatment.Time, contains('Picture.Time')) %>%
  # Make each picture its own piece of the data
  pivot_longer(contains('Picture.Time')) %>%
  mutate(TimeOfDay = gsub('.Picture.Time', '', name)) %>%
  
  #Remove the word aussie from baseline
  mutate(Treatment.Variable = ifelse(grepl('Baseline',Treatment.Variable),'Baseline',Treatment.Variable)) %>% 
  
  # Process the date and time information
  mutate(
    NextDay = grepl('24\\+', value), # Check if the time is on the next day
    RawTime = gsub('24\\+', '', value), # Remove the '24+' from the time value
    ActualDate = ifelse(NextDay, paste0(Month, '/', Day + 1, '/2023'), Date) # Adjust the date if NextDay is TRUE
  ) %>%
  
  # Create a new column with the combined date and time information
  mutate(HairPictureTime = as.POSIXct(paste(ActualDate, RawTime), format = '%m/%d/%Y %H:%M')) %>%
  
  # Select the necessary columns and remove rows with missing values
  select(-c(name, Date, Month, Day, NextDay, RawTime, ActualDate)) %>%
  filter(!is.na(HairPictureTime)) %>%
  
  # # Add a PictureID column with random sample values
  # mutate(PictureID = sample(1:nrow(.), replace = FALSE)) %>%
  mutate(PictureID = 1:nrow(.)) %>% 
  
  # Move the PictureID and HairPictureTime columns to the front
  select(PictureID, HairPictureTime, everything())

# Get some information on the treatments, specifically the times
# Read the HairResearchData.csv file again and select relevant columns
hairTreatmentDF <- read.csv('HairResearchData.csv') %>%
  select(Date, contains('Treatment')) %>%
  pivot_longer(contains('Time'), names_to = 'TreatmentType', values_to = 'TreatmentTime') %>%
  
  # Remove rows where hair wasn't treated values
  filter(!is.na(TreatmentTime)) %>%
  
  # Reformat the date and time information
  mutate(HairTreatmentTime = as.POSIXct(paste(Date, TreatmentTime), format = '%m/%d/%Y %H:%M')) %>%
  
  # Clean the additional treatment and rinse data
  mutate(Actual.Treatment = ifelse(grepl('Addit', TreatmentType), 'TREAT', Actual.Treatment)) %>%
  mutate(Actual.Treatment = ifelse(Treatment.Variable == 'Rinse', 'TREAT', Actual.Treatment)) %>%
  
  # Filter out untreated days
  filter(Actual.Treatment != 'NOTHING')

# Analysis of treatment info, sanity check
hairTreatmentDF %>% 
  group_by(Treatment.Variable) %>% 
  summarize(N = n())

#Grab just the treatment times
hairTreatmentTimes = hairTreatmentDF %>% pull(HairTreatmentTime)

#Now lets find how long each picture has been since its most recent treatment
hairPictures <- hairPictures %>% 
  mutate(LastTreatmentTime = lapply(1:nrow(hairPictures),function(x) 
    hairTreatmentTimes[max(which(hairTreatmentTimes < hairPictures$HairPictureTime[x]))]) %>% 
      unlist() %>% as.POSIXct(format = '%m/%d/%Y %H:%M')) %>% 
  mutate(TimeSinceLastTreatment = difftime(HairPictureTime,LastTreatmentTime,units = 'mins')) %>% 
  select(-c(Actual.Treatment,Treatment.Time,value))
hairPictures %>% head

################################################################################
#Now lets find which pictures we really want to compare
#First, grab the treatment options and number of pics
N = nrow(hairPictures)
treatVars = hairPictures %>% pull(Treatment.Variable) %>% unique
minNeededComps = 5

comparisonDF = data.frame(FirstID = numeric(0),
                          SecondID = numeric(0))

#Loop through every picture, pic out a few that we definitely need to compare
#and then get a few extra just in case
for(pic in 1:N){
  #Find the picture and its attributes
  curRow = hairPictures %>% filter(PictureID == pic)
  curTreat = curRow %>% pull(Treatment.Variable)
  curDay = curRow %>% pull(Treatment.Day)
  curTime = curRow %>% pull(TimeOfDay)
  
  #Pull out the directly comparable days
  subDF = hairPictures %>% 
    filter(Treatment.Day == curDay,TimeOfDay == curTime,Treatment.Variable != curTreat)
  
  #Add in each ID that needs to be compared, and then put it in a proper order
  if(nrow(subDF) > 0) { 
    comparisonDF <- comparisonDF %>% 
      bind_rows(data.frame(FirstID = pic,SecondID = subDF %>% pull(PictureID)),
                data.frame(SecondID = pic,FirstID = subDF %>% pull(PictureID))) %>% 
      filter(FirstID < SecondID) %>% 
      distinct()
  }
  
  #Now lets add some randos
  curCompsAndSelf = c(pic,comparisonDF %>% filter(FirstID == pic | SecondID == pic) %>% 
                        pivot_longer(everything()) %>% 
                        filter(value != pic) %>% pull(value))
  
  #Sample from the remaining pictures excluding itself, sample until we have at 
  #least minNeededComps, but at least one more so its not negative/zero
  otherComps = sample(setdiff(1:N,curCompsAndSelf),max(minNeededComps - (length(curCompsAndSelf)-1),1),replace = F)
  
  #Add the randos
  comparisonDF <- comparisonDF %>% 
    bind_rows(data.frame(FirstID = pic,SecondID = otherComps),
              data.frame(SecondID = pic,FirstID = otherComps)) %>% 
    filter(FirstID < SecondID) %>% 
    distinct()
}

#Randomize the IDs so we can avoid bias in rating order
comparisonDF <- comparisonDF %>% 
  rowwise() %>% 
  mutate(IDTop = sample(c(FirstID,SecondID),1),
         IDBot = setdiff(c(FirstID,SecondID),IDTop)) %>% 
  select(IDTop,IDBot)
comparisonDF <- comparisonDF[sample(1:nrow(comparisonDF),nrow(comparisonDF),replace = F),]

comparisonDF %>% pivot_longer(everything()) %>% pull(value) %>% table %>% hist
# save(comparisonDF,file = 'Min5CompsList.Rdata')
# write.csv(comparisonDF,file = 'Min5CompsList.csv',row.names = F)
