#Hair Trio Pairing
options(stringsAsFactors = F)
library(tidyverse)
library(magick)
library(exiftoolr)

setwd('Pictures/')

fileList <- list.files(pattern = '.HEIC')

# Create a data frame to store the file information
file_info <- data.frame(File = character(), DateTaken = character(), stringsAsFactors = FALSE)

# Loop through each image file and retrieve the "Date Taken" metadata
for (file in fileList) {
  metadata <- exiftoolr::exif_read(file)
  date_taken <- metadata$DateTimeOriginal
  file_info <- rbind(file_info, data.frame(File = file, DateTaken = date_taken))
}

file_info

write.csv(file_info,'HairImgDates.csv',row.names = F)
fileInfo = read.csv('HairImgDates.csv') %>% 
  mutate(Year = substr(DateTaken,1,4) %>% as.numeric(),
         Month = substr(DateTaken,6,7),
         Day = substr(DateTaken,9,10),
         Hour = substr(DateTaken,12,13),
         Minute = substr(DateTaken,15,16) %>% as.numeric(),
         Second = substr(DateTaken,18,19) %>% as.numeric()) %>% 
  mutate(MonthDay = paste(Month,Day,sep = '-')) %>% 
  group_by(MonthDay) %>% 
  mutate(TimeOfDay = case_when(Hour == min(Hour) ~ 'Morning',
                               Hour == max(Hour) ~ 'Night',
                               T ~ 'Midday')) 

issueDays = fileInfo %>% 
  summarize(NumMorn = sum(TimeOfDay == 'Morning'),
            NumMidday = sum(TimeOfDay == 'Midday'),
            NumNight = sum(TimeOfDay == 'Night')) %>% 
  filter(NumMorn != 3 | NumMidday != 3 | NumNight != 3) %>% 
  pull(MonthDay)
#2-28, 3-12, 4-21 (4-22 and 23) should be issues
#All other days are just overnight

cleanImageDF = fileInfo %>%
  select(File,Month,Day,Hour,Minute,Second,MonthDay,TimeOfDay) %>% 
  mutate(ImageLabel = ifelse(MonthDay %in% issueDays,'Unknown',paste(MonthDay,TimeOfDay,sep = '-'))) %>% 
  ungroup() %>% 
  mutate(ImageLabel = case_when(Month == '02' & Day == '28' & Hour == '10' ~ '02-28-Morning',
                                Month == '02' & Day == '28' & Hour == '21' ~ '02-28-Night',
                                
                                Month == '03' & Day == '03' & Hour == '06' ~ '03-03-Morning',
                                Month == '03' & Day == '03' & Hour == '20' ~ '03-03-Midday',
                                Month == '03' & Day == '04' & Hour == '03' ~ '03-03-Night',
                                
                                Month == '03' & Day == '04' & Hour == '07' ~ '03-04-Morning',
                                Month == '03' & Day == '04' & Hour == '12' ~ '03-04-Midday',
                                Month == '03' & Day == '05' & Hour == '01' ~ '03-04-Night',
                                
                                Month == '03' & Day == '05' & Hour == '05' ~ '03-05-Morning',
                                Month == '03' & Day == '05' & Hour == '17' ~ '03-05-Midday',
                                Month == '03' & Day == '05' & Hour == '22' ~ '03-05-Night',
                                
                                Month == '03' & Day == '10' & Hour == '11' ~ '03-10-Morning',
                                Month == '03' & Day == '10' & Hour == '14' ~ '03-10-Midday',
                                Month == '03' & Day == '11' & Hour == '10' ~ '03-10-Night',
                                
                                Month == '03' & Day == '11' & Hour == '12' ~ '03-11-Morning',
                                Month == '03' & Day == '11' & Hour == '15' ~ '03-11-Midday',
                                Month == '03' & Day == '11' & Hour == '22' ~ '03-11-Night',
                                
                                Month == '03' & Day == '12' & Hour == '17' ~ '03-12-Midday',
                                Month == '03' & Day == '12' & Hour == '21' ~ '03-12-Night',
                                
                                Month == '03' & Day == '29' & Hour == '12' ~ '03-29-Morning',
                                Month == '03' & Day == '29' & Hour == '22' ~ '03-29-Midday',
                                Month == '03' & Day == '30' & Hour == '05' ~ '03-29-Night',
                                
                                Month == '03' & Day == '30' & Hour == '08' ~ '03-30-Morning',
                                Month == '03' & Day == '30' & Hour == '19' ~ '03-30-Midday',
                                Month == '03' & Day == '31' & Hour == '07' ~ '03-30-Night',
                                
                                Month == '03' & Day == '31' & Hour == '11' ~ '03-31-Morning',
                                Month == '03' & Day == '31' & Hour == '17' ~ '03-31-Midday',
                                Month == '03' & Day == '31' & Hour == '20' ~ '03-31-Night',
                                
                                Month == '04' & Day == '14' & Hour == '09' ~ '04-14-Morning',
                                Month == '04' & Day == '14' & Hour == '14' ~ '04-14-Midday',
                                Month == '04' & Day == '15' & Hour == '00' ~ '04-14-Night',
                                
                                Month == '04' & Day == '15' & Hour == '09' ~ '04-15-Morning',
                                Month == '04' & Day == '15' & Hour == '14' ~ '04-15-Midday',
                                Month == '04' & Day == '16' & Hour == '01' ~ '04-15-Night',
                                
                                Month == '04' & Day == '16' & Hour == '16' ~ '04-16-Morning',
                                Month == '04' & Day == '16' & Hour == '20' ~ '04-16-Midday',
                                Month == '04' & Day == '16' & Hour == '23' ~ '04-16-Night',
                                
                                Month == '04' & Day == '21' & Hour == '07' ~ '04-21-Morning',
                                Month == '04' & Day == '22' & Hour == '00' ~ '04-21-Midday',
                                Month == '04' & Day == '22' & Hour == '10' ~ '04-21-Night',
                                
                                Month == '04' & Day == '25' & Hour == '17' ~ '04-25-Morning',
                                Month == '04' & Day == '25' & Hour == '20' ~ '04-25-Midday',
                                Month == '04' & Day == '26' & Hour == '00' ~ '04-25-Night',
                                
                                Month == '04' & Day == '26' & Hour == '11' ~ '04-26-Morning',
                                Month == '04' & Day == '26' & Hour == '14' ~ '04-26-Midday',
                                Month == '04' & Day == '26' & Hour == '22' ~ '04-26-Night',
                                
                                Month == '04' & Day == '27' & Hour == '10' ~ '04-27-Morning',
                                Month == '04' & Day == '27' & Hour == '17' ~ '04-27-Midday',
                                Month == '04' & Day == '28' & Hour == '00' ~ '04-27-Night',
                                
                                Month == '04' & Day == '28' & Hour == '11' ~ '04-28-Morning',
                                Month == '04' & Day == '28' & Hour == '14' ~ '04-28-Midday',
                                Month == '04' & Day == '28' & Hour == '22' ~ '04-28-Night',
                                
                                T ~ ImageLabel)) %>% 
  arrange(ImageLabel) %>% 
  mutate(SecondCounter = as.numeric(Second) + 
           60 * as.numeric(Minute) + 
           3600 * as.numeric(Hour)) %>% 
  as.data.frame() 
# cleanImageDF %>% filter(ImageLabel == 'Unknown')
# cleanImageDF %>% filter(grepl('04-26',ImageLabel)) %>% arrange(ImageLabel)
# fileInfo %>% filter(Month == '04' & Day == '25')

# save(cleanImageDF,file = 'CleanImageDateLabels.Rdata')
# write.csv(cleanImageDF,file = 'CleanImageDateLabels.csv')

threeImgEx = cleanImageDF %>% head(3) %>% arrange(SecondCounter)
imageTrio = image_read(threeImgEx$File)
image_append(imageTrio)

makeCombinedHairTrio <- function(dateTime){
  threeImg = cleanImageDF %>% filter(ImageLabel == dateTime) %>% arrange(SecondCounter)
  image_write(image_append(image_read(c(threeImg$File[1],threeImg$File[3],threeImg$File[2]))),
              path = paste0('Trios/',dateTime,'.png'),format = 'png')
}

uniquePicTrios = unique(cleanImageDF$ImageLabel)
lapply(uniquePicTrios,makeCombinedHairTrio)
