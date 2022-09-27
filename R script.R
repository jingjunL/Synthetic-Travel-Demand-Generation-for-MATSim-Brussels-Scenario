library(dplyr)
library(tidyr)
library(StatMatch)
library(readxl)
library(sf)
library(tmap)
library(sfheaders)
library(collapse)
library(tibble)
library(units)
library(XML)
library(tidyverse)
library(rstudioapi)

#set wd to current path
current_path <- getActiveDocumentContext()$path
if (getwd() != current_path){
  setwd(dirname(current_path ))
}

#Set the random seed for current script (as the current time for running this script)
set.seed(532034007)
oldTime <- Sys.time()

#Set the population percentage (0.01 represent 1% scenario)
populationPercent <- 0.00001

#setwd to forMONITOR folder
postal_codes <- read_excel("Postcode.xlsx")

#read in adult and child data
adult_intake <- read.csv2("MONITOR/Data/Adults/Intake.csv")
adult_trips <- read.csv2("MONITOR/Data/Adults/Trips.csv")
adult_moves <- read.csv2("MONITOR/Data/Adults/Moves.csv")

kid_intake <- read.csv2("MONITOR/Data/Kids/Intake_K.csv")
kid_trips <- read.csv2("MONITOR/Data/Kids/Trips_K.csv")
kid_moves <- read.csv2("MONITOR/Data/Kids/Moves_K.csv")

#selection of attributes for each data frame
individuals_attributes <- c("p_id", "Beroep", "Diploma", "Geslacht", "huishoud", 
                            "Leeftijd3N", "Postcode", "p_yesterday", "Region",
                            "V006b", "V007", "V014", "Weging",	"WegingPop",	"Year_Birth")

col_names <- c("p_id", "OccupationDomain", "HighestDegree", "Gender", "HouseholdStatus", "Age",
               "Postcode", "p_yesterday", "Region",
               "DriverLicenseAvailability", "WorkingStatus(Full/Part)",
               "HouseholdNetIncome", "Weging",	"WegingPop",	"Year_Birth")

trips_attributes <- c("t_id", "t_participant_id", "t_source_hour",
                      "t_source_minute", "t_source", "t_source_foreign",
                      "t_source_id",	"t_destination_hour",
                      "t_destination_minute",	"t_destination",
                      "t_destination_foreign", "t_reason",
                      "t_reason_other",	"duration",
                      "t_source_PostCode", "t_destination_PostCode")

moves_attributes <- c("m_id","m_track_id","m_participant_id", 
                      "m_way","m_distance","m_duration_tot")

#Step 1: Processing MONITOR data
#Processing adult MONITOR intake data
adult_intake <- adult_intake %>% 
  select(all_of(individuals_attributes))

#Change column name from original Dutch (in the MONITOR data) to English (col_names defined above)
names(adult_intake) <- col_names

adult_intake <- adult_intake %>% 
  mutate(p_yesterday = as.Date(p_yesterday, "%d/%m/%Y"))

#Adjust MONITOR age data to the age group used in the METHODOLOGY
#Filter out Brussels residents' records during weekdays, calculate the exact age
adult_intake <- adult_intake %>% 
  mutate(Week = weekdays(p_yesterday),
         AgeExact = 2017 - Year_Birth,
         HighestDegreeCat = recode(HighestDegree, '1' = "1-2", '2' = "1-2",
                                   '3' = "3-4", '4' = "3-4", '5' = "3-4", '6' = "5-6",
                                   '7' = "7-8", '8' = "7-8", '9' = "7-8")) %>%
  mutate(AgeGroupMethodology = ifelse(AgeExact >= 18 & AgeExact <= 34, 1,
                                      ifelse(AgeExact >= 35 & AgeExact <= 49, 2,
                                             ifelse(AgeExact >= 50 & AgeExact <= 64, 3, 4)))) %>% 
  filter(Week != "Saturday", Week != "Sunday") %>% 
  filter(Region == "Bruxelles")

#Processing adult MONITOR trip data
#300 people with corresponding attributes, same result with the excel. It means one-sixth of individual in Brussels don't have any trips at all during the whole day
adult_trips <- adult_trips %>% 
  select(all_of(trips_attributes)) %>% 
  filter(t_participant_id %in% adult_intake$p_id)

#Calculate the overall population home people represent
onlyHomePeople <- adult_intake %>% 
  filter(!p_id %in% adult_trips$t_participant_id)

#Sum the overall population of these people
sum(onlyHomePeople$WegingPop)

rm(onlyHomePeople)

#11 other reasons here
table(adult_trips$t_reason)

#Manually check these other reasons to see if it is indeed "other reasons"
adult_trips_otherReasons <- filter(adult_trips, adult_trips$t_reason == 8)

#trip_id 8768 should change to 7, 7727, 5652 to 6
adult_trips$t_reason[adult_trips$t_id == 8768] <- 7
adult_trips$t_reason[adult_trips$t_id %in% c(7727, 5652)] <- 6

#Justify whether trips' origins and destinations are foreign (=2), non-Bru (=1) or withinBru (=0)
adult_trips$tripRangeTypeOrigin <- ifelse(adult_trips$t_source_foreign == 1, 2, 
                                          ifelse(adult_trips$t_source_PostCode %in% postal_codes$Postcode, 0, 1))

adult_trips$tripRangeTypeDestin <- ifelse(adult_trips$t_destination_foreign == 1, 2,
                                          ifelse(adult_trips$t_destination_PostCode %in% postal_codes$Postcode, 0, 1))

#Initial a counter
adult_trips$counter <- 1

#Using formula order: https://www.cnblogs.com/liujiaxin2018/p/13735781.html
adult_trips <- adult_trips[order(adult_trips[,'t_participant_id'],adult_trips[,'t_id']),]

#Arrange the sequence of inidividual trips
adult_trips <- adult_trips %>% 
  group_by(t_participant_id) %>% 
  mutate(sequenceTrip = cumsum(counter))

#Calculate the typical duration of the last activity
adult_trips <- adult_trips %>% 
  mutate(endHourLastTrip = lag(t_destination_hour)) %>% 
  mutate(endMinuteLastTrip = lag(t_destination_minute))

adult_trips$typicalDurationLastActivity <- ifelse(is.na(adult_trips$t_source_id), 
                                                  adult_trips$t_source_hour*60 + adult_trips$t_source_minute,
                                                  adult_trips$t_source_hour*60 + adult_trips$t_source_minute - adult_trips$endHourLastTrip * 60 - adult_trips$endMinuteLastTrip)

#Table show 666 trips are within Brussels
table(adult_trips$tripRangeTypeOrigin, adult_trips$tripRangeTypeDestin)

#Processing adult MONITOR moves data
adult_moves <- adult_moves %>% 
  select(all_of(moves_attributes)) %>% 
  mutate(mode = recode(m_way, '1' = "walk", '2' = "bike", '3' = "motorcycle", '4' = "train",
                       '5' = "pt", '6' = "pt", '7' = "car", '8' = "ride",
                       '9' =  "car", '10' = "ride", '11' = "motorcycle",  '12' = "motorcycle",
                       '13' = "motorcycle", '14' = "motorcycle", '15' = "car", '16' = "motorcycle",
                       '17' = "car", '18' = 'car', '19' = "car", '20' = "other")) %>% 
  filter(m_participant_id %in% adult_intake$p_id)

#Calculate the trips that don't have a corresponding moves (1 trip)
length(unique(adult_trips$t_id)) - sum(adult_trips$t_id %in% adult_moves$m_track_id)
sum(!(adult_trips$t_id %in% adult_moves$m_track_id)) 

#Join the trip with the move data
adult_trip_move <- left_join(adult_trips, adult_moves, by = c("t_id" = "m_track_id"))

#Handling the NA scenario (p_id 8870, only one trip made within that day, no moves data)
#Filter out other trips that t_source and t_destination are "1140 - EVERE" and source_hour larger than 14 (same as missing NA trip)
adult_trip_move$m_id[adult_trip_move$t_participant_id == 8870] <- 99999
adult_trip_move$m_participant_id[adult_trip_move$t_participant_id == 8870] <- 8870
adult_trip_move$m_way[adult_trip_move$t_participant_id == 8870] <- 1
adult_trip_move$m_distance[adult_trip_move$t_participant_id == 8870] <- 1.0
adult_trip_move$m_duration_tot[adult_trip_move$t_participant_id == 8870] <- 20
adult_trip_move$mode[adult_trip_move$t_participant_id == 8870] <- "walk"

#Purpose for below step is ensure there is only one move for each trip, do so by combining different distances and filter out the unnecessary transport modes (like walk-pt-walk, it should be pt)
adult_trip_move$mode_order <- ifelse(adult_trip_move$mode == "other", 0, ifelse(adult_trip_move$mode == "walk", 1, ifelse(adult_trip_move$mode == "bike", 2, ifelse(adult_trip_move$mode == "motorcycle", 3, 4))))
adult_trip_move <- adult_trip_move[order(adult_trip_move[,'t_id'],-adult_trip_move[,'mode_order'],-adult_trip_move[,'m_distance']),]
adult_trip_distance <- adult_trip_move %>% 
  group_by(t_id) %>% 
  summarise(tripDistance = sum(m_distance)) 
adult_trip_move <- adult_trip_move[!duplicated(adult_trip_move$t_id),]
adult_trip_move <- left_join(adult_trip_move, adult_trip_distance, by = "t_id")
adult_trip_move <- adult_trip_move %>% 
  mutate(durationHour = duration/60) %>% 
  mutate(speedTrip = tripDistance/durationHour)

#Still, there are five others after this step, need to manually categorise these others into known transport modes (calculate the speed and estimate the mode)
adult_trip_move$mode[adult_trip_move$t_id == 342] <- "walk"
adult_trip_move$mode[adult_trip_move$t_id == 10071] <- "pt"
adult_trip_move$mode[adult_trip_move$t_id == 10072] <- "bike"
adult_trip_move$mode[adult_trip_move$t_id == 15623] <- "car"
adult_trip_move$mode[adult_trip_move$t_id == 21388] <- "bike"

adult_number_of_trips <- adult_trip_move %>% group_by(t_participant_id) %>% count()

adult_trip_move <- adult_trip_move %>% 
  mutate(tripOriginInMinute = t_source_hour*60 + t_source_minute) %>% 
  mutate(tripDestinInMinute = t_destination_hour*60 + t_destination_minute) %>% 
  group_by(t_participant_id) %>% 
  mutate(sequenceCheck = tripOriginInMinute - lag(tripDestinInMinute)) 
#By checking the negative value in the sequenceCheck, we find three people who didn't report their activities correctly in one day, need to correct them manully
adult_trip_move_editSequence <- filter(adult_trip_move, t_participant_id == 4194)
adult_trip_move <- adult_trip_move[-which(adult_trip_move$t_participant_id == 4194),]
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_id = t_id + 1) 
adult_trip_move_editSequence$t_id[adult_trip_move_editSequence$t_id == 5522] <- 5511
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_source_id = t_source_id + 1)
adult_trip_move_editSequence$t_source_id[is.na(adult_trip_move_editSequence$t_source_id)] <- 5511
adult_trip_move_editSequence$t_source_id[adult_trip_move_editSequence$t_source_id == 5521] <- NA
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, sequenceTrip = lead(sequenceTrip))
adult_trip_move_editSequence$sequenceTrip[adult_trip_move_editSequence$t_id == 5511] <- 1
adult_trip_move <- rbind(adult_trip_move, adult_trip_move_editSequence)

#For respondent 9800
adult_trip_move_editSequence <- filter(adult_trip_move, t_participant_id == 9800)
adult_trip_move <- adult_trip_move[-which(adult_trip_move$t_participant_id == 9800),]
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_id = lead(t_id))
adult_trip_move_editSequence$t_id[is.na(adult_trip_move_editSequence$t_id)] <- 12191
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_source_id = lead(t_source_id))
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, sequenceTrip = lead(sequenceTrip))
adult_trip_move_editSequence$sequenceTrip[adult_trip_move_editSequence$t_id == 12191] <- 1
adult_trip_move <- rbind(adult_trip_move, adult_trip_move_editSequence)

#For respondent 8925
adult_trip_move_editSequence <- filter(adult_trip_move, t_participant_id == 8925)
adult_trip_move <- adult_trip_move[-which(adult_trip_move$t_participant_id == 8925),]
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_id = lead(t_id))
adult_trip_move_editSequence$t_id[is.na(adult_trip_move_editSequence$t_id)] <- 11517
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_source_id = lead(t_source_id))
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, sequenceTrip = lead(sequenceTrip))
adult_trip_move_editSequence$sequenceTrip[adult_trip_move_editSequence$t_id == 11517] <- 1
adult_trip_move <- rbind(adult_trip_move, adult_trip_move_editSequence)

#Recode the trip purpose
adult_trip_move$tripReasonString <- recode(adult_trip_move$t_reason,
                                           '1' = "work", '2' = "school", '3' = "work", '4' = "work",
                                           '5' = "shopping", '6' = "leisure", '7' = "home", '8' = "other")

#View the activity sequences of each MONITOR individual
adult_activitiesSequence <- adult_trip_move %>% 
  group_by(t_participant_id) %>% 
  summarise(tripChain = paste(tripReasonString, collapse = ","))

#Reorder the trip_move data due to change of sequences in the previous steps
adult_trip_move <- adult_trip_move[order(adult_trip_move[,'t_participant_id'],adult_trip_move[,'t_id']),]

#Check the sequence again
adult_trip_move <- adult_trip_move %>% 
  mutate(tripOriginInMinute = t_source_hour*60 + t_source_minute) %>% 
  mutate(tripDestinInMinute = t_destination_hour*60 + t_destination_minute) %>% 
  group_by(t_participant_id) %>% 
  mutate(sequenceCheck = tripOriginInMinute - lag(tripDestinInMinute)) 

#Adapt the typical duration last activity to the right volume (remove the negative from line 180 to 220)
adult_trip_move <- adult_trip_move %>% 
  mutate(endHourLastTrip = lag(t_destination_hour)) %>% 
  mutate(endMinuteLastTrip = lag(t_destination_minute))

adult_trip_move$typicalDurationLastActivity <- ifelse(is.na(adult_trip_move$t_source_id), 
                                                      adult_trip_move$t_source_hour*60 + adult_trip_move$t_source_minute,
                                                      adult_trip_move$t_source_hour*60 + adult_trip_move$t_source_minute - adult_trip_move$endHourLastTrip * 60 - adult_trip_move$endMinuteLastTrip)

#For kids trip
kid_intake <- kid_intake %>% 
  select(p_id, Gender = Geslacht, AgeGroup = Leeftijd, Postcode, p_yesterday, Region = RegionFR, WegingPop)

kid_intake <- kid_intake %>% 
  mutate(p_yesterday = as.Date(p_yesterday, "%Y-%m-%d")) %>% 
  mutate(Week = weekdays(p_yesterday)) %>% 
  mutate(AgeGroupMethodology = ifelse(AgeGroup == 3, "2K", ifelse(AgeGroup == 2, "2K", "1K"))) %>% 
  filter(Week != "Saturday", Week != "Sunday") %>% 
  filter(Region == "Bruxelles")

kid_trips <- kid_trips %>% 
  select(all_of(trips_attributes)) %>% 
  filter(t_participant_id %in% kid_intake$p_id)

kid_trips$counter <- 1

kid_trips <- kid_trips[order(kid_trips[,'t_participant_id'], kid_trips[,'t_id']),]

kid_trips <- kid_trips %>% 
  group_by(t_participant_id) %>% 
  mutate(sequenceTrip = cumsum(counter))

kid_trips <- kid_trips %>% 
  mutate(endHourLastTrip = lag(t_destination_hour)) %>% 
  mutate(endMinuteLastTrip = lag(t_destination_minute))

kid_trips$typicalDurationLastActivity <- ifelse(is.na(kid_trips$t_source_id), 
                                                kid_trips$t_source_hour*60 + kid_trips$t_source_minute,
                                                kid_trips$t_source_hour*60 + kid_trips$t_source_minute - kid_trips$endHourLastTrip * 60 - kid_trips$endMinuteLastTrip)
#No other reason (t_reason = 8) and foreign trip in kids data

kid_trips$tripRangeTypeOrigin <- ifelse(kid_trips$t_source_PostCode %in% postal_codes$Postcode, 0, 1)
kid_trips$tripRangeTypeDestin <- ifelse(kid_trips$t_destination_PostCode %in% postal_codes$Postcode, 0, 1)

kid_trips <- kid_trips[order(kid_trips[,'t_participant_id'],kid_trips[,'t_id']),]

kid_moves <- kid_moves %>% 
  select(all_of(moves_attributes)) %>% 
  mutate(mode = recode(m_way, '1' = "walk", '2' = "bike", '3' = "motorcycle", '4' = "train",
                       '5' = "pt", '6' = "pt", '7' = "car", '8' = "ride",
                       '9' =  "car", '10' = "ride", '11' = "motorcycle",  '12' = "motorcycle",
                       '13' = "motorcycle", '14' = "motorcycle", '15' = "car", '16' = "motorcycle",
                       '17' = "car", '18' = 'car', '19' = "car", '20' = "other")) %>% 
  filter(m_participant_id %in% kid_intake$p_id)

#All kid trip data has a corresponding moves
kid_trip_move <- left_join(kid_trips, kid_moves, by = c("t_id" = "m_track_id"))

kid_trip_move$mode_order <- ifelse(kid_trip_move$mode == "other", 0, ifelse(kid_trip_move$mode == "walk", 1, ifelse(kid_trip_move$mode == "bike", 2, ifelse(kid_trip_move$mode == "motorcycle", 3, 4))))
kid_trip_move <- kid_trip_move[order(kid_trip_move[, 't_id'], -kid_trip_move[,'mode_order'], -kid_trip_move[,'m_distance']),]
kid_trip_distance <- kid_trip_move %>% 
  group_by(t_id) %>% 
  summarise(tripDistance = sum(m_distance))
kid_trip_move <- kid_trip_move[!duplicated(kid_trip_move$t_id),]
kid_trip_move <- left_join(kid_trip_move, kid_trip_distance, by = "t_id")
kid_trip_move <- kid_trip_move %>% 
  mutate(durationHour = duration/60) %>% 
  mutate(speedTrip = tripDistance/durationHour)

#Change to "pt" as kid cannot drive own cars
kid_trip_move$mode[kid_trip_move$t_id == 42007 | kid_trip_move$t_id == 42009] <- "pt"

kid_number_of_trips <- kid_trip_move %>% group_by(t_participant_id) %>% count()

kid_trip_move <- kid_trip_move %>% 
  mutate(tripOriginInMinute = t_source_hour*60 + t_source_minute) %>% 
  mutate(tripDestinInMinute = t_destination_hour*60 + t_destination_minute) %>% 
  group_by(t_participant_id) %>% 
  mutate(sequenceCheck = tripOriginInMinute - lag(tripDestinInMinute)) 

#Only one person who didn't report activities correctly in time sequence (t_participant_id == 10447)
kid_trip_move_editSequence <- filter(kid_trip_move, t_participant_id == 10447)
kid_trip_move <- kid_trip_move[-which(kid_trip_move$t_participant_id == 10447),]
kid_trip_move_editSequence <- mutate(kid_trip_move_editSequence, t_id = lead(t_id))
kid_trip_move_editSequence$t_id[is.na(kid_trip_move_editSequence$t_id)] <- 12459
kid_trip_move_editSequence <- mutate(kid_trip_move_editSequence, t_source_id = lead(t_source_id))
kid_trip_move_editSequence <- mutate(kid_trip_move_editSequence, sequenceTrip = lead(sequenceTrip))
kid_trip_move_editSequence$sequenceTrip[kid_trip_move_editSequence$t_id == 12459] <- 1
kid_trip_move <- rbind(kid_trip_move, kid_trip_move_editSequence)

kid_trip_move$tripReasonString <- recode(kid_trip_move$t_reason,
                                         '1' = "work", '2' = "school", '3' = "work", '4' = "work",
                                         '5' = "shopping", '6' = "leisure", '7' = "home", '8' = "other")

kid_trip_move <- kid_trip_move[order(kid_trip_move[,'t_participant_id'], kid_trip_move[, 't_id']),]

kid_trip_move <- kid_trip_move %>% 
  mutate(endHourLastTrip = lag(t_destination_hour)) %>% 
  mutate(endMinuteLastTrip = lag(t_destination_minute))

kid_trip_move$typicalDurationLastActivity <- ifelse(is.na(kid_trip_move$t_source_id), 
                                                    kid_trip_move$t_source_hour*60 + kid_trip_move$t_source_minute,
                                                    kid_trip_move$t_source_hour*60 + kid_trip_move$t_source_minute - kid_trip_move$endHourLastTrip * 60 - kid_trip_move$endMinuteLastTrip)

#Finish kid MONITOR data processing

adult_trip_move[adult_trip_move$t_id == 10837,]$tripRangeTypeOrigin <- 0

adult_trip_move[adult_trip_move$t_id == 55569,]$tripDistance <- 0.1


adult_trip_move$tripDistance <- ifelse(adult_trip_move$tripDistance >= 200,
                                       160,
                                       adult_trip_move$tripDistance)

#Change the "pt" trip mode that is within Brussels (!(tripRangeTypeOrigin == 0 & tripRangeTypeDestin == 0)) to "ptOutsideBRU" (so that in MATSim they are teleported instead of using the Brussels pt schedule)
adult_trip_move[adult_trip_move$t_id == 2461,]$mode <- "ptOutsideBRU"
adult_trip_move[adult_trip_move$t_id == 2462,]$mode <- "ptOutsideBRU"
adult_trip_move[adult_trip_move$t_id == 5354,]$mode <- "ptOutsideBRU"
adult_trip_move[adult_trip_move$t_id == 5355,]$mode <- "ptOutsideBRU"
adult_trip_move[adult_trip_move$t_id == 54840,]$mode <- "ptOutsideBRU"
adult_trip_move[adult_trip_move$t_id == 54841,]$mode <- "ptOutsideBRU"

kid_trip_move[kid_trip_move$t_id == 42007,]$mode <- "ptOutsideBRU"
kid_trip_move[kid_trip_move$t_id == 42009,]$mode <- "ptOutsideBRU"


#for adult_trip_move
#for id 304
adult_trip_move[adult_trip_move$t_id %in% c(749, 750, 751),]$tripReasonString <- "other"

#for id 3347
adult_trip_move[adult_trip_move$t_id %in% c(10252),]$tripReasonString <- "other"

#for id 17458
adult_trip_move[adult_trip_move$t_id %in% c(19916, 19917),]$tripReasonString <- "other"

#for id 23504
adult_trip_move[adult_trip_move$t_id %in% c(24178, 24179),]$tripReasonString <- "other"



#for kid_trip_move
#for id 26044
kid_trip_move[kid_trip_move$t_id %in% c(25235, 25236),]$tripReasonString <- "other"

#for id 27086
kid_trip_move[kid_trip_move$t_id %in% c(27651, 27652),]$tripReasonString <- "other"


#Step 2: Processing the DemoBel data
demobel <- read.csv("demobelStatbel.csv")
#545919 households in demobel

#Parameter: the percent of people modelled in the plan file (1% for now)
#Randomly select 1% of the rows representing 10% Brussels population
demobel <- demobel[sample(nrow(demobel), populationPercent*nrow(demobel)),]

#As there are 55344 records in demoBel don't have employment records, need to replace them with a random selection
demobel_personWithoutEmployment <- demobel %>% 
  filter(CAS == "")

demobel <- demobel %>% 
  filter(CAS != "")

CASDistribution <- data.frame(CASCat = c("ACT", "EDUC", "EMP", "HOME_IO", "INC", "LT_MWA", "UNE"),
                              CASFrequency = c(21301, 83963, 458120, 169733, 144842, 224422, 33879))

#Assign job based on the percent of the available data
for (i in 1:nrow(demobel_personWithoutEmployment)) {
  randomCAS = slice_sample(CASDistribution, weight_by = CASDistribution$CASFrequency)
  demobel_personWithoutEmployment[i,]$CAS <- randomCAS$CASCat
  
  if (i %% 500 == 0) {
    print(paste("Step 2", i, "/", nrow(demobel_personWithoutEmployment), "people have assigned a random job!"))
  }
}

#Bind the new data with the remaining demobel data
demobel <- rbind(demobel, demobel_personWithoutEmployment)

rm(demobel_personWithoutEmployment, CASDistribution, randomCAS)

#rename the demobel people ID to the counting number in order for a easier tracking in the coming stage
demobel <- rowid_to_column(demobel)

demobel_reference <- demobel %>% 
  select(personID = rowid, householdID = ID_HH_C, statisticalSector = CD_SECTOR,
         age = MS_AGE, gender = CD_SEX, highestDegree = edu2017, 
         incomeLevel = INC_HH_EQUI_DEC, carAvailability = hasvhc, workingStatus = CAS)

demobel <- demobel_reference %>% 
  select(personID, householdID, statisticalSector, age, gender, highestDegree)

#Transfer the statistical sector to NB
BrusselsSSDivision <- st_read("BelgiumStatisticalSector/sh_statbel_statistical_sectors_20180101.shp")
BrusselsSSDivision <- BrusselsSSDivision %>% filter(T_REGIO_FR == "Région de Bruxelles-Capitale")
BruSSInPoint <- st_centroid(BrusselsSSDivision)
BrusselsNBShape <- st_read("BrusselsNeighbourhood/UrbAdm_MONITORING_DISTRICT.shp", crs = 31370)
BrusselsNBShape[128,]$NAME_FRE <- "CHAUSSEE DE WAVRE - SAINT-JULIEN"

BruSSJoinedWithNB <- st_join(BruSSInPoint, BrusselsNBShape)
BruSSJoinedWithNB <- BruSSJoinedWithNB %>% 
  select(SSName = CS01012018, NBFrench = NAME_FRE)

demobel <- demobel %>% 
  left_join(BruSSJoinedWithNB, by = c("statisticalSector" = "SSName")) %>% 
  select(personID, householdID, neighbourhood = NBFrench, age, gender, highestDegree)

demobel <- demobel %>% 
  filter(!is.na(neighbourhood))

demobel$xHomeCoord <- 0
demobel$yHomeCoord <- 0

#Read the home location URBIS data
home_location_full <- read.csv("homeLocationDeriveByR.csv")
home_location_loop <- home_location_full

demobel_split <- demobel %>%
  group_by(householdID) %>%
  group_split()

counter <- 0

demobel_collector <- demobel[0,]

for (a in 1:length(demobel_split)) {
  demobel <- demobel_split[[a]]
  
  #Get the neighbourhood household belongs
  neighbourhood_a <- as.character(demobel[1,]$neighbourhood)
  
  #Get all current home location for this nb
  home_location_selection <- home_location_loop[home_location_loop$NAME_FRE == neighbourhood_a,]
  
  #(in case there is no home available within the selected neighbourhood area)
  if (nrow(home_location_selection) == 0) {
    home_location_selection <- home_location_full[home_location_full$NAME_FRE == neighbourhood_a,]
  }
  
  random_house <- slice_sample(home_location_selection, replace = TRUE)
  home_location_loop <- home_location_loop[home_location_loop$homeID != random_house$homeID,]
  
  demobel$xHomeCoord<- random_house$xHomeCoord
  demobel$yHomeCoord <- random_house$yHomeCoord
  
  demobel_collector <- rbind(demobel_collector, demobel)
  
  counter <- counter + 1
  
  if (counter %% 100 == 0) {
    print(paste("Step 2", counter, "/", length(demobel_split), "households have assigned a random home location!"))
  }
}

demobel <- demobel_collector

rm(demobel_collector, demobel_split)

#Filter out the demoBel agents that are less than 6 (as we assume they are not moving within a day and we do not have the corresponding data)
demobel <- demobel %>% filter(age >5)

#Assign demoBel individual age, gender and highest degree to the same code with MONITOR data
demobel$ageGroup <- ifelse(demobel$age <= 12, "1K", 
                           ifelse(demobel$age < 18, "2K", 
                                  ifelse(demobel$age >= 18 & demobel$age <= 34, "1", 
                                         ifelse(demobel$age >= 35 & demobel$age <= 49, "2",
                                                ifelse(demobel$age >= 50 & demobel$age <= 64, "3", "4")))))

demobel$genderN <- ifelse(demobel$gender == "1", 1, 2)

demobel$degree <- recode(demobel$highestDegree, 
                         'NAP' = "1-2", '0' = "1-2",
                         '1' = "1-2", '2' = "1-2",
                         '3' = "3-4", '4' = "3-4", 
                         '5' = "5-6", '6' = "7-8",
                         '7' = "7-8", '8' = "7-8", "UNK" = "UNK")

#For UNK people (unknown edu level), randomly assign one based on percentage of the existing data
BrusselsDegreeLevel <- data.frame(degree = c("1-2", "3-4", "5-6", "7-8"),
                                  weight = c(41.75, 15.14, 0.07, 18.85))

demobel_unknowEDUFilter <- demobel %>% 
  filter(highestDegree == "UNK")

demobel <- demobel %>% 
  filter(highestDegree != "UNK")

for (i in 1:nrow(demobel_unknowEDUFilter)) {
  degreeSelected <- slice_sample(BrusselsDegreeLevel, weight_by = BrusselsDegreeLevel$weight)
  demobel_unknowEDUFilter[i,]$degree <- degreeSelected$degree
}

demobel <- rbind(demobel, demobel_unknowEDUFilter)

rm(demobel_unknowEDUFilter, degreeSelected, BrusselsDegreeLevel)

#Matching age, gender, highest degree for adults
demobel_adults <- demobel %>% filter(age >= 18)

#Starting of the matching demoBel data to the MONITOR data for predicting individual activity chain
matching_df <- adult_intake %>% 
  select(p_id, AgeGroupMethodology, Gender, AgeExact, HighestDegreeCat, WegingPop) %>% 
  mutate(AgeGroupMethodology = as.character(AgeGroupMethodology))

matching_df <- matching_df %>% 
  mutate(WegingPop = round(WegingPop*populationPercent))

demobel_adultsSummary <- demobel_adults %>% 
  count(genderN, ageGroup, degree)

monitorSummary <- matching_df %>% 
  count(Gender, AgeGroupMethodology, HighestDegreeCat, wt = WegingPop)

monitorSummary <- complete(monitorSummary, Gender, AgeGroupMethodology, HighestDegreeCat, fill = list(n = 0))

demobel_adult_matched_attribute <- left_join(demobel_adults[1,], matching_df, by = c("ageGroup" = "AgeGroupMethodology", "genderN" = "Gender", "degree" = "HighestDegreeCat"))[0,]

for (i in 1:nrow(demobel_adultsSummary)) {

  attributeCombination <- demobel_adultsSummary[i,]
  
  genderSelected <- attributeCombination$genderN
  ageGroupSelected <- attributeCombination$ageGroup
  degreeSelected <- attributeCombination$degree

  attributeAdults <- demobel_adults %>% 
    filter(genderN == genderSelected & ageGroup == ageGroupSelected & degree == degreeSelected)
  
  monitorNumber <- monitorSummary %>% 
    filter(Gender == genderSelected & AgeGroupMethodology == ageGroupSelected & HighestDegreeCat == degreeSelected)
  
  if (nrow(attributeAdults) <= monitorNumber$n) {
    #in this case, we have less demobel people with the 3 attribute combination than real monitor
    attributeAdults <- left_join(attributeAdults, matching_df, by = c("ageGroup" = "AgeGroupMethodology", "genderN" = "Gender", "degree" = "HighestDegreeCat"))
    
    #Select one monitor sample for each demobel person according to the weight (as we did before)
    attributeAdults <- attributeAdults %>% 
      group_by(personID) %>% 
      slice_sample(n=1, weight_by = WegingPop)
    
    #For this situation, finish
  } else {
    #first, randomly select the number of monitor represent people from the real demobel data
    attributeAdults_monitorWeightNumber <- attributeAdults %>% 
      slice_sample(n = monitorNumber$n)
    
    #for these people, do the matching based on three attribute (so that we have at least monitor weight number of these people's travel behaviour in the data)
    attributeAdults_monitorWeightNumber <- left_join(attributeAdults_monitorWeightNumber, matching_df, by = c("ageGroup" = "AgeGroupMethodology", "genderN" = "Gender", "degree" = "HighestDegreeCat"))
    
    #randomly select based on weight
    attributeAdults_monitorWeightNumber <- attributeAdults_monitorWeightNumber %>% 
      group_by(personID) %>% 
      slice_sample(n=1, weight_by = WegingPop)
    
    #filter the remaining demobel people still unmatched
    attributeAdults_matchedByTwo <- attributeAdults %>% 
      filter(!personID %in% attributeAdults_monitorWeightNumber$personID)
    
    #we have ensured that at least 15 (minimal 18 in table) samples are available for each age+gender combination, so that the overfitting problem will be much released
    attributeAdults_matchedByTwo <- left_join(attributeAdults_matchedByTwo, matching_df, by = c("ageGroup" = "AgeGroupMethodology", "genderN" = "Gender"))
    
    attributeAdults_matchedByTwo <- attributeAdults_matchedByTwo %>% 
      group_by(personID) %>% 
      slice_sample(n=1, weight_by = WegingPop)
    
    #Delete the additional HighestDrgree column added because of the left_join of two variables
    attributeAdults_matchedByTwo <- attributeAdults_matchedByTwo %>% 
      select(-HighestDegreeCat)
    
    #all matched, join the two tables for the final results
    attributeAdults <- rbind(attributeAdults_monitorWeightNumber, attributeAdults_matchedByTwo)
  }
  
  #collect attribute adult outside the for loop
  demobel_adult_matched_attribute <- rbind(demobel_adult_matched_attribute, attributeAdults)
}


demobel_adult_matched_attribute <- left_join(demobel_adult_matched_attribute, adult_number_of_trips, by = c("p_id" = "t_participant_id"))

adultNoActivity <- demobel_adult_matched_attribute %>% 
  filter(is.na(n))

demobel_adult_matched_attribute <- na.omit(demobel_adult_matched_attribute)

demobel_adult_matched_attribute <- demobel_adult_matched_attribute %>% 
  select(personID, householdID, neighbourhood, age, ageGroup, gender, highestDegree = degree, 
         xHomeCoord, yHomeCoord, p_id, WegingPop, numberOfTrips = n)

#Step 3: Matching the Demobel with Trip information and complete all information
demobel_adult_matched_activities <- left_join(demobel_adult_matched_attribute, adult_trip_move, by = c("p_id" = "t_participant_id"))

demobel_adult_matched_activities <- demobel_adult_matched_activities %>%
  select(personID, homeNB = neighbourhood, ageGroup, xHomeCoord, yHomeCoord, p_id, t_id, t_source_hour, t_source_minute, t_source_id, duration, tripRangeTypeOrigin, 
         tripRangeTypeDestin, sequenceTrip, typicalDurationLastActivity, m_id, mode, tripDistance, tripReasonString, desPostcode = t_destination_PostCode)

demobel_adult_matched_activities <- demobel_adult_matched_activities %>% 
  mutate(randomTimeNormalVariants = round(rnorm(1, mean = 0, sd=15))) %>% 
  mutate(startTimeInMinute = t_source_hour*60 + t_source_minute + randomTimeNormalVariants)

demobel_adult_matched_activities$startTimeInMinute <- ifelse(demobel_adult_matched_activities$startTimeInMinute < 0,
                                                             0,
                                                             demobel_adult_matched_activities$startTimeInMinute)

demobel_adult_matched_activities$startTimeInMinute <- ifelse(demobel_adult_matched_activities$startTimeInMinute > 1440,
                                                             1440,
                                                             demobel_adult_matched_activities$startTimeInMinute)

#Assign the new start time for each first trip
demobel_adult_matched_activities$t_source_hour <- ifelse(is.na(demobel_adult_matched_activities$t_source_id),
                                                         floor(demobel_adult_matched_activities$startTimeInMinute/60),
                                                         demobel_adult_matched_activities$t_source_hour)

demobel_adult_matched_activities$t_source_minute <- ifelse(is.na(demobel_adult_matched_activities$t_source_id),
                                                           demobel_adult_matched_activities$startTimeInMinute %% 60,
                                                           NA)

demobel_adult_matched_activities <- demobel_adult_matched_activities %>% 
  select(-randomTimeNormalVariants, -startTimeInMinute)

#Calcute the first departure, arrival, endNextActivity time of individual
demobel_adult_matched_activities <- demobel_adult_matched_activities %>% 
  mutate(departureTimeInMinute = t_source_hour*60 + t_source_minute) %>%
  mutate(arrivalTimeInMinute = departureTimeInMinute + duration) %>% 
  mutate(arrivalHour = arrivalTimeInMinute %/% 60) %>% 
  mutate(arrivalMinute = arrivalTimeInMinute %% 60) %>% 
  mutate(typicalDurationNextActivity = lead(typicalDurationLastActivity)) %>% 
  mutate(finishActivityTimeInMinute = arrivalTimeInMinute + typicalDurationNextActivity) %>% 
  mutate(nextTripDepartHour = finishActivityTimeInMinute %/% 60) %>% 
  mutate(nextTripDepartMinute = finishActivityTimeInMinute %% 60)

#Calculate the rest of departure and arrival minutes
demobel_adult_matched_activities_split <- group_split(demobel_adult_matched_activities)

counter <- 0

demobel_adult_matched_activities_collector <- demobel_adult_matched_activities_split[[1]][0,]

for (a in 1:length(demobel_adult_matched_activities_split)){
  demobel_adult_matched_activities <- demobel_adult_matched_activities_split[[a]]
  
  #Loop as before (but now the looped DF is the smaller component with 4 lines)
  for (i in 1:nrow(demobel_adult_matched_activities)) {
    currentTrip <- demobel_adult_matched_activities[i,]
    if (currentTrip$sequenceTrip != 1) {
      formerTrip <- demobel_adult_matched_activities[i-1,]
      demobel_adult_matched_activities[i,]$t_source_hour <- formerTrip$nextTripDepartHour
      demobel_adult_matched_activities[i,]$t_source_minute <- formerTrip$nextTripDepartMinute
      demobel_adult_matched_activities[i,]$departureTimeInMinute <- demobel_adult_matched_activities[i,]$t_source_hour*60 + demobel_adult_matched_activities[i,]$t_source_minute
      demobel_adult_matched_activities[i,]$arrivalTimeInMinute <- demobel_adult_matched_activities[i,]$departureTimeInMinute + demobel_adult_matched_activities[i,]$duration
      demobel_adult_matched_activities[i,]$arrivalHour <- demobel_adult_matched_activities[i,]$arrivalTimeInMinute %/% 60
      demobel_adult_matched_activities[i,]$arrivalMinute <- demobel_adult_matched_activities[i,]$arrivalTimeInMinute %% 60
      demobel_adult_matched_activities[i,]$finishActivityTimeInMinute <- demobel_adult_matched_activities[i,]$arrivalTimeInMinute + demobel_adult_matched_activities[i,]$typicalDurationNextActivity
      demobel_adult_matched_activities[i,]$nextTripDepartHour <- demobel_adult_matched_activities[i,]$finishActivityTimeInMinute %/% 60
      demobel_adult_matched_activities[i,]$nextTripDepartMinute <- demobel_adult_matched_activities[i,]$finishActivityTimeInMinute %% 60
    }
  }
  
  #Bind the obtained DF with the collector
  demobel_adult_matched_activities_collector <- rbind(demobel_adult_matched_activities_collector, demobel_adult_matched_activities)
  
  counter <- counter + 1
  
  if (counter %% 100 == 0) {
    print(paste("Step 3", counter, "/", length(demobel_adult_matched_activities_split), "adults have assigned a random start time"))
  }
}

demobel_adult_matched_activities <- demobel_adult_matched_activities_collector
demobel_adult_matched_activities <- demobel_adult_matched_activities %>% 
  group_by(personID)

rm(demobel_adult_matched_activities_split, demobel_adult_matched_activities_collector)

demobel_adult_matched_activities <- select(demobel_adult_matched_activities, personID, homeNB, ageGroup, sequenceTrip, xHomeCoord, yHomeCoord, 
                                           departureHour = t_source_hour, departureMinute = t_source_minute, 
                                           tripRangeTypeOrigin, arrivalHour, arrivalMinute, tripRangeTypeDestin, 
                                           tripPurpose = tripReasonString,
                                           mode, tripDistance, typicalDurationNextActivity, desPostcode)

demobel_adult_matched_activities_lastTripHandling <- demobel_adult_matched_activities %>% 
  filter(is.na(typicalDurationNextActivity)) %>% 
  mutate(arrivalTimeInMInute = arrivalHour*60 + arrivalMinute) %>% 
  mutate(typicalDurationNextActivity = 1500 - arrivalTimeInMInute)

demobel_adult_matched_activities <- filter(demobel_adult_matched_activities, !is.na(typicalDurationNextActivity))

demobel_adult_matched_activities <- rbind(demobel_adult_matched_activities, demobel_adult_matched_activities_lastTripHandling)
demobel_adult_matched_activities <- demobel_adult_matched_activities[order(demobel_adult_matched_activities[,'personID'], demobel_adult_matched_activities[,'sequenceTrip']),]

#Transform the typical duration from minute to second (as it is the unit for MATSim)
demobel_adult_matched_activities <- mutate(demobel_adult_matched_activities, typicalDurationNextActivity = typicalDurationNextActivity*60)

#Step 3.1 Do the same attribute, trip, move matching for kids and integrate with the adult data
demobel_kids <- demobel %>% filter(age < 18)

matching_df <- kid_intake %>% 
  select(p_id, AgeGroupMethodology, Gender, WegingPop) %>% 
  mutate(AgeGroupMethodology = as.character(AgeGroupMethodology))

demobel_kid_matched_attribute <- left_join(demobel_kids, matching_df, by = c("ageGroup" = "AgeGroupMethodology", "genderN" = "Gender"))

length(demobel_kid_matched_attribute[is.na(demobel_kid_matched_attribute)])

demobel_kid_matched_attribute <- demobel_kid_matched_attribute %>% 
  group_by(personID) %>% 
  slice_sample(n = 1, weight_by = WegingPop)

demobel_kid_matched_attribute <- left_join(demobel_kid_matched_attribute, kid_number_of_trips, by = c("p_id" = "t_participant_id"))

kidNoActivity <- demobel_kid_matched_attribute %>% 
  filter(is.na(n))

demobel_kid_matched_attribute <- na.omit(demobel_kid_matched_attribute)

demobel_kid_matched_attribute <- demobel_kid_matched_attribute %>% 
  select(personID, householdID, neighbourhood, age, ageGroup, gender, highestDegree = degree,
         xHomeCoord, yHomeCoord, p_id, WegingPop, numberOfTrips = n)

demobel_kid_matched_activities <- left_join(demobel_kid_matched_attribute, kid_trip_move, by = c("p_id" = "t_participant_id"))

demobel_kid_matched_activities <- demobel_kid_matched_activities %>% 
  select(personID, homeNB = neighbourhood, ageGroup, xHomeCoord, yHomeCoord, p_id, t_id, 
         t_source_hour, t_source_minute, t_source_id, duration, tripRangeTypeOrigin, 
         tripRangeTypeDestin, sequenceTrip, typicalDurationLastActivity, m_id, 
         mode, tripDistance, tripReasonString, desPostcode = t_destination_PostCode)

demobel_kid_matched_activities <- demobel_kid_matched_activities %>% 
  mutate(randomTimeNormalVariants = round(rnorm(1, mean = 0, sd=15))) %>% 
  mutate(startTimeInMinute = t_source_hour*60 + t_source_minute + randomTimeNormalVariants)

demobel_kid_matched_activities$startTimeInMinute <- ifelse(demobel_kid_matched_activities$startTimeInMinute < 0,
                                                           0,
                                                           demobel_kid_matched_activities$startTimeInMinute)

demobel_kid_matched_activities$startTimeInMinute <- ifelse(demobel_kid_matched_activities$startTimeInMinute > 1440,
                                                           1440,
                                                           demobel_kid_matched_activities$startTimeInMinute)

demobel_kid_matched_activities$t_source_hour <- ifelse(is.na(demobel_kid_matched_activities$t_source_id),
                                                       floor(demobel_kid_matched_activities$startTimeInMinute/60),
                                                       demobel_kid_matched_activities$t_source_hour)

demobel_kid_matched_activities$t_source_minute <- ifelse(is.na(demobel_kid_matched_activities$t_source_id),
                                                         demobel_kid_matched_activities$startTimeInMinute %% 60,
                                                         NA)

demobel_kid_matched_activities <- demobel_kid_matched_activities %>% 
  select(-randomTimeNormalVariants, -startTimeInMinute)

demobel_kid_matched_activities$t_source_minute <- ifelse(is.na(demobel_kid_matched_activities$t_source_id), sample(0:59, replace = TRUE), NA)

demobel_kid_matched_activities <- demobel_kid_matched_activities %>% 
  mutate(departureTimeInMinute = t_source_hour*60 + t_source_minute) %>%
  mutate(arrivalTimeInMinute = departureTimeInMinute + duration) %>% 
  mutate(arrivalHour = arrivalTimeInMinute %/% 60) %>% 
  mutate(arrivalMinute = arrivalTimeInMinute %% 60) %>% 
  mutate(typicalDurationNextActivity = lead(typicalDurationLastActivity)) %>% 
  mutate(finishActivityTimeInMinute = arrivalTimeInMinute + typicalDurationNextActivity) %>% 
  mutate(nextTripDepartHour = finishActivityTimeInMinute %/% 60) %>% 
  mutate(nextTripDepartMinute = finishActivityTimeInMinute %% 60)

demobel_kid_matched_activities_split <- group_split(demobel_kid_matched_activities)

counter <- 0

demobel_kid_matched_activities_collector <- demobel_kid_matched_activities_split[[1]][0,]

for (a in 1:length(demobel_kid_matched_activities_split)){
  demobel_kid_matched_activities <- demobel_kid_matched_activities_split[[a]]
  
  for (i in 1:nrow(demobel_kid_matched_activities)) {
    currentTrip <- demobel_kid_matched_activities[i,]
    if (currentTrip$sequenceTrip != 1) {
      formerTrip <- demobel_kid_matched_activities[i-1,]
      demobel_kid_matched_activities[i,]$t_source_hour <- formerTrip$nextTripDepartHour
      demobel_kid_matched_activities[i,]$t_source_minute <- formerTrip$nextTripDepartMinute
      demobel_kid_matched_activities[i,]$departureTimeInMinute <- demobel_kid_matched_activities[i,]$t_source_hour*60 + demobel_kid_matched_activities[i,]$t_source_minute
      demobel_kid_matched_activities[i,]$arrivalTimeInMinute <- demobel_kid_matched_activities[i,]$departureTimeInMinute + demobel_kid_matched_activities[i,]$duration
      demobel_kid_matched_activities[i,]$arrivalHour <- demobel_kid_matched_activities[i,]$arrivalTimeInMinute %/% 60
      demobel_kid_matched_activities[i,]$arrivalMinute <- demobel_kid_matched_activities[i,]$arrivalTimeInMinute %% 60
      demobel_kid_matched_activities[i,]$finishActivityTimeInMinute <- demobel_kid_matched_activities[i,]$arrivalTimeInMinute + demobel_kid_matched_activities[i,]$typicalDurationNextActivity
      demobel_kid_matched_activities[i,]$nextTripDepartHour <- demobel_kid_matched_activities[i,]$finishActivityTimeInMinute %/% 60
      demobel_kid_matched_activities[i,]$nextTripDepartMinute <- demobel_kid_matched_activities[i,]$finishActivityTimeInMinute %% 60
    }
  }
  
  demobel_kid_matched_activities_collector <- rbind(demobel_kid_matched_activities_collector, demobel_kid_matched_activities)
  
  counter <- counter + 1
  
  if (counter %% 100 == 0) {
    print(paste("Step 3", counter, "/", length(demobel_kid_matched_activities_split), "kids have assigned a random start time"))
  }
}

demobel_kid_matched_activities <- demobel_kid_matched_activities_collector
demobel_kid_matched_activities <- demobel_kid_matched_activities %>% 
  group_by(personID)

rm(demobel_kid_matched_activities_split, demobel_kid_matched_activities_collector)

demobel_kid_matched_activities <- select(demobel_kid_matched_activities, personID, homeNB, ageGroup, sequenceTrip, xHomeCoord, yHomeCoord, 
                                         departureHour = t_source_hour, departureMinute = t_source_minute, 
                                         tripRangeTypeOrigin, arrivalHour, arrivalMinute, tripRangeTypeDestin, 
                                         tripPurpose = tripReasonString,
                                         mode, tripDistance, typicalDurationNextActivity, desPostcode)

demobel_kid_matched_activities_lastTripHandling <- demobel_kid_matched_activities %>% 
  filter(is.na(typicalDurationNextActivity)) %>% 
  mutate(arrivalTimeInMInute = arrivalHour*60 + arrivalMinute) %>% 
  mutate(typicalDurationNextActivity = 1500 - arrivalTimeInMInute)

demobel_kid_matched_activities <- filter(demobel_kid_matched_activities, !is.na(typicalDurationNextActivity))

demobel_kid_matched_activities <- rbind(demobel_kid_matched_activities, demobel_kid_matched_activities_lastTripHandling)
demobel_kid_matched_activities <- demobel_kid_matched_activities[order(demobel_kid_matched_activities[,'personID'], demobel_kid_matched_activities[,'sequenceTrip']),]


demobel_kid_matched_activities <- mutate(demobel_kid_matched_activities, typicalDurationNextActivity = typicalDurationNextActivity*60)


demobel_adult_matched_attribute <- demobel_adult_matched_attribute %>% 
  mutate(highestDegree = as.character(highestDegree))


demobel_matched_attribute <- rbind(demobel_adult_matched_attribute, demobel_kid_matched_attribute)
demobel_matched_attribute <- demobel_matched_attribute[order(demobel_matched_attribute[, 'householdID'], demobel_matched_attribute[, 'personID']),]

demobel_matched_activities <- rbind(demobel_adult_matched_activities, demobel_kid_matched_activities)
demobel_matched_activities <- demobel_matched_activities[order(demobel_matched_activities[,'personID'], demobel_matched_activities[,'sequenceTrip']),]
#So far, we have matched every individual's activity chain. Next will be assigning primary locations

#Remove the unnecessary variables in the environment
rm(adult_activitiesSequence, adult_intake, adult_moves, adult_number_of_trips, 
   adult_trip_distance, adult_trip_move, adult_trip_move_editSequence, adult_trips,
   adult_trips_otherReasons, currentTrip, demobel,
   demobel_adult_matched_activities_lastTripHandling, demobel_adults, demobel_kid_matched_activities_lastTripHandling,
   demobel_kids, demobel_unmatched, formerTrip, home_location_loop, 
   home_location_selection, kid_intake, kid_moves, kid_number_of_trips, kid_trip_distance,
   kid_trip_move, kid_trip_move_editSequence, kid_trips, matching_df, postal_codes, random_house)

#Step 4: Processing Proxmimus flow data

proximus_CellDivisionShape <- st_read("ProximusData/cell_locations_20200120_20200220.shp")


BrusselsNBShape <- st_read("BrusselsNeighbourhood/UrbAdm_MONITORING_DISTRICT.shp", crs = 31370)
BrusselsNBShape[128,]$NAME_FRE <- "CHAUSSEE DE WAVRE - SAINT-JULIEN"

tmap_mode("view")

proximusCellInPoint <- st_centroid(proximus_CellDivisionShape)

proximusCellJoinedWithNB <- st_join(proximusCellInPoint, BrusselsNBShape)

proximusCellJoinedWithNB[57,] $NAME_FRE <- "HAREN"

proximusCellJoinedWithNB <- select(proximusCellJoinedWithNB,
                                   cellID = id, NBFrench = NAME_FRE, geometry)

#Step 4.1 Processing the frequent trip data
proximus_frequentTrip <- read.csv2("ProximusData/daily_regular_split_20200120_20200220.csv")

proximus_frequentTrip <- proximus_frequentTrip %>% 
  mutate(regular_trips = as.numeric(regular_trips), irregular_trips = as.numeric(irregular_trips), total_trips = as.numeric(total_trips))

proximus_frequentTrip <- left_join(proximus_frequentTrip, proximusCellJoinedWithNB, by = c("origin" = "cellID"))
proximus_frequentTrip <- select(proximus_frequentTrip,
                                origin, destination, regular_trips, irregular_trips, total_trips, originNB = NBFrench)
proximus_frequentTrip <- left_join(proximus_frequentTrip, proximusCellJoinedWithNB, by = c("destination" = "cellID"))
proximus_frequentTrip <- select(proximus_frequentTrip,
                                origin, destination, regular_trips, irregular_trips, total_trips, originNB, destinationNB = NBFrench)

proximus_frequentTrip_NAGDPR <- proximus_frequentTrip[grep("N/A GDPR", proximus_frequentTrip$origin),]
proximus_frequentTrip <- proximus_frequentTrip[-grep("N/A GDPR", proximus_frequentTrip$origin),]
proximus_frequentTrip_NAGDPR <- separate_rows(proximus_frequentTrip_NAGDPR, origin, sep = ",")
proximus_frequentTrip_NAGDPR$origin <- gsub("N/A GDPR:", "", proximus_frequentTrip_NAGDPR$origin)
proximus_frequentTrip_NAGDPR <- left_join(proximus_frequentTrip_NAGDPR, proximusCellJoinedWithNB, by = c("origin" = "cellID"))
proximus_frequentTrip_NAGDPR <- select(proximus_frequentTrip_NAGDPR, 
                                       origin, destination, regular_trips, irregular_trips,
                                       total_trips, originNB = NBFrench, destinationNB)

proximus_frequentTrip_NAGDPR$counter = 1
proximus_frequentTrip_NAGDPR <- proximus_frequentTrip_NAGDPR %>% 
  group_by(destination) %>% 
  mutate(cellCounter = cumsum(counter)) %>% 
  mutate(maxCount = max(cellCounter)) %>% 
  mutate(regular_trips = regular_trips/maxCount, 
         irregular_trips = irregular_trips/maxCount, 
         total_trips = total_trips/maxCount) %>% 
  select(origin, destination, regular_trips, irregular_trips, total_trips, originNB, destinationNB)

proximus_frequentTrip <- rbind(proximus_frequentTrip, proximus_frequentTrip_NAGDPR)

proximus_frequentTripSummarise <- proximus_frequentTrip %>% 
  group_by(originNB) %>% 
  summarise(regularSum = sum(regular_trips), irregularSum = sum(irregular_trips)) %>% 
  mutate(knownTripTotal = regularSum + irregularSum) %>% 
  mutate(regularPercent = regularSum/knownTripTotal) %>% 
  select(originNB, regularPercent)

proximus_frequentTripWithZero <- filter(proximus_frequentTrip, regular_trips == 0)
proximus_frequentTrip <- filter(proximus_frequentTrip, regular_trips != 0)
proximus_frequentTripWithZero <- left_join(proximus_frequentTripWithZero, proximus_frequentTripSummarise, by= c("originNB" = "originNB"))
proximus_frequentTripWithZero <- proximus_frequentTripWithZero %>% 
  mutate(regular_trips = total_trips * regularPercent) %>% 
  mutate(irregular_trips = total_trips * (1-regularPercent)) %>% 
  select(origin, destination, regular_trips, irregular_trips, total_trips, originNB, destinationNB)
proximus_frequentTrip <- rbind(proximus_frequentTrip, proximus_frequentTripWithZero)

proximus_NBfrequentTrip <- proximus_frequentTrip %>% 
  group_by(originNB, destinationNB) %>% 
  summarise(regularTrip = sum(regular_trips), irregularTrip = sum(irregular_trips))

origin <- unique(proximus_NBfrequentTrip$originNB)

allNBBru <- unique(BrusselsNBShape$NAME_FRE)

originMissing <- setdiff(allNBBru, origin)

originMissing <- as.data.frame(originMissing)
allNBBru <- as.data.frame(allNBBru)

originMissing <- uncount(originMissing, 145)

allNBBru <- bind_rows(replicate(6, allNBBru, simplify = FALSE))

missingFlowData <- cbind(originMissing, allNBBru)

missingFlowData <- missingFlowData %>% 
  select(originNB = originMissing, destinationNB = allNBBru)

missingFlowData <- missingFlowData %>% 
  mutate(regularTrip = 0.1, irregularTrip = 0.1)

#Bind the new table with the original Proximus data
proximus_NBfrequentTrip <- rbind(proximus_NBfrequentTrip, missingFlowData)

#Step 4.2 processing the hourly spilt data (for the flow determination of secondary activities)
proximus_hourlyTrip <- read.csv2("ProximusData/hourly_split_20200120_20200220.csv")

proximus_hourlyTrip <- proximus_hourlyTrip %>% 
  mutate(trips = as.numeric(trips))

proximus_hourlyTrip <- left_join(proximus_hourlyTrip, select(proximusCellJoinedWithNB, cellID, NBFrench), by = c("origin" = "cellID"))

proximus_hourlyTrip <- proximus_hourlyTrip %>% 
  select(origin, originNB = NBFrench, destination, arrival_hr, trips)

proximus_hourlyTrip <- left_join(proximus_hourlyTrip, select(proximusCellJoinedWithNB, cellID, NBFrench), by = c("destination" = "cellID"))

proximus_hourlyTrip <- proximus_hourlyTrip %>% 
  select(origin, originNB, destination, destinationNB = NBFrench, arrival_hr, trips)

proximus_hourlyTrip_NAGDPR <- proximus_hourlyTrip[grep("N/A GDPR", proximus_hourlyTrip$origin),]

proximus_hourlyTrip <- proximus_hourlyTrip[-grep("N/A GDPR", proximus_hourlyTrip$origin),]

proximus_hourlyTrip_NAGDPR <- separate_rows(proximus_hourlyTrip_NAGDPR, origin, sep = ",")
proximus_hourlyTrip_NAGDPR$origin <- gsub("N/A GDPR:", "", proximus_hourlyTrip_NAGDPR$origin)
proximus_hourlyTrip_NAGDPR <- left_join(proximus_hourlyTrip_NAGDPR, proximusCellJoinedWithNB, by = c("origin" = "cellID"))
proximus_hourlyTrip_NAGDPR <- proximus_hourlyTrip_NAGDPR %>% 
  select(origin, originNB = NBFrench, destination, destinationNB, arrival_hr, trips)

proximus_hourlyTrip_NAGDPR$counter = 1
proximus_hourlyTrip_NAGDPR <- proximus_hourlyTrip_NAGDPR %>% 
  group_by(destination, arrival_hr) %>% 
  mutate(cellCounter = cumsum(counter)) %>% 
  mutate(maxCount = max(cellCounter)) %>% 
  mutate(trips = trips/maxCount) %>% 
  select(origin, originNB, destination, destinationNB, arrival_hr, trips)

proximus_hourlyTrip <- rbind(proximus_hourlyTrip, proximus_hourlyTrip_NAGDPR)

proximus_NBhourlyTrip <- proximus_hourlyTrip %>% 
  group_by(originNB, destinationNB, arrival_hr) %>% 
  summarise(trips = sum(trips))

proximus_NBhourlyTrip <- proximus_NBhourlyTrip %>% 
  mutate(originDestinationCombin = paste(originNB, destinationNB))
proximus_NBFrequentTrip_forValidation <- proximus_NBfrequentTrip %>% 
  mutate(originDestinationCombin = paste(originNB, destinationNB))

proximus_NBFrequentTrip_forValidation <- proximus_NBFrequentTrip_forValidation %>% 
  mutate(totalTrip = regularTrip + irregularTrip) %>%
  mutate(irregularPercent = irregularTrip/totalTrip)

proximus_NBhourlyTrip <- left_join(proximus_NBhourlyTrip, 
                                   select(proximus_NBFrequentTrip_forValidation, originDestinationCombin, irregularPercent),
                                   by = c("originDestinationCombin" = "originDestinationCombin"))

proximus_NBhourlyTrip <- proximus_NBhourlyTrip %>% 
  select(-originNB.y) %>% 
  mutate(irregularTrips = trips * irregularPercent) %>% 
  select(originNB = originNB.x, destinationNB, arrival_hr, irregularTrips)

origin <- unique(proximus_NBhourlyTrip$originNB)
allNBBru <- unique(BrusselsNBShape$NAME_FRE)
originMissing <- setdiff(allNBBru, origin)
originMissing <- as.data.frame(originMissing)
allNBBru <- as.data.frame(allNBBru)
originMissing <- uncount(originMissing, 145)
allNBBru <- bind_rows(replicate(6, allNBBru, simplify = FALSE))
missingFlowData <- cbind(originMissing, allNBBru)
missingFlowData <- uncount(missingFlowData,26)
hourInADay <- as.data.frame(c(0:25))
hourInADay <- bind_rows(replicate(870, hourInADay, simplify = FALSE))
missingFlowData <- cbind(missingFlowData, hourInADay)
trafficVolume <- as.data.frame(rep(0.1, 22620))
missingFlowData <- cbind(missingFlowData, trafficVolume)

missingFlowData <- missingFlowData %>% 
  select(originNB = originMissing, destinationNB = allNBBru, 
         arrival_hr = `c(0:25)`, irregularTrips = `rep(0.1, 22620)`)

proximus_NBhourlyTrip <- rbind(proximus_NBhourlyTrip, missingFlowData)

proximus_NBhourlyTrip <- as.data.frame(proximus_NBhourlyTrip)
proximus_NBhourlyTrip <- complete(proximus_NBhourlyTrip, 
                                  originNB, destinationNB, arrival_hr,
                                  fill = list(irregularTrips = 0.1))

rm(proximus_CellDivisionShape, proximus_frequentTrip_NAGDPR, proximus_frequentTripSummarise,
   proximus_frequentTripWithZero, proximus_frequentTrip, proximusCellInPoint, proximusCellJoinedWithNB)

rm(origin, allNBBru, originMissing, missingFlowData, hourInADay, trafficVolume)

rm(proximus_NBFrequentTrip_forValidation, proximus_hourlyTrip, proximus_hourlyTrip_NAGDPR)

#Step 5 Determining locations NB (by new algorithm and regular trip for primary activity, irregular for secondary)

#Step 5.1 Create a home geometry for calculating the home-(first)primaryTrip distance
homeCoord <- sf_point(demobel_matched_attribute, x = "xHomeCoord", y = "yHomeCoord")
homeCoord <- st_set_crs(homeCoord, 31370)
demobel_matched_attribute <- cbind(demobel_matched_attribute, homeCoord)
rm(homeCoord)

#Step 5.2 Filtering the POIs in the respective category

BrusselsPOI <- st_read("POI/UrbAdm_POINT_OF_INTEREST.shp", crs = 31370)

BrusselsPOI <- st_zm(BrusselsPOI, drop = TRUE, what = "ZM")

BrusselsPOI <- st_join(BrusselsPOI, BrusselsNBShape)

BrusselsPOI <- select(BrusselsPOI, ID = ID.x, TYPE, CATEGORY, xCoord = X, yCoord = Y, NBFre = NAME_FRE)


#Step 5.2.1 for primary locations
#Step 5.2.1.1 Filter work locations
BrusselsPOI_work <- BrusselsPOI %>% 
  filter(TYPE == "BCR" | TYPE == "BTK" | TYPE == "CIN" | TYPE == "DST" | 
           TYPE == "EM" | TYPE == "EU" | TYPE == "FIR" | TYPE == "HO" | 
           TYPE == "HSD" | TYPE == "HSF" | TYPE == "IND" | TYPE == "ISC" | 
           TYPE == "LIB" | TYPE == "LID" | TYPE == "LIF" | TYPE == "MA" | TYPE == "MKT" | 
           TYPE == "MUS" | TYPE == "NDS" | TYPE == "NFS" | TYPE == "PDS" | TYPE == "PFS" | 
           TYPE == "PHA" | TYPE == "PO" | TYPE == "POL" | TYPE == "SDS" | TYPE == "SAS" | 
           TYPE == "SFS" | TYPE == "SHP" | TYPE == "SPO" | TYPE == "SW" | TYPE == "THE" | 
           TYPE == "WST")

missingNB <- setdiff(BrusselsNBShape$NAME_FRE, BrusselsPOI_work$NBFre)

missingNBRandomGeneratePOI <- BrusselsNBShape %>% 
  filter(NAME_FRE %in% missingNB) %>% 
  st_sample(c(22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22)) %>% 
  st_as_sf() %>%
  mutate(ID = 99999999, TYPE = "VUBADDED", CATEGORY = "VUBADDED")


missingNBWorkPOICoord <- st_coordinates(missingNBRandomGeneratePOI)

missingNBRandomGeneratePOI <- cbind(missingNBRandomGeneratePOI, missingNBWorkPOICoord)
rm(missingNBWorkPOICoord)

missingNBRandomGeneratePOI <- st_join(missingNBRandomGeneratePOI, select(BrusselsNBShape, NAME_FRE))

missingNBRandomGeneratePOI <- missingNBRandomGeneratePOI %>% 
  select(ID, TYPE, CATEGORY, xCoord = X, yCoord = Y, NBFre = NAME_FRE, geometry = x)

BrusselsPOI_work <- rbind(BrusselsPOI_work, missingNBRandomGeneratePOI)

BrusselsPOI_work <- st_transform(BrusselsPOI_work, 31370)

#Step 5.2.1.2 filter school locations
#Filter the POI for adult education
BrusselsPOI_adultSchool <- BrusselsPOI %>% 
  filter(TYPE == "HSD" | TYPE == "HSF" | TYPE == "NDS" | TYPE == "NFS")

BrusselsPOI_kidSchool <- BrusselsPOI %>% 
  filter(TYPE == "ISC" | TYPE == "PDS" | TYPE == "PFS" | TYPE == "SDS" | TYPE == "SFS")

missingNBRandomGeneratePOI <- BrusselsNBShape %>% 
  filter(NAME_FRE == "REYERS" | NAME_FRE == "GARE DE SCHAERBEEK") %>% 
  st_sample(c(5 ,5)) %>% 
  st_as_sf() %>% 
  mutate(ID = 99999999, TYPE = "VUBADDED", CATEGORY = "VUBADDED")

missingNBKidEDUPOICoord <- st_coordinates(missingNBRandomGeneratePOI)
missingNBRandomGeneratePOI <- cbind(missingNBRandomGeneratePOI, missingNBKidEDUPOICoord)
rm(missingNBKidEDUPOICoord)

missingNBRandomGeneratePOI <- st_join(missingNBRandomGeneratePOI, select(BrusselsNBShape, NAME_FRE))

missingNBRandomGeneratePOI <- missingNBRandomGeneratePOI %>% 
  select(ID, TYPE, CATEGORY, xCoord = X, yCoord = Y, NBFre = NAME_FRE, geometry = x)

BrusselsPOI_kidSchool <- rbind(BrusselsPOI_kidSchool, missingNBRandomGeneratePOI)
BrusselsPOI_kidSchool <- st_transform(BrusselsPOI_kidSchool, 31370)
#Step 5.2.2 for secondary locations

#Step 5.2.2.1 shopping secondary POIs and get random shop for missing NBs
BrusselsPOI_shopping <- BrusselsPOI %>% 
  filter(TYPE == "BTK" | TYPE == "CIN" | TYPE == "DST" | TYPE == "IND" | TYPE == "LIB" |
           TYPE == "LID" | TYPE == "LIF" | TYPE == "MKT" | TYPE == "PHA" | TYPE == "PO" |
           TYPE == "SHP")

missingNB <- setdiff(BrusselsNBShape$NAME_FRE, BrusselsPOI_shopping$NBFre)

missingNBRandomGeneratePOI <- BrusselsNBShape %>% 
  filter(NAME_FRE %in% missingNB) %>% 
  st_sample(rep(9, 24)) %>% 
  st_as_sf() %>%
  mutate(ID = 99999999, TYPE = "VUBADDED", CATEGORY = "VUBADDED")

missingNBShopPOICoord <- st_coordinates(missingNBRandomGeneratePOI)
missingNBRandomGeneratePOI <- cbind(missingNBRandomGeneratePOI, missingNBShopPOICoord)
rm(missingNBShopPOICoord)
missingNBRandomGeneratePOI <- st_join(missingNBRandomGeneratePOI, select(BrusselsNBShape, NAME_FRE))
missingNBRandomGeneratePOI <- missingNBRandomGeneratePOI %>% 
  select(ID, TYPE, CATEGORY, xCoord = X, yCoord = Y, NBFre = NAME_FRE, geometry = x)
BrusselsPOI_shopping <- rbind(BrusselsPOI_shopping, missingNBRandomGeneratePOI)
BrusselsPOI_shopping <- st_transform(BrusselsPOI_shopping, 31370)

#Step 5.2.2.2 leisure secondary POIs and get random shop for missing NBs
BrusselsPOI_leisure <- BrusselsPOI %>% 
  filter(TYPE == "AMP" | TYPE == "CIN" | TYPE == "DST" | TYPE == "FNT" | TYPE == "FO" | 
           TYPE == "IND" | TYPE == "KP" | TYPE == "LIB" | TYPE == "LID" | TYPE == "LIF" |
           TYPE == "MNM" | TYPE == "MUS" | TYPE == "REC" | TYPE == "REM" | TYPE == "REO" |
           TYPE == "REP" | TYPE == "SHP" | TYPE == "SPO" | TYPE == "SW" | TYPE == "THE")

missingNB <- setdiff(BrusselsNBShape$NAME_FRE, BrusselsPOI_leisure$NBFre)

missingNBRandomGeneratePOI <- BrusselsNBShape %>% 
  filter(NAME_FRE %in% missingNB) %>% 
  st_sample(rep(12, 4)) %>% 
  st_as_sf() %>%
  mutate(ID = 99999999, TYPE = "VUBADDED", CATEGORY = "VUBADDED")
missingNBLeisurePOICoord <- st_coordinates(missingNBRandomGeneratePOI)
missingNBRandomGeneratePOI <- cbind(missingNBRandomGeneratePOI, missingNBLeisurePOICoord)
rm(missingNBLeisurePOICoord)
missingNBRandomGeneratePOI <- st_join(missingNBRandomGeneratePOI, select(BrusselsNBShape, NAME_FRE))
missingNBRandomGeneratePOI <- missingNBRandomGeneratePOI %>% 
  select(ID, TYPE, CATEGORY, xCoord = X, yCoord = Y, NBFre = NAME_FRE, geometry = x)
BrusselsPOI_leisure <- rbind(BrusselsPOI_leisure, missingNBRandomGeneratePOI)
BrusselsPOI_leisure <- st_transform(BrusselsPOI_leisure, 31370)

#Step 5.2.2.3 other secondary POI (just used all POI as we don't know what the trip purposes are)
BrusselsPOI_other <- BrusselsPOI

rm(BrusselsPOI, demobel_adult_matched_activities, demobel_adult_matched_attribute, demobel_kid_matched_activities,
   demobel_kid_matched_attribute, missingNBRandomGeneratePOI)

#Step 5.3 Assign POI Attractive level to POI with different purposes
#Step 5.3.1 For kid EDU trip
#Step 5.3.1.1 Processing BISA data (for kid edu flow and POI attractiveness level)
BISAKidEduData <- read_xlsx("BISAKidEDUData.xlsx")
BISAKidEduData <- BISAKidEduData %>% 
  select(homeSS = SS_R, schoolSS = SS_Scol, schoolID = Num_Scol, numberOfStudent = Nb_indiv)

BrusselsSSDivision <- st_read("BelgiumStatisticalSector/sh_statbel_statistical_sectors_20180101.shp")

BrusselsSSDivision <- BrusselsSSDivision %>% filter(T_REGIO_FR == "Région de Bruxelles-Capitale")

BruSSInPoint <- st_centroid(BrusselsSSDivision)

BruSSJoinedWithNB <- st_join(BruSSInPoint, BrusselsNBShape)

BruSSJoinedWithNB <- BruSSJoinedWithNB %>% 
  select(SSName = CS01012018, NBFrench = NAME_FRE)

BISAKidEduData <- BISAKidEduData %>% 
  left_join(BruSSJoinedWithNB, by = c("homeSS" = "SSName")) %>% 
  left_join(BruSSJoinedWithNB, by = c("schoolSS" = "SSName")) %>% 
  select(-geometry.x, -geometry.y) %>% 
  select(homeSS, homeNB = NBFrench.x, schoolSS, schoolNB = NBFrench.y, schoolID, numberOfStudent)

BISAKidEduData <- BISAKidEduData %>% 
  filter(!(is.na(homeNB) & is.na(schoolNB)))

BISA_kidEduNBFlow <- BISAKidEduData %>% 
  filter(!is.na(homeNB) & !is.na(schoolNB)) %>% 
  group_by(homeNB, schoolNB) %>% 
  summarise(studentNumber = sum(numberOfStudent))

#After a check, we ensure that every destination NB has at least one kid edu POI in it

origin <- unique(BISA_kidEduNBFlow$homeNB)

allNBBru <- unique(BrusselsNBShape$NAME_FRE)

originMissing <- setdiff(allNBBru, origin)

originMissing <- as.data.frame(originMissing)

destinationsForKidEDU <- as.data.frame(unique(BISA_kidEduNBFlow$schoolNB))

destinationsForKidEDU <- bind_rows(replicate(2, destinationsForKidEDU, simplify = FALSE))

originMissing <- uncount(originMissing, 120)

missingFlow <- cbind(originMissing, destinationsForKidEDU)

missingFlow <- missingFlow %>% 
  select(homeNB = originMissing, schoolNB = `unique(BISA_kidEduNBFlow$schoolNB)`) %>% 
  mutate(studentNumber = 1)

BISA_kidEduNBFlow <- rbind(BISA_kidEduNBFlow, missingFlow)

BISA_kidEduNBFlow <- as.data.frame(BISA_kidEduNBFlow)

BISA_kidEduNBFlow <- complete(BISA_kidEduNBFlow, homeNB, schoolNB, fill = list(studentNumber = 0.1))

#Step 5.3.1.2 Assigning an attractiveness level for each kid edu POI based on student registered
BISAKidSchoolAttractivness <- BISAKidEduData %>% 
  filter(!is.na(schoolNB)) %>% 
  select(schoolNB, schoolID, numberOfStudent) %>% 
  group_by(schoolNB, schoolID) %>% 
  summarise(studentRegistered = sum(numberOfStudent))

#Randomly assign a registered student number to POI based on BISA data
BrusselsPOI_kidSchool <- BrusselsPOI_kidSchool %>% 
  mutate(attractiveness = -1)

nbNotAvailableINBISA <- BrusselsPOI_kidSchool %>% 
  filter(!(NBFre %in% BISAKidSchoolAttractivness$schoolNB))

nbNotAvailableINBISA <- unique(nbNotAvailableINBISA$NBFre)

nbNotAvailableINBISA <- data.frame(schoolNB = nbNotAvailableINBISA, schoolID = "VUBADDED", studentRegistered = 100)

BISAKidSchoolAttractivness <- rbind(BISAKidSchoolAttractivness, nbNotAvailableINBISA)

#Assigning student registered number to kid school
for (i in 1:nrow(BrusselsPOI_kidSchool)) {
  schoolSelected <- BrusselsPOI_kidSchool[i,]
  BISASchoolsInSelectedNB <- BISAKidSchoolAttractivness %>% filter(schoolNB == schoolSelected$NBFre)
  BISASchoolSelected <- slice_sample(BISASchoolsInSelectedNB, n = 1, weight_by = studentRegistered, replace = TRUE)
  BrusselsPOI_kidSchool[i,]$attractiveness <- BISASchoolSelected$studentRegistered
  
  if (i %% 100 == 0) {
    print(paste("Step 5.3.1.1", i, "/", nrow(BrusselsPOI_kidSchool), "kid schools have randomly assigned a student registered number!"))
  }
}

#Step 5.3.2 Work and adult edu POI attractiveness level
#Assign an "attractiveness level" for each POI based on Statbel data
EnterpriseCrossroad <- read.csv("EnterpriseCrossroad/POIData.csv")

enterpriseCategory <- data.frame(RSZ_CLASS = 1:9, 
                                 attractivenss = c(2.5, 7, 14.5, 34.5, 74.5, 149.5, 249.5, 749.5, 1300))

BrusselsMunicipality <- st_read("BrusselsMunicipality/UrbAdm_MUNICIPALITY.shp", crs = 31370)

BrusselsPOI_work <- st_join(BrusselsPOI_work, BrusselsMunicipality)

BrusselsPOI_work <- BrusselsPOI_work %>% 
  select(ID = ID.x, TYPE, CATEGORY, xCoord, yCoord, NBFre, Refnis = NAT_CODE)

BrusselsPOI_adultSchool <- st_join(BrusselsPOI_adultSchool, BrusselsMunicipality)

BrusselsPOI_adultSchool <- BrusselsPOI_adultSchool %>%
  select(ID = ID.x, TYPE, CATEGORY, xCoord, yCoord, NBFre, Refnis = NAT_CODE)

EnterpriseCrossroad <- EnterpriseCrossroad %>% 
  mutate(count = 1) %>% 
  group_by(Refnis, RSZ_CLASS) %>% 
  summarise(totalEnterprise = sum(count))

EnterpriseCrossroad <- left_join(EnterpriseCrossroad, enterpriseCategory, by = c("RSZ_CLASS" = "RSZ_CLASS"))

BrusselsPOI_work <- BrusselsPOI_work %>% 
  mutate(attractiveness = -1)

#Loop through work and adult school for a random attractiveness level
for (i in 1:nrow(BrusselsPOI_work)) {
  communeSelected <- BrusselsPOI_work[i,]$Refnis
  communeAttractiveness <- EnterpriseCrossroad %>% 
    filter(Refnis == communeSelected)
  attractivenessSelected <- slice_sample(communeAttractiveness, n = 1, weight_by = communeAttractiveness$totalEnterprise)
  BrusselsPOI_work[i,]$attractiveness <- attractivenessSelected$attractivenss
  if (i %% 100 == 0) {
    print(paste("Step 5.3.2", i, "/", nrow(BrusselsPOI_work), "work POIs have randomly assigned an attractiveness level!"))
  }
}

BrusselsPOI_work <- BrusselsPOI_work %>% 
  select(-Refnis)

BrusselsPOI_adultSchool <- st_join(BrusselsPOI_adultSchool, BrusselsMunicipality)

BrusselsPOI_adultSchool <- BrusselsPOI_adultSchool %>% 
  select(ID = ID.x, TYPE, CATEGORY, xCoord, yCoord, NBFre, Refnis = NAT_CODE)

rm(BrusselsMunicipality)

BrusselsPOI_adultSchool <- BrusselsPOI_adultSchool %>% 
  mutate(attractiveness = -1)

for (i in 1:nrow(BrusselsPOI_adultSchool)) {
  communeSelected <- BrusselsPOI_adultSchool[i,]$Refnis
  communeAttractiveness <- EnterpriseCrossroad %>% 
    filter(Refnis == communeSelected)
  attractivenessSelected <- slice_sample(communeAttractiveness, n = 1, weight_by = communeAttractiveness$totalEnterprise)
  BrusselsPOI_adultSchool[i,]$attractiveness <- attractivenessSelected$attractivenss
  if (i %% 100 == 0) {
    print(paste("Step 5.3.2", i, "/", nrow(BrusselsPOI_adultSchool), "adult EDU POI have randomly assigned an attractiveness level!"))
  }
}

BrusselsPOI_adultSchool <- BrusselsPOI_adultSchool %>% 
  select(-Refnis)

#Step 5.3.3 Secondary location POI attractiveness level


#Step 5.4 Calculating the top 10 furtherest distances between each NB (for assigning long distance in the next step)

distancesBetweenNB <- st_distance(BrusselsNBShape, BrusselsNBShape)
colnames(distancesBetweenNB) <- BrusselsNBShape$NAME_FRE
rownames(distancesBetweenNB) <- BrusselsNBShape$NAME_FRE

distancesBetweenNB <- as.data.frame(distancesBetweenNB)
distancesBetweenNBs <- distancesBetweenNB %>% 
  rownames_to_column(var = "NBName")
tenFurthestNBForEachNB <- slt(dapply(distancesBetweenNB, MARGIN = 1, FUN = function(x) colnames(distancesBetweenNB)[order(-x)]), 1:10)
rm(distancesBetweenNB)
colnames(tenFurthestNBForEachNB) <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10')

#Step 5.5 Mutate a origin and destination geometry with the respective NB (do this because only one geometry for each role but we need one origin and one destination for each activity)
demobel_matched_activities <- uncount(demobel_matched_activities, 2)
demobel_matched_activities <- demobel_matched_activities %>% 
  mutate(originDestinType = rep(c(0, 1), length.out = n()))???

#Add the first activity geometry (and remaining activity is NA)
demobel_matched_activities <- left_join(demobel_matched_activities, select(demobel_matched_attribute, personID, geometry), by = c("personID" = "personID"))

demobel_matched_activities <- demobel_matched_activities %>% 
  mutate(tripNB = homeNB)

demobel_matched_activities$geometry <- ifelse(demobel_matched_activities$sequenceTrip == 1 & demobel_matched_activities$originDestinType == 0,
                                              demobel_matched_activities$geometry,
                                              NA)

demobel_matched_activities$tripNB <- ifelse(demobel_matched_activities$sequenceTrip == 1 & demobel_matched_activities$originDestinType == 0,
                                            demobel_matched_activities$tripNB,
                                            NA)

demobel_matched_activities$geometry <- st_as_sfc(demobel_matched_activities$geometry, crs = st_crs(31370))

#Step 5.6 Looping of people's activity chain

demobel_matched_activities$adultOrKid <- ifelse(demobel_matched_activities$ageGroup == "1K" | demobel_matched_activities$ageGroup == "2K",
                                                "kid",
                                                "adult")


proximus_NBfrequentTrip_forAdultEDU <- proximus_NBfrequentTrip %>% 
  filter(destinationNB %in% BrusselsPOI_adultSchool$NBFre)

proximus_NBfrequentTrip_forAdultEDU <- as.data.frame(proximus_NBfrequentTrip_forAdultEDU)

proximus_NBfrequentTrip_forAdultEDU <- complete(proximus_NBfrequentTrip_forAdultEDU, 
                                                originNB, destinationNB,
                                                fill = list(regularTrip = 0.1, irregularTrip = 0.1))

proximus_NBfrequentTrip_forAdultEDU <- proximus_NBfrequentTrip_forAdultEDU %>% 
  filter(destinationNB %in% BrusselsPOI_adultSchool$NBFre)


BISA_kidEduNBFlow <- BISA_kidEduNBFlow %>% 
  select(originNB = homeNB, destinationNB = schoolNB, regularTrip = studentNumber)

BISA_kidEduNBFlow <- BISA_kidEduNBFlow %>% 
  filter(destinationNB %in% BrusselsPOI_kidSchool$NBFre)

proximus_NBfrequentTrip_forGeneral <- proximus_NBfrequentTrip

proximus_NBfrequentTrip_forGeneral <- as.data.frame(proximus_NBfrequentTrip_forGeneral)

proximus_NBfrequentTrip_forGeneral <- complete(proximus_NBfrequentTrip_forGeneral, originNB, destinationNB, fill = list(regularTrip = 0.1, irregularTrip = 0.1))

BrusselsPOI_home <- home_location_full
homeCoord <- sf_point(BrusselsPOI_home, x = "xHomeCoord", y = "yHomeCoord")
homeCoord <- st_set_crs(homeCoord, 31370)
BrusselsPOI_home <- cbind(BrusselsPOI_home, homeCoord)
rm(home_location_full, homeCoord)
BrusselsPOI_home <- BrusselsPOI_home %>% 
  select(X, homeID, xHomeCoord, yHomeCoord, NBFre = NAME_FRE, geometry)


BrusselsPOI_other <- BrusselsPOI_other %>% 
  filter(!is.na(NBFre))

proximus_NBfrequentTrip_forGeneral_Popularity <- proximus_NBfrequentTrip_forGeneral %>% 
  select(-irregularTrip) %>% 
  group_by(destinationNB) %>% 
  summarise(totalArrivalTrip = sum(regularTrip))

proximus_NBfrequentTrip_forAdultEDU_popularity <- proximus_NBfrequentTrip_forAdultEDU %>% 
  group_by(destinationNB) %>% 
  summarise(totalArrivalTrip = sum(regularTrip))

BISA_kidEduNBFlow_Popularity <- BISA_kidEduNBFlow %>% 
  group_by(destinationNB) %>% 
  summarise(totalArrivalTrip = sum(regularTrip))

proximus_NBhourlyTrip_popularity <- proximus_NBhourlyTrip %>% 
  group_by(destinationNB, arrival_hr) %>% 
  summarise(totalArrivalTrip = sum(irregularTrips))

BrusselsSSDivision <- st_read("BelgiumStatisticalSector/sh_statbel_statistical_sectors_20180101.shp")
nonBrusselsSSDivision <- BrusselsSSDivision

BESSInPoint <- st_centroid(nonBrusselsSSDivision)

rm(nonBrusselsSSDivision)

demobel_matched_activities <- as.data.frame(demobel_matched_activities)


BelgiumPostcode <- st_read("BelgiumPostcode/postaldistricts.shp")
BelgiumPostcode <- BelgiumPostcode %>% 
  select(postcode = nouveau_PO, geometry)
BelgiumPostcode <- st_centroid(BelgiumPostcode)







demobel_matched_activities <- left_join(demobel_matched_activities, select(demobel_matched_attribute, personID, p_id))
differentHomeMONITORPeople <- read.csv("MONITORPeopleNotGoingBackFirstLocation.csv")
differentHomeMONITORPeople <- differentHomeMONITORPeople %>% 
  select(t_participant_id = ID)



workSequence1ID <- read.csv("primaryActivityID/workSequence1ID.csv")
workSequence3ID <- read.csv("primaryActivityID/workSequence3ID.csv")
eduSequence1ID <- read.csv("primaryActivityID/eduSequence1ID.csv")
eduSequence2ID <- read.csv("primaryActivityID/eduSequence2ID.csv")




demobel_matched_activities_original <- demobel_matched_activities

maxPersonID <- max(demobel_matched_activities$personID)

#Split the data frame into 20 pieces
split1 <- ceiling(maxPersonID/20)
split2 <- ceiling(maxPersonID*2/20)
split3 <- ceiling(maxPersonID*3/20)
split4 <- ceiling(maxPersonID*4/20)
split5 <- ceiling(maxPersonID*5/20)
split6 <- ceiling(maxPersonID*6/20)
split7 <- ceiling(maxPersonID*7/20)
split8 <- ceiling(maxPersonID*8/20)
split9 <- ceiling(maxPersonID*9/20)
split10 <- ceiling(maxPersonID*10/20)
split11 <- ceiling(maxPersonID*11/20)
split12 <- ceiling(maxPersonID*12/20)
split13 <- ceiling(maxPersonID*13/20)
split14 <- ceiling(maxPersonID*14/20)
split15 <- ceiling(maxPersonID*15/20)
split16 <- ceiling(maxPersonID*16/20)
split17 <- ceiling(maxPersonID*17/20)
split18 <- ceiling(maxPersonID*18/20)
split19 <- ceiling(maxPersonID*19/20)

demobel_matched_activities_part1 <- demobel_matched_activities_original %>% 
  filter(personID < split1)

demobel_matched_activities_part2 <- demobel_matched_activities_original %>% 
  filter(personID >= split1 & personID < split2)

demobel_matched_activities_part3 <- demobel_matched_activities_original %>% 
  filter(personID >= split2 & personID < split3)

demobel_matched_activities_part4 <- demobel_matched_activities_original %>% 
  filter(personID >= split3 & personID < split4)

demobel_matched_activities_part5 <- demobel_matched_activities_original %>% 
  filter(personID >= split4 & personID < split5)

demobel_matched_activities_part6 <- demobel_matched_activities_original %>% 
  filter(personID >= split5 & personID < split6)

demobel_matched_activities_part7 <- demobel_matched_activities_original %>% 
  filter(personID >= split6 & personID < split7)

demobel_matched_activities_part8 <- demobel_matched_activities_original %>% 
  filter(personID >= split7 & personID < split8)

demobel_matched_activities_part9 <- demobel_matched_activities_original %>% 
  filter(personID >= split8 & personID < split9)

demobel_matched_activities_part10 <- demobel_matched_activities_original %>% 
  filter(personID >= split9 & personID < split10)

demobel_matched_activities_part11 <- demobel_matched_activities_original %>% 
  filter(personID >= split10 & personID < split11)

demobel_matched_activities_part12 <- demobel_matched_activities_original %>% 
  filter(personID >= split11 & personID < split12)

demobel_matched_activities_part13 <- demobel_matched_activities_original %>% 
  filter(personID >= split12 & personID < split13)

demobel_matched_activities_part14 <- demobel_matched_activities_original %>% 
  filter(personID >= split13 & personID < split14)

demobel_matched_activities_part15 <- demobel_matched_activities_original %>% 
  filter(personID >= split14 & personID < split15)

demobel_matched_activities_part16 <- demobel_matched_activities_original %>% 
  filter(personID >= split15 & personID < split16)

demobel_matched_activities_part17 <- demobel_matched_activities_original %>% 
  filter(personID >= split16 & personID < split17)

demobel_matched_activities_part18 <- demobel_matched_activities_original %>% 
  filter(personID >= split17 & personID < split18)

demobel_matched_activities_part19 <- demobel_matched_activities_original %>% 
  filter(personID >= split18 & personID < split19)

demobel_matched_activities_part20 <- demobel_matched_activities_original %>% 
  filter(personID >= split19)

locationAssignment <- function(demobel_matched_activities){
  
  #Starting of the loop for determining NBs and POI locations
  demobel_matched_activities_split <- demobel_matched_activities %>% 
    group_by(personID) %>% 
    group_split()
  
  demobel_matched_activities_collector <- demobel_matched_activities_split[[1]][0,]
  
  for (a in 1:length(demobel_matched_activities_split)) {
    demobel_matched_activities <- demobel_matched_activities_split[[a]]
    personID <- demobel_matched_activities[1,]$personID
    individualActivityChain <- demobel_matched_activities
    
    for (trip in 1:nrow(individualActivityChain)) {
      individualActivityChain <- demobel_matched_activities
      tripSelected <- individualActivityChain[trip,]
      if (tripSelected$originDestinType == 1) {

        adultOrKid <- tripSelected$adultOrKid
        tripPurpose <- tripSelected$tripPurpose
        tripStraightLineDistance <- (tripSelected$tripDistance)/1.3
      
        tripRangeTypeOrigin <- tripSelected$tripRangeTypeOrigin
        tripRangeTypeDestin <- tripSelected$tripRangeTypeDestin
        tripDepartHour <- tripSelected$departureHour
        tripSequence <- tripSelected$sequenceTrip
        
        MONITORReference <- tripSelected$p_id
        
      
        if (tripPurpose == "home" & !MONITORReference %in% differentHomeMONITORPeople$t_participant_id) {
          
          #Indicating that people should go back to their original home according to MONITOR sample
          homeLocation <- demobel_matched_activities[1,]$geometry
          originalNB <- demobel_matched_activities[1,]$tripNB
          
          demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- homeLocation
          demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- originalNB
          
          print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a return home activity (according to what the reference reported at MONITOR)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
          
        } else if (tripPurpose == "work" & MONITORReference %in% workSequence1ID$t_participant_id & tripSequence>1) {
          
        
          #indicating that this is a work trip that belongs to samples who reported (multiple) return fixed work places at the 1st sequence of activity (and this is not the first work trip
          fixedWorkLocation <- demobel_matched_activities[2,]$geometry
          originalNB <- demobel_matched_activities[2,]$tripNB
          
          demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- fixedWorkLocation
          demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- originalNB
          
          print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a return fixed workplace activity (as MONITOR sample reported fixed work location with multiple work activities)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
        } else if (tripPurpose == "work" & MONITORReference %in% workSequence3ID$t_participant_id & tripSequence>3) {
          
        
          fixedWorkLocation <- demobel_matched_activities[6,]$geometry
          originalNB <- demobel_matched_activities[6,]$tripNB
          
          demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- fixedWorkLocation
          demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- originalNB
          
          print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a return fixed workplace activity (as MONITOR sample reported fixed work location with multiple work activities)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
        } else if (tripPurpose == "school" & MONITORReference %in% eduSequence1ID$t_participant_id & tripSequence>1) {
          
        
          fixedEDULocation <- demobel_matched_activities[2,]$geometry
          originalNB <- demobel_matched_activities[2,]$tripNB
          
          demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- fixedEDULocation
          demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- originalNB
          
          print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a return fixed EDU activity (as MONITOR sample reported fixed EDU location with multiple EDU activities)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
        } else if (tripPurpose == "school" & MONITORReference %in% eduSequence2ID$t_participant_id & tripSequence>2) {
          
        
          fixedEDULocation <- demobel_matched_activities[4,]$geometry
          originalNB <- demobel_matched_activities[4,]$tripNB
          
          demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- fixedEDULocation
          demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- originalNB
          
          print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a return fixed EDU activity (as MONITOR sample reported fixed EDU location with multiple EDU activities)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
        } else {
          #In this case, we are searching for: 
          #1) primary activity that first occurs at the multiple "return" activity chain (for the above if justification)
          #2) other activities including multiple-primary-locations, or other secondary activities 
          #For within Brussels Trip
          if (tripRangeTypeOrigin == 0 & tripRangeTypeDestin == 0) {
           
            tripOrigin <- individualActivityChain[trip-1,]$geometry
            
            tripOriginNB <- individualActivityChain[trip-1,]$tripNB
            furtherestNBDistance <- distancesBetweenNBs %>% 
              filter(NBName == tripOriginNB)
            furtherestNBDistance <- max(furtherestNBDistance[, c(2:146)])/1000
            
            if (tripStraightLineDistance <= furtherestNBDistance) {
              #In this case, we can find a possible NB by our new "buffer" algorithm
              buffer <- st_buffer(tripOrigin, tripStraightLineDistance * 1000)
              buffer <- buffer %>% st_cast("LINESTRING")
              BrusselsNBShape$intersectsWithRing <- ifelse(st_intersects(BrusselsNBShape, buffer, sparse = FALSE), "Yes", "No")
              NBSelected <- BrusselsNBShape %>% filter(intersectsWithRing == "Yes")
              #The NB used for selection based on different flow data
              
              if (tripPurpose == "work") {
                #For trip type work (regular trip for work activity)
                tripFlowFromOriginNB <- proximus_NBfrequentTrip_forGeneral %>% 
                  filter(originNB == tripOriginNB) %>% 
                  filter(destinationNB %in% NBSelected$NAME_FRE) %>% 
                  select(-irregularTrip)
                NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$regularTrip)
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB

                POISelected <- BrusselsPOI_work %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n = 1, weight_by = attractiveness)
                
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a work destination! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
                
              } else if (tripPurpose == "school" & adultOrKid == "adult") {
                #For adult EDU trip
                if (!all(!NBSelected$NAME_FRE %in% proximus_NBfrequentTrip_forAdultEDU$destinationNB)) {
                  tripFlowFromOriginNB <- proximus_NBfrequentTrip_forAdultEDU %>% 
                    filter(originNB == tripOriginNB) %>% 
                    filter(destinationNB %in% NBSelected$NAME_FRE) %>% 
                    select(-irregularTrip)
                  
                  NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$regularTrip)
                  demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                  
                  POISelected <- BrusselsPOI_adultSchool %>% 
                    filter(NBFre == NBSelected$destinationNB) %>% 
                    slice_sample(n = 1, weight_by = attractiveness)
                  
                  demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                  
                  print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned an adult EDU destination! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
                } else {
                  
                  closestNB <- distancesBetweenNBs %>% 
                    filter(NBName == tripOriginNB)
                  closestNB <- closestNB[, which(names(closestNB) %in% proximus_NBfrequentTrip_forAdultEDU$destinationNB)]
                  
                  closestNB <- abs(drop_units(closestNB) - tripStraightLineDistance * 1000)
           
                  closestNB <- colnames(closestNB)[which.min(closestNB)]
                  
                  tripFlowFromOriginNB <- proximus_NBfrequentTrip_forAdultEDU %>% 
                    filter(originNB == tripOriginNB) %>%
                    filter(destinationNB == closestNB) %>% 
                    select(-irregularTrip)
                  
                  NBSelected <- tripFlowFromOriginNB
                  demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                  
                  POISelected <- BrusselsPOI_adultSchool %>% 
                    filter(NBFre == NBSelected$destinationNB) %>% 
                    slice_sample(n = 1, weight_by = attractiveness)
                  
                  demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                  print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned an adult EDU destination by closest random selection! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
                }
                
              } else if (tripPurpose == "school" & adultOrKid == "kid") {
                #Scenario number 3.1
                #For kid EDU trip
                if (!all(!NBSelected$NAME_FRE %in% BISA_kidEduNBFlow$destinationNB)) {
                  tripFlowFromOriginNB <- BISA_kidEduNBFlow %>% 
                    filter(originNB == tripOriginNB) %>% 
                    filter(destinationNB %in% NBSelected$NAME_FRE)
                  
                  NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$regularTrip)
                  demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                  
                  POISelected <- BrusselsPOI_kidSchool %>% 
                    filter(NBFre == NBSelected$destinationNB) %>% 
                    slice_sample(n = 1, weight_by = attractiveness)
                  
                  demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                  
                  print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a kid EDU destination! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
                } else {
                  #Scenario number 3.2
                  #Which means here no NB with a kid school within this trip distance is available
                  closestNB <- distancesBetweenNBs %>% 
                    filter(NBName == tripOriginNB)
                  closestNB <- closestNB[, which(names(closestNB) %in% BISA_kidEduNBFlow$destinationNB)]
   
                  closestNB <- abs(drop_units(closestNB) - tripStraightLineDistance * 1000)
 
                  closestNB <- colnames(closestNB)[which.min(closestNB)]
                  
                  tripFlowFromOriginNB <- BISA_kidEduNBFlow %>% 
                    filter(originNB == tripOriginNB) %>%
                    filter(destinationNB == closestNB)
                  
                  NBSelected <- tripFlowFromOriginNB
                  demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                  
                  POISelected <- BrusselsPOI_kidSchool %>% 
                    filter(NBFre == NBSelected$destinationNB) %>% 
                    slice_sample(n = 1, weight_by = attractiveness)
                  
                  demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                  
                  print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a kid EDU destination by closest random selection! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
                }
              } else if (tripPurpose == "home") {
                #Scenario number 4
                #For return home activity
                tripFlowFromOriginNB <- proximus_NBfrequentTrip_forGeneral %>% 
                  filter(originNB == tripOriginNB) %>% 
                  filter(destinationNB %in% NBSelected$NAME_FRE) %>% 
                  select(-irregularTrip)
                
                NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$regularTrip)
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_home %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n=1)
      
                
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a return home destination! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
              } else if (tripPurpose == "shopping") {
                #For shopping activity/irregular trip/145 NBs in shopping POI
                tripFlowFromOriginNB <- proximus_NBhourlyTrip %>% 
                  filter(originNB == tripOriginNB) %>% 
                  filter(destinationNB %in% NBSelected$NAME_FRE) %>% 
                  filter(arrival_hr == tripDepartHour)
                
                NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$irregularTrips)
                
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_shopping %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n=1)
                
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a shopping destination! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
                
              } else if (tripPurpose == "other") {
                #For other activity/irregular trip/145 NBs in other POIs
                tripFlowFromOriginNB <- proximus_NBhourlyTrip %>% 
                  filter(originNB == tripOriginNB) %>% 
                  filter(destinationNB %in% NBSelected$NAME_FRE) %>% 
                  filter(arrival_hr == tripDepartHour)
                
                NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$irregularTrips)
                
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_other %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n=1)
                
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned an other secondary destination! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
                
              } else {
                #For trip purpose leisure/irregular trip/145 NBs in leisure POI
                tripFlowFromOriginNB <- proximus_NBhourlyTrip %>% 
                  filter(originNB == tripOriginNB) %>% 
                  filter(destinationNB %in% NBSelected$NAME_FRE) %>% 
                  filter(arrival_hr == tripDepartHour)
                
                NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$irregularTrips)
                
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_other %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n=1)
                
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a leisure destination! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
                
              }
              
            } else{
              #For trip exceed furtherest distance (need to assign a NB within Brussels randomly based on NB popularity)
              if (tripPurpose == "work") {
                #For Work Trip
                NBSelected <- slice_sample(proximus_NBfrequentTrip_forGeneral_Popularity, n = 1, weight_by = proximus_NBfrequentTrip_forGeneral_Popularity$totalArrivalTrip)
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_work %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n=1, weight_by = attractiveness)
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a working destination (exceed furtherest distance)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
              } else if (tripPurpose == "school" & adultOrKid == "adult") {
                #For adult EDU trip
                NBSelected <- slice_sample(proximus_NBfrequentTrip_forAdultEDU_popularity, n = 1, weight_by = proximus_NBfrequentTrip_forAdultEDU_popularity$totalArrivalTrip)
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_adultSchool %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n = 1, weight_by = attractiveness)
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned an adult EDU destination (exceed furtherest distance)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
              } else if (tripPurpose == "school" & adultOrKid == "kid") {
                #For kid EDU trip
                NBSelected <- slice_sample(BISA_kidEduNBFlow_Popularity, n = 1, weight_by = BISA_kidEduNBFlow_Popularity$totalArrivalTrip)
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_kidSchool %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n = 1, weight_by = attractiveness)
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a kid EDU destination (exceed furtherest distance)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
              } else if (tripPurpose == "home") {
                #For return home trip
                NBSelected <- slice_sample(proximus_NBfrequentTrip_forGeneral_Popularity, n = 1, weight_by = proximus_NBfrequentTrip_forGeneral_Popularity$totalArrivalTrip)
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_home %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n=1)
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a home destination (exceed furtherest distance)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
              } else {
                #For other secondary trips
                tripFlowFromOriginNB <- proximus_NBhourlyTrip_popularity %>% 
                  filter(arrival_hr == tripDepartHour)
                tripFlowFromOriginNB <- as.data.frame(tripFlowFromOriginNB)
                NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$totalArrivalTrip)
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                
                if (tripPurpose == "shopping") {
                  POISelected <- slice_sample(filter(BrusselsPOI_shopping, NBFre == NBSelected$destinationNB), n=1)
                } else if (tripPurpose == "leisure") {
                  POISelected <- slice_sample(filter(BrusselsPOI_leisure, NBFre == NBSelected$destinationNB), n=1)
                } else if (tripPurpose == "other") {
                  POISelected <- slice_sample(filter(BrusselsPOI_other, NBFre == NBSelected$destinationNB), n=1)
                }
                
                demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
                print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a secondary destination (exceed furtherest distance)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
                #Finish all within Brussels trip possibility
              }
            }
          } else if (tripRangeTypeOrigin == 0 & (tripRangeTypeDestin == 1 | tripRangeTypeDestin == 2)) {
            #For trip type "Brussels going to outside Brussels"
           
            NBSelected <- "reported outside Brussels trip"
            demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected
 
            tripDestinPostcode <- tripSelected$desPostcode
            if (!is.na(tripDestinPostcode)) {
              postcodePoint <- BelgiumPostcode %>% filter(postcode == tripDestinPostcode)
             
              buffer <- st_buffer(postcodePoint, 5 * 1000)
              pointSelected <- st_sample(buffer, 1)
              pointSelected <- st_as_sf(pointSelected)
              pointSelected <- st_transform(pointSelected, 31370)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- pointSelected$x
              print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a outside Brussels destination (Brussels to Outside with detailed destination postcode)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
            } else {
              
              tripDestinPostcode <- slice_sample(BelgiumPostcode, n = 1)$postcode
              
          
              postcodePoint <- BelgiumPostcode %>% filter(postcode == tripDestinPostcode)
         
              buffer <- st_buffer(postcodePoint, 5 * 1000)
              pointSelected <- st_sample(buffer, 1)
              pointSelected <- st_as_sf(pointSelected)
              pointSelected <- st_transform(pointSelected, 31370)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- pointSelected$x
              print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a outside Brussels destination (Brussels to Outside without detailed destination postcode)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
            }
          } else if ((tripRangeTypeOrigin == 1 | tripRangeTypeOrigin == 2) & tripRangeTypeDestin == 0){
            #For "outside brussels trip going to Brussels"
   
            if (tripPurpose == "work") {
              #For Work Trip
              NBSelected <- slice_sample(proximus_NBfrequentTrip_forGeneral_Popularity, n = 1, weight_by = proximus_NBfrequentTrip_forGeneral_Popularity$totalArrivalTrip)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
              POISelected <- BrusselsPOI_work %>% 
                filter(NBFre == NBSelected$destinationNB) %>% 
                slice_sample(n=1, weight_by = attractiveness)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
              
              print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a working destination (outside Brussels to Brussels)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
            } else if (tripPurpose == "school" & adultOrKid == "adult") {
              #For adult EDU trip
              NBSelected <- slice_sample(proximus_NBfrequentTrip_forAdultEDU_popularity, n = 1, weight_by = proximus_NBfrequentTrip_forAdultEDU_popularity$totalArrivalTrip)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
              POISelected <- BrusselsPOI_adultSchool %>% 
                filter(NBFre == NBSelected$destinationNB) %>% 
                slice_sample(n = 1, weight_by = attractiveness)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
              
              print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned an adult EDU destination (outside Brussels to Brussels)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
            } else if (tripPurpose == "school" & adultOrKid == "kid") {
              #For kid EDU trip
              NBSelected <- slice_sample(BISA_kidEduNBFlow_Popularity, n = 1, weight_by = BISA_kidEduNBFlow_Popularity$totalArrivalTrip)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
              POISelected <- BrusselsPOI_kidSchool %>% 
                filter(NBFre == NBSelected$destinationNB) %>% 
                slice_sample(n = 1, weight_by = attractiveness)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
              
              print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a kid EDU destination (outside Brussels to Brussels)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
            } else if (tripPurpose == "home") {
              #For return home trip
              NBSelected <- slice_sample(proximus_NBfrequentTrip_forGeneral_Popularity, n = 1, weight_by = proximus_NBfrequentTrip_forGeneral_Popularity$totalArrivalTrip)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
              POISelected <- BrusselsPOI_home %>% 
                filter(NBFre == NBSelected$destinationNB) %>% 
                slice_sample(n=1)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
              
              print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a home destination (outside Brussels to Brussels)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
            } else {
              #For other secondary trips
              tripFlowFromOriginNB <- proximus_NBhourlyTrip_popularity %>% 
                filter(arrival_hr == tripDepartHour)
              tripFlowFromOriginNB <- as.data.frame(tripFlowFromOriginNB)
              NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$totalArrivalTrip)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
              
              if (tripPurpose == "shopping") {
                POISelected <- slice_sample(filter(BrusselsPOI_shopping, NBFre == NBSelected$destinationNB), n=1)
              } else if (tripPurpose == "leisure") {
                POISelected <- slice_sample(filter(BrusselsPOI_leisure, NBFre == NBSelected$destinationNB), n=1)
              } else if (tripPurpose == "other") {
                POISelected <- slice_sample(filter(BrusselsPOI_other, NBFre == NBSelected$destinationNB), n=1)
              }
              
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- POISelected$geometry
              print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a secondary destination (outside Brussels to Brussels)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
              #Finish all within Brussels trip possibility
            }
          } else if ((tripRangeTypeOrigin == 1 | tripRangeTypeOrigin == 2) & (tripRangeTypeDestin == 1 | tripRangeTypeDestin == 2)) {
            #For trip type "outside Brussels going to outside Brussels"
           
            NBSelected <- "reported outside Brussels trip"
            demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$tripNB <- NBSelected
            #Selecting detailed geometry
            tripDestinPostcode <- tripSelected$desPostcode
            if (!is.na(tripDestinPostcode)) {
              postcodePoint <- BelgiumPostcode %>% filter(postcode == tripDestinPostcode)
              
              buffer <- st_buffer(postcodePoint, 5 * 1000)
              pointSelected <- st_sample(buffer, 1)
              pointSelected <- st_as_sf(pointSelected)
              pointSelected <- st_transform(pointSelected, 31370)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- pointSelected$x
              print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a outside Brussels destination (outside Brussels to outside Brussels with detailed destination postcode)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
            } else {

              tripDestinPostcode <- slice_sample(BelgiumPostcode, n = 1)$postcode
              
            
              postcodePoint <- BelgiumPostcode %>% filter(postcode == tripDestinPostcode)
             
              buffer <- st_buffer(postcodePoint, 5 * 1000)
              pointSelected <- st_sample(buffer, 1)
              pointSelected <- st_as_sf(pointSelected)
              pointSelected <- st_transform(pointSelected, 31370)
              demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 1,]$geometry <- pointSelected$x
              print(paste("Step 5.6 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a outside Brussels destination (outside Brussels to Outside without detailed destination postcode)! Max ID is", demobel_matched_activities_split[[length(demobel_matched_activities_split)]][1,]$personID))
            }
          }
        }
      } else if (tripSelected$originDestinType == 0) {

        tripSequence <- tripSelected$sequenceTrip
        if (tripSequence != 1) {
          demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 0,]$geometry <- demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip - 1 & demobel_matched_activities$originDestinType == 1,]$geometry
          demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & demobel_matched_activities$originDestinType == 0,]$tripNB <- demobel_matched_activities[demobel_matched_activities$personID == personID & demobel_matched_activities$sequenceTrip == individualActivityChain[trip,]$sequenceTrip - 1 & demobel_matched_activities$originDestinType == 1,]$tripNB
        }
      }
    }
    demobel_matched_activities_collector <- rbind(demobel_matched_activities_collector, demobel_matched_activities)
  }
  
  demobel_matched_activities <- demobel_matched_activities_collector
  
  return(demobel_matched_activities)
}


demobel_matched_activities_part1 <- locationAssignment(demobel_matched_activities_part1)
demobel_matched_activities_part2 <- locationAssignment(demobel_matched_activities_part2)
demobel_matched_activities_part3 <- locationAssignment(demobel_matched_activities_part3)
demobel_matched_activities_part4 <- locationAssignment(demobel_matched_activities_part4)
demobel_matched_activities_part5 <- locationAssignment(demobel_matched_activities_part5)
demobel_matched_activities_part6 <- locationAssignment(demobel_matched_activities_part6)
demobel_matched_activities_part7 <- locationAssignment(demobel_matched_activities_part7)
demobel_matched_activities_part8 <- locationAssignment(demobel_matched_activities_part8)
demobel_matched_activities_part9 <- locationAssignment(demobel_matched_activities_part9)
demobel_matched_activities_part10 <- locationAssignment(demobel_matched_activities_part10)
demobel_matched_activities_part11 <- locationAssignment(demobel_matched_activities_part11)
demobel_matched_activities_part12 <- locationAssignment(demobel_matched_activities_part12)
demobel_matched_activities_part13 <- locationAssignment(demobel_matched_activities_part13)
demobel_matched_activities_part14 <- locationAssignment(demobel_matched_activities_part14)
demobel_matched_activities_part15 <- locationAssignment(demobel_matched_activities_part15)
demobel_matched_activities_part16 <- locationAssignment(demobel_matched_activities_part16)
demobel_matched_activities_part17 <- locationAssignment(demobel_matched_activities_part17)
demobel_matched_activities_part18 <- locationAssignment(demobel_matched_activities_part18)
demobel_matched_activities_part19 <- locationAssignment(demobel_matched_activities_part19)
demobel_matched_activities_part20 <- locationAssignment(demobel_matched_activities_part20)

demobel_matched_activities <- rbind(demobel_matched_activities_part1, 
                                    demobel_matched_activities_part2,
                                    demobel_matched_activities_part3,
                                    demobel_matched_activities_part4,
                                    demobel_matched_activities_part5,
                                    demobel_matched_activities_part6,
                                    demobel_matched_activities_part7,
                                    demobel_matched_activities_part8,
                                    demobel_matched_activities_part9,
                                    demobel_matched_activities_part10,
                                    demobel_matched_activities_part11,
                                    demobel_matched_activities_part12,
                                    demobel_matched_activities_part13,
                                    demobel_matched_activities_part14,
                                    demobel_matched_activities_part15,
                                    demobel_matched_activities_part16,
                                    demobel_matched_activities_part17,
                                    demobel_matched_activities_part18,
                                    demobel_matched_activities_part19,
                                    demobel_matched_activities_part20)



rm(demobel_matched_activities_part1, 
   demobel_matched_activities_part2,
   demobel_matched_activities_part3,
   demobel_matched_activities_part4,
   demobel_matched_activities_part5,
   demobel_matched_activities_part6,
   demobel_matched_activities_part7,
   demobel_matched_activities_part8,
   demobel_matched_activities_part9,
   demobel_matched_activities_part10,
   demobel_matched_activities_part11,
   demobel_matched_activities_part12,
   demobel_matched_activities_part13,
   demobel_matched_activities_part14,
   demobel_matched_activities_part15,
   demobel_matched_activities_part16,
   demobel_matched_activities_part17,
   demobel_matched_activities_part18,
   demobel_matched_activities_part19,
   demobel_matched_activities_part20)



#Step 6: Processing demobel data
adultNoActivity <- adultNoActivity %>% 
  select(personID, householdID, xHomeCoord, yHomeCoord, ageGroup, gender, highestDegree = degree)

kidNoActivity <- kidNoActivity %>% 
  select(personID, householdID, xHomeCoord, yHomeCoord, ageGroup, gender, highestDegree = degree)

agentWithNoTrip <- rbind(adultNoActivity, kidNoActivity)

rm(adultNoActivity, kidNoActivity)

demobel_matched_attribute <- left_join(demobel_matched_attribute,
                                       select(demobel_reference, personID, incomeLevel, carAvailability, workingStatus),
                                       by = c("personID" = "personID"))


demobel_matched_attribute <- demobel_matched_attribute %>% 
  select(-age, -xHomeCoord, -yHomeCoord, -WegingPop, -numberOfTrips, -geometry)

demobel_matched_attribute <- demobel_matched_attribute[order(demobel_matched_attribute[, "personID"]),]


demobel_matched_attribute <- demobel_matched_attribute %>% 
  select(-p_id, p_id)

demobel_matched_activities <- demobel_matched_activities %>% 
  select(-homeNB, -ageGroup, -xHomeCoord, -yHomeCoord, -adultOrKid)


agentWithNoTrip <- left_join(agentWithNoTrip,
                             select(demobel_reference, personID, statisticalSector, incomeLevel, carAvailability, workingStatus),
                             by = c("personID" = "personID"))

rm(demobel_reference)


agentWithNoTrip <- agentWithNoTrip %>% 
  left_join(BruSSJoinedWithNB, by = c("statisticalSector" = "SSName"))

agentWithNoTrip <- agentWithNoTrip %>% 
  select(-statisticalSector, -geometry)


demobel_matched_activities <- as.data.frame(demobel_matched_activities)

demobel_matched_activities <- demobel_matched_activities %>% 
  group_by(personID) %>% 
  filter(sequenceTrip == 1 & originDestinType == 0 | originDestinType == 1)


demobel_matched_activities <- demobel_matched_activities %>%
  ungroup() %>% 
  mutate(xActivityCoord = unlist(map(demobel_matched_activities$geometry, 1)), 
         yActivityCoord = unlist(map(demobel_matched_activities$geometry, 2)))

demobel_matched_activities <- demobel_matched_activities %>% 
  select(-geometry)

demobel_matched_legs <- demobel_matched_activities

#Processing activities
demobel_matched_activities <- demobel_matched_activities %>% 
  mutate(firstOrigin = ifelse(originDestinType == 0, 1, 0))

demobel_matched_activities[demobel_matched_activities$sequenceTrip == 1 & demobel_matched_activities$firstOrigin == 1,]$sequenceTrip <- 0


demobel_matched_activities <- rename(demobel_matched_activities, activityStartHour = departureHour)
demobel_matched_activities <- rename(demobel_matched_activities, activityStartMinute = departureMinute)
demobel_matched_activities <- rename(demobel_matched_activities, activityEndHour = arrivalHour)
demobel_matched_activities <- rename(demobel_matched_activities, activityEndMinute = arrivalMinute)

demobel_matched_activities <- demobel_matched_activities %>% 
  group_by(personID) %>% 
  mutate(activityStartHourNextRow = lead(activityStartHour)) %>% 
  mutate(activityStartMinuteNextRow = lead(activityStartMinute))

demobel_matched_activities$activityStartHour <- demobel_matched_activities$activityEndHour
demobel_matched_activities$activityStartMinute <- demobel_matched_activities$activityEndMinute

demobel_matched_activities$activityEndHour <- demobel_matched_activities$activityStartHourNextRow
demobel_matched_activities$activityEndMinute <- demobel_matched_activities$activityStartMinuteNextRow


demobel_matched_activities[demobel_matched_activities$firstOrigin == 1,]$tripPurpose <- "home"
demobel_matched_activities[demobel_matched_activities$firstOrigin == 1,]$activityStartHour <- 0
demobel_matched_activities[demobel_matched_activities$firstOrigin == 1,]$activityStartMinute <- 0


demobel_matched_activities <- as.data.frame(demobel_matched_activities)
demobel_matched_activities[is.na(demobel_matched_activities$activityEndHour),]$activityEndHour <- 25
demobel_matched_activities[is.na(demobel_matched_activities$activityEndMinute),]$activityEndMinute <- 0

demobel_matched_activities <- demobel_matched_activities %>% 
  select(-activityStartHourNextRow, -activityStartMinuteNextRow) %>% 
  select(-tripRangeTypeOrigin, -tripRangeTypeDestin, -mode, -tripDistance, 
         -arrivalTimeInMInute, -originDestinType) %>% 
  select(-tripNB, -firstOrigin)

demobel_matched_activities <- rename(demobel_matched_activities, typicalDuration = typicalDurationNextActivity)
demobel_matched_activities[demobel_matched_activities$sequenceTrip == 0, ]$typicalDuration <- demobel_matched_activities[demobel_matched_activities$sequenceTrip == 0, ]$activityEndHour * 3600 + demobel_matched_activities[demobel_matched_activities$sequenceTrip == 0, ]$activityEndMinute * 60

demobel_matched_activities <- rename(demobel_matched_activities, activityPurpose = tripPurpose)


demobel_matched_activities$typicalDuration <- ifelse(demobel_matched_activities$typicalDuration < 601,
                                                     600,
                                                     round(demobel_matched_activities$typicalDuration/600) * 600)
demobel_matched_activities <- select(demobel_matched_activities, -desPostcode)



demobel_matched_legs <- demobel_matched_legs %>% 
  filter(originDestinType != 0)


demobel_matched_legs <- demobel_matched_legs %>% 
  mutate(travelTime = arrivalHour * 60 + arrivalMinute - departureHour * 60 - departureMinute)

demobel_matched_legs <- demobel_matched_legs %>% 
  mutate(travelHour = floor(travelTime/60)) %>% 
  mutate(travelMinute = travelTime %% 60)

demobel_matched_legs <- demobel_matched_legs %>% 
  select(personID, sequenceTrip, departureHour, departureMinute, mode, travelHour, travelMinute)

postal_codes <- read_excel("Postcode.xlsx")

#read in adult and child data
adult_intake <- read.csv2("MONITOR/Data/Adults/Intake.csv")
adult_trips <- read.csv2("MONITOR/Data/Adults/Trips.csv")
adult_moves <- read.csv2("MONITOR/Data/Adults/Moves.csv")

kid_intake <- read.csv2("MONITOR/Data/Kids/Intake_K.csv")
kid_trips <- read.csv2("MONITOR/Data/Kids/Trips_K.csv")
kid_moves <- read.csv2("MONITOR/Data/Kids/Moves_K.csv")

#selection of attributes for each data frame
individuals_attributes <- c("p_id", "Beroep", "Diploma", "Geslacht", "huishoud", 
                            "Leeftijd3N", "Postcode", "p_yesterday", "Region", "V003a_10",
                            "V006b", "V007", "V014", "Weging",	"WegingPop",	"Year_Birth")

col_names <- c("p_id", "OccupationDomain", "HighestDegree", "Gender", "HouseholdStatus", "Age",
               "Postcode", "p_yesterday", "Region", "CarAvailability",
               "DriverLicenseAvailability", "WorkingStatus(Full/Part)",
               "HouseholdNetIncome", "Weging",	"WegingPop",	"Year_Birth")

trips_attributes <- c("t_id", "t_participant_id", "t_source_hour",
                      "t_source_minute", "t_source", "t_source_foreign",
                      "t_source_id",	"t_destination_hour",
                      "t_destination_minute",	"t_destination",
                      "t_destination_foreign", "t_reason",
                      "t_reason_other",	"duration",
                      "t_source_PostCode", "t_destination_PostCode")

moves_attributes <- c("m_id","m_track_id","m_participant_id", 
                      "m_way","m_distance","m_duration_tot")

#Step 6: Processing MONITOR data
#Step 6.1: Processing adult MONITOR data
adult_intake <- adult_intake %>% 
  select(all_of(individuals_attributes))


names(adult_intake) <- col_names

adult_intake <- adult_intake %>% 
  mutate(p_yesterday = as.Date(p_yesterday, "%d/%m/%Y"))


adult_intake <- adult_intake %>% 
  mutate(Week = weekdays(p_yesterday),
         AgeExact = 2017 - Year_Birth,
         HighestDegreeCat = recode(HighestDegree, '1' = "1-2", '2' = "1-2",
                                   '3' = "3-4", '4' = "3-4", '5' = "3-4", '6' = "5-6",
                                   '7' = "7-8", '8' = "7-8", '9' = "7-8")) %>%
  mutate(AgeGroupMethodology = ifelse(AgeExact >= 18 & AgeExact <= 24, 1, 
                                      ifelse(AgeExact >= 25 & AgeExact <= 34, 2,
                                             ifelse(AgeExact >= 35 & AgeExact <= 44, 3, 
                                                    ifelse(AgeExact >= 45 & AgeExact <= 54, 4,
                                                           ifelse(AgeExact >= 55 & AgeExact <= 64, 5,
                                                                  ifelse(AgeExact >= 65 & AgeExact <= 74, 6, 7))))))) %>% 
  filter(Week != "Saturday", Week != "Sunday") %>% 
  filter(Region != "Bruxelles")

adult_trips <- adult_trips %>% 
  select(all_of(trips_attributes)) %>% 
  filter(t_participant_id %in% adult_intake$p_id)


adult_trips$tripRangeTypeOrigin <- ifelse(adult_trips$t_source_foreign == 1, 2, 
                                          ifelse(adult_trips$t_source_PostCode %in% postal_codes$Postcode, 0, 1))

adult_trips$tripRangeTypeDestin <- ifelse(adult_trips$t_destination_foreign == 1, 2,
                                          ifelse(adult_trips$t_destination_PostCode %in% postal_codes$Postcode, 0, 1))


peopleWithBruTrip <- adult_trips %>% 
  filter(tripRangeTypeOrigin == 0 | tripRangeTypeDestin == 0) %>% 
  select(personID = t_participant_id) %>% 
  filter(!duplicated(personID))



adult_intake <- adult_intake %>% filter(p_id %in% peopleWithBruTrip$personID)
adult_trips <- adult_trips %>% filter(t_participant_id %in% peopleWithBruTrip$personID)
adult_moves <- adult_moves %>% filter(m_participant_id %in% peopleWithBruTrip$personID)

table(adult_trips$t_reason)

adult_trips_otherReasons <- filter(adult_trips, adult_trips$t_reason == 8)

adult_trips$t_reason[adult_trips$t_id == 8768] <- 2
adult_trips$t_reason[adult_trips$t_id == 4570] <- 1
adult_trips$t_reason[adult_trips$t_id == 13335] <- 1
adult_trips$t_reason[adult_trips$t_id == 20902] <- 1
adult_trips$t_reason[adult_trips$t_id == 20903] <- 1
adult_trips$t_reason[adult_trips$t_id == 32683] <- 1
adult_trips$t_reason[adult_trips$t_id == 36232] <- 1
adult_trips$t_reason[adult_trips$t_id == 36235] <- 7
adult_trips$t_reason[adult_trips$t_id == 37629] <- 2
adult_trips$t_reason[adult_trips$t_id == 39215] <- 1
adult_trips$t_reason[adult_trips$t_id == 55518] <- 2
adult_trips$t_reason[adult_trips$t_id == 40249] <- 1

rm(adult_trips_otherReasons, peopleWithBruTrip)


adult_trips$counter <- 1
adult_trips <- adult_trips[order(adult_trips[,'t_participant_id'],adult_trips[,'t_id']),]
adult_trips <- adult_trips %>% 
  group_by(t_participant_id) %>% 
  mutate(sequenceTrip = cumsum(counter))

adult_trips <- adult_trips %>% 
  mutate(endHourLastTrip = lag(t_destination_hour)) %>% 
  mutate(endMinuteLastTrip = lag(t_destination_minute))

adult_trips$typicalDurationLastActivity <- ifelse(is.na(adult_trips$t_source_id), 
                                                  adult_trips$t_source_hour*60 + adult_trips$t_source_minute,
                                                  adult_trips$t_source_hour*60 + adult_trips$t_source_minute - adult_trips$endHourLastTrip * 60 - adult_trips$endMinuteLastTrip)

adult_moves <- adult_moves %>% 
  select(all_of(moves_attributes)) %>% 
  mutate(mode = recode(m_way, '1' = "walk", '2' = "bike", '3' = "motorcycle", '4' = "train",
                       '5' = "pt", '6' = "pt", '7' = "car", '8' = "ride",
                       '9' =  "car", '10' = "ride", '11' = "motorcycle",  '12' = "motorcycle",
                       '13' = "motorcycle", '14' = "motorcycle", '15' = "car", '16' = "motorcycle",
                       '17' = "car", '18' = 'car', '19' = "car", '20' = "other")) %>% 
  filter(m_participant_id %in% adult_intake$p_id)


tripWithoutMovePeople <- adult_trips %>%
  filter(!(t_id %in% adult_moves$m_track_id)) %>% 
  select(peopleID = t_participant_id) %>% 
  filter(!duplicated(peopleID))

adult_intake <- adult_intake %>% filter(!(p_id %in% tripWithoutMovePeople$peopleID))
adult_trips <- adult_trips %>% filter(!(t_participant_id %in% tripWithoutMovePeople$peopleID))
adult_moves <- adult_moves %>% filter(!(m_participant_id %in% tripWithoutMovePeople$peopleID))


adult_trip_move <- left_join(adult_trips, adult_moves, by = c("t_id" = "m_track_id"))


adult_trip_move$mode_order <- ifelse(adult_trip_move$mode == "other", 0, ifelse(adult_trip_move$mode == "walk", 1, ifelse(adult_trip_move$mode == "bike", 2, ifelse(adult_trip_move$mode == "motorcycle", 3, 4))))
adult_trip_move <- adult_trip_move[order(adult_trip_move[,'t_id'],-adult_trip_move[,'mode_order'],-adult_trip_move[,'m_distance']),]
adult_trip_distance <- adult_trip_move %>% 
  group_by(t_id) %>% 
  summarise(tripDistance = sum(m_distance)) 
adult_trip_move <- adult_trip_move[!duplicated(adult_trip_move$t_id),]
adult_trip_move <- left_join(adult_trip_move, adult_trip_distance, by = "t_id")
adult_trip_move <- adult_trip_move %>% 
  mutate(durationHour = duration/60) %>% 
  mutate(speedTrip = tripDistance/durationHour)

adult_trip_move$mode[adult_trip_move$t_id == 27145] <- "walk"
adult_trip_move$mode[adult_trip_move$t_id == 27591] <- "car"
adult_trip_move$mode[adult_trip_move$t_id == 40228] <- "train"
adult_trip_move$mode[adult_trip_move$t_id == 40229] <- "train"
adult_trip_move$mode[adult_trip_move$t_id == 45319] <- "car"
adult_trip_move$mode[adult_trip_move$t_id == 45320] <- "car"

adult_number_of_trips <- adult_trip_move %>% group_by(t_participant_id) %>% count()

adult_trip_move <- adult_trip_move %>% 
  mutate(tripOriginInMinute = t_source_hour*60 + t_source_minute) %>% 
  mutate(tripDestinInMinute = t_destination_hour*60 + t_destination_minute) %>% 
  group_by(t_participant_id) %>% 
  mutate(sequenceCheck = tripOriginInMinute - lag(tripDestinInMinute))


adult_trip_move_editSequence <- filter(adult_trip_move, t_participant_id == 36615)
adult_trip_move <- adult_trip_move[-which(adult_trip_move$t_participant_id == 36615),]
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_id = lead(t_id))
adult_trip_move_editSequence$t_id[is.na(adult_trip_move_editSequence$t_id)] <- 32446
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_source_id = lead(t_source_id))
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, sequenceTrip = lead(sequenceTrip))
adult_trip_move_editSequence$sequenceTrip[adult_trip_move_editSequence$t_id == 32446] <- 1
adult_trip_move <- rbind(adult_trip_move, adult_trip_move_editSequence)


adult_trip_move_editSequence <- filter(adult_trip_move, t_participant_id == 399)
adult_trip_move <- adult_trip_move[-which(adult_trip_move$t_participant_id == 399),]
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_id = lead(t_id))
adult_trip_move_editSequence$t_id[is.na(adult_trip_move_editSequence$t_id)] <- 1051
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_id = lead(t_id))
adult_trip_move_editSequence$t_id[is.na(adult_trip_move_editSequence$t_id)] <- 1052
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_source_id = lead(t_source_id))
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_source_id = lead(t_source_id))
adult_trip_move_editSequence$t_source_id[adult_trip_move_editSequence$t_id == 1052] <- 1051
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, sequenceTrip = lead(sequenceTrip))
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, sequenceTrip = lead(sequenceTrip))
adult_trip_move_editSequence$sequenceTrip[adult_trip_move_editSequence$t_id == 1052] <- 2
adult_trip_move_editSequence$sequenceTrip[adult_trip_move_editSequence$t_id == 1051] <- 1
adult_trip_move <- rbind(adult_trip_move, adult_trip_move_editSequence)


adult_trip_move_editSequence <- filter(adult_trip_move, t_participant_id == 2381)
adult_trip_move <- adult_trip_move[-which(adult_trip_move$t_participant_id == 2381),]
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_id = lead(t_id))
adult_trip_move_editSequence$t_id[is.na(adult_trip_move_editSequence$t_id)] <- 3119
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_source_id = lead(t_source_id))
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, sequenceTrip = lead(sequenceTrip))
adult_trip_move_editSequence$sequenceTrip[adult_trip_move_editSequence$t_id == 3119] <- 1
adult_trip_move <- rbind(adult_trip_move, adult_trip_move_editSequence)


adult_trip_move_editSequence <- filter(adult_trip_move, t_participant_id == 15805)
adult_trip_move <- adult_trip_move[-which(adult_trip_move$t_participant_id == 15805),]
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_id = lead(t_id))
adult_trip_move_editSequence$t_id[is.na(adult_trip_move_editSequence$t_id)] <- 23478
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_source_id = lead(t_source_id))
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, sequenceTrip = lead(sequenceTrip))
adult_trip_move_editSequence$sequenceTrip[adult_trip_move_editSequence$t_id == 23478] <- 1
adult_trip_move <- rbind(adult_trip_move, adult_trip_move_editSequence)


adult_trip_move_editSequence <- filter(adult_trip_move, t_participant_id == 56711)
adult_trip_move <- adult_trip_move[-which(adult_trip_move$t_participant_id == 56711),]
adult_intake <- adult_intake[-which(adult_intake$p_id == 56711),]

adult_trip_move_editSequence <- filter(adult_trip_move, t_participant_id == 5450)
adult_trip_move <- adult_trip_move[-which(adult_trip_move$t_participant_id == 5450),]
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_id = lead(t_id))
adult_trip_move_editSequence$t_id[is.na(adult_trip_move_editSequence$t_id)] <- 5265
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, t_source_id = lead(t_source_id))
adult_trip_move_editSequence <- mutate(adult_trip_move_editSequence, sequenceTrip = lead(sequenceTrip))
adult_trip_move_editSequence$sequenceTrip[adult_trip_move_editSequence$t_id == 5265] <- 1
adult_trip_move <- rbind(adult_trip_move, adult_trip_move_editSequence)

adult_trip_move <- adult_trip_move[order(adult_trip_move[,'t_participant_id'],adult_trip_move[,'t_id']),]

#Recode the tripPurpose
adult_trip_move$tripReasonString <- recode(adult_trip_move$t_reason,
                                           '1' = "work", '2' = "school", '3' = "work", '4' = "work",
                                           '5' = "shopping", '6' = "leisure", '7' = "home", '8' = "other")

adult_trip_move <- adult_trip_move %>% 
  mutate(endHourLastTrip = lag(t_destination_hour)) %>% 
  mutate(endMinuteLastTrip = lag(t_destination_minute))

adult_trip_move$typicalDurationLastActivity <- ifelse(is.na(adult_trip_move$t_source_id), 
                                                      adult_trip_move$t_source_hour*60 + adult_trip_move$t_source_minute,
                                                      adult_trip_move$t_source_hour*60 + adult_trip_move$t_source_minute - adult_trip_move$endHourLastTrip * 60 - adult_trip_move$endMinuteLastTrip)

#Step 6.2: processing kid MONITOR data
kid_intake <- kid_intake %>% 
  select(p_id, Gender = Geslacht, AgeGroup = Leeftijd, Postcode, p_yesterday, Region = RegionFR, WegingPop)

kid_intake <- kid_intake %>% 
  mutate(p_yesterday = as.Date(p_yesterday, "%Y-%m-%d")) %>% 
  mutate(Week = weekdays(p_yesterday)) %>% 
  mutate(AgeGroupMethodology = ifelse(AgeGroup == 3, "2K", ifelse(AgeGroup == 2, "2K", "1K"))) %>% 
  filter(Week != "Saturday", Week != "Sunday") %>% 
  filter(Region != "Bruxelles")

kid_trips <- kid_trips %>% 
  select(all_of(trips_attributes)) %>% 
  filter(t_participant_id %in% kid_intake$p_id)

kid_trips$tripRangeTypeOrigin <- ifelse(kid_trips$t_source_foreign == 1, 2, 
                                        ifelse(kid_trips$t_source_PostCode %in% postal_codes$Postcode, 0, 1))

kid_trips$tripRangeTypeDestin <- ifelse(kid_trips$t_destination_foreign == 1, 2, 
                                        ifelse(kid_trips$t_destination_PostCode %in% postal_codes$Postcode, 0, 1))

peopleWithBruTrip <- kid_trips %>% 
  filter(tripRangeTypeOrigin == 0 | tripRangeTypeDestin == 0) %>% 
  select(personID = t_participant_id) %>% 
  filter(!duplicated(personID))???

kid_intake <- kid_intake %>% filter(p_id %in% peopleWithBruTrip$personID)
kid_trips <- kid_trips %>% filter(t_participant_id %in% peopleWithBruTrip$personID)
kid_moves <- kid_moves %>% filter(m_participant_id %in% peopleWithBruTrip$personID)

table(kid_trips$t_reason)


rm(peopleWithBruTrip)


kid_trips$counter <- 1
kid_trips <- kid_trips[order(kid_trips[,'t_participant_id'],kid_trips[,'t_id']),]
kid_trips <- kid_trips %>% 
  group_by(t_participant_id) %>% 
  mutate(sequenceTrip = cumsum(counter))

kid_trips <- kid_trips %>% 
  mutate(endHourLastTrip = lag(t_destination_hour)) %>% 
  mutate(endMinuteLastTrip = lag(t_destination_minute))

kid_trips$typicalDurationLastActivity <- ifelse(is.na(kid_trips$t_source_id), 
                                                kid_trips$t_source_hour*60 + kid_trips$t_source_minute,
                                                kid_trips$t_source_hour*60 + kid_trips$t_source_minute - kid_trips$endHourLastTrip * 60 - kid_trips$endMinuteLastTrip)


kid_moves <- kid_moves %>% 
  select(all_of(moves_attributes)) %>% 
  mutate(mode = recode(m_way, '1' = "walk", '2' = "bike", '3' = "motorcycle", '4' = "train",
                       '5' = "pt", '6' = "pt", '7' = "car", '8' = "ride",
                       '9' =  "car", '10' = "ride", '11' = "motorcycle",  '12' = "motorcycle",
                       '13' = "motorcycle", '14' = "motorcycle", '15' = "car", '16' = "motorcycle",
                       '17' = "car", '18' = 'car', '19' = "car", '20' = "other")) %>% 
  filter(m_participant_id %in% kid_intake$p_id)

tripWithoutMovePeople <- kid_trips %>%
  filter(!(t_id %in% kid_moves$m_track_id)) %>% 
  select(peopleID = t_participant_id) %>% 
  filter(!duplicated(peopleID))


kid_trip_move <- left_join(kid_trips, kid_moves, by = c("t_id" = "m_track_id"))
kid_trip_move$mode_order <- ifelse(kid_trip_move$mode == "other", 0, ifelse(kid_trip_move$mode == "walk", 1, ifelse(kid_trip_move$mode == "bike", 2, ifelse(kid_trip_move$mode == "motorcycle", 3, 4))))
kid_trip_move <- kid_trip_move[order(kid_trip_move[, 't_id'], -kid_trip_move[,'mode_order'], -kid_trip_move[,'m_distance']),]
kid_trip_distance <- kid_trip_move %>% 
  group_by(t_id) %>% 
  summarise(tripDistance = sum(m_distance))
kid_trip_move <- kid_trip_move[!duplicated(kid_trip_move$t_id),]
kid_trip_move <- left_join(kid_trip_move, kid_trip_distance, by = "t_id")
kid_trip_move <- kid_trip_move %>% 
  mutate(durationHour = duration/60) %>% 
  mutate(speedTrip = tripDistance/durationHour)

kid_trip_move$mode[kid_trip_move$t_id == 57645 | kid_trip_move$t_id == 57646] <- "train"


kid_trip_move <- kid_trip_move %>% 
  mutate(tripOriginInMinute = t_source_hour*60 + t_source_minute) %>% 
  mutate(tripDestinInMinute = t_destination_hour*60 + t_destination_minute) %>% 
  group_by(t_participant_id) %>% 
  mutate(sequenceCheck = tripOriginInMinute - lag(tripDestinInMinute))


kid_trip_move <- kid_trip_move[order(kid_trip_move[, 't_participant_id'], kid_trip_move[,'t_id']),]

kid_trip_move$tripReasonString <- recode(kid_trip_move$t_reason,
                                         '1' = "work", '2' = "school", '3' = "work", '4' = "work",
                                         '5' = "shopping", '6' = "leisure", '7' = "home", '8' = "other")


rm(adult_moves, adult_number_of_trips, adult_trip_move_editSequence, adult_trips, kid_moves, 
   kid_trip_distance, kid_trips, tripWithoutMovePeople, adult_trip_distance)


adult_intake <- adult_intake %>% 
  mutate(householdID = "OutsideBru") %>% 
  mutate(incomeLevel = 99) %>% 
  select(p_id, AgeGroupMethodology, highestDegree = HighestDegreeCat, homePostcode = Postcode,
         householdID, incomeLevel, sex = Gender, workingStatus = OccupationDomain, 
         populationRepresent = WegingPop)

kid_intake <- kid_intake %>% 
  mutate(householdID = "OutsideBru") %>% 
  mutate(highestDegree = "MONITORKid") %>% 
  mutate(incomeLevel = 99) %>% 
  mutate(OccupationDomain = 17) %>% 
  select(p_id, AgeGroupMethodology, highestDegree, homePostcode = Postcode,
         householdID, incomeLevel, sex = Gender, workingStatus = OccupationDomain, 
         populationRepresent = WegingPop)

nonBruPeople_attribute <- rbind(adult_intake, kid_intake) 
nonBruPeopleTrip <- rbind(adult_trip_move, kid_trip_move)

nonBruPeopleTrip[which(nonBruPeopleTrip$t_id == 3121),]$tripRangeTypeOrigin <- 1
nonBruPeopleTrip[which(nonBruPeopleTrip$t_id == 5266),]$tripRangeTypeOrigin <- 0

nonBruPeople_attribute <- nonBruPeople_attribute %>% 
  filter(!p_id == 16213)

nonBruPeopleTrip <- nonBruPeopleTrip %>% 
  filter(!t_participant_id == 16213)


nonBruPeople_attribute <- nonBruPeople_attribute %>% 
  filter(!p_id == 5450)

nonBruPeopleTrip <- nonBruPeopleTrip %>% 
  filter(!t_participant_id == 5450)


firstActivityTrip <- nonBruPeopleTrip %>% 
  filter(sequenceTrip == 1) %>% 
  select(t_participant_id, t_source_PostCode)

nonBruPeople_attribute <- left_join(nonBruPeople_attribute, firstActivityTrip, by = c("p_id" = "t_participant_id"))


nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(8700, 8701, 903, 904, 905, 906, 2710, 4453, 4456, 5489, 5787, 5788, 7622, 7652, 11267, 14838, 14840, 21294, 21298, 31654, 33540, 33541, 34747, 34348, 34349, 35463, 35464, 39518, 40306, 40309, 41439, 46959, 46961, 46732, 46736, 56426),]$mode <- "ptOutsideBRU"

nonBruPeople_attribute <- nonBruPeople_attribute %>% 
  filter(!p_id == 2381)

nonBruPeopleTrip <- nonBruPeopleTrip %>% 
  filter(!t_participant_id == 2381)



#for id 383
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(8694),]$tripReasonString <- "other"

#for id 485
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(325),]$tripReasonString <- "other"

#for id 699
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(598, 599),]$tripReasonString <- "other"

#for id 1165
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(1436),]$tripReasonString <- "other"

#for id 1349
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(1661, 1662, 1663),]$tripReasonString <- "other"

#for id 1887
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(2319),]$tripReasonString <- "other"

#for id 2112
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(2511),]$tripReasonString <- "other"

#for id 2552
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(2907, 2908),]$tripReasonString <- "other"

#for id 2630
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(2865, 2866),]$tripReasonString <- "other"

#for id 2834
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(4498, 4499),]$tripReasonString <- "other"

#for id 3281
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(4236, 4237, 4238),]$tripReasonString <- "other"

#for id 3823
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(4763),]$tripReasonString <- "other"

#for id 4554
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(5785, 5786, 5788, 5789),]$tripReasonString <- "other"

#for id 6089
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(7658),]$tripReasonString <- "other"

#for id 7203
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(11097),]$tripReasonString <- "other"

#for id 8254
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(10260),]$tripReasonString <- "other"

#for id 8263
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(10267, 10268),]$tripReasonString <- "other"

#for id 10887
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(13821),]$tripReasonString <- "other"

#for id 11057
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(14105),]$tripReasonString <- "other"

#for id 12219
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(14880, 14892),]$tripReasonString <- "other"

#for id 12535
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(15790, 15791),]$tripReasonString <- "other"

#for id 15887
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(17847),]$tripReasonString <- "other"

#for id 16322
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(19240, 19242, 19243),]$tripReasonString <- "other"

#for id 16332
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(18851),]$tripReasonString <- "other"

#for id 20161
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(22492, 22493, 22494),]$tripReasonString <- "other"

#for id 20405
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(22510, 22511),]$tripReasonString <- "other"

#for id 23974
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(25415),]$tripReasonString <- "other"

#for id 28801
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(30492, 30493, 30494),]$tripReasonString <- "other"

#for id 29783
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(28993, 28994),]$tripReasonString <- "other"

#for id 36071
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(30638, 30639),]$tripReasonString <- "other"

#for id 36830
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(31657, 31658),]$tripReasonString <- "other"

#for id 37630
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(32775, 32776, 32777),]$tripReasonString <- "other"

#for id 41055
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(36422),]$tripReasonString <- "other"

#for id 41118
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(36725, 36729, 36731),]$tripReasonString <- "other"

#for id 41176
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(36750, 36751),]$tripReasonString <- "other"

#for id 41216
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(36707),]$tripReasonString <- "other"

#for id 41853
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(39180),]$tripReasonString <- "other"

#for id 42765
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(38369, 38370),]$tripReasonString <- "other"

#for id 45236
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(39170, 39171),]$tripReasonString <- "other"

#for id 55193
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(45235, 45236, 45237),]$tripReasonString <- "other"

#for id 55933
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(44272, 44273),]$tripReasonString <- "other"

#for id 55986
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(45175),]$tripReasonString <- "other"

#for id 56049
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(45669, 45670, 45671),]$tripReasonString <- "other"

#for id 64357
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(52427, 52429),]$tripReasonString <- "other"

#for id 70297
nonBruPeopleTrip[nonBruPeopleTrip$t_id %in% c(54519, 54521),]$tripReasonString <- "other"

#==================(finished editing)



#Step 8 Activity Start time randomisation for selected people

nonBruPeople_attribute <- nonBruPeople_attribute %>% 
  mutate(floorPopulationProb = 1-(populationRepresent - floor(populationRepresent))) %>% 
  mutate(randomNumber = 0) %>% 
  mutate(sampledWeight = 0)

for (number in 1:nrow(nonBruPeople_attribute)) {
  randomNumber <- runif(1)
  nonBruPeople_attribute[number,]$randomNumber <- randomNumber
  if (randomNumber <= nonBruPeople_attribute[number,]$floorPopulationProb) {
    nonBruPeople_attribute[number,]$sampledWeight <- floor(nonBruPeople_attribute[number,]$populationRepresent)
  } else {
    nonBruPeople_attribute[number,]$sampledWeight <- floor(nonBruPeople_attribute[number,]$populationRepresent) + 1
  }
}

nonBruPeople_attribute$populationRepresent <- nonBruPeople_attribute$sampledWeight
nonBruPeople_attribute <- nonBruPeople_attribute %>% 
  select(-sampledWeight, -randomNumber, -floorPopulationProb)

nonBruPeople_attribute <- uncount(nonBruPeople_attribute, nonBruPeople_attribute$populationRepresent)
nonBruPeople_attribute <- select(nonBruPeople_attribute, -populationRepresent)


nonBruPeople_attribute <- nonBruPeople_attribute[sample(nrow(nonBruPeople_attribute), populationPercent * nrow(nonBruPeople_attribute)),]
rownames(nonBruPeople_attribute) <- 1:nrow(nonBruPeople_attribute)
nonBruPeople_attribute <- mutate(nonBruPeople_attribute, personID = rownames(nonBruPeople_attribute))
nonBruPeople_attribute <- select(nonBruPeople_attribute, personID, everything())

nonBruPeople_trip <- left_join(select(nonBruPeople_attribute, personID, p_id), nonBruPeopleTrip, by = c("p_id" = "t_participant_id"))


nonBruPeople_trip <- nonBruPeople_trip %>% 
  group_by(personID) %>% 
  select(personID, p_id, t_id, t_source_hour, t_source_minute, t_source_id, duration, tripRangeTypeOrigin, tripRangeTypeDestin, sequenceTrip,
         typicalDurationLastActivity, m_id, mode, tripDistance, tripReasonString, t_source_PostCode, t_destination_PostCode)


nonBruPeople_trip <- nonBruPeople_trip %>% 
  mutate(randomTimeNormalVariants = round(rnorm(1, mean = 0, sd=15))) %>% 
  mutate(startTimeInMinute = t_source_hour*60 + t_source_minute + randomTimeNormalVariants)


nonBruPeople_trip$startTimeInMinute <- ifelse(nonBruPeople_trip$startTimeInMinute < 0,
                                              0,
                                              nonBruPeople_trip$startTimeInMinute)

nonBruPeople_trip$startTimeInMinute <- ifelse(nonBruPeople_trip$startTimeInMinute > 1440,
                                              1440,
                                              nonBruPeople_trip$startTimeInMinute)

nonBruPeople_trip$t_source_hour <- ifelse(is.na(nonBruPeople_trip$t_source_id),
                                          floor(nonBruPeople_trip$startTimeInMinute/60),
                                          nonBruPeople_trip$t_source_hour)

nonBruPeople_trip$t_source_minute <- ifelse(is.na(nonBruPeople_trip$t_source_id),
                                            nonBruPeople_trip$startTimeInMinute %% 60,
                                            NA)

nonBruPeople_trip <- nonBruPeople_trip %>% 
  select(-randomTimeNormalVariants, -startTimeInMinute)




nonBruPeople_trip <- nonBruPeople_trip %>% 
  mutate(departureTimeInMinute = t_source_hour*60 + t_source_minute) %>%
  mutate(arrivalTimeInMinute = departureTimeInMinute + duration) %>% 
  mutate(arrivalHour = arrivalTimeInMinute %/% 60) %>% 
  mutate(arrivalMinute = arrivalTimeInMinute %% 60) %>% 
  mutate(typicalDurationNextActivity = lead(typicalDurationLastActivity)) %>% 
  mutate(finishActivityTimeInMinute = arrivalTimeInMinute + typicalDurationNextActivity) %>% 
  mutate(nextTripDepartHour = finishActivityTimeInMinute %/% 60) %>% 
  mutate(nextTripDepartMinute = finishActivityTimeInMinute %% 60)

nonBruPeople_trip_split <- group_split(nonBruPeople_trip)

counter <- 0

nonBruPeople_trip_collector <- nonBruPeople_trip_split[[1]][0,]

for (a in 1:length(nonBruPeople_trip_split)) {
  nonBruPeople_trip <- nonBruPeople_trip_split[[a]]
  
  for (i in 1:nrow(nonBruPeople_trip)) {
    currentTrip <- nonBruPeople_trip[i,]
    if (currentTrip$sequenceTrip != 1) {
      formerTrip <- nonBruPeople_trip[i-1,]
      nonBruPeople_trip[i,]$t_source_hour <- formerTrip$nextTripDepartHour
      nonBruPeople_trip[i,]$t_source_minute <- formerTrip$nextTripDepartMinute
      nonBruPeople_trip[i,]$departureTimeInMinute <- nonBruPeople_trip[i,]$t_source_hour*60 + nonBruPeople_trip[i,]$t_source_minute
      nonBruPeople_trip[i,]$arrivalTimeInMinute <- nonBruPeople_trip[i,]$departureTimeInMinute + nonBruPeople_trip[i,]$duration
      nonBruPeople_trip[i,]$arrivalHour <- nonBruPeople_trip[i,]$arrivalTimeInMinute %/% 60
      nonBruPeople_trip[i,]$arrivalMinute <- nonBruPeople_trip[i,]$arrivalTimeInMinute %% 60
      nonBruPeople_trip[i,]$finishActivityTimeInMinute <- nonBruPeople_trip[i,]$arrivalTimeInMinute + nonBruPeople_trip[i,]$typicalDurationNextActivity
      nonBruPeople_trip[i,]$nextTripDepartHour <- nonBruPeople_trip[i,]$finishActivityTimeInMinute %/% 60
      nonBruPeople_trip[i,]$nextTripDepartMinute <- nonBruPeople_trip[i,]$finishActivityTimeInMinute %% 60
    }
  }
  
  nonBruPeople_trip_collector <- rbind(nonBruPeople_trip_collector, nonBruPeople_trip)
  
  counter <- counter + 1
  
  if (counter %% 100 == 0) {
    print(paste("Step 8", counter, "/", length(nonBruPeople_trip_split), "adults have assigned a random start time"))
  }
}

nonBruPeople_trip <- nonBruPeople_trip_collector
nonBruPeople_trip <- group_by(nonBruPeople_trip, personID)
rm(nonBruPeople_trip_split, nonBruPeople_trip_collector)


nonBruPeople_trip <- select(nonBruPeople_trip, personID, sequenceTrip, departureHour = t_source_hour, 
                            departureMinute = t_source_minute, 
                            tripRangeTypeOrigin, arrivalHour, arrivalMinute, tripRangeTypeDestin, 
                            tripPurpose = tripReasonString,
                            mode, tripDistance, typicalDurationNextActivity, t_source_PostCode, t_destination_PostCode)

nonBruPeople_lastTripHandling <- nonBruPeople_trip %>% 
  filter(is.na(typicalDurationNextActivity)) %>% 
  mutate(arrivalTimeInMInute = arrivalHour*60 + arrivalMinute) %>% 
  mutate(typicalDurationNextActivity = 1500 - arrivalTimeInMInute)

nonBruPeople_trip <- filter(nonBruPeople_trip, !is.na(typicalDurationNextActivity))

nonBruPeople_trip <- rbind(nonBruPeople_trip, nonBruPeople_lastTripHandling)

nonBruPeople_trip$personID <- as.numeric(nonBruPeople_trip$personID)

nonBruPeople_trip <- nonBruPeople_trip[order(nonBruPeople_trip[,'personID'], nonBruPeople_trip[,'sequenceTrip']),]

nonBruPeople_trip <- mutate(nonBruPeople_trip, typicalDurationNextActivity = typicalDurationNextActivity*60)





#Step 9: randomly assign a first activity location for each agent within the departure postcode
rm(nonBruPeopleTrip, firstActivityTrip)

#Determining adultOrKid information in the trip table
nonBruPeople_attribute$adultOrKid <- ifelse(nonBruPeople_attribute$AgeGroupMethodology == "1K" | nonBruPeople_attribute$AgeGroupMethodology == "2K",
                                            "kid",
                                            "adult")
nonBruPeople_attribute$personID <- as.numeric(nonBruPeople_attribute$personID)

nonBruPeople_trip <- left_join(nonBruPeople_trip, select(nonBruPeople_attribute, personID, adultOrKid), by = c("personID" = "personID"))

nonBruPeople_trip <- uncount(nonBruPeople_trip, 2)
nonBruPeople_trip <- nonBruPeople_trip %>% 
  mutate(originDestinType = rep(c(0, 1), length.out = n()))

nonBruPeople_trip <- nonBruPeople_trip %>% 
  mutate(geometry = NA) %>% 
  mutate(tripNB = "")

nonBRUStartLocation <- read.csv("nonBRUHomeLocation.csv")

nonBRUStartLocation <- nonBRUStartLocation %>% 
  select(-X)

nonBruPeople_trip_original <- nonBruPeople_trip

maxPersonID <- max(nonBruPeople_trip_original$personID)

split1 <- ceiling(maxPersonID/8)
split2 <- ceiling(maxPersonID*2/8)
split3 <- ceiling(maxPersonID*3/8)
split4 <- ceiling(maxPersonID*4/8)
split5 <- ceiling(maxPersonID*5/8)
split6 <- ceiling(maxPersonID*6/8)
split7 <- ceiling(maxPersonID*7/8)

nonBruPeople_trip_part1 <- nonBruPeople_trip_original %>% 
  filter(personID < split1)

nonBruPeople_trip_part2 <- nonBruPeople_trip_original %>% 
  filter(personID >= split1 & personID < split2)

nonBruPeople_trip_part3 <- nonBruPeople_trip_original %>% 
  filter(personID >= split2 & personID < split3)

nonBruPeople_trip_part4 <- nonBruPeople_trip_original %>% 
  filter(personID >= split3 & personID < split4)

nonBruPeople_trip_part5 <- nonBruPeople_trip_original %>% 
  filter(personID >= split4 & personID < split5)

nonBruPeople_trip_part6 <- nonBruPeople_trip_original %>% 
  filter(personID >= split5 & personID < split6)

nonBruPeople_trip_part7 <- nonBruPeople_trip_original %>% 
  filter(personID >= split6 & personID < split7)

nonBruPeople_trip_part8 <- nonBruPeople_trip_original %>% 
  filter(personID >= split7)

nonBRUStartLocationAssignment <- function(nonBruPeople_trip){

  nonBruPeople_trip_split <- nonBruPeople_trip %>% 
    group_by(personID) %>% 
    group_split()
  
  counter <- 0
  
  nonBruPeople_trip <- nonBruPeople_trip_split[[1]]
  
  for (i in 1:nrow(nonBruPeople_trip)) {
    tripSequence <- nonBruPeople_trip[i,]$sequenceTrip
    originDestinType <- nonBruPeople_trip[i,]$originDestinType
    
    if (tripSequence == 1 & originDestinType == 0) {
      sourcePostcode <- nonBruPeople_trip[i,]$t_source_PostCode
      locationsInPostcode <- filter(nonBRUStartLocation, postcode == sourcePostcode)

      randomLocation <- slice_sample(locationsInPostcode, n=1)
      nonBruPeople_trip[i,]$tripNB <- randomLocation$NAME_FRE
      selectedHome <- sf_point(randomLocation, x = "xHomeCoord", y = "yHomeCoord")
      selectedHome <- st_set_crs(selectedHome, 31370)
      nonBruPeople_trip[i,]$geometry <- selectedHome$geometry
    }
  }
  
  nonBruPeople_trip_collector <- nonBruPeople_trip
  
  
  for (a in 2:length(nonBruPeople_trip_split)) {
    
    nonBruPeople_trip <- nonBruPeople_trip_split[[a]]
    
    for (i in 1:nrow(nonBruPeople_trip)) {
      tripSequence <- nonBruPeople_trip[i,]$sequenceTrip
      originDestinType <- nonBruPeople_trip[i,]$originDestinType
      
      if (tripSequence == 1 & originDestinType == 0) {
        sourcePostcode <- nonBruPeople_trip[i,]$t_source_PostCode
        locationsInPostcode <- filter(nonBRUStartLocation, postcode == sourcePostcode)

        randomLocation <- slice_sample(locationsInPostcode, n=1)
        nonBruPeople_trip[i,]$tripNB <- randomLocation$NAME_FRE
        selectedHome <- sf_point(randomLocation, x = "xHomeCoord", y = "yHomeCoord")
        selectedHome <- st_set_crs(selectedHome, 31370)
        nonBruPeople_trip[i,]$geometry <- selectedHome$geometry
      }
      
      
    }
    nonBruPeople_trip_collector <- rbind(nonBruPeople_trip_collector, nonBruPeople_trip)
    
    if (a %% 100 == 0) {
      print(paste(a, "/", length(nonBruPeople_trip_split), " has determined the start locations"))
    }
  }
  
  nonBruPeople_trip <- nonBruPeople_trip_collector
  return(nonBruPeople_trip)
}

nonBruPeople_trip_part1 <- nonBRUStartLocationAssignment(nonBruPeople_trip_part1)
nonBruPeople_trip_part2 <- nonBRUStartLocationAssignment(nonBruPeople_trip_part2)
nonBruPeople_trip_part3 <- nonBRUStartLocationAssignment(nonBruPeople_trip_part3)
nonBruPeople_trip_part4 <- nonBRUStartLocationAssignment(nonBruPeople_trip_part4)
nonBruPeople_trip_part5 <- nonBRUStartLocationAssignment(nonBruPeople_trip_part5)
nonBruPeople_trip_part6 <- nonBRUStartLocationAssignment(nonBruPeople_trip_part6)
nonBruPeople_trip_part7 <- nonBRUStartLocationAssignment(nonBruPeople_trip_part7)
nonBruPeople_trip_part8 <- nonBRUStartLocationAssignment(nonBruPeople_trip_part8)

nonBruPeople_trip <- rbind(nonBruPeople_trip_part1,
                           nonBruPeople_trip_part2,
                           nonBruPeople_trip_part3,
                           nonBruPeople_trip_part4,
                           nonBruPeople_trip_part5,
                           nonBruPeople_trip_part6,
                           nonBruPeople_trip_part7,
                           nonBruPeople_trip_part8)

nonBruPeople_trip <- group_by(nonBruPeople_trip, personID)

rm(nonBruPeople_trip_part1,
   nonBruPeople_trip_part2,
   nonBruPeople_trip_part3,
   nonBruPeople_trip_part4,
   nonBruPeople_trip_part5,
   nonBruPeople_trip_part6,
   nonBruPeople_trip_part7,
   nonBruPeople_trip_part8)



nonBruPeople_trip <- nonBruPeople_trip %>% 
  select(-t_source_PostCode) %>% 
  rename(desPostcode = t_destination_PostCode)


nonBruPeople_trip <- left_join(nonBruPeople_trip, select(nonBruPeople_attribute, personID, p_id))


nonBruPeople_trip_original <- nonBruPeople_trip

maxPersonID <- max(nonBruPeople_trip_original$personID)

split1 <- ceiling(maxPersonID/8)
split2 <- ceiling(maxPersonID*2/8)
split3 <- ceiling(maxPersonID*3/8)
split4 <- ceiling(maxPersonID*4/8)
split5 <- ceiling(maxPersonID*5/8)
split6 <- ceiling(maxPersonID*6/8)
split7 <- ceiling(maxPersonID*7/8)

nonBruPeople_trip_part1 <- nonBruPeople_trip_original %>% 
  filter(personID < split1)

nonBruPeople_trip_part2 <- nonBruPeople_trip_original %>% 
  filter(personID >= split1 & personID < split2)

nonBruPeople_trip_part3 <- nonBruPeople_trip_original %>% 
  filter(personID >= split2 & personID < split3)

nonBruPeople_trip_part4 <- nonBruPeople_trip_original %>% 
  filter(personID >= split3 & personID < split4)

nonBruPeople_trip_part5 <- nonBruPeople_trip_original %>% 
  filter(personID >= split4 & personID < split5)

nonBruPeople_trip_part6 <- nonBruPeople_trip_original %>% 
  filter(personID >= split5 & personID < split6)

nonBruPeople_trip_part7 <- nonBruPeople_trip_original %>% 
  filter(personID >= split6 & personID < split7)

nonBruPeople_trip_part8 <- nonBruPeople_trip_original %>% 
  filter(personID >= split7)

nonBRULocationAssignment <- function(nonBruPeople_trip){
  nonBruPeople_trip_split <- nonBruPeople_trip %>% 
    group_by(personID) %>% 
    group_split()

  counter <- 0
  
  nonBruPeople_trip_collector <- nonBruPeople_trip_split[[1]][0,]
  
  for (a in 1:length(nonBruPeople_trip_split)) {
    nonBruPeople_trip <- nonBruPeople_trip_split[[a]]
    personID <- nonBruPeople_trip[1,]$personID
    individualActivityChain <- nonBruPeople_trip
    
    for (trip in 1:nrow(individualActivityChain)) {
      individualActivityChain <- nonBruPeople_trip
      tripSelected <- individualActivityChain[trip,]
      if (tripSelected$originDestinType == 1) {
        adultOrKid <- tripSelected$adultOrKid
        tripPurpose <- tripSelected$tripPurpose
        tripStraightLineDistance <- (tripSelected$tripDistance)/1.3
        tripRangeTypeOrigin <- tripSelected$tripRangeTypeOrigin
        tripRangeTypeDestin <- tripSelected$tripRangeTypeDestin
        tripDepartHour <- tripSelected$departureHour
        tripSequence <- tripSelected$sequenceTrip

        MONITORReference <- tripSelected$p_id
        
        
        if (tripPurpose == "home" & !MONITORReference %in% differentHomeMONITORPeople$t_participant_id) {
          
          homeLocation <- nonBruPeople_trip[1,]$geometry
          originalNB <- nonBruPeople_trip[1,]$tripNB
          
          nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- homeLocation
          nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- originalNB
          
          print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a return home activity (according to what the reference reported at MONITOR)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
          
        } else if (tripPurpose == "work" & MONITORReference %in% workSequence1ID$t_participant_id & tripSequence>1) {
          
          fixedWorkLocation <- nonBruPeople_trip[2,]$geometry
          originalNB <- nonBruPeople_trip[2,]$tripNB
          
          nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- fixedWorkLocation
          nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- originalNB
          
          print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a return fixed workplace activity (as MONITOR sample reported fixed work location with multiple work activities)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
        } else if (tripPurpose == "work" & MONITORReference %in% workSequence3ID$t_participant_id & tripSequence>3) {
          
          fixedWorkLocation <- nonBruPeople_trip[6,]$geometry
          originalNB <- nonBruPeople_trip[6,]$tripNB
          
          nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- fixedWorkLocation
          nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- originalNB
          
          print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a return fixed workplace activity (as MONITOR sample reported fixed work location with multiple work activities)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
        } else if (tripPurpose == "school" & MONITORReference %in% eduSequence1ID$t_participant_id & tripSequence>1) {
          
          fixedEDULocation <- nonBruPeople_trip[2,]$geometry
          originalNB <- nonBruPeople_trip[2,]$tripNB
          
          nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- fixedEDULocation
          nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- originalNB
          
          print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a return fixed EDU activity (as MONITOR sample reported fixed EDU location with multiple EDU activities)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
        } else if (tripPurpose == "school" & MONITORReference %in% eduSequence2ID$t_participant_id & tripSequence>2) {
          
          fixedEDULocation <- nonBruPeople_trip[4,]$geometry
          originalNB <- nonBruPeople_trip[4,]$tripNB
          
          nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- fixedEDULocation
          nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- originalNB
          
          print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a return fixed EDU activity (as MONITOR sample reported fixed EDU location with multiple EDU activities)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
        } else {
         
          if (tripRangeTypeOrigin == 0 & tripRangeTypeDestin == 0) {
            tripOrigin <- individualActivityChain[trip-1,]$geometry
            tripOriginNB <- individualActivityChain[trip-1,]$tripNB
            furtherestNBDistance <- distancesBetweenNBs %>% 
              filter(NBName == tripOriginNB)
            furtherestNBDistance <- max(furtherestNBDistance[, c(2:146)])/1000
            
            if (tripStraightLineDistance <= furtherestNBDistance) {
              buffer <- st_buffer(tripOrigin, tripStraightLineDistance * 1000)
              buffer <- buffer %>% st_cast("LINESTRING")
              BrusselsNBShape$intersectsWithRing <- ifelse(st_intersects(BrusselsNBShape, buffer, sparse = FALSE), "Yes", "No")
              NBSelected <- BrusselsNBShape %>% filter(intersectsWithRing == "Yes")
              
              if (tripPurpose == "work") {
                tripFlowFromOriginNB <- proximus_NBfrequentTrip_forGeneral %>% 
                  filter(originNB == tripOriginNB) %>% 
                  filter(destinationNB %in% NBSelected$NAME_FRE) %>% 
                  select(-irregularTrip)
                NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$regularTrip)
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                
                POISelected <- BrusselsPOI_work %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n = 1, weight_by = attractiveness)
                
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a work destination! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
                
              } else if (tripPurpose == "school" & adultOrKid == "adult") {
                if (!all(!NBSelected$NAME_FRE %in% proximus_NBfrequentTrip_forAdultEDU$destinationNB)) {
                  tripFlowFromOriginNB <- proximus_NBfrequentTrip_forAdultEDU %>% 
                    filter(originNB == tripOriginNB) %>% 
                    filter(destinationNB %in% NBSelected$NAME_FRE) %>% 
                    select(-irregularTrip)
                  
                  NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$regularTrip)
                  nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                  
                  POISelected <- BrusselsPOI_adultSchool %>% 
                    filter(NBFre == NBSelected$destinationNB) %>% 
                    slice_sample(n = 1, weight_by = attractiveness)
                  
                  nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                  
                  print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned an adult EDU destination! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
                } else {
                  closestNB <- distancesBetweenNBs %>% 
                    filter(NBName == tripOriginNB)
                  closestNB <- closestNB[, which(names(closestNB) %in% proximus_NBfrequentTrip_forAdultEDU$destinationNB)]
                  closestNB <- abs(drop_units(closestNB) - tripStraightLineDistance * 1000)
                  closestNB <- colnames(closestNB)[which.min(closestNB)]
                  
                  tripFlowFromOriginNB <- proximus_NBfrequentTrip_forAdultEDU %>% 
                    filter(originNB == tripOriginNB) %>%
                    filter(destinationNB == closestNB) %>% 
                    select(-irregularTrip)
                  
                  NBSelected <- tripFlowFromOriginNB
                  nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                  
                  POISelected <- BrusselsPOI_adultSchool %>% 
                    filter(NBFre == NBSelected$destinationNB) %>% 
                    slice_sample(n = 1, weight_by = attractiveness)
                  
                  nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                  print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned an adult EDU destination by closest random selection! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
                }
                
              } else if (tripPurpose == "school" & adultOrKid == "kid") {
                if (!all(!NBSelected$NAME_FRE %in% BISA_kidEduNBFlow$destinationNB)) {
                  tripFlowFromOriginNB <- BISA_kidEduNBFlow %>% 
                    filter(originNB == tripOriginNB) %>% 
                    filter(destinationNB %in% NBSelected$NAME_FRE)
                  
                  NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$regularTrip)
                  nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                  
                  POISelected <- BrusselsPOI_kidSchool %>% 
                    filter(NBFre == NBSelected$destinationNB) %>% 
                    slice_sample(n = 1, weight_by = attractiveness)
                  
                  nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                  
                  print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a kid EDU destination! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
                } else {
                  closestNB <- distancesBetweenNBs %>% 
                    filter(NBName == tripOriginNB)
                  closestNB <- closestNB[, which(names(closestNB) %in% BISA_kidEduNBFlow$destinationNB)]
                  closestNB <- abs(drop_units(closestNB) - tripStraightLineDistance * 1000)
                  closestNB <- colnames(closestNB)[which.min(closestNB)]
                  
                  tripFlowFromOriginNB <- BISA_kidEduNBFlow %>% 
                    filter(originNB == tripOriginNB) %>%
                    filter(destinationNB == closestNB)
                  
                  NBSelected <- tripFlowFromOriginNB
                  nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                  
                  POISelected <- BrusselsPOI_kidSchool %>% 
                    filter(NBFre == NBSelected$destinationNB) %>% 
                    slice_sample(n = 1, weight_by = attractiveness)
                  
                  nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                  
                  print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a kid EDU destination by closest random selection! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
                }
              } else if (tripPurpose == "home") {
                tripFlowFromOriginNB <- proximus_NBfrequentTrip_forGeneral %>% 
                  filter(originNB == tripOriginNB) %>% 
                  filter(destinationNB %in% NBSelected$NAME_FRE) %>% 
                  select(-irregularTrip)
                
                NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$regularTrip)
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_home %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n=1)
                
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a return home destination! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
              } else if (tripPurpose == "shopping") {
                tripFlowFromOriginNB <- proximus_NBhourlyTrip %>% 
                  filter(originNB == tripOriginNB) %>% 
                  filter(destinationNB %in% NBSelected$NAME_FRE) %>% 
                  filter(arrival_hr == tripDepartHour)
                
                NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$irregularTrips)
                
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_shopping %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n=1)
                
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a shopping destination! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
                
              } else if (tripPurpose == "other") {
                tripFlowFromOriginNB <- proximus_NBhourlyTrip %>% 
                  filter(originNB == tripOriginNB) %>% 
                  filter(destinationNB %in% NBSelected$NAME_FRE) %>% 
                  filter(arrival_hr == tripDepartHour)
                
                NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$irregularTrips)
                
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_other %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n=1)
                
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned an other secondary destination! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
                
              } else {
                tripFlowFromOriginNB <- proximus_NBhourlyTrip %>% 
                  filter(originNB == tripOriginNB) %>% 
                  filter(destinationNB %in% NBSelected$NAME_FRE) %>% 
                  filter(arrival_hr == tripDepartHour)
                
                NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$irregularTrips)
                
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_other %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n=1)
                
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a leisure destination! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
              }
              
            } else{
              if (tripPurpose == "work") {
                NBSelected <- slice_sample(proximus_NBfrequentTrip_forGeneral_Popularity, n = 1, weight_by = proximus_NBfrequentTrip_forGeneral_Popularity$totalArrivalTrip)
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_work %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n=1, weight_by = attractiveness)
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a working destination (exceed furtherest distance)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
              } else if (tripPurpose == "school" & adultOrKid == "adult") {
                NBSelected <- slice_sample(proximus_NBfrequentTrip_forAdultEDU_popularity, n = 1, weight_by = proximus_NBfrequentTrip_forAdultEDU_popularity$totalArrivalTrip)
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_adultSchool %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n = 1, weight_by = attractiveness)
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned an adult EDU destination (exceed furtherest distance)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
              } else if (tripPurpose == "school" & adultOrKid == "kid") {
                NBSelected <- slice_sample(BISA_kidEduNBFlow_Popularity, n = 1, weight_by = BISA_kidEduNBFlow_Popularity$totalArrivalTrip)
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_kidSchool %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n = 1, weight_by = attractiveness)
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a kid EDU destination (exceed furtherest distance)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
              } else if (tripPurpose == "home") {
                NBSelected <- slice_sample(proximus_NBfrequentTrip_forGeneral_Popularity, n = 1, weight_by = proximus_NBfrequentTrip_forGeneral_Popularity$totalArrivalTrip)
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                POISelected <- BrusselsPOI_home %>% 
                  filter(NBFre == NBSelected$destinationNB) %>% 
                  slice_sample(n=1)
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                
                print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a home destination (exceed furtherest distance)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
              } else {
                tripFlowFromOriginNB <- proximus_NBhourlyTrip_popularity %>% 
                  filter(arrival_hr == tripDepartHour)
                tripFlowFromOriginNB <- as.data.frame(tripFlowFromOriginNB)
                NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$totalArrivalTrip)
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
                
                if (tripPurpose == "shopping") {
                  POISelected <- slice_sample(filter(BrusselsPOI_shopping, NBFre == NBSelected$destinationNB), n=1)
                } else if (tripPurpose == "leisure") {
                  POISelected <- slice_sample(filter(BrusselsPOI_leisure, NBFre == NBSelected$destinationNB), n=1)
                } else if (tripPurpose == "other") {
                  POISelected <- slice_sample(filter(BrusselsPOI_other, NBFre == NBSelected$destinationNB), n=1)
                }
                
                nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
                print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a secondary destination (exceed furtherest distance)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
              }
            }
          } else if (tripRangeTypeOrigin == 0 & (tripRangeTypeDestin == 1 | tripRangeTypeDestin == 2)) {
            NBSelected <- "reported outside Brussels trip"
            nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected
            tripDestinPostcode <- tripSelected$desPostcode
            if (!is.na(tripDestinPostcode)) {
              postcodePoint <- BelgiumPostcode %>% filter(postcode == tripDestinPostcode)
              buffer <- st_buffer(postcodePoint, 5 * 1000)
              pointSelected <- st_sample(buffer, 1)
              pointSelected <- st_as_sf(pointSelected)
              pointSelected <- st_transform(pointSelected, 31370)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- pointSelected$x
              print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a outside Brussels destination (Brussels to Outside with detailed destination postcode)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
            } else {
              tripDestinPostcode <- slice_sample(BelgiumPostcode, n = 1)$postcode
              
              postcodePoint <- BelgiumPostcode %>% filter(postcode == tripDestinPostcode)
              buffer <- st_buffer(postcodePoint, 5 * 1000)
              pointSelected <- st_sample(buffer, 1)
              pointSelected <- st_as_sf(pointSelected)
              pointSelected <- st_transform(pointSelected, 31370)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- pointSelected$x
              print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a outside Brussels destination (Brussels to Outside without detailed destination postcode)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
            }
          } else if ((tripRangeTypeOrigin == 1 | tripRangeTypeOrigin == 2) & tripRangeTypeDestin == 0){
            if (tripPurpose == "work") {
              #For Work Trip
              NBSelected <- slice_sample(proximus_NBfrequentTrip_forGeneral_Popularity, n = 1, weight_by = proximus_NBfrequentTrip_forGeneral_Popularity$totalArrivalTrip)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
              POISelected <- BrusselsPOI_work %>% 
                filter(NBFre == NBSelected$destinationNB) %>% 
                slice_sample(n=1, weight_by = attractiveness)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
              
              print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a working destination (outside Brussels to Brussels)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
            } else if (tripPurpose == "school" & adultOrKid == "adult") {
              #For adult EDU trip
              NBSelected <- slice_sample(proximus_NBfrequentTrip_forAdultEDU_popularity, n = 1, weight_by = proximus_NBfrequentTrip_forAdultEDU_popularity$totalArrivalTrip)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
              POISelected <- BrusselsPOI_adultSchool %>% 
                filter(NBFre == NBSelected$destinationNB) %>% 
                slice_sample(n = 1, weight_by = attractiveness)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
              
              print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned an adult EDU destination (outside Brussels to Brussels)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
            } else if (tripPurpose == "school" & adultOrKid == "kid") {
              #For kid EDU trip
              NBSelected <- slice_sample(BISA_kidEduNBFlow_Popularity, n = 1, weight_by = BISA_kidEduNBFlow_Popularity$totalArrivalTrip)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
              POISelected <- BrusselsPOI_kidSchool %>% 
                filter(NBFre == NBSelected$destinationNB) %>% 
                slice_sample(n = 1, weight_by = attractiveness)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
              
              print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a kid EDU destination (outside Brussels to Brussels)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
            } else if (tripPurpose == "home") {
              #For return home trip
              NBSelected <- slice_sample(proximus_NBfrequentTrip_forGeneral_Popularity, n = 1, weight_by = proximus_NBfrequentTrip_forGeneral_Popularity$totalArrivalTrip)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
              POISelected <- BrusselsPOI_home %>% 
                filter(NBFre == NBSelected$destinationNB) %>% 
                slice_sample(n=1)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
              
              print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a home destination (outside Brussels to Brussels)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
            } else {
              #For other secondary trips
              tripFlowFromOriginNB <- proximus_NBhourlyTrip_popularity %>% 
                filter(arrival_hr == tripDepartHour)
              tripFlowFromOriginNB <- as.data.frame(tripFlowFromOriginNB)
              NBSelected <- slice_sample(tripFlowFromOriginNB, n = 1, weight_by = tripFlowFromOriginNB$totalArrivalTrip)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected$destinationNB
              
              if (tripPurpose == "shopping") {
                POISelected <- slice_sample(filter(BrusselsPOI_shopping, NBFre == NBSelected$destinationNB), n=1)
              } else if (tripPurpose == "leisure") {
                POISelected <- slice_sample(filter(BrusselsPOI_leisure, NBFre == NBSelected$destinationNB), n=1)
              } else if (tripPurpose == "other") {
                POISelected <- slice_sample(filter(BrusselsPOI_other, NBFre == NBSelected$destinationNB), n=1)
              }
              
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- POISelected$geometry
              print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a secondary destination (outside Brussels to Brussels)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
            }
          } else if ((tripRangeTypeOrigin == 1 | tripRangeTypeOrigin == 2) & (tripRangeTypeDestin == 1 | tripRangeTypeDestin == 2)) {
            NBSelected <- "reported outside Brussels trip"
            nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$tripNB <- NBSelected
            tripDestinPostcode <- tripSelected$desPostcode
            if (!is.na(tripDestinPostcode)) {
              postcodePoint <- BelgiumPostcode %>% filter(postcode == tripDestinPostcode)
              buffer <- st_buffer(postcodePoint, 5 * 1000)
              pointSelected <- st_sample(buffer, 1)
              pointSelected <- st_as_sf(pointSelected)
              pointSelected <- st_transform(pointSelected, 31370)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- pointSelected$x
              print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a outside Brussels destination (outside Brussels to outside Brussels with detailed destination postcode)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
            } else {
              tripDestinPostcode <- slice_sample(BelgiumPostcode, n = 1)$postcode
              
              postcodePoint <- BelgiumPostcode %>% filter(postcode == tripDestinPostcode)
              buffer <- st_buffer(postcodePoint, 5 * 1000)
              pointSelected <- st_sample(buffer, 1)
              pointSelected <- st_as_sf(pointSelected)
              pointSelected <- st_transform(pointSelected, 31370)
              nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 1,]$geometry <- pointSelected$x
              print(paste("Step 9 person ID ", personID, " Sequence Trip Number ", individualActivityChain[trip,]$sequenceTrip, ". have assigned a outside Brussels destination (outside Brussels to Outside without detailed destination postcode)! Max ID is", nonBruPeople_trip_split[[length(nonBruPeople_trip_split)]][1,]$personID))
            }
          }
        }
      } else if (tripSelected$originDestinType == 0) {
        tripSequence <- tripSelected$sequenceTrip
        if (tripSequence != 1) {
          nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 0,]$geometry <- nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip - 1 & nonBruPeople_trip$originDestinType == 1,]$geometry
          nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip & nonBruPeople_trip$originDestinType == 0,]$tripNB <- nonBruPeople_trip[nonBruPeople_trip$personID == personID & nonBruPeople_trip$sequenceTrip == individualActivityChain[trip,]$sequenceTrip - 1 & nonBruPeople_trip$originDestinType == 1,]$tripNB
        }
      }
    }
    nonBruPeople_trip_collector <- rbind(nonBruPeople_trip_collector, nonBruPeople_trip)
  }
  
  nonBruPeople_trip <- nonBruPeople_trip_collector
  
  return(nonBruPeople_trip)
}

nonBruPeople_trip_part1 <- nonBRULocationAssignment(nonBruPeople_trip_part1)
nonBruPeople_trip_part2 <- nonBRULocationAssignment(nonBruPeople_trip_part2)
nonBruPeople_trip_part3 <- nonBRULocationAssignment(nonBruPeople_trip_part3)
nonBruPeople_trip_part4 <- nonBRULocationAssignment(nonBruPeople_trip_part4)
nonBruPeople_trip_part5 <- nonBRULocationAssignment(nonBruPeople_trip_part5)
nonBruPeople_trip_part6 <- nonBRULocationAssignment(nonBruPeople_trip_part6)
nonBruPeople_trip_part7 <- nonBRULocationAssignment(nonBruPeople_trip_part7)
nonBruPeople_trip_part8 <- nonBRULocationAssignment(nonBruPeople_trip_part8)

nonBruPeople_trip <- rbind(nonBruPeople_trip_part1,
                           nonBruPeople_trip_part2,
                           nonBruPeople_trip_part3,
                           nonBruPeople_trip_part4,
                           nonBruPeople_trip_part5,
                           nonBruPeople_trip_part6,
                           nonBruPeople_trip_part7,
                           nonBruPeople_trip_part8)

rm(nonBruPeople_trip_part1,
   nonBruPeople_trip_part2,
   nonBruPeople_trip_part3,
   nonBruPeople_trip_part4,
   nonBruPeople_trip_part5,
   nonBruPeople_trip_part6,
   nonBruPeople_trip_part7,
   nonBruPeople_trip_part8)

#Step 10 Processing the outcomes of for loop
#Step 10.1 for attribute table
nonBruPeople_attribute <- nonBruPeople_attribute %>% 
  select(-t_source_PostCode, -homePostcode, -adultOrKid) %>% 
  mutate(householdID = "nonBruResidents") %>% 
  rename(ageGroup = AgeGroupMethodology) %>% 
  mutate(neighbourhood = "outsideBRU") %>% 
  mutate(carAvailability = 99) %>% 
  rename(gender = sex) %>% 
  select(personID, householdID, neighbourhood, ageGroup, gender, highestDegree, incomeLevel, carAvailability, workingStatus, p_id)

#Step 10.2 for activities table
nonBruPeople_trip <- as.data.frame(nonBruPeople_trip)

nonBruPeople_trip <- nonBruPeople_trip %>% 
  group_by(personID) %>% 
  filter(sequenceTrip == 1 & originDestinType == 0 | originDestinType == 1)

nonbrupeople_trip_coordinates <- as.data.frame(st_coordinates(nonBruPeople_trip$geometry))

nonBruPeople_trip <- nonBruPeople_trip %>% 
  select(-geometry) %>% 
  cbind(nonbrupeople_trip_coordinates) %>% 
  rename(xActivityCoord = X) %>% 
  rename(yActivityCoord = Y)

rm(nonbrupeople_trip_coordinates)

nonBruPeople_leg <- nonBruPeople_trip
nonBruPeople_activities <- nonBruPeople_trip
rm(nonBruPeople_trip)

nonBruPeople_activities <- nonBruPeople_activities %>% 
  mutate(firstOrigin = ifelse(originDestinType == 0, 1, 0))

nonBruPeople_activities[nonBruPeople_activities$sequenceTrip == 1 & nonBruPeople_activities$firstOrigin == 1,]$sequenceTrip <- 0

nonBruPeople_activities <- rename(nonBruPeople_activities, activityStartHour = departureHour)
nonBruPeople_activities <- rename(nonBruPeople_activities, activityStartMinute = departureMinute)
nonBruPeople_activities <- rename(nonBruPeople_activities, activityEndHour = arrivalHour)
nonBruPeople_activities <- rename(nonBruPeople_activities, activityEndMinute = arrivalMinute)

nonBruPeople_activities <- nonBruPeople_activities %>% 
  group_by(personID) %>% 
  mutate(activityStartHourNextRow = lead(activityStartHour)) %>% 
  mutate(activityStartMinuteNextRow = lead(activityStartMinute))

nonBruPeople_activities$activityStartHour <- nonBruPeople_activities$activityEndHour
nonBruPeople_activities$activityStartMinute <- nonBruPeople_activities$activityEndMinute

nonBruPeople_activities$activityEndHour <- nonBruPeople_activities$activityStartHourNextRow
nonBruPeople_activities$activityEndMinute <- nonBruPeople_activities$activityStartMinuteNextRow

nonBruPeople_activities[nonBruPeople_activities$firstOrigin == 1,]$tripPurpose <- "home"
nonBruPeople_activities[nonBruPeople_activities$firstOrigin == 1,]$activityStartHour <- 0
nonBruPeople_activities[nonBruPeople_activities$firstOrigin == 1,]$activityStartMinute <- 0

nonBruPeople_activities <- as.data.frame(nonBruPeople_activities)
nonBruPeople_activities[is.na(nonBruPeople_activities$activityEndHour),]$activityEndHour <- 25
nonBruPeople_activities[is.na(nonBruPeople_activities$activityEndMinute),]$activityEndMinute <- 0

nonBruPeople_activities <- nonBruPeople_activities %>% 
  select(-activityStartHourNextRow, -activityStartMinuteNextRow) %>% 
  select(-tripRangeTypeOrigin, -tripRangeTypeDestin, -mode, -tripDistance, 
         -arrivalTimeInMInute, -originDestinType) %>% 
  select(-tripNB, -firstOrigin)

nonBruPeople_activities <- rename(nonBruPeople_activities, typicalDuration = typicalDurationNextActivity)
nonBruPeople_activities[nonBruPeople_activities$sequenceTrip == 0, ]$typicalDuration <- nonBruPeople_activities[nonBruPeople_activities$sequenceTrip == 0, ]$activityEndHour * 3600 + nonBruPeople_activities[nonBruPeople_activities$sequenceTrip == 0, ]$activityEndMinute * 60

nonBruPeople_activities <- rename(nonBruPeople_activities, activityPurpose = tripPurpose)

nonBruPeople_activities$typicalDuration <- ifelse(nonBruPeople_activities$typicalDuration < 601,
                                                  600,
                                                  round(nonBruPeople_activities$typicalDuration/600) * 600)

nonBruPeople_activities <- select(nonBruPeople_activities, -adultOrKid, -desPostcode)

#Step 10.3 Processing legs
nonBruPeople_leg <- nonBruPeople_leg %>% 
  filter(originDestinType != 0)

#Calculating the travel time of legs
nonBruPeople_leg <- nonBruPeople_leg %>% 
  mutate(travelTime = arrivalHour * 60 + arrivalMinute - departureHour * 60 - departureMinute)

nonBruPeople_leg <- nonBruPeople_leg %>% 
  mutate(travelHour = floor(travelTime/60)) %>% 
  mutate(travelMinute = travelTime %% 60)

nonBruPeople_leg <- nonBruPeople_leg %>% 
  select(personID, sequenceTrip, departureHour, departureMinute, mode, travelHour, travelMinute)

#Step 11 export the plan file according to MATSim format
#Bind the two DF

rm(attractivenessSelected, BESSInPoint, BISA_kidEduNBFlow, BISA_kidEduNBFlow_Popularity,
   BISAKidEduData, BISAKidSchoolAttractivness, BISASchoolSelected, BISASchoolsInSelectedNB,
   BrusselsNBShape, BrusselsPOI_adultSchool, BrusselsPOI_home, BrusselsPOI_kidSchool,
   BrusselsPOI_leisure, BrusselsPOI_other, BrusselsPOI_shopping, BrusselsPOI_work,
   BruSSInPoint, BruSSJoinedWithNB, buffer, communeAttractiveness, destinationsForKidEDU,
   distancesBetweenNBs, enterpriseCategory, EnterpriseCrossroad, individualActivityChain,
   missingFlow, nbNotAvailableINBISA, NBSelected, originMissing, POISelected, 
   proximus_NBfrequentTrip, proximus_NBfrequentTrip_forAdultEDU, proximus_NBfrequentTrip_forAdultEDU_popularity,
   proximus_NBfrequentTrip_forGeneral, proximus_NBfrequentTrip_forGeneral_Popularity,
   proximus_NBhourlyTrip, proximus_NBhourlyTrip_popularity, schoolSelected,
   tenFurthestNBForEachNB, tripFlowFromOriginNB, tripOrigin, tripSelected, adultOrKid,
   allNBBru, col_names, communeSelected, furtherestNBDistance, i, individuals_attributes,
   missingNB, moves_attributes, neighbourhood_i, origin, personID, trip, tripDepartHour,
   tripOriginNB, tripPurpose, tripRangeTypeDestin, tripRangeTypeOrigin, trips_attributes,
   tripSequence, tripStraightLineDistance, BrusselsSSDivision, BelgiumPostcode, pointSelected,
   postcodePoint)

rm(adult_intake, adult_trip_move, currentTrip, formerTrip, kid_intake, kid_trip_move,
   nonBruPeople_lastTripHandling, pointLocation, postal_codes, postcodeArea, startLocation)


nonBruPeople_attribute <- nonBruPeople_attribute %>% 
  mutate(personID = paste(personID, "_nonBRUResident", sep = ""))
nonBruPeople_activities <- nonBruPeople_activities %>% 
  mutate(personID = paste(personID, "_nonBRUResident", sep = ""))
nonBruPeople_leg <- nonBruPeople_leg %>% 
  mutate(personID = paste(personID, "_nonBRUResident", sep = ""))

demobel_matched_attribute <- demobel_matched_attribute %>% 
  mutate(personID = paste(personID, "_BRUResident", sep = ""))
demobel_matched_activities <- demobel_matched_activities %>% 
  mutate(personID = paste(personID, "_BRUResident", sep = ""))
demobel_matched_legs <- demobel_matched_legs %>% 
  mutate(personID = paste(personID, "_BRUResident", sep = ""))

runningDuration <- Sys.time() - oldTime
print(runningDuration)

all_activities <- rbind(demobel_matched_activities, nonBruPeople_activities)
nonBruPeople_attribute$workingStatus <- as.character(nonBruPeople_attribute$workingStatus)
all_attribute <- rbind(demobel_matched_attribute, nonBruPeople_attribute)
all_legs <- rbind(demobel_matched_legs, nonBruPeople_leg)

write.table(all_attribute, file = "reallyFinalAttribute.csv", 
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)

write.table(all_activities, file = "reallyFinalActivity.csv",
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)

write.table(all_legs, file = "reallyFinalLeg.csv",
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)

write.table(agentWithNoTrip, file = "reallyFinalNoTripAgents.csv",
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)



paste("Finish generating Brussels population! Next need to move to MATSim preparePopulation.java")

