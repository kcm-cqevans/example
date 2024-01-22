rm(list=ls(all=TRUE))
library(here)
library(dplyr)
library(stringr)
library(tidyr)
library(hablar)
library(tidyverse)
library(writexl)
library(openxlsx)
library(XML)


load("Data_Merged_2022-2023.RData")
#data <- data %>% dplyr::select(-contains("_bv"))

dplyr::mutate_if(data, is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))})

demographics <- data %>% 
  dplyr::select(uniqueid, wave, wgt, region, userlanguage, zipcode, CensusBlockGroup, census_tract, 
                cbg_group, ridestat, ridestat_metro, ridestat_kcmbus, agecat, gender3cat, income_5cat, 
                race_eth_exl, disability, rentown, language,  driverslic, vehicle_hh, hhsiz_total, 
                hhsiz_under18, kids_hh)

transituse <- data %>% 
  dplyr::select(uniqueid, wave,  wgt, transit_freq, stop_access, stop_dist, rapidrideuser,
                starts_with("transit_mode"), starts_with("purpose_"), starts_with("accessbarrier_"))  

primtrip <- data %>% 
  dplyr::select(uniqueid, wave, wgt, primary_trip, prim_trip_purp_new, primtrip_traveltime, 
                starts_with("prim_transit"),starts_with("todtravel"), prim_trip_freq)

wishtrip <- data %>% 
  dplyr::select(uniqueid, wave,  wgt, starts_with("wish"))

commuter <- data %>% 
  dplyr::select(uniqueid, wave,  wgt, commute, starts_with("comm_"), commute_area)

behave<-data %>% 
  dplyr::select(uniqueid, wave,  wgt, starts_with("bhv_"),  pcvd_behnorm )

brand<- data %>% 
  dplyr::select(uniqueid, wave,  wgt,  nps_overall, sat_overall  ,  starts_with("sat_"),  interest)

other_travel<-data %>% 
  dplyr::select(uniqueid, wave,  wgt,  starts_with("mode_") , nonr_tr_freq)

fare<-data %>% 
  dplyr::select(uniqueid, wave,  wgt,  starts_with("fare"), starts_with("orca"), no_orcareason,
                starts_with("load_store"), starts_with("load_type"))

safety<-data %>% 
  dplyr::select(uniqueid, wave,  wgt,   sat_safebusday, sat_safewaitday, sat_safebusnight, sat_safewaitnight, trip_dark)


open_ended<-data %>% 
  dplyr::select(uniqueid, wave,  wgt,   int_oe, int_oeCoded, barrier_verb, barrierCoded)

satisfaction<-data %>% 
  dplyr::select(uniqueid, wave,  wgt, starts_with("sat_"))

tripplan<-data %>% 
  dplyr::select(uniqueid, wave,  wgt, starts_with("plantrip"), starts_with("info_"), plantrip_primary, starts_with("plan_"), 
                starts_with("sat_plan"), nps_plantrip_tool)

infonews<-data %>% 
  dplyr::select(uniqueid, wave,  wgt, uniqueid,	wave,	wgt,	bhv_pos_news,	bhv_positive,	bhv_tom,	
                bhv_tomCoded,	plan_know,	plan_easy,	plan_wayfind,	plan_tripchange,	hear_source_localtv,
                hear_source_localonline,	hear_source_localradio,	hear_source_natlnews,	hear_source_socialmedia,
                hear_source_other,	hear_source_other_txt,	hear_source_dk,	hear_source_donthear)

sexharass<-data %>% 
  dplyr::select(uniqueid, wave,  wgt, sexharass)

routesused<-data %>% 
  dplyr::select(uniqueid, wave,  wgt, route1,	route2,	route3,	route4,	route5,	route6,	route7,	route8)

rapidride<-data %>% 
  dplyr::select(uniqueid, wave,  wgt, rrexpect_traveltime,	rrexpect_comfort,	rrexpect_encourage,
                rrexpect_reliability,	rrexper_overall,	rrexper_safety,	rrexper_stops,	rrexper_freq,
                rrexper_traveltime,	rrexper_reliability,	nps_rapidride,	rapidride_use,	rapidride_sat)
needsmet<-data %>% 
  dplyr::select(uniqueid, wave,  wgt, transit_needsmet)


wb<-createWorkbook()
addWorksheet(wb, "Demographics")
addWorksheet(wb, "Transit Use")
addWorksheet(wb, "Primary Trip")
addWorksheet(wb, "Wish Trip")
addWorksheet(wb, "Commuter")
addWorksheet(wb, "Behaviors")
addWorksheet(wb, "Brand and Satisfaction")
addWorksheet(wb, "Other travel")
addWorksheet(wb, "Fares")
addWorksheet(wb, "Safety")
addWorksheet(wb, "Open-ended")
addWorksheet(wb, "Satisfaction")
addWorksheet(wb, "Plan and Info")
addWorksheet(wb, "Information and news")
addWorksheet(wb, "Sexual harassment")
addWorksheet(wb, "Routes used")
addWorksheet(wb, "RapidRide")
addWorksheet(wb, "Needs Met")



writeData(wb, sheet = "Demographics", x = demographics)
writeData(wb, sheet = "Transit Use", x = transituse)
writeData(wb, sheet = "Primary Trip", x = primtrip)
writeData(wb, sheet = "Wish Trip", x = wishtrip)
writeData(wb, sheet = "Commuter", x = commuter)
writeData(wb, sheet = "Behaviors", x = behave)
writeData(wb, sheet = "Brand and Satisfaction", x = brand)
writeData(wb, sheet = "Other travel", x = other_travel)
writeData(wb, sheet = "Fares", x = fare)
writeData(wb, sheet = "Safety", x = safety)
writeData(wb, sheet = "Open-ended", x = open_ended)
writeData(wb, sheet = "Satisfaction", x = satisfaction)
writeData(wb, sheet = "Plan and Info", x = tripplan)
writeData(wb, sheet = "Information and news", x = infonews)
writeData(wb, sheet = "Sexual harassment", x = sexharass)
writeData(wb, sheet = "Routes used", x = routesused)
writeData(wb, sheet = "RapidRide", x = rapidride)
writeData(wb, sheet = "Needs Met", x = needsmet)


saveWorkbook(wb, "C:/Users/clevans/OneDrive - King County/01_RiderNonRider2.0/Dashboard/KCM Rider NonRider_RVersion.xlsx", 
             overwrite=TRUE)


#saveXML(wb, "C:/Users/clevans/OneDrive - King County/04_Rider_NonRider2020/Dashboard/KCM Rider NonRider_New2.xml")

