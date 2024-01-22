rm(list=ls(all=TRUE))
library(here)
library(dplyr)
library(stringr)
library(tidyr)
library(hablar)
library(tidyverse)
library(epiDisplay)
library(readxl)


load("Data_2022_2.RData")
q2_2022<-data
load("Data_2022_3.RData")
q3_2022<-data
load("Data_2022_4.RData")
q4_2022<-data
load("Data_2023_1.RData")
q1_2023<-data
load("Data_2023_2.RData")
q2_2023<-data
load("Data_2023_3.RData")
q3_2023<-data
rm(data)


#Corrected Weights for 2023
new_weights <- read_excel("C:/Users/clevans/OneDrive - King County/04_Rider_NonRider2020/Metro_EMC_RiderNonRider/KCM Q2-Q4 Revised Regions and Weights 3-23.xlsx")
#let's append Q2, Q3, and Q4
data<-dplyr::bind_rows(q2_2022, q3_2022, q4_2022, q1_2023, q2_2023, q3_2023)
rm(q2_2022, q3_2022, q4_2022, q1_2023, q2_2023, q3_2023)
#Merging in the new weights
data<-left_join(data, new_weights, by = join_by(uniqueid == uniqueid))
rm(new_weights)

#Correcting weights wiht the new weights from above
data<- data %>% 
  mutate(wgt = case_when(
    weights2_revised != NA ~ weights2_revised,
    TRUE ~ wgt)) %>% 
  mutate(region_new = case_when(
      wave<=20224  ~ region3_revised,
      wave>20224 ~ region))


dropvars<-c("region3_revised", "weights2_revised", "region")
data= data[,!(names(data) %in% dropvars)] 
data <- data %>%
  dplyr::rename(region = region_new)


data<- data %>%
  mutate(rapidrideuser = case_when(str_detect(route1, "Line") | 
                                      str_detect(route2, "Line") |
                                      str_detect(route3, "Line") ~ 1,
                                    TRUE ~ 0)) 

data<- data %>% 
  mutate(priority = case_when(disability==1 | english_yes==0 | bipoc==1 | poverty=="At/Below 200% Fed. Poverty Level" ~ 1,
                              disability==0 & english_yes==1 & bipoc==0  & poverty=="Above 200% Fed. Poverty Level" ~0))


save(data, file = "Data_Merged_2022-2023.RData")

dashdata <- data %>% 
  select(uniqueid, wave, wgt,  region, userlanguage,	zipcode,	CensusBlockGroup,	census_tract,	cbg_group	,
         ridestat,	ridestat_metro,	ridestat_kcmbus,	agecat,	gender4cat,	income_5cat,	race_eth_exl,	disability,
         rentown,	language,	driverslic,	vehicle_hh,	hhsiz_total,	hhsiz_under18,	kids_hh,	southlink,
         madisonst_area,	hline_area,	route120rider,	metro_link, commute, comm_return,	comm_days,	comm_mode_drivealone,
         comm_mode_transit,	comm_mode_carpool,	comm_mode_school,	comm_mode_moto,	comm_mode_bike,	comm_mode_walk,
         comm_mode_uber,	comm_mode_sharedcar,	comm_mode_access,	comm_mode_ferry,	comm_mode_other,
         comm_ampeak,	comm_midday,	comm_pmpeak,	comm_night,	comm_latenite,	comm_saturday,
         comm_sunday,	commute_area, transit_freq,	transit_mode_kcmbus,	transit_mode_link,	transit_mode_sounder,
         transit_mode_vanpool,	transit_mode_streetcar,	transit_mode_watertaxi,	transit_mode_monorail,
         transit_mode_stbus	, transit_mode_pierce,	transit_mode_ctbus,	transit_mode_kitsap,	transit_mode_access,
         transit_mode_dart,	transit_mode_via,	transit_mode_ridepingo,	transit_mode_commride,	transit_mode_other,
         purpose_work,	purpose_school,	purpose_childcare,	purpose_shopping,	purpose_fun,	purpose_specialevents,
         purpose_airport,	purpose_medical,	purpose_socialservices,	purpose_judicial,	purpose_other,
         purpose_alltrips,	stop_access,	accessbarrier_improvesidewalk,	accessbarrier_newsidewalk,
         accessbarrier_improvebikelane,	accessbarrier_newbikelanes,	accessbarrier_lighting,
         accessbarrier_scootershare,	accessbarrier_bikeparking,	accessbarrier_parking,
         accessbarrier_flexservice,	accessbarrier_othersafety,	accessbarrier_nothing,	accessbarrier_other,
         accessbarrier_dk,	stop_dist,	rapidrideuser, primary_trip,	big_area,	prim_trip_purp_new,	
         prim_trip_travel_tim,	prim_transit_kcmbus,	prim_transit_linklight,	prim_transit_sounder,	
         prim_transit_vanpool,	prim_transit_streetcar,	prim_transit_watertaxi,	prim_transit_monorail,	
         prim_transit_stbus,	prim_transit_piercebus,	prim_transit_ctbus,	prim_transit_kitsapferry,	
         prim_transit_access,	prim_transit_dart,	prim_transit_via,	prim_transit_ridepingo,	
         prim_transit_commride,	prim_transit_other,	todtravel_ampeak,	todtravel_midday,	todtravel_pmpeak,	
         todtravel_night,	todtravel_latenite,	todtravel_saturday,	todtravel_sunday,	todtravel_missing,	
         prim_trip_freq, wishtrip_ampeak,	wishtrip_midday,	wishtrip_pmpeak,	wishtrip_night,	wishtrip_latenite,	
         wishtrip_saturday,	wishtrip_sunday,	wishtrip_tod_99,	wish_purp_work,	wish_purp_school,	
         wish_purp_childcare,	wish_purp_shopping,	wish_purp_fun,	wish_purp_special,	wish_purp_airport,	
         wish_purp_medical,	wish_purp_social,	wish_purp_judicial,	wish_purp_other,
         wish_purp_alltrips,	wish_purp_dk, bhv_enjoy,	bhv_safe,	bhv_imp,	bhv_sus, bhv_positive,
         bhv_pos_news,	pcvd_behnorm, nps_overall,	sat_overall,	sat_overall,	nps_overall,	sat_bus,	
         bus_noshow,	sat_freq_serv,	sat_ontime_stop,	sat_ontime_dest,	sat_traveltime,	sat_cleanstop,	
         sat_cleanbus,	sat_seatbus,	sat_seatstop,	sat_transitwait,	sat_driver,	sat_orca,	sat_orcaload,	
         sat_safebusday,	sat_safewaitday,	sat_safeflm,	sat_safebusnight,	sat_safewaitnight,	interest
         
)




