rm(list=ls(all=TRUE))
library(here)
library(dplyr)
library(stringr)
library(tidyr)
library(hablar)
library(tidyverse)
library(epiDisplay)
library(readxl)

#Loading Data in --- Make sure to change your working directory. 
load("C:/Users/clevans/OneDrive - King County/04_Rider_NonRider2020/Metro_EMC_RiderNonRider/2022 Q3/Q3 Dataset/22-8605 KCM Raw Data Subset v2.RData")
data$wave<-20223



data <- data %>%
  rename(uniqueid = externalreference,
         wgt = weights2,
         mode_drivealone=mode_1,
         mode_transit=mode_2,
         mode_carpoolhh=mode_3,
         mode_carpool=mode_4,
         mode_school=mode_5,
         mode_moto=mode_6,
         mode_bike=mode_7,
         mode_walk=mode_8,
         mode_uber=mode_9,
         mode_sharedcar=mode_10,
         mode_access=mode_11,
         mode_ferry=mode_12,
         mode_other=mode_98,
         mode_other_txt=mode_98_text,
         mode_dk=mode_99, 
         
         modehh_drivealone=mode_hh_1,
         modehh_transit=mode_hh_2,
         modehh_carpoolhh=mode_hh_3,
         modehh_carpool=mode_hh_4,
         modehh_school=mode_hh_5,
         modehh_moto=mode_hh_6,
         modehh_bike=mode_hh_7,
         modehh_walk=mode_hh_8,
         modehh_uber=mode_hh_9,
         modehh_sharedcar=mode_hh_10,
         modehh_access=mode_hh_11,
         modehh_ferry=mode_hh_12,
         modehh_other=mode_hh_98,
         modehh_other_txt=mode_hh_98_text,
         modehh_dk=mode_hh_99, 
         
         transit_mode_kcmbus=transit_mode_1,
         transit_mode_link=transit_mode_2,
         transit_mode_sounder=transit_mode_3,
         transit_mode_vanpool=transit_mode_4,
         transit_mode_streetcar=transit_mode_5,
         transit_mode_watertaxi=transit_mode_6,
         transit_mode_monorail=transit_mode_7,
         transit_mode_stbus=transit_mode_8,
         transit_mode_pierce=transit_mode_9,
         transit_mode_ctbus=transit_mode_10,
         transit_mode_kitsap=transit_mode_11,
         transit_mode_access=transit_mode_12,
         transit_mode_dart=transit_mode_13,
         transit_mode_via=transit_mode_14,
         transit_mode_ridepingo=transit_mode_15,
         transit_mode_commride=transit_mode_16,
         transit_mode_other=transit_mode_98,
         transit_mode_other_txt=transit_mode_98_text,
         transit_mode_dk=transit_mode_99,
         
         route_other_kcm=route_other_1,
         route_other_st=route_other_2,
         route_other = route_other_3,
         route_other_dk = route_other_4,
         route_other_kcm_text=route_other_1_text,
         route_other_text = route_other_3_text,
         
         
         purpose_work=trip_purp_1,
         purpose_school=trip_purp_2,
         purpose_childcare=trip_purp_3,
         purpose_shopping=trip_purp_4,
         purpose_fun=trip_purp_5,
         purpose_specialevents=trip_purp_6,
         purpose_airport=trip_purp_7,
         purpose_medical=trip_purp_8,
         purpose_socialservices=trip_purp_9,
         purpose_judicial=trip_purp_10,
         purpose_other=trip_purp_98,
         purpose_other_txt=trip_purp_98_text,
         purpose_alltrips=trip_purp_12,
         purpose_dk=trip_purp_99,
         
         purpose_primtrip=prim_trip_purp_new,
         
         
         prim_transit_kcmbus=prim_transit_mode_1_fix,
         prim_transit_linklight=prim_transit_mode_2_fix,
         prim_transit_sounder=prim_transit_mode_3_fix,
         prim_transit_vanpool=prim_transit_mode_4_fix,
         prim_transit_streetcar=prim_transit_mode_5_fix,
         prim_transit_watertaxi=prim_transit_mode_6_fix,
         prim_transit_monorail=prim_transit_mode_7_fix,
         prim_transit_stbus=prim_transit_mode_8_fix,
         prim_transit_piercebus=prim_transit_mode_9_fix,
         prim_transit_ctbus=prim_transit_mode_10_fix,
         prim_transit_kitsapferry=prim_transit_mode_11_fix,
         prim_transit_access=prim_transit_mode_12_fix,
         prim_transit_dart=prim_transit_mode_13_fix,
         prim_transit_via=prim_transit_mode_14_fix,
         prim_transit_ridepingo=prim_transit_mode_15_fix,
         prim_transit_commride=prim_transit_mode_16_fix,
         prim_transit_other=prim_transit_mode_98_fix,
         prim_transit_other_txt=prim_transit_mode_98_text,
         prim_transit_dk=prim_transit_mode_99_fix,
         
         todtravel_ampeak=prim_trip_tod_1,
         todtravel_midday=prim_trip_tod_2,
         todtravel_pmpeak=prim_trip_tod_3,
         todtravel_night=prim_trip_tod_4,
         todtravel_latenite=prim_trip_tod_5,
         todtravel_saturday=prim_trip_tod_6,
         todtravel_sunday=prim_trip_tod_7,
         todtravel_missing=prim_trip_tod_99,
         
         primarytrip_area = primary_trip,
         
         primtrip_traveltime = prim_trip_travel_tim,
         prim_tripfreq = prim_trip_freq,
         
         transfer_mode_kcm = transfer_mode_1,
         transfer_mode_link = transfer_mode_2,
         transfer_mode_sounder = transfer_mode_3,
         transfer_mode_vanpool = transfer_mode_4,
         transfer_mode_streetcar = transfer_mode_5,
         transfer_mode_watertaxi = transfer_mode_6,
         transfer_mode_monrail = transfer_mode_7,
         transfer_mode_stbus = transfer_mode_8,
         transfer_mode_pt = transfer_mode_9,
         transfer_mode_ct = transfer_mode_10,
         transfer_mode_kitsap = transfer_mode_11,
         transfer_mode_access = transfer_mode_12,
         transfer_mode_dart = transfer_mode_13,
         transfer_mode_via = transfer_mode_14,
         transfer_mode_pingo = transfer_mode_15,
         transfer_mode_commride=transfer_mode_16,
         transfer_mode_other = transfer_mode_98,
         transfer_mode_other_txt = transfer_mode_98_text,
         transfer_mode_dk= transfer_mode_99,
         
         stop_access_other = stop_access_9_text,
         
         accessbarrier_improvesidewalk=stop_access_barrier_1,
         accessbarrier_newsidewalk=stop_access_barrier_2,
         accessbarrier_improvebikelane=stop_access_barrier_3,
         accessbarrier_newbikelanes=stop_access_barrier_4,
         accessbarrier_lighting=stop_access_barrier_5,
         accessbarrier_scootershare=stop_access_barrier_6,
         accessbarrier_bikeparking=stop_access_barrier_7,
         accessbarrier_parking=stop_access_barrier_8,
         accessbarrier_flexservice=stop_access_barrier_9,
         accessbarrier_othersafety=stop_access_barrier_10,
         accessbarrier_nothing=stop_access_barrier_11,
         accessbarrier_other=stop_access_barrier_12,
         accessbarrier_dk=stop_access_barrier_13,
         accessbarrier_othersafe_txt=stop_access_barrier_10_text,
         accessbarrier_other_txt=stop_access_barrier_12_text,
         
         
         fare_orca=farepay_1,
         fare_cash=farepay_2,
         fare_tix=farepay_3,
         fare_access=farepay_4,
         fare_tgt=farepay_5,
         fare_unpaid=farepay_6,
         fare_other=farepay_98,
         fare_other_txt=farepay_98_text,
         fare_dk=farepay_99,
         
         orcacard_lift=orca_type_1,
         orcacard_work=orca_type_2,
         orcacard_youth=orca_type_3,
         orcacard_regular=orca_type_4,
         orcacard_rrfp_senior=orca_type_5,
         orcacard_rrfp_disabled=orca_type_6,
         orcacard_notsure=orca_type_7,
         orcacard_other=orca_type_98,
         orcacard_other_txt=orca_type_98_text,
         orcacard_dk=orca_type_99,
         

         plantrip_kcplanner=tripplan_1,
         plantrip_stopinfo=tripplan_2,
         plantrip_pugetsoundreg=tripplan_3,
         plantrip_googlemaps=tripplan_4,
         plantrip_applemaps=tripplan_5,
         plantrip_transitapp=tripplan_6,
         plantrip_onebusaway=tripplan_7,
         plantrip_friendsfamily=tripplan_8,
         plantrip_printedtime=tripplan_9,
         plantrip_metrocustserv=tripplan_10,
         plantrip_access=tripplan_11,
         plantrip_textdepart=tripplan_12,
         plantrip_other=tripplan_98,
         plantrip_other_txt=tripplan_98_text,
         plantrip_notool=tripplan_14,
         plantrip_dk=tripplan_99,
         
         plantrip_primary = tripplan_fix,
         nps_plantrip_tool = nps_plantool_comb,
         
         comm_mode_drivealone=comm_mode_1_fix,
         comm_mode_transit=comm_mode_2_fix,
         comm_mode_carpool=comm_mode_3_fix,
         comm_mode_school=comm_mode_4_fix,
         comm_mode_moto=comm_mode_5_fix,
         comm_mode_bike=comm_mode_6_fix,
         comm_mode_walk=comm_mode_7_fix,
         comm_mode_uber=comm_mode_8_fix,
         comm_mode_sharedcar=comm_mode_9_fix,
         comm_mode_access=comm_mode_10_fix,
         comm_mode_ferry=comm_mode_11_fix,
         comm_mode_other=comm_mode_98_fix,
         comm_mode_other_text=comm_mode_98_text,
         comm_mode_dk=comm_mode_99_fix,
         
         comm_transit_kcmbus=comm_transit_mode_1_fix,
         comm_transit_linklight=comm_transit_mode_2_fix,
         comm_transit_sounder=comm_transit_mode_3_fix,
         comm_transit_vanpool=comm_transit_mode_4_fix,
         comm_transit_steetcar=comm_transit_mode_5_fix,
         comm_transit_watertaxi=comm_transit_mode_6_fix,
         comm_transit_monorail=comm_transit_mode_7_fix,
         comm_transit_stbus=comm_transit_mode_8_fix,
         comm_transit_piercebus=comm_transit_mode_9_fix,
         comm_transit_ctbus=comm_transit_mode_10_fix,
         comm_transit_kitsap=comm_transit_mode_11_fix,
         comm_transit_access=comm_transit_mode_12_fix,
         comm_transit_dart=comm_transit_mode_13_fix,
         comm_transit_via=comm_transit_mode_14_fix,
         comm_transit_ridepingo=comm_transit_mode_15_fix,
         comm_transit_commride=comm_transit_mode_16_fix,
         comm_transit_other=comm_transit_mode_98_fix,
         comm_transit_other_txt=comm_transit_mode_98_text,
         comm_transit_dk=comm_transit_mode_99_fix,
         
         comm_ampeak=comm_tod_1_fix,
         comm_midday=comm_tod_2_fix,
         comm_pmpeak=comm_tod_3_fix,
         comm_night=comm_tod_4_fix,
         comm_latenite=comm_tod_5_fix,
         comm_saturday=comm_tod_6_fix,
         comm_sunday=comm_tod_7_fix,
         comm_dk=comm_tod_99_fix,
         
         commute_bigarea = comm_area_new,
         commute_interest = comm_interest_fix,
         comm_time = comm_time_new,
         
         vehicle_num = vehicle_hh_1_text,
         
         race_eth_african=race_eth_1,
         race_eth_black=race_eth_2,
         race_eth_native=race_eth_3,
         race_eth_eastasian=race_eth_4,
         race_eth_seasian=race_eth_5,
         race_eth_southasian=race_eth_6,
         race_eth_latinx=race_eth_7,
         race_eth_mideast=race_eth_8,
         race_eth_islander=race_eth_9,
         race_eth_white=race_eth_10,
         race_eth_other=race_eth_11,
         race_eth_noresponse=race_eth_12,
         race_eth_other_txt=race_eth_11_text,
         
         language_other_txt = language_9_text,
         
         region = region3,
         ridestat_kcmbus = metro_bus_rider,
         ridestat_metro = metro_service_rider,
         income_10cat = d5_web, 
         poverty = povleveln
  )

data$primarytrip_area <- as.numeric(substr(data$primarytrip_area, 1, 3))
data$commute_area <- as.numeric(substr(data$commute_area, 1, 3))

data$age_year<-as.character(data$age_year)
data$age_year[data$age_year=="Prefer not to respond"]<-"NA"
data$age_year <- as.numeric(data$age_year)

data<- data %>%
  mutate(
    # Create categories
    agecat = case_when(
      age_year <= 1957  ~ "65+",
      age_year > 1957 & age_year <= 1967 ~ "55-64",
      age_year > 1967 & age_year <= 1977 ~ "45-54",
      age_year > 1977 & age_year <= 1987 ~ "35-44",
      age_year > 1987 & age_year <= 1997 ~ "25-34",
      age_year > 1997  ~ "16-24"),
    # Convert to factor
    agecat = factor(
      agecat,
      level = c("16-24", "25-34","35-44", "45-54", "55-64", "65+"))) %>% 
  mutate(disability=case_when(disability == "Yes"~1,
                              disability == "No" ~ 0,
                              disability == "Prefer not to respond" ~ 99)) %>% 
  mutate(english_yes = case_when(language=="English" ~ 1,
                                 language!="English" & language!="Prefer not to answer" ~0)) %>% 
  mutate(income_5cat = 
           case_when(income_10cat=="Less than $7,500" | income_10cat=="$7,500 up to $15,000" 
                     | income_10cat=="$15,000 up to $25,000" | income_10cat=="$25,000 up to $35,000" ~ "< $35k",
                     income_10cat=="$35,000 up to $55,000" | income_10cat=="$55,000 up to $75,000" ~ "$35k - $75k",
                     income_10cat=="$75,000 up to $100,000" ~ "$75k - $100k",
                     income_10cat=="$100,000 up to $150,000" ~ "$100k - $150k",
                     income_10cat=="$150,000 and up" ~ "$150k +", 
                     income_10cat== "Prefer not to respond" | income_10cat=="Don't know" ~ "DK/Ref"),
         income_5cat = factor(income_5cat,
                              level = c("< $35k", "$35k - $75k","$75k - $100k", "$100k - $150k", "$150k +", "DK/Ref"))) %>% 
  mutate(kids_hh = case_when(hhsiz_under18!=0 ~ 1, 
                             hhsiz_under18==0 ~0)) 




#Race and Ethnicity Recoding

data<- data %>% 
  rowwise() %>% 
  mutate(race_eth_total = sum(c(race_eth_african, race_eth_black, race_eth_native,
                                race_eth_eastasian, race_eth_seasian, race_eth_islander,
                                race_eth_southasian, race_eth_mideast, race_eth_islander,
                                race_eth_white, race_eth_latinx)))  


data<- data %>% 
  mutate(race_eth_total = case_when(str_detect(race_eth_other_txt, "mix") | 
                                      str_detect(race_eth_other_txt, "Mix") |
                                      str_detect(race_eth_other_txt, "Multi") |
                                      str_detect(race_eth_other_txt, "melting") |
                                      str_detect(race_eth_other_txt, "Two or more") |
                                      str_detect(race_eth_other_txt, "two or more") |
                                      str_detect(race_eth_other_txt, "Filipino American") |
                                      str_detect(race_eth_other_txt, "Thai/Caucasian") |
                                      str_detect(race_eth_other_txt, "Black asian") ~ 2,
                                    TRUE ~ race_eth_total)) %>% 
  mutate(race_eth_black = case_when(str_detect(race_eth_other_txt, "Black") ~ 1,
                                    TRUE ~ race_eth_black)) %>% 
  mutate(
    # Create categories
    race_eth_exl = case_when(
      race_eth_african ==1 & race_eth_total==1  ~ "African, African American, Black",
      race_eth_black ==1 & race_eth_total==1  ~ "African, African American, Black",
      race_eth_native ==1 & race_eth_total==1  ~ "Indigenous",
      race_eth_eastasian ==1 & race_eth_total==1  ~ "East Asian",
      race_eth_seasian ==1 & race_eth_total==1  ~ "Southeast Asian",
      race_eth_southasian ==1 & race_eth_total==1  ~ "South Asian",
      race_eth_latinx ==1 & race_eth_total==1  ~ "Hispanic, Latinx",
      race_eth_mideast ==1 & race_eth_total==1  ~ "Middle Eastern",
      race_eth_islander ==1 & race_eth_total==1  ~ "Native Hawaiian, Pacific Islander",
      race_eth_white ==1 & race_eth_total==1  ~ "White",
      race_eth_total >=2  ~ "Multiple races/ethnicities selected",
      race_eth_noresponse==1 ~"DK/Refuse")) %>% 
  
  mutate(bipoc = case_when(race_eth_exl!="White" & race_eth_exl!="DK/Refuse" & race_eth_exl!="" ~ 1,
                           race_eth_exl=="White" | race_eth_exl =="DK/Refuse" ~ 0))  


data$comm_return <- recode_factor(data$comm_return, "Never, I don't plan on making regular trips to work or school" = "Never")
data$nonr_tr_freq<- recode_factor(data$nonr_tr_freq, "I have never used public transit" = "Never")

dropvars<-c("age_elig", "age_year", "age_cat", "other_partic", "kcres", "route_other_1","route_other_2", "route_other_3", 
            "route_other_4", "route_other_1_text", "route_other_3_text", "tripplan_prim",
            "prim_trip_northwest", "nps_plantool_prim", "nps_plantool",
            "prim_trip_northwest_12_text", "prim_trip_cent_south_18_text",
            "prim_trip_central", "prim_trip_central_13_text", "prim_trip_east",
            "prim_trip_cent_south", "prim_trip_areas_7_text", "prim_trip_areas_sr_d",
            "prim_trip_purp_11_text","prim_trip_east_11_text", "prim_trip_south", "prim_trip_south_12_text", 
            "prim_trip_outside", "prim_trip_outside_5_text", "prim_trip_purp","prim_trip_areas", 
            "comm_area_central_98_text", "comm_area_northwest_fix", "comm_area_cent_south_fix",
            "comm-area_central_fix", "comm_area_east_fix", "comm_area_south_fix", "comm_area_central_fix",
            "comm_area_98_text", "comm_area_northwest_98_text", "comm_area_cent_south_98_text", "commute_bigarea",
            "comm_area_east_98_text", "comm_area_south_98_text", "comm_area_outside_fix", "comm_area_outside_98_text",
            "wishtrip_outside_5_text", "d5c", "d5d", "d5e", "d5f", "d5g", "d5h", "d5i", "d5j", "d5k", "d5l", "d5_web_2"
)
data= data[,!(names(data) %in% dropvars)] 


data <- data %>% 
  mutate_at(vars(ridestat), 
            ~(case_when(. == "Current Transit Rider" ~ 1,
                        . == "Past Transit Rider" ~ 0,
                        . == "Non-Transit Rider" ~ -1))) %>% 
  mutate_at(vars(ridestat_kcmbus), 
            ~(case_when(. == "Uses KC Metro bus service" ~ 1,
                        . == "Does not use KC Metro bus service" ~ 0,
                        . == "Non-Rider" ~ -99))) %>% 
  mutate_at(vars(ridestat_metro), 
            ~(case_when(. == "Uses KC Metro service" ~ 1,
                        . == "Does not use KC Metro service" ~ 0,
                        . == "Non-Rider" ~ -99)))

data$CensusBlockGroup <- as.numeric(str_remove(data$CensusBlockGroup, "#"))
data$census_tract <- as.numeric(substr(as.character(data$CensusBlockGroup), 1, 11))
data$zipcode <- as.numeric(substr(as.character(data$zipcode), 1, 5))




save(data, file = "Data_2022_3.RData")
