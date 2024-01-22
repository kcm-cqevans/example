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
load("C:/Users/clevans/OneDrive - King County/04_Rider_NonRider2020/Metro_EMC_RiderNonRider/2023 Q3/Q3 Datasets/23-9011 KCM Q3 Raw Data Subset 12.20.RData")
data$wave<-20233


data <- data %>%
  dplyr::rename(uniqueid = externaldatareference,
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
         transit_mode_flex = transit_mode_17, 
         transit_mode_other=transit_mode_98,
         # transit_mode_other_txt=transit_mode_98_text,
         transit_mode_dk=transit_mode_99,
         
         
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
         purpose_alltrips=trip_purp_95,
         purpose_dk=trip_purp_99,
         
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
         
         
         wish_purp_work=wishtrip_purp_1,
         wish_purp_school=wishtrip_purp_2,
         wish_purp_childcare=wishtrip_purp_3,
         wish_purp_shopping=wishtrip_purp_4,
         wish_purp_fun=wishtrip_purp_5,
         wish_purp_special=wishtrip_purp_6,
         wish_purp_airport=wishtrip_purp_7,
         wish_purp_medical=wishtrip_purp_8,
         wish_purp_social=wishtrip_purp_9,
         wish_purp_judicial=wishtrip_purp_10,
         wish_purp_other=wishtrip_purp_98,
         wish_purp_other_txt=wishtrip_purp_98_text,
         wish_purp_alltrips=wishtrip_purp_95,
         wish_purp_dk=wishtrip_purp_99,
         
         wishtrip_ampeak=wishtrip_tod_1,
         wishtrip_midday=wishtrip_tod_2,
         wishtrip_pmpeak=wishtrip_tod_3,
         wishtrip_night=wishtrip_tod_4,
         wishtrip_latenite=wishtrip_tod_5,
         wishtrip_saturday=wishtrip_tod_6,
         wishtrip_sunday=wishtrip_tod_7,
         
         
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
         plantrip_notool=tripplan_95,
         plantrip_dk=tripplan_99,
         
         info_kcweb=info_1,
         info_otherweb=info_2,
         info_kcplanner=info_3,
         info_stopinfo=info_4,
         info_realtime=info_5,
         info_kcalerts=info_6,
         info_pugetsoundreg=info_7,
         info_metrofacebook=info_8,
         info_metrotwitter=info_9,
         info_metroinstagram=info_10,
         info_friendsfamily=info_11,
         info_metromatters=info_12,
         info_printedtime=info_13,
         info_metrocustserv=info_14,
         info_other=info_98,
         info_other_txt=info_98_text,
         info_dk=info_99,
         info_noneabove = info_95,
         
         comm_mode_drivealone=comm_mode_1,
         comm_mode_transit=comm_mode_2,
         comm_mode_carpool=comm_mode_3,
         comm_mode_school=comm_mode_4,
         comm_mode_moto=comm_mode_5,
         comm_mode_bike=comm_mode_6,
         comm_mode_walk=comm_mode_7,
         comm_mode_uber=comm_mode_8,
         comm_mode_sharedcar=comm_mode_9,
         comm_mode_access=comm_mode_10,
         comm_mode_ferry=comm_mode_11,
         comm_mode_other=comm_mode_98,
         comm_mode_other_text=comm_mode_98_text,
         comm_mode_dk=comm_mode_99,
         
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
         
         barrier_toolong = barrier_1_new, 
         barrier_noflex = barrier_2_new,
         barrier_notfreq = barrier_3_new,
         barrier_nostophome = barrier_4_new,
         barrier_nostopdest = barrier_5_new,
         barrier_transfer = barrier_6_new,
         barrier_safeothers = barrier_7_new,
         barrier_safedrive = barrier_8_new,
         barrier_notearly = barrier_9_new,
         barrier_notlate = barrier_10_new, 
         barrier_ontime = barrier_11_new,
         barrier_health = barrier_12_new,
         barrier_knowhow = barrier_13_new,
         barrier_cost = barrier_14_new,
         barrier_nottried = barrier_15_new,
         barrier_other = barrier_98_new,
         barrier_other_txt = barrier_98_text,
         barrier_dontknow = barrier_99_new,
         
         wishtrip_area = Wishtrip,
         
         region = region3,
         ridestat_kcmbus = metro_bus_rider,
         ridestat_metro = metro_service_rider,
         income_10cat = d5_web,
         poverty = povleveln, 
         
         choice_appt = tripchoice_a_overall, 
         choice_event = tripchoice_e_overall, 
         choice_dinner = tripchoice_d_overall, 
         
         
  )


data <- data %>% 
  mutate(treat_event = case_when(tripchoice_e_v1!="" ~ 0,
                             tripchoice_e_v2!="" ~ 1))

data <- data %>% 
  mutate(answer_event = case_when(choice_event=="Much more likely to use Option A" ~ 0,
                              choice_event=="Somewhat more likely to use Option A" ~ 0,
                              choice_event=="Somewhat more likely to use Option B" ~ 1,
                              choice_event=="Much more likely to use Option B" ~ 1))

data <- data %>% 
  mutate(treat_appt = case_when(tripchoice_a_v1!="" ~ 0,
                             tripchoice_a_v2!="" ~ 1,
                             tripchoice_a_v3!="" ~ 2)) 
data$treat_a<-as.factor(data$treat_a)

data <- data %>% 
  mutate(answer_appt = case_when(choice_appt=="Much more likely to use Option A" ~ 0,
                              choice_appt=="Somewhat more likely to use Option A" ~ 0,
                              choice_appt=="Somewhat more likely to use Option B" ~ 1,
                              choice_appt=="Much more likely to use Option B" ~ 1))

data <- data %>% 
  mutate(treat_dinner = case_when(tripchoice_d_v1!="" ~ 0,
                             tripchoice_d_v2!="" ~ 1))


data <- data %>% 
  mutate(answer_dinner = case_when(choice_dinner=="Much more likely to use Option A" ~ 0,
                              choice_dinner=="Somewhat more likely to use Option A" ~ 0,
                              choice_dinner=="Somewhat more likely to use Option B" ~ 1,
                              choice_dinner=="Much more likely to use Option B" ~ 1))



data$age_year<-as.character(data$age_year)
data$age_year[data$age_year=="Prefer not to respond"]<-"NA"
data$age_year <- as.numeric(data$age_year)

data$wishtrip_area <- as.numeric(substr(data$wishtrip_area, 1, 3))


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



data$nonr_tr_freq<- recode_factor(data$nonr_tr_freq, "I have never used public transit" = "Never")

dropvars<-c("age_elig", "age_year", "age_cat", "other_partic", "kcres", "route_other_1","route_other_2", "route_other_3", 
            "route_other_4", "route_other_1_text", "route_other_3_text", "prim_trip_northwest", 
            "prim_trip_northwest_12_text", "prim_trip_cent_south_18_text",
            "prim_trip_central", "prim_trip_central_13_text", "prim_trip_east", 
            "prim_trip_east_11_text", "prim_trip_south", "prim_trip_south_12_text", 
            "prim_trip_outside", "prim_trip_outside_5_text", "wishtrip_areas", 
            "wishtrip_areas_7_text", "wishtrip_northeast", "wishtrip_northeast_12_text", 
            "wishtrip_cent_south", "wishtrip_cent_south_18_text", 
            "wishtrip_south", "wishtrip_south_12_text", "wishtrip_central", "wishtrip_central_13_text",
            "wishtrip_east", "wishtrip_east_11_text", "wishtrip_outside", 
            "wishtrip_outside_5_text", "d5c", "d5d", "d5e", "d5f", "d5g", "d5h", "d5i", "d5j", "d5k", "d5l",
            "tripchoice_d_v1", "tripchoice_d_v2", "tripchoice_a_v1", "tripchoice_a_v2", "tripchoice_a_v1",
            "tripchoice_e_v1", "tripchoice_e_v1"
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
data$cbg_group<-as.numeric(data$cbg_group)




save(data, file = "Data_2023_3.RData")
