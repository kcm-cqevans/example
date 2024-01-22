

svywgt_collapse <- function(data, groupingvar=data$groupingvar, depvar=data$depvar,
                            response=data$response, wgt=data$wgt, group_var=FALSE) {
if(group_var==TRUE){
  data %>% 
    filter(!is.na(groupingvar)) %>%  filter(!is.na(depvar)) %>% filter(!is.na(response))%>%
    as_survey_design(1, weight = wgt) %>% 
    group_by(groupingvar, depvar, response) %>% 
    summarize(prop = 100*survey_prop(proportion = TRUE, vartype="ci"))  %>% 
    filter(response==100 | response==1) %>% 
    mutate(proplabel=paste0(round(prop,1),"%")) 
    }  
else{
  data %>% 
    filter(!is.na(depvar)) %>% filter(!is.na(response))%>%
    as_survey_design(1, weight = wgt) %>% 
    group_by(depvar, response) %>% 
    summarize(prop = 100*survey_prop(proportion = TRUE, vartype="ci"))  %>% 
    filter(response==100 | response==1) %>% 
    mutate(proplabel=paste0(round(prop,1),"%")) 
    }
}



svy_reshape <- function(data, group_var=FALSE) {
  if(group_var == TRUE){
    data %>% 
      pivot_longer(data=.,
                   cols = 4:ncol(data),
                   names_to = "depvar",
                   values_to = "response") %>% 
      as.data.frame() 
  }
  else{
    data %>% 
      pivot_longer(data=.,
                   cols = 3:ncol(data),
                   names_to = "depvar",
                   values_to = "response") %>% 
      as.data.frame()
  }
}
#explicitly provide list of variables that should not be pivoted
#not list --> otherwise could include some quosures


rnr_rename_purp <-function(data) {
data %>% 
  mutate(depvar = case_when(str_detect(depvar, "work") ~ "Work",
                            str_detect(depvar, "school") ~ "School",
                            str_detect(depvar, "childcare") ~ "Childcare",
                            str_detect(depvar, "shopping") ~ "Shopping, errands",
                            str_detect(depvar, "fun") ~ "Fun, social, rec.",
                            str_detect(depvar, "special") ~ "Special events",
                            str_detect(depvar, "airport") ~ "Airport",
                            str_detect(depvar, "medical") ~ "Medical appts.",
                            str_detect(depvar, "social") ~ "Social services",
                            str_detect(depvar, "judicial") ~ "Judicial",
                            str_detect(depvar, "other") ~ "Other",
                            str_detect(depvar, "all") ~ "All trips")) %>% 
  as.data.frame()
  
}
## add some error messaging, what happens if none of those are true, give some feedback on 
## what is and isn't changed, what if detects something new


rnr_rename_race_eth <-function(data) {
  data %>% 
    mutate(depvar = case_when(str_detect(depvar, "eth_black") ~ "Black, African American",
                              str_detect(depvar, "easian") ~ "East Asian, East Asian American",
                              str_detect(depvar, "indigen") ~ "Indigenous, Native American",
                              str_detect(depvar, "islander") ~ "Pacific Islander, Native Hawaiian",
                              str_detect(depvar, "latinx") ~ "Hispanic, Latinx",
                              str_detect(depvar, "mena") ~ "Middle Eastern, North African",
                              str_detect(depvar, "other") ~ "Other",
                              str_detect(depvar, "prefer") ~ "Prefer not",
                              str_detect(depvar, "sasian") ~ "South Asian, South Asian American",
                              str_detect(depvar, "seasian") ~ "Southeast Asian, Southeast Asian American",
                              str_detect(depvar, "ssub") ~ "Sub-Saharan African",
                              str_detect(depvar, "_white") ~ "White")) %>% 
    as.data.frame()
  
}

rnr_rename_barriers <-function(data) {
  data %>% 
    mutate(depvar = case_when(str_detect(depvar, "toolong") ~ "Not flexible enough",
                              str_detect(depvar, "noflex") ~ "Not frequent enough",
                              str_detect(depvar, "nostophome") ~ "No stop near home",
                              str_detect(depvar, "nostopdest") ~ "No stop near dest.",
                              str_detect(depvar, "transfer") ~ "Transfer wait time",
                              str_detect(depvar, "safeothers") ~ "Safety, others",
                              str_detect(depvar, "safedrive") ~ "Safety, driver",
                              str_detect(depvar, "notearly") ~ "Not early enough",
                              str_detect(depvar, "notlate") ~ "Not late enough",
                              str_detect(depvar, "ontime") ~ "Not on time",
                              str_detect(depvar, "health") ~ "Health concerns",
                              str_detect(depvar, "knowhow") ~ "Not sure how to",
                              str_detect(depvar, "cost") ~ "Too expensive",
                              str_detect(depvar, "nottried") ~ "Never tried",
                              str_detect(depvar, "other") ~ "Other", 
                              str_detect(depvar, "dontknow") ~ "DK" )) %>% 
    as.data.frame()
  
}









