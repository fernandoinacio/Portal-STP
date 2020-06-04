
#source("dbConnect/Script_PostgreSql.R")

aux <- data
aux <- aux[-1]
aux <- aux[-8]
colnames(aux) <- c("suspeitos", "testados","confirmed","death", "recovered","date","countryName")
aux$active <- aux$confirmed-aux$recovered-aux$death
aux<- aux[c(6,7,1,2,3,4,5,8)]
aux <- as.data.frame(aux)
aux[is.na(aux)]<- 0
aux$countryName <- as.factor(aux$countryName)


dataframeTotal<- aux %>%
  select(-c(date,countryName))%>%
  summarise_all(sum)

NewCases_tbl <- 
  aux %>% 
  
  group_by(countryName) %>%
  mutate(
    recovered = case_when(
      is.na(recovered) ~ lag(recovered),
      TRUE ~ recovered
    ),
    confirmed = case_when(
      is.na(confirmed) ~ lag(confirmed),
      TRUE ~ confirmed
    ),
    death = case_when(
      is.na(death) ~ lag(death),
      TRUE ~ death
    ),
    Active = as.numeric(confirmed) - as.numeric(death) - as.numeric(recovered)
  ) %>%
  mutate(
    NewConfirmed = case_when(
      !is.na(lag(as.numeric(confirmed))) ~ abs(as.numeric(confirmed) - lag(as.numeric(confirmed))),
      TRUE ~ 0),
    NewRecovered = case_when(
      !is.na(lag(as.numeric(recovered))) ~ abs(as.numeric(recovered) - lag(as.numeric(recovered))),
      TRUE ~ 0),
    NewDeaths = case_when(
      !is.na(lag(as.numeric(death))) ~ abs(as.numeric(death) - lag(as.numeric(death))),
      TRUE ~ 0)
    
  ) %>% 
  ungroup()

NewCases_tbl$NewActive <- NewCases_tbl$NewConfirmed-NewCases_tbl$NewRecovered-NewCases_tbl$NewDeaths

NewCases_tbl$Taxa_activos <- round(NewCases_tbl$active/NewCases_tbl$confirmed,2)
NewCases_tbl$Taxa_recuperados <- round(NewCases_tbl$recovered/NewCases_tbl$confirmed,2)
NewCases_tbl$Taxa_mortes <- round(NewCases_tbl$death/NewCases_tbl$confirmed,2)
#
NewCases_tbl$Taxa_novos_activos <- round(NewCases_tbl$NewActive/NewCases_tbl$NewConfirmed,2)
NewCases_tbl$Taxa_novos_recuperados <- round(NewCases_tbl$NewRecovered/NewCases_tbl$NewConfirmed,2)
NewCases_tbl$Taxa_novas_mortes <- round(NewCases_tbl$NewDeaths/NewCases_tbl$NewConfirmed,2)
#
NewCases_tbl$Novos_confirmados <- round(NewCases_tbl$NewConfirmed,2)
NewCases_tbl$Novos_activos <- round(NewCases_tbl$NewActive,2)
NewCases_tbl$Novos_recuperados <- round(NewCases_tbl$NewRecovered,2)
NewCases_tbl$Novas_mortes <- round(NewCases_tbl$NewDeaths,2)


