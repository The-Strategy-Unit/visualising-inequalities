library(readxl)
library(tidyr)
library(tidyverse)

#Get the IMD Deciles for each GP Practice

gp_patients_by_lsoa <- read_excel(here::here("data", "patients_by_lsoa_to_gp.xlsx"))

imd_2019 <- read_excel(here::here("data", "imd_2019.xlsx"))

practice_to_icb_lookup <- read_excel(here::here("data", "gp-reg-pat-prac-map-jul22.xlsx"))


#appends LSOA IMD score to GPpatients_by_LSOA
gp_patients_by_lsoa<- gp_patients_by_lsoa%>% 
  left_join(select(imd_2019,`LSOA code (2011)`,`Index of Multiple Deprivation (IMD) Score`), 
            by = c("LSOA_CODE" = "LSOA code (2011)"))

#creates new field of LSOA IMDscore multiplied by the number of patients from that LSOA
gp_patients_by_lsoa<-gp_patients_by_lsoa%>%
  mutate(scoretimespopLSOA = `Index of Multiple Deprivation (IMD) Score`*`Number of Patients`)

#groups data by Practice and sums number of patients and IMD score
patientweighted_practice_imd<-gp_patients_by_lsoa%>%filter(!is.na(scoretimespopLSOA))%>%
  group_by (`PRACTICE_CODE`)%>% 
  summarise(totalIMD = sum(`scoretimespopLSOA`),Patients = sum(`Number of Patients`)) %>%
  arrange()%>%
  #then calculates the average IMD (total IMD score/total Patients) 
  mutate(averageIMD = `totalIMD`/`Patients`)%>%
  #then calculates IMDDecile 
  mutate(GP_IMDquantile = ntile(-`averageIMD`, 10))%>%
  #adds in organisational data
  left_join(select(practice_to_icb_lookup,`CCG_CODE`,`CCG_NAME`,`STP_CODE`,`STP_NAME`,
                   `COMM_REGION_NAME`,`ICB22ons`,`ICB22`,`ICB22NM`,`New Sub-ICB location name`,
                   `Reg22NM`,`PRACTICE_CODE`), 
            by = c("PRACTICE_CODE" = "PRACTICE_CODE"))

 # write.table(patientweighted_practice_imd, file = "patientweighted_practice_imd.csv", append = FALSE, quote = TRUE, sep = ",",
 #             eol = "\n", na = "NA", dec = ".", row.names = TRUE,
 #             col.names = TRUE, qmethod = c("escape", "double"),
 #             fileEncoding = "")
# 
#View(patientweighted_practice_imd)
