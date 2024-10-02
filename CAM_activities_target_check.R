library(tidyverse)
library(ggalluvial)
library(scales)
library(glue)
library(here)
library(sf)
library(summarytools)
library(leaflet)
library(gtsummary)
library(cowplot)
library(data.table)
library("readxl")
library(ggplot2)
library(writexl)

submissions_cr_2025 <- read_excel("xxx/org_submissions_template.xlsx", 
                                  sheet = "2025_direct_assistance")%>%
  mutate(across(everything(), ~ replace(., is.na(.), 0)))


# Costa Rica, 2025

# proportion girls, boys, women and men 

submissions_cr_2025<- submissions_cr_2025 %>% 
  rowwise() %>% 
  mutate(prop_girls = girls/sum(girls, boys, women, men, na.rm = TRUE),
         prop_boys = boys/sum(girls, boys, women, men, na.rm = TRUE),
         prop_women = women/sum(girls, boys, women, men, na.rm = TRUE),
         prop_men = men/sum(girls, boys, women, men, na.rm = TRUE)) %>% 
  ungroup()%>%
  
  # calculate proportion of sex and age by population group
  rowwise() %>% 
  mutate(total = total_dest+total_hc+total_move,
         girls_total = as.integer(round(prop_girls*total,0)),
         boys_total = as.integer(round(prop_boys*total,0)),
         women_total = as.integer(round(prop_women*total,0)),
         men_total = as.integer(round(prop_men*total,0)),
         girls_dest= as.integer(round(prop_girls*total_dest,0)),
         boys_dest = as.integer(round(prop_boys*total_dest,0)),
         women_dest = as.integer(round(prop_women*total_dest,0)),
         men_dest = as.integer(round(prop_men*total_dest,0)),
         girls_hc = as.integer(round(prop_girls*total_hc,0)),
         boys_hc = as.integer(round(prop_boys*total_hc,0)),
         women_hc = as.integer(round(prop_women*total_hc,0)),
         men_hc = as.integer(round(prop_men*total_hc,0)),
         girls_move = as.integer(round(prop_girls*total_move,0)),
         boys_move = as.integer(round(prop_boys*total_move,0)),
         women_move = as.integer(round(prop_women*total_move,0)),
         men_move = as.integer(round(prop_men*total_move,0)),
  ) %>%
  ungroup()%>%
  mutate(sector = ifelse(sector == "Agua, Saneamiento e Higiene", "WASH",sector))%>%
  mutate(sector = ifelse(sector == "Alojamiento", "Shelter",sector))%>%
  mutate(sector = ifelse(sector == "Educación", "Education",sector))%>%
  mutate(sector = ifelse(sector == "Seguridad", "Food Security",sector))%>%
  mutate(sector = ifelse(sector == "Transferencias Monetarias Multipropósito (MPC)", "Multipurpose Cash Assistance (MPC)",sector))%>%
  mutate(sector = ifelse(sector == "Integración", "Integration",sector))%>%
  mutate(sector = ifelse(sector == "Protección (General)", "Protection (General)",sector))%>%
  mutate(sector = ifelse(sector == "Protección (Protección de la Infancia)", "Protection (Child Protection)",sector))%>%
  mutate(sector = ifelse(sector == "Salud", "Health",sector))%>%
  mutate(sector = ifelse(sector == "Protección (Trata y Tráfico de Personas)", "Protection (Human Trafficking and Smuggling)",sector))%>%
  mutate(sector = ifelse(sector == "Protección (VBG)", "Protection (GBV)",sector))


#Pin 2025

pin_check_costa_rica_2025 <- read_excel("XXX/pin_check_costa_rica.xlsx", 
                                        sheet = "2025")%>%
  #calculate overall on the move numbers (venezuela and other nationalities)
  rowwise() %>% 
  mutate(pin_girls_move = sum(pin_girls_move_ven, pin_girls_move_nven, na.rm = TRUE),
         pin_boys_move = sum(pin_boys_move_ven, pin_boys_move_nven, na.rm = TRUE),
         pin_men_move = sum(pin_men_move_ven, pin_men_move_nven, na.rm = TRUE),
         pin_women_move = sum(pin_women_move_ven, pin_women_move_nven, na.rm = TRUE),
         pin_total_move = sum(pin_total_move_ven, pin_total_move_nven, na.rm = TRUE),
         # Adjust men_move if total_move is not equal to the sum
         pin_men_move = ifelse(
           pin_total_move != (pin_girls_move + pin_boys_move + pin_men_move + pin_women_move),
           pin_men_move - ((pin_girls_move + pin_boys_move + pin_men_move + pin_women_move) - pin_total_move),
           pin_men_move))%>%
  ungroup()


pin_check_costa_rica_2025 %>%
  filter(sector!="Intersector")%>%
  select(sector, pin_girls, pin_boys, pin_women,pin_men, pin_total, 
         pin_girls_dest, pin_boys_dest, pin_women_dest, pin_men_dest, pin_total_dest, 
         pin_girls_hc, pin_boys_hc, pin_women_hc, pin_men_hc, pin_total_hc,
         pin_girls_move, pin_boys_move, pin_women_move, pin_men_move, pin_total_move)

activities_pin_cr_2025 = merge(x=submissions_cr_2025,y=pin_cr_2025, by = c ("sector"), all.x=TRUE)%>%
  mutate(
    dest_flag = case_when(total_dest > pin_total_dest~ 'Review',
                          TRUE ~ 'OK'))%>%
  mutate(
    girls_dest_flag = case_when(girls_dest > pin_girls_dest~ 'Review',
                                TRUE ~ 'OK'))%>%
  mutate(
    boys_dest_flag = case_when(boys_dest > pin_boys_dest~ 'Review',
                               TRUE ~ 'OK'))%>%
  mutate(
    women_dest_flag = case_when(women_dest > pin_women_dest~ 'Review',
                                TRUE ~ 'OK'))%>%
  mutate(
    men_dest_flag = case_when(men_dest > pin_men_dest~ 'Review',
                              TRUE ~ 'OK'))%>%
  mutate(
    hc_flag = case_when(total_hc > pin_total_hc~ 'Review',
                        TRUE ~ 'OK'))%>%
  mutate(
    girls_hc_flag = case_when(girls_hc > pin_girls_hc~ 'Review',
                              TRUE ~ 'OK'))%>%
  mutate(
    boys_hc_flag = case_when(boys_hc > pin_boys_hc~ 'Review',
                             TRUE ~ 'OK'))%>%
  mutate(
    women_hc_flag = case_when(women_hc > pin_women_hc~ 'Review',
                              TRUE ~ 'OK'))%>%
  mutate(
    men_hc_flag = case_when(men_hc > pin_men_hc~ 'Review',
                            TRUE ~ 'OK'))%>%
  mutate(
    move_flag = case_when(total_move > pin_total_move~ 'Review',
                          TRUE ~ 'OK'))%>%
  mutate(
    girls_move_flag = case_when(girls_move > pin_girls_move~ 'Review',
                                TRUE ~ 'OK'))%>%
  mutate(
    boys_move_flag = case_when(boys_move > pin_boys_move~ 'Review',
                               TRUE ~ 'OK'))%>%
  mutate(
    women_move_flag = case_when(women_move > pin_women_move~ 'Review',
                                TRUE ~ 'OK'))%>%
  mutate(
    men_move_flag = case_when(men_move > pin_men_move~ 'Review',
                              TRUE ~ 'OK'))%>%

# New variable to add review comment if any flag is "Review"
mutate(review_comment = case_when(
  dest_flag == 'Review' | girls_dest_flag == 'Review' | boys_dest_flag == 'Review' |
    women_dest_flag == 'Review' | men_dest_flag == 'Review' |
    hc_flag == 'Review' | girls_hc_flag == 'Review' | boys_hc_flag == 'Review' |
    women_hc_flag == 'Review' | men_hc_flag == 'Review' |
    move_flag == 'Review' | girls_move_flag == 'Review' | boys_move_flag == 'Review' |
    women_move_flag == 'Review' | men_move_flag == 'Review' ~ 'Review required',
  TRUE ~ 'No review needed'))

write_xlsx(activities_pin_cr_2025,"xxX/activities_pin_cr_2025.xlsx")


# 2026
pin_check_costa_rica_2026 <- read_excel("XXX/pin_check_costa_rica.xlsx", 
                                        sheet = "2026")

pin_gender_age_proportions <- pin_check_costa_rica_2025 %>%
  rowwise() %>%
  mutate(
    prop_pin_girls = pin_girls / sum(pin_girls, pin_boys, pin_women, pin_men, na.rm = TRUE),
    prop_pin_boys = pin_boys / sum(pin_girls, pin_boys, pin_women, pin_men, na.rm = TRUE),
    prop_pin_women = pin_women / sum(pin_girls, pin_boys, pin_women, pin_men, na.rm = TRUE),
    prop_pin_men = pin_men / sum(pin_girls, pin_boys, pin_women, pin_men, na.rm = TRUE),
    prop_pin_girls_dest = pin_girls_dest / sum(pin_girls_dest, pin_boys_dest, pin_women_dest, pin_men_dest, na.rm = TRUE),
    prop_pin_boys_dest = pin_boys_dest / sum(pin_girls_dest, pin_boys_dest, pin_women_dest, pin_men_dest, na.rm = TRUE),
    prop_pin_women_dest = pin_women_dest / sum(pin_girls_dest, pin_boys_dest, pin_women_dest, pin_men_dest, na.rm = TRUE),
    prop_pin_men_dest = pin_men_dest / sum(pin_girls_dest, pin_boys_dest, pin_women_dest, pin_men_dest, na.rm = TRUE),
    prop_pin_girls_move_ven = pin_girls_move_ven / sum(pin_girls_move_ven, pin_boys_move_ven, pin_women_move_ven, pin_men_move_ven, na.rm = TRUE),
    prop_pin_boys_move_ven = pin_boys_move_ven / sum(pin_girls_move_ven, pin_boys_move_ven, pin_women_move_ven, pin_men_move_ven, na.rm = TRUE),
    prop_pin_women_move_ven = pin_women_move_ven / sum(pin_girls_move_ven, pin_boys_move_ven, pin_women_move_ven, pin_men_move_ven, na.rm = TRUE),
    prop_pin_men_move_ven = pin_men_move_ven / sum(pin_girls_move_ven, pin_boys_move_ven, pin_women_move_ven, pin_men_move_ven, na.rm = TRUE),
    prop_pin_girls_move_nven = pin_girls_move_nven / sum(pin_girls_move_nven, pin_boys_move_nven, pin_women_move_nven, pin_men_move_nven, na.rm = TRUE),
    prop_pin_boys_move_nven = pin_boys_move_nven / sum(pin_girls_move_nven, pin_boys_move_nven, pin_women_move_nven, pin_men_move_nven, na.rm = TRUE),
    prop_pin_women_move_nven = pin_women_move_nven / sum(pin_girls_move_nven, pin_boys_move_nven, pin_women_move_nven, pin_men_move_nven, na.rm = TRUE),
    prop_pin_men_move_nven = pin_men_move_nven / sum(pin_girls_move_nven, pin_boys_move_nven, pin_women_move_nven, pin_men_move_nven, na.rm = TRUE),
    prop_pin_girls_hc = pin_girls_hc / sum(pin_girls_hc, pin_boys_hc, pin_women_hc, pin_men_hc, na.rm = TRUE),
    prop_pin_boys_hc = pin_boys_hc / sum(pin_girls_hc, pin_boys_hc, pin_women_hc, pin_men_hc, na.rm = TRUE),
    prop_pin_women_hc = pin_women_hc / sum(pin_girls_hc, pin_boys_hc, pin_women_hc, pin_men_hc, na.rm = TRUE),
    prop_pin_men_hc = pin_men_hc / sum(pin_girls_hc, pin_boys_hc, pin_women_hc, pin_men_hc, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(starts_with("prop_pin"), contains("sector"))%>%
  left_join(pin_check_costa_rica_2026, by = "sector")

#calculate age gender disaggregations pin 2026

pin_cr_2026 <- pin_gender_age_proportions %>%
  rowwise() %>%
  mutate(pin_girls = as.integer(round(prop_pin_girls * pin_total, 0)),
         pin_boys = as.integer(round(prop_pin_boys * pin_total, 0)),
         pin_women = as.integer(round(prop_pin_women * pin_total, 0)),
         pin_men = as.integer(round(prop_pin_men * pin_total, 0)),
         pin_girls_dest = as.integer(round(prop_pin_girls_dest * pin_total_dest, 0)),
         pin_boys_dest = as.integer(round(prop_pin_boys_dest * pin_total_dest, 0)),
         pin_women_dest = as.integer(round(prop_pin_women_dest * pin_total_dest, 0)),
         pin_men_dest = as.integer(round(prop_pin_men_dest * pin_total_dest, 0)),
         pin_girls_hc = as.integer(round(prop_pin_girls_hc * pin_total_hc, 0)),
         pin_boys_hc = as.integer(round(prop_pin_boys_hc * pin_total_hc, 0)),
         pin_women_hc = as.integer(round(prop_pin_women_hc * pin_total_hc, 0)),
         pin_men_hc = as.integer(round(prop_pin_men_hc * pin_total_hc, 0)),
         pin_girls_move_ven = as.integer(round(prop_pin_girls_move_ven * pin_total_move_ven, 0)),
         pin_boys_move_ven = as.integer(round(prop_pin_boys_move_ven * pin_total_move_ven, 0)),
         pin_women_move_ven = as.integer(round(prop_pin_women_move_ven * pin_total_move_ven, 0)),
         pin_men_move_ven = as.integer(round(prop_pin_men_move_ven * pin_total_move_ven, 0)),
         pin_girls_move_nven = as.integer(round(prop_pin_girls_move_nven * pin_total_move_nven, 0)),
         pin_boys_move_nven = as.integer(round(prop_pin_boys_move_nven * pin_total_move_nven, 0)),
         pin_women_move_nven = as.integer(round(prop_pin_women_move_nven * pin_total_move_nven, 0)),
         pin_men_move_nven = as.integer(round(prop_pin_men_move_nven * pin_total_move_nven, 0)),
         # Adjust men if total (destination, move, hc, total) pin is not equal to the sum
         pin_men = ifelse(pin_total != 
                            (pin_girls + pin_boys + pin_men + pin_women),
                          pin_men - 
                            ((pin_girls + pin_boys + pin_men + pin_women) - pin_total),
                          pin_men),
         # Adjust men_move_ven if total (destination, move, hc, total) pin is not equal to the sum
         pin_men_move_ven = ifelse(pin_total_move_ven != 
                                     (pin_girls_move_ven + pin_boys_move_ven + pin_men_move_ven + pin_women_move_ven),
                                   pin_men_move_ven - 
                                     ((pin_girls_move_ven + pin_boys_move_ven + pin_men_move_ven + pin_women_move_ven) - pin_total_move_ven),
                                   pin_men_move_ven),
         # Adjust men_move_nven if total (destination, move, hc, total) pin is not equal to the sum
         pin_men_move_nven = ifelse(pin_total_move_nven != 
                                      (pin_girls_move_nven + pin_boys_move_nven + pin_men_move_nven + pin_women_move_nven),
                                    pin_men_move_nven - 
                                      ((pin_girls_move_nven + pin_boys_move_nven + pin_men_move_nven + pin_women_move_nven) - pin_total_move_nven),
                                    pin_men_move_nven),
         # Adjust men_dest if total (destination) pin is not equal to the sum
         pin_men_dest = ifelse(pin_total_dest != 
                                 (pin_girls_dest + pin_boys_dest + pin_men_dest + pin_women_dest),
                               pin_men_dest - 
                                 ((pin_girls_dest + pin_boys_dest + pin_men_dest + pin_women_dest) - pin_total_dest),
                               pin_men_dest),
         # Adjust men_hc if total (host community) pin is not equal to the sum
         pin_men_hc = ifelse(pin_total_hc != 
                               (pin_girls_hc + pin_boys_hc + pin_men_hc + pin_women_hc),
                             pin_men_hc - 
                               ((pin_girls_hc + pin_boys_hc + pin_men_hc + pin_women_hc) - pin_total_hc),
                             pin_men_hc)) %>%
  #calculate overall on the move numbers (venezuela and other nationalities)
  mutate(pin_girls_move = sum(pin_girls_move_ven, pin_girls_move_nven, na.rm = TRUE),
         pin_boys_move = sum(pin_boys_move_ven, pin_boys_move_nven, na.rm = TRUE),
         pin_men_move = sum(pin_men_move_ven, pin_men_move_nven, na.rm = TRUE),
         pin_women_move = sum(pin_women_move_ven, pin_women_move_nven, na.rm = TRUE),
         pin_total_move = sum(pin_total_move_ven, pin_total_move_nven, na.rm = TRUE),
         # Adjust men_move if total_move is not equal to the sum
         pin_men_move = ifelse(
           pin_total_move != (pin_girls_move + pin_boys_move + pin_men_move + pin_women_move),
           pin_men_move - ((pin_girls_move + pin_boys_move + pin_men_move + pin_women_move) - pin_total_move),
           pin_men_move))%>%
  ungroup()

#Submissions 2026

submissions_cr_2026 <- read_excel("XXX/org_submissions_template.xlsx", 
                                  sheet = "2026_direct_assistance")%>%
  mutate(across(everything(), ~ replace(., is.na(.), 0))) 

submissions_cr_2026<- submissions_cr_2026 %>% 
  rowwise() %>% 
  mutate(prop_girls = girls/sum(girls, boys, women, men, na.rm = TRUE),
         prop_boys = boys/sum(girls, boys, women, men, na.rm = TRUE),
         prop_women = women/sum(girls, boys, women, men, na.rm = TRUE),
         prop_men = men/sum(girls, boys, women, men, na.rm = TRUE)) %>% 
  ungroup()%>%
  
  # calculate proportion of sex and age by population group
  rowwise() %>% 
  mutate(total = total_dest+total_hc+total_move,
         girls_total = as.integer(round(prop_girls*total,0)),
         boys_total = as.integer(round(prop_boys*total,0)),
         women_total = as.integer(round(prop_women*total,0)),
         men_total = as.integer(round(prop_men*total,0)),
         girls_dest= as.integer(round(prop_girls*total_dest,0)),
         boys_dest = as.integer(round(prop_boys*total_dest,0)),
         women_dest = as.integer(round(prop_women*total_dest,0)),
         men_dest = as.integer(round(prop_men*total_dest,0)),
         girls_hc = as.integer(round(prop_girls*total_hc,0)),
         boys_hc = as.integer(round(prop_boys*total_hc,0)),
         women_hc = as.integer(round(prop_women*total_hc,0)),
         men_hc = as.integer(round(prop_men*total_hc,0)),
         girls_move = as.integer(round(prop_girls*total_move,0)),
         boys_move = as.integer(round(prop_boys*total_move,0)),
         women_move = as.integer(round(prop_women*total_move,0)),
         men_move = as.integer(round(prop_men*total_move,0)),
  ) %>%
  ungroup()%>%
  mutate(sector = ifelse(sector == "Agua, Saneamiento e Higiene", "WASH",sector))%>%
  mutate(sector = ifelse(sector == "Alojamiento", "Shelter",sector))%>%
  mutate(sector = ifelse(sector == "Educación", "Education",sector))%>%
  mutate(sector = ifelse(sector == "Seguridad", "Food Security",sector))%>%
  mutate(sector = ifelse(sector == "Transferencias Monetarias Multipropósito (MPC)", "Multipurpose Cash Assistance (MPC)",sector))%>%
  mutate(sector = ifelse(sector == "Integración", "Integration",sector))%>%
  mutate(sector = ifelse(sector == "Protección (General)", "Protection (General)",sector))%>%
  mutate(sector = ifelse(sector == "Protección (Protección de la Infancia)", "Protection (Child Protection)",sector))%>%
  mutate(sector = ifelse(sector == "Salud", "Health",sector))%>%
  mutate(sector = ifelse(sector == "Protección (Trata y Tráfico de Personas)", "Protection (Human Trafficking and Smuggling)",sector))%>%
  mutate(sector = ifelse(sector == "Protección (VBG)", "Protection (GBV)",sector))

#Check Activities targets 

pin_cr_2026<-pin_cr_2026 %>%
  filter(sector!="Intersector")%>%
  select(sector, pin_girls, pin_boys, pin_women,pin_men, pin_total, 
         pin_girls_dest, pin_boys_dest, pin_women_dest, pin_men_dest, pin_total_dest, 
         pin_girls_hc, pin_boys_hc, pin_women_hc, pin_men_hc, pin_total_hc,
         pin_girls_move, pin_boys_move, pin_women_move, pin_men_move, pin_total_move)

activities_pin_cr_2026 = merge(x=submissions_cr_2026,y=pin_cr_2026, by = c ("sector"), all.x=TRUE)%>%
  mutate(
    dest_flag = case_when(total_dest > pin_total_dest~ 'Review',
                          TRUE ~ 'OK'))%>%
  mutate(
    girls_dest_flag = case_when(girls_dest > pin_girls_dest~ 'Review',
                                TRUE ~ 'OK'))%>%
  mutate(
    boys_dest_flag = case_when(boys_dest > pin_boys_dest~ 'Review',
                               TRUE ~ 'OK'))%>%
  mutate(
    women_dest_flag = case_when(women_dest > pin_women_dest~ 'Review',
                                TRUE ~ 'OK'))%>%
  mutate(
    men_dest_flag = case_when(men_dest > pin_men_dest~ 'Review',
                              TRUE ~ 'OK'))%>%
  mutate(
    hc_flag = case_when(total_hc > pin_total_hc~ 'Review',
                        TRUE ~ 'OK'))%>%
  mutate(
    girls_hc_flag = case_when(girls_hc > pin_girls_hc~ 'Review',
                              TRUE ~ 'OK'))%>%
  mutate(
    boys_hc_flag = case_when(boys_hc > pin_boys_hc~ 'Review',
                             TRUE ~ 'OK'))%>%
  mutate(
    women_hc_flag = case_when(women_hc > pin_women_hc~ 'Review',
                              TRUE ~ 'OK'))%>%
  mutate(
    men_hc_flag = case_when(men_hc > pin_men_hc~ 'Review',
                            TRUE ~ 'OK'))%>%
  mutate(
    move_flag = case_when(total_move > pin_total_move~ 'Review',
                          TRUE ~ 'OK'))%>%
  mutate(
    girls_move_flag = case_when(girls_move > pin_girls_move~ 'Review',
                                TRUE ~ 'OK'))%>%
  mutate(
    boys_move_flag = case_when(boys_move > pin_boys_move~ 'Review',
                               TRUE ~ 'OK'))%>%
  mutate(
    women_move_flag = case_when(women_move > pin_women_move~ 'Review',
                                TRUE ~ 'OK'))%>%
  mutate(
    men_move_flag = case_when(men_move > pin_men_move~ 'Review',
                              TRUE ~ 'OK'))%>%

# New variable to add review comment if any flag is "Review"
mutate(review_comment = case_when(
  dest_flag == 'Review' | girls_dest_flag == 'Review' | boys_dest_flag == 'Review' |
    women_dest_flag == 'Review' | men_dest_flag == 'Review' |
    hc_flag == 'Review' | girls_hc_flag == 'Review' | boys_hc_flag == 'Review' |
    women_hc_flag == 'Review' | men_hc_flag == 'Review' |
    move_flag == 'Review' | girls_move_flag == 'Review' | boys_move_flag == 'Review' |
    women_move_flag == 'Review' | men_move_flag == 'Review' ~ 'Review required',
  TRUE ~ 'No review needed'))


write_xlsx(activities_pin_cr_2026,"XXX/activities_pin_cr_2026.xlsx")


