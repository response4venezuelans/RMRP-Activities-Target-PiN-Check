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

library(readxl)
library(dplyr)
library(writexl)

# === LOAD AND PREPARE SUBMISSIONS 2025 ===

submissions_cr_2025 <- read_excel("xxx/org_submissions_template.xlsx", sheet = "2025_direct_assistance") %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  rowwise() %>%
  mutate(
    prop_girls = girls / sum(girls, boys, women, men, na.rm = TRUE),
    prop_boys  = boys  / sum(girls, boys, women, men, na.rm = TRUE),
    prop_women = women / sum(girls, boys, women, men, na.rm = TRUE),
    prop_men   = men   / sum(girls, boys, women, men, na.rm = TRUE),
    total = total_dest + total_hc + total_move,
    girls_total = round(prop_girls * total),
    boys_total  = round(prop_boys  * total),
    women_total = round(prop_women * total),
    men_total   = round(prop_men   * total),
    girls_dest  = round(prop_girls * total_dest),
    boys_dest   = round(prop_boys  * total_dest),
    women_dest  = round(prop_women * total_dest),
    men_dest    = round(prop_men   * total_dest),
    girls_hc    = round(prop_girls * total_hc),
    boys_hc     = round(prop_boys  * total_hc),
    women_hc    = round(prop_women * total_hc),
    men_hc      = round(prop_men   * total_hc),
    girls_move  = round(prop_girls * total_move),
    boys_move   = round(prop_boys  * total_move),
    women_move  = round(prop_women * total_move),
    men_move    = round(prop_men   * total_move)
  ) %>%
  ungroup() %>%
  mutate(
    sector = recode(sector,
      "Agua, Saneamiento e Higiene" = "WASH",
      "Alojamiento" = "Shelter",
      "Educación" = "Education",
      "Seguridad" = "Food Security",
      "Transferencias Monetarias Multipropósito (MPC)" = "Multipurpose Cash Assistance (MPC)",
      "Integración" = "Integration",
      "Protección (General)" = "Protection (General)",
      "Protección (Protección de la Infancia)" = "Protection (Child Protection)",
      "Salud" = "Health",
      "Protección (Trata y Tráfico de Personas)" = "Protection (Human Trafficking and Smuggling)",
      "Protección (VBG)" = "Protection (GBV)"
    )
  )


# === PIN CHECK 2025 ===

pin_check_costa_rica_2025 <- read_excel("XXX/pin_check_costa_rica.xlsx", sheet = "2025") %>%
  rowwise() %>%
  mutate(
    pin_girls_move = sum(pin_girls_move_ven, pin_girls_move_nven, na.rm = TRUE),
    pin_boys_move  = sum(pin_boys_move_ven,  pin_boys_move_nven,  na.rm = TRUE),
    pin_women_move = sum(pin_women_move_ven, pin_women_move_nven, na.rm = TRUE),
    pin_men_move   = sum(pin_men_move_ven,   pin_men_move_nven,   na.rm = TRUE),
    pin_total_move = sum(pin_total_move_ven, pin_total_move_nven, na.rm = TRUE),
    pin_men_move = ifelse(
      pin_total_move != (pin_girls_move + pin_boys_move + pin_women_move + pin_men_move),
      pin_men_move - ((pin_girls_move + pin_boys_move + pin_women_move + pin_men_move) - pin_total_move),
      pin_men_move
    )
  ) %>%
  ungroup()

pin_cr_2025 <- pin_check_costa_rica_2025 %>%
  filter(sector != "Intersector") %>%
  select(sector,
         starts_with("pin_girls"), starts_with("pin_boys"),
         starts_with("pin_women"), starts_with("pin_men"),
         starts_with("pin_total"))

# === ACTIVITY CHECKS 2025 ===

activities_pin_cr_2025 <- merge(submissions_cr_2025, pin_cr_2025, by = "sector", all.x = TRUE) %>%
  rowwise() %>%
  mutate(across(c(
    total_dest, girls_dest, boys_dest, women_dest, men_dest,
    total_hc, girls_hc, boys_hc, women_hc, men_hc,
    total_move, girls_move, boys_move, women_move, men_move
  ), ~ replace_na(., 0))) %>%
  mutate(
    dest_flag = ifelse(total_dest > pin_total_dest, "Review", "OK"),
    girls_dest_flag = ifelse(girls_dest > pin_girls_dest, "Review", "OK"),
    boys_dest_flag = ifelse(boys_dest > pin_boys_dest, "Review", "OK"),
    women_dest_flag = ifelse(women_dest > pin_women_dest, "Review", "OK"),
    men_dest_flag = ifelse(men_dest > pin_men_dest, "Review", "OK"),
    
    hc_flag = ifelse(total_hc > pin_total_hc, "Review", "OK"),
    girls_hc_flag = ifelse(girls_hc > pin_girls_hc, "Review", "OK"),
    boys_hc_flag = ifelse(boys_hc > pin_boys_hc, "Review", "OK"),
    women_hc_flag = ifelse(women_hc > pin_women_hc, "Review", "OK"),
    men_hc_flag = ifelse(men_hc > pin_men_hc, "Review", "OK"),
    
    move_flag = ifelse(total_move > pin_total_move, "Review", "OK"),
    girls_move_flag = ifelse(girls_move > pin_girls_move, "Review", "OK"),
    boys_move_flag = ifelse(boys_move > pin_boys_move, "Review", "OK"),
    women_move_flag = ifelse(women_move > pin_women_move, "Review", "OK"),
    men_move_flag = ifelse(men_move > pin_men_move, "Review", "OK"),
    
    review_comment = ifelse(any(c_across(ends_with("_flag")) == "Review"), "Review required", "No review needed")
  ) %>%
  ungroup()

write_xlsx(activities_pin_cr_2025, "xxx/activities_pin_cr_2025.xlsx")





