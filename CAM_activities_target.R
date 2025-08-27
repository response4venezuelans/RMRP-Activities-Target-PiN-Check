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



# TARGET 2025 Costa Rica

library(dplyr)
library(writexl)

activities_cr_2025 <- activities_cr_2025_2026 %>%
  # Select relevant columns
  select(sector, girls, boys, men, women, total, total_dest, total_move, total_hc, ID) %>%
  
  # Calculate proportions and distribute numbers by population group
  rowwise() %>%
  mutate(
    total_people = sum(c_across(girls:men), na.rm = TRUE),
    
    prop_girls = girls / total_people,
    prop_boys = boys / total_people,
    prop_women = women / total_people,
    prop_men = men / total_people,
    
    girls_dest = round(prop_girls * total_dest),
    boys_dest = round(prop_boys * total_dest),
    women_dest = round(prop_women * total_dest),
    men_dest = round(prop_men * total_dest),
    
    girls_move = round(prop_girls * total_move),
    boys_move = round(prop_boys * total_move),
    women_move = round(prop_women * total_move),
    men_move = round(prop_men * total_move),
    
    girls_hc = round(prop_girls * total_hc),
    boys_hc = round(prop_boys * total_hc),
    women_hc = round(prop_women * total_hc),
    men_hc = round(prop_men * total_hc)
  ) %>%
  ungroup() %>%
  
  # Aggregate and apply special conditions for some sectors
  group_by(sector) %>%
  summarise(
    ID_concat = paste(ID, collapse = " "),
    
    girls_dest = sum(girls_dest, na.rm = TRUE),
    boys_dest = ifelse(sector == "Protección (Protección de la Infancia)",
                       max(boys_dest, na.rm = TRUE), sum(boys_dest, na.rm = TRUE)),
    women_dest = ifelse(sector == "Salud",
                        max(women_dest, na.rm = TRUE), sum(women_dest, na.rm = TRUE)),
    men_dest = sum(men_dest, na.rm = TRUE),
    total_dest = sum(total_dest, na.rm = TRUE),
    
    girls_move = max(girls_move, na.rm = TRUE),
    boys_move = max(boys_move, na.rm = TRUE),
    women_move = max(women_move, na.rm = TRUE),
    men_move = max(men_move, na.rm = TRUE),
    total_move = max(total_move, na.rm = TRUE),
    
    girls_hc = sum(girls_hc, na.rm = TRUE),
    boys_hc = sum(boys_hc, na.rm = TRUE),
    women_hc = sum(women_hc, na.rm = TRUE),
    men_hc = sum(men_hc, na.rm = TRUE),
    total_hc = sum(total_hc, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # Rename sectors
  mutate(
    sector = case_when(
      sector == "Agua, Saneamiento e Higiene" ~ "WASH",
      sector == "Alojamiento" ~ "Shelter",
      sector == "Educación" ~ "Education",
      sector == "Integración" ~ "Integration",
      sector == "Nutrición" ~ "Nutrition",
      sector == "Protección (General)" ~ "Protection (General)",
      sector == "Protección (Protección de la Infancia)" ~ "Protection (Child Protection)",
      sector == "Protección (VBG)" ~ "Protection (GBV)",
      sector == "Salud" ~ "Health",
      sector == "Seguridad Alimentaria" ~ "Food Security",
      sector == "Transporte Humanitario" ~ "Humanitarian transportation",
      sector == "Transferencias Monetarias Multipropósito (MPC)" ~ "Multipurpose Cash Assistance (MPC)",
      TRUE ~ sector
    ),
    
    sector_order = case_when(
      sector == "Education" ~ 1,
      sector == "Food Security" ~ 2,
      sector == "Health" ~ 3,
      sector == "Humanitarian transportation" ~ 4,
      sector == "Integration" ~ 5,
      sector == "Nutrition" ~ 6,
      sector == "Protection (Child Protection)" ~ 7,
      sector == "Protection (GBV)" ~ 8,
      sector == "Protection (General)" ~ 9,
      sector == "Shelter" ~ 10,
      sector == "WASH" ~ 11,
      sector == "Multipurpose Cash Assistance (MPC)" ~ 12,
      TRUE ~ NA_real_
    )
  ) %>%
  arrange(sector_order)

# Save to Excel
write_xlsx(activities_cr_2025, "path")



# TARGET 2026 Costa Rica

library(dplyr)
library(writexl)

# === Define lookup vectors for sector renaming and ordering ===
sector_rename <- c(
  "Agua, Saneamiento e Higiene" = "WASH",
  "Alojamiento" = "Shelter",
  "Educación" = "Education",
  "Integración" = "Integration",
  "Nutrición" = "Nutrition",
  "Protección (General)" = "Protection (General)",
  "Protección (Protección de la Infancia)" = "Protection (Child Protection)",
  "Protección (VBG)" = "Protection (GBV)",
  "Salud" = "Health",
  "Seguridad Alimentaria" = "Food Security",
  "Transporte Humanitario" = "Humanitarian transportation",
  "Transferencias Monetarias Multipropósito (MPC)" = "Multipurpose Cash Assistance (MPC)"
)

sector_order <- c(
  "Education" = 1,
  "Food Security" = 2,
  "Health" = 3,
  "Humanitarian transportation" = 4,
  "Integration" = 5,
  "Nutrition" = 6,
  "Protection (Child Protection)" = 7,
  "Protection (GBV)" = 8,
  "Protection (General)" = 9,
  "Shelter" = 10,
  "WASH" = 11,
  "Multipurpose Cash Assistance (MPC)" = 12
)


# === calculate proportions and disaggregations ===
calculate_disaggregations <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      prop_girls = girls / sum(girls, boys, women, men, na.rm = TRUE),
      prop_boys  = boys  / sum(girls, boys, women, men, na.rm = TRUE),
      prop_women = women / sum(girls, boys, women, men, na.rm = TRUE),
      prop_men   = men   / sum(girls, boys, women, men, na.rm = TRUE),
      
      girls_dest = as.integer(round(prop_girls * total_dest, 0)),
      boys_dest  = as.integer(round(prop_boys * total_dest, 0)),
      women_dest = as.integer(round(prop_women * total_dest, 0)),
      men_dest   = as.integer(round(prop_men * total_dest, 0)),
      
      girls_move = as.integer(round(prop_girls * total_move, 0)),
      boys_move  = as.integer(round(prop_boys * total_move, 0)),
      women_move = as.integer(round(prop_women * total_move, 0)),
      men_move   = as.integer(round(prop_men * total_move, 0)),
      
      girls_hc   = as.integer(round(prop_girls * total_hc, 0)),
      boys_hc    = as.integer(round(prop_boys * total_hc, 0)),
      women_hc   = as.integer(round(prop_women * total_hc, 0)),
      men_hc     = as.integer(round(prop_men * total_hc, 0))
    ) %>%
    ungroup()
}


# === summarise by sector with optional max sectors ===
summarise_by_sector <- function(df, max_dest_sectors = character(), max_move_sectors = character()) {
  df %>%
    group_by(sector) %>%
    summarise(
      ID_concat   = paste(ID, collapse = " "),
      
      girls_dest  = ifelse(sector %in% max_dest_sectors, max(girls_dest, na.rm = TRUE), sum(girls_dest, na.rm = TRUE)),
      boys_dest   = ifelse(sector %in% max_dest_sectors, max(boys_dest, na.rm = TRUE), sum(boys_dest, na.rm = TRUE)),
      women_dest  = ifelse(sector %in% max_dest_sectors, max(women_dest, na.rm = TRUE), sum(women_dest, na.rm = TRUE)),
      men_dest    = ifelse(sector %in% max_dest_sectors, max(men_dest, na.rm = TRUE), sum(men_dest, na.rm = TRUE)),
      total_dest  = sum(total_dest, na.rm = TRUE),
      
      girls_move  = ifelse(sector %in% max_move_sectors, max(girls_move, na.rm = TRUE), sum(girls_move, na.rm = TRUE)),
      boys_move   = ifelse(sector %in% max_move_sectors, max(boys_move, na.rm = TRUE), sum(boys_move, na.rm = TRUE)),
      women_move  = ifelse(sector %in% max_move_sectors, max(women_move, na.rm = TRUE), sum(women_move, na.rm = TRUE)),
      men_move    = ifelse(sector %in% max_move_sectors, max(men_move, na.rm = TRUE), sum(men_move, na.rm = TRUE)),
      total_move  = sum(total_move, na.rm = TRUE),
      
      girls_hc    = sum(girls_hc, na.rm = TRUE),
      boys_hc     = sum(boys_hc, na.rm = TRUE),
      women_hc    = sum(women_hc, na.rm = TRUE),
      men_hc      = sum(men_hc, na.rm = TRUE),
      total_hc    = sum(total_hc, na.rm = TRUE)
    ) %>%
    ungroup()
}


# === rename, order, and arrange sectors ===
finalize_sectors <- function(df) {
  df %>%
    mutate(
      sector = sector_rename[sector] %||% sector,
      sector_order = sector_order[sector] %||% NA_real_
    ) %>%
    arrange(sector_order)
}


# === fully process dataset ===
process_targets <- function(df, max_dest_sectors = character(), max_move_sectors = character()) {
  df %>%
    select(sector, girls, boys, men, women, total, total_dest, total_move, total_hc, ID) %>%
    calculate_disaggregations() %>%
    summarise_by_sector(max_dest_sectors, max_move_sectors) %>%
    finalize_sectors()
}

activities_cr_2026 <- process_targets(
  df = activities_cr_2025_2026,
  max_dest_sectors = c("Protección (Protección de la Infancia)", "Salud")
)

activities_pa_2025 <- process_targets(
  df = activities_pa_2025_2026,
  max_dest_sectors = c("Protección (Protección de la Infancia)")
)

activities_pa_2026 <- process_targets(
  df = activities_pa_2025_2026,
  max_dest_sectors = c("Protección (Protección de la Infancia)")
)

activities_mx_2025 <- process_targets(
  df = activities_mx_2025_2026,
  max_dest_sectors = c("Alojamiento"),
  max_move_sectors = c("Agua, Saneamiento e Higiene")
)

activities_mx_2026 <- process_targets(
  df = activities_mx_2025_2026,
  max_dest_sectors = c("Alojamiento"),
  max_move_sectors = c("Agua, Saneamiento e Higiene")
)



write_xlsx(activities_cr_2026, "path/to/cr_2026.xlsx")
write_xlsx(activities_pa_2025, "path/to/pa_2025.xlsx")
write_xlsx(activities_pa_2026, "path/to/pa_2026.xlsx")
write_xlsx(activities_mx_2025, "path/to/mx_2025.xlsx")
write_xlsx(activities_mx_2026, "path/to/mx_2026.xlsx")




