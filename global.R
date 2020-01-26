library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(tidyselect)
library(DT)
library(googleVis)
library(maps)
library(dplyr)
library(plotly)
library(lubridate)
library(data.table)
library(devtools)
library(dashboardthemes)

# reading in solar CSV
solar = read.csv("solar_data.csv", stringsAsFactors = FALSE)
solar$Date = as.Date(solar$Date)

# creating separate dataframes for gvisAnnotataionCharts by segment (utility, sector, installer)
ute_daily = solar %>% select(., Date, Utility, MW) %>% group_by(Date, Utility) %>% summarise(totalMW=sum(MW))

sec_daily = solar %>% select(., Date, Sector, MW) %>% group_by(Date, Sector) %>% summarise(totalMW=sum(MW))

inst_daily = solar %>% select(., Date, Installer, MW) %>% group_by(Date, Installer) %>% summarise(totalMW=sum(MW))
inst_daily = inst_daily %>% spread(Installer, totalMW)
inst_daily = inst_daily %>% select(., -V1)
inst_daily = inst_daily %>% gather(key="Installer", value="totalMW", RUN:VSLR, na.rm = TRUE)

# creating separate dataframes for gvisColumnCharts
qtrly_util = solar %>% group_by(Yr.Qtr, Utility) %>% summarise(totalMW=sum(MW)) %>% arrange(Yr.Qtr)
qtrly_util = qtrly_util %>% spread(Utility, totalMW)

qtrly_sect = solar %>% group_by(Yr.Qtr, Sector) %>% summarise(totalMW=sum(MW)) %>% arrange(Yr.Qtr)
qtrly_sect = qtrly_sect %>% spread(Sector, totalMW)

qtrly_inst = solar %>% group_by(Yr.Qtr, Installer) %>% summarise(totalMW=sum(MW)) %>% arrange(Yr.Qtr)
qtrly_inst = qtrly_inst %>% spread(Installer, totalMW)
qtrly_inst = qtrly_inst %>% select(., -V1)

# creating separate dataframes for gvisComboCharts
PGE_combo = qtrly_util %>% select(., Yr.Qtr, PGE)
PGE_combo = PGE_combo %>% rename(., MW = "PGE")
PGE_combo$YoY = c(rep(NA,4),(PGE_combo$MW[5:nrow(PGE_combo)]-PGE_combo$MW[1:(nrow(PGE_combo)-4)])/PGE_combo$MW[1:(nrow(PGE_combo)-4)])

SCE_combo = qtrly_util %>% select(., Yr.Qtr, SCE)
SCE_combo = SCE_combo %>% rename(., MW = "SCE")
SCE_combo$YoY = c(rep(NA,4),(SCE_combo$MW[5:nrow(SCE_combo)]-SCE_combo$MW[1:(nrow(SCE_combo)-4)])/SCE_combo$MW[1:(nrow(SCE_combo)-4)])

SDGE_combo = qtrly_util %>% select(., Yr.Qtr, SDGE)
SDGE_combo = SDGE_combo %>% rename(., MW = "SDGE")
SDGE_combo$YoY = c(rep(NA,4),(SDGE_combo$MW[5:nrow(SDGE_combo)]-SDGE_combo$MW[1:(nrow(SDGE_combo)-4)])/SDGE_combo$MW[1:(nrow(SDGE_combo)-4)])

TSLA_combo = qtrly_inst %>% select(., Yr.Qtr, TSLA)
TSLA_combo = TSLA_combo %>% rename(., MW = "TSLA")
TSLA_combo$YoY = c(rep(NA,4),(TSLA_combo$MW[5:nrow(TSLA_combo)]-TSLA_combo$MW[1:(nrow(TSLA_combo)-4)])/TSLA_combo$MW[1:(nrow(TSLA_combo)-4)])

VSLR_combo = qtrly_inst %>% select(., Yr.Qtr, VSLR)
VSLR_combo = VSLR_combo %>% rename(., MW = "VSLR")
VSLR_combo$YoY = c(rep(NA,4),(VSLR_combo$MW[5:nrow(VSLR_combo)]-VSLR_combo$MW[1:(nrow(VSLR_combo)-4)])/VSLR_combo$MW[1:(nrow(VSLR_combo)-4)])

RUN_combo = qtrly_inst %>% select(., Yr.Qtr, RUN)
RUN_combo = RUN_combo %>% rename(., MW = "RUN")
RUN_combo$YoY = c(rep(NA,4),(RUN_combo$MW[5:nrow(RUN_combo)]-RUN_combo$MW[1:(nrow(RUN_combo)-4)])/RUN_combo$MW[1:(nrow(RUN_combo)-4)])

SPWR_combo = qtrly_inst %>% select(., Yr.Qtr, SPWR)
SPWR_combo = SPWR_combo %>% rename(., MW = "SPWR")
SPWR_combo$YoY = c(rep(NA,4),(SPWR_combo$MW[5:nrow(SPWR_combo)]-SPWR_combo$MW[1:(nrow(SPWR_combo)-4)])/SPWR_combo$MW[1:(nrow(SPWR_combo)-4)])

# creating County dataframes for gvisGeoChart and gvisPieChart
counties = map_data("county")
ca_counties = counties %>% filter(., region == "california")
ca_counties = ca_counties %>% select(., -group, -order, -region)
ca_counties = ca_counties %>% rename(County = subregion, Lat = lat, Long = long)
ca_counties$County = str_to_title(ca_counties$County)
ca_counties = ca_counties %>% group_by(County) %>% summarise(Lat=mean(Lat), Long=mean(Long))
countyMW2019 = solar %>% select(., County, Year, MW) %>% 
  filter(., Year == "2019") %>% 
  group_by(County) %>% 
  summarise(MW=sum(MW))
ca_countiesMW2019 = left_join(ca_counties, countyMW2019, by = "County")
ca_countiesMW2019$LatLong = paste(ca_countiesMW2019$Lat, ca_countiesMW2019$Long, sep=":")
ca_countiesMW2019 = ca_countiesMW2019 %>% filter(., MW > 0)
ca_pie_2019 = ca_countiesMW2019 %>% select(., -Lat, -Long, -LatLong) %>% arrange(desc(MW))