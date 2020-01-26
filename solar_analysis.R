library(dplyr)
library(ggplot2)

data = read.csv("NEM_InterconnectionApplicationsDataset_2019-10-31.csv", stringsAsFactors = FALSE) # read in the original csv
solar = data %>% select(Application.Id, # only want these columns
                   Application.Status, 
                   Utility,
                   Service.City,
                   Service.County,
                   Service.Zip,
                   System.Size.AC,
                   Customer.Sector,
                   App.Approved.Date,
                   Third.Party.Owned,
                   Third.Party.Owned.Type,
                   Third.Party.Name,
                   Pace.Financed,
                   Electric.Vehicle,
                   Total.System.Cost,
                   Cost.Watt)
solar = filter(solar, Application.Status == 'Interconnected') # only want rows with this type of application (removes 'decommissioned' applications) 

# We need to fix the Third.Party.Name values
# third_party_df = solar %>% # creating temp dataframe to see what's
#  group_by(Third.Party.Name) %>% 
#  summarise(total.AC=sum(System.Size.AC)) %>% 
#  arrange(desc(total.AC))
# View(third_party_df)

SCTY_df = c("SolarCity",
            "SolarCity Corporation",
            "Solarcity",
            "solarcity",
            "SolarCity Corp",
            "Solar City",
            "SolarCity Corporatio",
            "Solar City Corp.",
            "SolarCity Corporation DBA Tesla Energy Operations, Inc",
            "SOLARCITY",
            "Tesla",
            "Tesla Energy",
            "TESLA ENERGY OPERATIONS, INC.",
            "Tesla Energy Operations, Inc.",
            "SolarCity Corporation dba Tesla",
            "Tesla Energy Operations",
            "Tesla Inc.",
            "SolarCIty",
            "SolarCity DBA Tesla",
            "Tesla Energy Operations, Inc",
            "SolarCity Corporation DBA Tesla",
            "SolarCity DBA",
            "Solarcity Corporation",
            "SOLARCITY CORPORATION",
            "tesla energy Ops",
            "SolarCity - Tesla",
            "Tesla Energy Operations Inc",
            "Tesla Energy Operations, INC",
            "TESLA ENERGY OPERATIONS INC",
            "SolarCIty Corporation",
            "solarcity corporation",
            "Tesla Inc",
            "SolarCity Corp. dba Tesla",
            "TESLA",
            "SolarCity corp",
            "SolarCity Corporation DBA",
            "SOLARCITY DBA TESLA",
            "Tesla Energy Ops",
            "tesla")

VSLR_df = c("Vivint Solar Developer LLC",
            "Vivint Solar Developer, LLC",
            "Vivint Solar",
            "Vivint Solar Developer, LLC.",
            "Vivint",
            "Vivint Solar Developer,llc",
            "Vivint Solar Developer",
            "vivint solar",
            "Vivint Solar Developers, LLC",
            "Vivint Solar Developer, LLC",
            "Vivint Solar Developer. LLC",
            "Vivint Solar Developer, LLc",
            "Vivint solar",
            "VIVINT SOLAR",
            "VIVINT")

RUN_df = c("Sunrun Inc",
           "Sunrun",
           "SunRun",
           "Sunrun, Inc.",
           "Sunrun, inc.",
           "Sunrun Inc.",
           "SunRun, Inc.",
           "Sunrun Installation Services",
           "sunrun",
           "SUNRUN, INC.",
           "Sunrun Installation Services, Inc.",
           "Sun Run, Inc.",
           "Sunrun, Inc",
           "SunRun, Inc",
           "Sunrun Installation Services, Inc",
           "SunRun Inc.",
           "Sun Run Inc",
           "SunRun Inc",
           "Sun Run",
           "Sun run",
           "sun run",
           "SUNRUN INC",
           "Sun Run Inc.",
           "SUNRUN INC.",
           "SUNRUN, INC",
           "SunRun Solar Owner I, LLC",
           "SunRun , Inc",
           "Sun Run, inc",
           "Sunrun,Inc.",
           "Sunrun inc",
           "SunRun Solar Owner 1, LLC",
           "Sun Run, Inc",
           "SunRun,Inc.",
           "SunRun Solar Owner II, LLC",
           "Sun Run INC",
           "SunRun Solar Owner, LLC")

SPWR_df = c("SunPower",
            "SunPower Capital, LLC",
            "Sunpower",
            "Sunpower Capital LLC",
            "SunPower Capital Services, LLC",
            "SunPower Capital LLC",
            "Sun Power Capital LLC",
            "SunPower Corporation",
            "SUNPOWER",
            "Sunpower Capital Services, LLC",
            "SunPower Solar Program",
            "SunPower Solar Program I, LLC",
            "SUNPOWER CAPITAL LLC",
            "SUNPOWER CAPITAL, LLC",
            "sunpower",
            "SunPower SolarProgram I, LLC",
            "SUNPOWER CAPITAL SERVICES LLC",
            "SunPower Capital Services LLC",
            "SunPower Corp",
            "Sunpower Capital Service, LLC",
            "SunPower Corporation, Systems",
            "SUNPOWER SOLAR PROGRAM 1, LLC",
            "Sunpower Corp",
            "Sunpower Capital, LLC.",
            "SunPower LLC",
            "SunPower Corporation Systems",
            "Sunpower LLC",
            "Sun Power Solar Program I, LLC",
            "Sun Power Capital, LLC",
            "SunPower Capital LLC.",
            "SunPower Corp.",
            "Sunpower Capital Services LLC",
            "SunPower Capital")

# Function to sapply ticker to Third.Party.Name
third_party_ticker = function(x) {
  if (x %in% SCTY_df) {return("TSLA")
  } else if (x %in% VSLR_df) {return("VSLR")
  } else if (x %in% RUN_df) {return("RUN")
  } else if (x %in% SPWR_df) {return("SPWR")
  } else {return("")}}

solar$Third.Party.Ticker = sapply(solar$Third.Party.Name, FUN = third_party_ticker) # mutating new col with third party ticker
solar = solar %>% select(., -Third.Party.Name)

#third_party_AC = solar %>% # creating temp dataframe to see third-party installer
#  group_by(Third.Party.Ticker) %>% 
#  summarise(total.AC=sum(System.Size.AC)) %>% 
#  arrange(desc(total.AC))
#View(third_party_AC)
#ggplot(third_party_AC, aes(x=Third.Party.Ticker, y=total.AC)) + geom_col(fill="Blue")

#unique(solar$Service.City) # check to see if city column needs to be converted to same case
#unique(solar$Service.County) # check to see if county column needs to be converted to same case

solar$Service.City = toupper(solar$Service.City) # converting city column to all caps
solar$Service.County = toupper(solar$Service.County) # converting county column to all caps
solar$App.Approved.Date = as.Date(solar$App.Approved.Date) # ensuring in date format

View(solar)
class(solar$App.Approved.Date)

solar = solar %>% select(., -Application.Id, -Application.Status) # removing Application.Id and Application.Status columns

View(solar)

#sector = solar %>% # creating temp dataframe to see solar power by Sector
#  group_by(Customer.Sector) %>% 
#  summarise(total.AC=sum(System.Size.AC)) %>% 
#  arrange(desc(total.AC))
#View(sector) # viewing sector dataframe
#ggplot(sector, aes(x=Customer.Sector, y=total.AC)) + geom_col(fill="Red") # viewing column chart

unique(solar$Third.Party.Owned.Type)

solar = solar %>% select(., -Third.Party.Owned, -Third.Party.Owned.Type, -Pace.Financed, -Electric.Vehicle)
solar = solar %>% arrange(., (App.Approved.Date))
solar = solar %>% select(., App.Approved.Date, 
                         Utility, 
                         Service.City, 
                         Service.County, 
                         Service.Zip, 
                         System.Size.AC, 
                         Customer.Sector, 
                         Third.Party.Ticker, 
                         Cost.Watt, 
                         Total.System.Cost)

solar = solar %>% rename(., Date = 'App.Approved.Date',
                         City = 'Service.City',
                         County = 'Service.County', 
                         Zip = 'Service.Zip', 
                         AC.KW = 'System.Size.AC', 
                         Sector = 'Customer.Sector', 
                         Installer = 'Third.Party.Ticker', 
                         Total.Cost = 'Total.System.Cost')

solar = solar %>% mutate(MW = (AC.KW*.001)) # create new col with calculation
solar = solar %>% select(., -AC.KW) # removing column

solar$City = str_to_title(solar$City) # changing to standard case (Capitalize First Letter)
solar$County = str_to_title(solar$County) # changing to standard case (Capitalize First Letter)

solar = solar %>% filter(., MW > 0) # filtering out rows with column condition
solar = solar %>% filter(., Date > "2014-12-31") # filtering out rows with date condition

library(lubridate)
library(zoo)
solar = solar %>% mutate(., Year = lubridate::year(Date)) # needed library(lubridate) for this
solar = solar %>% mutate(., Quarter = lubridate::quarter(solar$Date, with_year = FALSE, fiscal_start = 1))
solar$Quarter = as.numeric(solar$Quarter) # changing to numeric class
solar = solar %>% mutate(., Yr.Qtr=as.yearqtr(Date)) # needed library(zoo) for this

View(solar)
str(solar)
class(solar$MW)

### CREATING CSV FOR MAIN SOLAR DATAFRAME
write.csv(solar, file="solar_data.csv", row.names=FALSE)

# creating separate quarterly datasets by type
qtrly_util = solar %>% group_by(Yr.Qtr, Utility) %>% summarise(totalMW=sum(MW)) %>% arrange(Yr.Qtr)
qtrly_util = qtrly_util %>% spread(Utility, totalMW)
View(qtrly_util)

qtrly_sect = solar %>% group_by(Yr.Qtr, Sector) %>% summarise(totalMW=sum(MW)) %>% arrange(Yr.Qtr)
qtrly_sect = qtrly_sect %>% spread(Sector, totalMW) # used TIDYR 'spread' function to split one col into several
View(qtrly_sect)

qtrly_inst = solar %>% group_by(Yr.Qtr, Installer) %>% summarise(totalMW=sum(MW)) %>% arrange(Yr.Qtr)
qtrly_inst = qtrly_inst %>% spread(Installer, totalMW)
qtrly_inst = qtrly_inst %>% select(., -V1)
View(qtrly_inst)

qtrly_solar = left_join(qtrly_util, qtrly_sect, by = "Yr.Qtr")
qtrly_solar = left_join(qtrly_solar, qtrly_inst, by = "Yr.Qtr")
View(qtrly_solar)

# creating quarterly datasets by MW
utility_qtrly_MW = solar %>% group_by(Yr.Qtr, Utility) %>% 
  summarise(totalMW=sum(MW)) %>%
  arrange(Yr.Qtr)
View(utility_qtrly_MW)

sector_qtrly_MW = solar %>% group_by(Yr.Qtr, Sector) %>% 
  summarise(totalMW=sum(MW)) %>% 
  arrange(Yr.Qtr)
View(sector_qtrly_MW)

# below used TIDYR 'gather' function to combine Installers (separate columns) into one column
inst_qtrly_MW = qtrly_inst %>% gather(key="Installer", value="totalMW", RUN:VSLR, na.rm = TRUE) 
View(inst_qtrly_MW)

inst_daily = solar %>% select(., Date, Installer, MW) %>% group_by(Date, Installer) %>% summarise(totalMW=sum(MW))
inst_daily = inst_daily %>% spread(Installer, totalMW)
inst_daily = inst_daily %>% select(., -V1)
inst_daily = inst_daily %>% gather(key="Installer", value="totalMW", RUN:VSLR, na.rm = TRUE)

# WRITING ALL NEW QUARTERLY DATASETS INTO CSV
write.csv(qtrly_solar, file = "qtrly_solar.csv", row.names = FALSE)
write.csv(utility_qtrly_MW, file = "utility_qtrly_MW.csv", row.names = FALSE)
write.csv(sector_qtrly_MW, file = "sector_qtrly_MW.csv", row.names = FALSE)
write.csv(inst_qtrly_MW, file = "inst_qtrly_MW.csv", row.names = FALSE)

### CHARTS ###
ggplot(solar, aes(x = Date, y = MW)) + geom_col(aes(fill=Utility)) # daily column chart stacking utilities

ggplot(utility_qtrly_MW, aes(x = Yr.Qtr, y = totalMW)) + 
  geom_col(aes(fill=Utility), position='dodge') # quarterly column chart of utes side-by-side

ggplot(sector_qtrly_MW, aes(x = Yr.Qtr, y = totalMW)) + 
  geom_col(aes(fill=Sector), position='dodge')

ggplot(inst_qtrly_MW, aes(x = Yr.Qtr, y = totalMW)) + 
  geom_col(aes(fill=Installer), position='dodge')

ggplot(qtrly_util, aes(x=Yr.Qtr, y=PGE)) + geom_bar(stat='identity')

View(solar)
View(utility_qtrly_MW)

library(maps)
counties = map_data("county")
ca_counties = counties %>% filter(., region == "california")
ca_counties = ca_counties %>% select(., -group, -order, -region)
ca_counties = ca_counties %>% rename(County = subregion, Lat = lat, Long = long)
ca_counties$County = str_to_title(ca_counties$County)
View(ca_counties)

View(solar)
countyMW2019 = solar %>% select(., County, Year, MW) %>% 
  filter(., Year == "2019") %>% 
  group_by(County) %>% 
  summarise(MW=sum(MW))
View(countyMW2019)

ca_countiesMW2019 = left_join(ca_counties, countyMW2019, by = "County")
View(ca_countiesMW2019)
ggplot(data = ca_countiesMW2019, aes(x = Long, y = Lat)) + 
  geom_polygon(aes(group = County, fill = County), show.legend = FALSE)

summary(ca_countiesMW2019$MW)

# geotargets = read.csv("geotargets-2019-10-09.csv")
# View(geotargets)
# US = geotargets %>% filter(Country.Code == "US")
# View(US)
