## ---------------------------
##
## Script name: 
## Purpose of script:
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-04-07
## Date Last Updated: 2021-04-07
## Email: christopher.smith@cookcountyhealth.org
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


# Activate packages
library(censusapi)
library(tigris) 
library(tidyverse)
library(tidyr)
library(dplyr) 
library(sf) 
library(clipr)
library(readxl)
library(leaflet)
library(scales)
library(ggmap)
library(data.table)
library(XML)
library(reshape2)
library(ggplot2)

# Download, bind data -----------------------------------------------------

# List of ACS tables to be downloaded
grouplist <- c("B01001","B25002", "B25004", "B07001")
grouplist <- c("B07001")

# Download places
yearlist <- c(2010:2019)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "place:*", # tracts
                           regionin="state:17", # places, counties, not msas
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    attach(acs_group)
    acs_group <- acs_group %>% select(-contains("EA"))
    acs_group <- acs_group %>% select(-contains("MA"))
    acs_group <- acs_group %>% select(-contains("GEO_ID"))
    acs_group <- acs_group %>% select(-contains("M_1"))
    acs_group <- acs_group %>% select(-contains("M"))
    acs_group$year<-ayear 
    acs_group$GEOID_place<-paste0(state,place)
    assign(paste(agroup,"place",ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
  }
}

# Download tracts
yearlist <- c(2010:2019)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "tract:*", # tracts
                           regionin="state:17", # places, counties, not msas
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    attach(acs_group)
    acs_group <- acs_group %>% select(-contains("EA"))
    acs_group <- acs_group %>% select(-contains("MA"))
    acs_group <- acs_group %>% select(-contains("GEO_ID"))
    acs_group <- acs_group %>% select(-contains("M_1"))
    acs_group <- acs_group %>% select(-contains("M"))
    acs_group$year<-ayear 
    acs_group$GEOID_tract<-paste0(state,county,tract)
    assign(paste(agroup,"tract",ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
  }
}

# merge datasets
for (agroup in grouplist) {
  apattern_tract <- paste(agroup,"tract",sep="_")
  apattern_place <- paste(agroup,"place",sep="_")
  alist_tract <- mget(ls(pattern = apattern_tract))
  alist_place <- mget(ls(pattern = apattern_place))
  adf_tract <- rbindlist(alist_tract)
  adf_place <- rbindlist(alist_place)
  assign(apattern_tract,adf_tract)
  assign(apattern_place,adf_place)
}

rm(list=ls(pattern="adf_"))
rm(list=ls(pattern="alist_"))
rm(list=ls(pattern="_20"))

# Create merged data tables -----------------------------------------------------
## B01001: POPULATION BY AGE, SEX by place and tract------------------------------------

### Totals -------------------------------------------------------------

# B01001_place_sub_m <- B01001_place %>% 
B01001_tract_sub_m <- B01001_tract %>% 
  rowwise() %>%
  mutate(
    Total = sum(B01001_002E),
    PopUnd5 = sum(B01001_003E),
    Pop5to9 = sum(B01001_004E),
    Pop10to14 = sum(B01001_005E),
    Pop15to19 = sum(c_across(B01001_006E:B01001_007E)),
    Pop20to24 = sum(c_across(B01001_008E:B01001_010E)),
    Pop25to29 = sum(B01001_011E),
    Pop30to34 = sum(B01001_012E),
    Pop35to39 = sum(B01001_013E),
    Pop40to44 = sum(B01001_014E),
    Pop45to49 = sum(B01001_015E),
    Pop50to54 = sum(B01001_016E),
    Pop55to59 = sum(B01001_017E),
    Pop60to64 = sum(c_across(B01001_018E:B01001_019E)),
    Pop65to69 = sum(c_across(B01001_020E:B01001_021E)),
    Pop70to74 = sum(B01001_022E),
    Pop75to79 = sum(B01001_023E),
    Pop80to84 = sum(B01001_024E),
    Pop85Over = sum(B01001_025E)
  ) %>%
  select(GEOID_tract, Total:Pop85Over, year) %>%
  # select(GEOID_place, Total:Pop85Over, year) %>%
  mutate(sex="MALE")

# B01001_place_sub_f <- B01001_place %>% 
B01001_tract_sub_f <- B01001_tract %>%
  rowwise() %>%
  mutate(
    Total = sum(B01001_026E),
    PopUnd5 = sum(B01001_027E),
    Pop5to9 = sum(B01001_028E),
    Pop10to14 = sum(B01001_029E),
    Pop15to19 = sum(c_across(B01001_030E:B01001_031E)),
    Pop20to24 = sum(c_across(B01001_032E:B01001_034E)),
    Pop25to29 = sum(B01001_035E),
    Pop30to34 = sum(B01001_036E),
    Pop35to39 = sum(B01001_037E),
    Pop40to44 = sum(B01001_038E),
    Pop45to49 = sum(B01001_039E),
    Pop50to54 = sum(B01001_040E),
    Pop55to59 = sum(B01001_041E),
    Pop60to64 = sum(c_across(B01001_042E:B01001_043E)),
    Pop65to69 = sum(c_across(B01001_044E:B01001_045E)),
    Pop70to74 = sum(B01001_046E),
    Pop75to79 = sum(B01001_047E),
    Pop80to84 = sum(B01001_048E),
    Pop85Over = sum(B01001_049E)
  ) %>%
    select(GEOID_tract, Total:Pop85Over, year) %>%
    # select(GEOID_place, Total:Pop85Over, year) %>%
  mutate(sex="FEMALE")

B01001_place_sub <- B01001_place_sub_m %>%
  bind_rows(B01001_place_sub_f)

B01001_tract_sub <- B01001_tract_sub_m %>%
  bind_rows(B01001_tract_sub_f)

### Percentages -------------------------------------------------------------

B01001_place_sub_pct_m <- B01001_place %>% 
  rowwise() %>%
  mutate(
    Total = sum(B01001_002E),
    PopUnd5 = sum(B01001_003E)/Total,
    Pop5to9 = sum(B01001_004E)/Total,
    Pop10to14 = sum(B01001_005E)/Total,
    Pop15to19 = sum(c_across(B01001_006E:B01001_007E))/Total,
    Pop20to24 = sum(c_across(B01001_008E:B01001_010E))/Total,
    Pop25to29 = sum(B01001_011E)/Total,
    Pop30to34 = sum(B01001_012E)/Total,
    Pop35to39 = sum(B01001_013E)/Total,
    Pop40to44 = sum(B01001_014E)/Total,
    Pop45to49 = sum(B01001_015E)/Total,
    Pop50to54 = sum(B01001_016E)/Total,
    Pop55to59 = sum(B01001_017E)/Total,
    Pop60to64 = sum(c_across(B01001_018E:B01001_019E))/Total,
    Pop65to69 = sum(c_across(B01001_020E:B01001_021E))/Total,
    Pop70to74 = sum(B01001_022E)/Total,
    Pop75to79 = sum(B01001_023E)/Total,
    Pop80to84 = sum(B01001_024E)/Total,
    Pop85Over = sum(B01001_025E)/Total
  ) %>%
  select(GEOID_place, Total:Pop85Over, year) %>%
  mutate(sex="MALE")

B01001_place_sub_pct_f <- B01001_place %>% 
  rowwise() %>%
  mutate(
    Total = sum(B01001_026E),
    PopUnd5 = sum(B01001_027E)/Total,
    Pop5to9 = sum(B01001_028E)/Total,
    Pop10to14 = sum(B01001_029E)/Total,
    Pop15to19 = sum(c_across(B01001_030E:B01001_031E))/Total,
    Pop20to24 = sum(c_across(B01001_032E:B01001_034E))/Total,
    Pop25to29 = sum(B01001_035E)/Total,
    Pop30to34 = sum(B01001_036E)/Total,
    Pop35to39 = sum(B01001_037E)/Total,
    Pop40to44 = sum(B01001_038E)/Total,
    Pop45to49 = sum(B01001_039E)/Total,
    Pop50to54 = sum(B01001_040E)/Total,
    Pop55to59 = sum(B01001_041E)/Total,
    Pop60to64 = sum(c_across(B01001_042E:B01001_043E))/Total,
    Pop65to69 = sum(c_across(B01001_044E:B01001_045E))/Total,
    Pop70to74 = sum(B01001_046E)/Total,
    Pop75to79 = sum(B01001_047E)/Total,
    Pop80to84 = sum(B01001_048E)/Total,
    Pop85Over = sum(B01001_049E)/Total
  ) %>%
  select(GEOID_place, Total:Pop85Over, year) %>%
  mutate(sex="FEMALE")

B01001_place_sub_pct <- B01001_place_sub_pct_m %>%
  bind_rows(B01001_place_sub_pct_f)

B01001_tract_sub_pct <- B01001_tract_sub_pct_m %>%
  bind_rows(B01001_tract_sub_pct_f)

## B25002: OCCUPANCY STATUS by place and tract------------------------------------
### Totals -------------------------------------------------------------

B25002_tract_sub <- B25002_tract %>%
# B25002_place_sub <- B25002_place %>%
  mutate(
    TotalHU = B25002_001E,
    HUOcc = B25002_002E,
    HUVac = B25002_003E) %>%
  # select(GEOID_place,
  #        TotalHU:HUVac,
  #        year)
  select(GEOID_tract,
         TotalHU:HUVac,
         year)

## B25004: VACANCY STATUS by place and tract------------------------------------
### Totals -------------------------------------------------------------

# B25004_tract_sub <- B25004_tract %>%
  B25004_place_sub <- B25004_place %>%
  mutate(
    HUVac = B25004_001E,
    VacForRent = B25004_002E,
    VacRented = B25004_003E,
    VacForSale = B25004_004E,
    VacSold = B25004_005E,
    VacSeasonal = B25004_006E,
    VacMigWorkers = B25004_007E,
    VacOther = B25004_008E) %>%
  select(GEOID_place,
         HUVac:VacOther,
         year)
  # select(GEOID_tract,
  #        HUVac:VacOther,
  #        year)

## Aerial weight population data ------------------------------------
### Create areal weighting geometry ------------------------------------

IL_Tracts_geom <- tracts("IL", cb=TRUE, class="sf")
IL_Places_geom <- places("IL", cb=TRUE, class="sf")
IL_Counties_geom <- counties("IL", cb=TRUE, class="sf")
IL_Tracts_geom <- st_transform(IL_Tracts_geom, crs = 26916)
IL_Places_geom <- st_transform(IL_Places_geom, crs = 26916)
IL_Counties_geom <- st_transform(IL_Counties_geom, crs = 26916)

# Filter out only Cook County census tracts
# Intersect the place and tract geometries and create a areal weight field, AREA_pct
CookCounty_Tracts_geom <- IL_Tracts_geom %>% filter(COUNTYFP=="031")

CookCounty_Tracts_geom <- CookCounty_Tracts_geom %>% 
  select(GEOID_tract = GEOID) %>% 
  mutate(AREA_tract = as.numeric(st_area(geometry)))

IL_Places_geom <- IL_Places_geom %>% 
  select(GEOID_place=GEOID,NAME_place=NAME) %>% 
  mutate(AREA_place = as.numeric(st_area(geometry)))

CookCounty_TractsByPlace_geom_int <- st_intersection(CookCounty_Tracts_geom,IL_Places_geom)

CookCounty_TractsByPlace_geom <- CookCounty_TractsByPlace_geom_int %>% 
  mutate(AREA_int = as.numeric(st_area(geometry)),
         AREA_pct = as.numeric(AREA_int/AREA_tract))

plot(CookCounty_TractsByPlace_geom["AREA_pct"])

### Areal weight attribute data ------------------------------------

# generalize intersection table name
# remove unnecessary fields from tract attribute table
# join tract attributes with intersected tract geometries by shared GEOID id

table_name <- "B01001_TractsByPlace_std"
table_int <- CookCounty_TractsByPlace_geom
table_att <- B01001_tract_sub
table_int_att <- left_join(table_int,
                           table_att, 
                           by=c("GEOID_tract"="GEOID_tract"))
# function used to apportion attributes
# create weighted values by geometry
# summarize weighted values by place
fx_weightedval <- function(x) (x*table_int_att$AREA_pct) # percent of total area

table_int_att_app <- table_int_att %>% 
  mutate_at(vars(Total:Pop85Over), 
            fx_weightedval)
table_int_att_app_sum <- table_int_att_app %>% 
  st_drop_geometry() %>% 
  group_by(GEOID_place, year, sex) %>% 
  summarize_at(vars(Total:Pop85Over),
            funs(sum))
assign(table_name,table_int_att_app_sum)
rm(list=ls(pattern="table_"))

munistandardized <- read_excel("C:/Users/scott/OneDrive - CCDPH/OneDrive - Cook County Health/Projects/COVID/Vaccinations/Data/Archive/SummaryTablesChartsByAgeRace_CCDPH_20210228.xlsx", sheet="ACS_AgeByRace_2015-2019_Muni") %>%
  filter(type=="All") %>%
  select(GEOID_place,
         muni,
         label,
         district,
         partial) %>%
  mutate(fips = as.character(GEOID_place),
         GEOID_place=substr(as.character(fips),3,7))

munistandardized_partial <- munistandardized %>% 
  left_join(B01001_TractsByPlace_std,by=c("fips"="GEOID_place")) %>%
  filter(partial=="YES")

munistandardized_complete <- munistandardized %>% 
  left_join(B01001_place_sub,by=c("fips"="GEOID_place")) %>%
  filter(partial=="NO")

munistandardized_all <- munistandardized_complete %>% 
  bind_rows(munistandardized_partial)

# Figures and tables-----------------------------------------------------------------
## Figure 1: Population pyramids, 2010 and 2019 ------------------------------------
fig01_pyramid_2010 <- gather(munistandardized_all, 
                       key = "Age",
                       value = "Population", 
                       PopUnd5:Pop85Over) %>% 
  arrange(GEOID_place, year) %>% 
  filter(year==2010) %>%
  mutate(Population=ifelse(sex=="MALE",
                           Population*-1,
                           Population)) %>%
  group_by(Age,sex) %>%
  summarise(Population = sum(Population)) %>%
  drop_na()

fig01_pyramid_2019 %>% 
  mutate(Age= factor(Age,levels = c("PopUnd5",
                          "Pop5to9",
                          "Pop10to14",
                          "Pop15to19",
                          "Pop20to24",
                          "Pop25to29",
                          "Pop30to34",
                          "Pop35to39",
                          "Pop40to44",
                          "Pop45to49",
                          "Pop50to54",
                          "Pop55to59",
                          "Pop60to64",
                          "Pop65to69",
                          "Pop70to74",
                          "Pop75to79",
                          "Pop80to84",
                          "Pop85Over"))) %>%
  ggplot(aes(x = Age, y = Population, fill = sex)) +
  geom_bar(stat = "identity", width = 0.85) + 
  scale_y_continuous(limits= max(fig01_pyramid_2010$Population) * c(-1,1)) + 
  scale_fill_manual(values=as.vector(c("grey45","grey63"))) +
  coord_flip() +
  labs(x="",y="",fill="") + 
  theme_minimal()


