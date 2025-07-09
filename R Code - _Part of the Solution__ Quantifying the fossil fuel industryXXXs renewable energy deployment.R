#Anonymised code for peer-review. 
#"Part of the Solution"? Quantifying the fossil fuel industry's renewable energy deployment

####FUNCTIONS####

#It aggregates installed capacity of projects under a given technology, company type
#and status by region of ownership and region of operation. Used in table S1
ownership_table<-function(Technology = All, Company.Type = All, Status = All){
  All <- levels(as.factor(FF250$Technology))
  d <- FF250[which(FF250$Technology %in% Technology),]
  All <- levels(as.factor(FF250$Company.Type))
  d <- d[which(d$Company.Type %in% Company.Type),]
  All <- levels(as.factor(FF250$Status))
  d <- d[which(d$Status %in% Status),]
  
  d <- d %>% group_by(RegionOperation,RegionOwnership) %>% summarise(Capacity = sum(ParentEqCap))
  h <- NULL
  for(i in levels(as.factor(FF250$RegionOperation))){
    g<- NULL
    for(j in levels(as.factor(FF250$RegionOwnership))){
      f <- d$Capacity[which(d$RegionOperation %in% i & d$RegionOwnership %in% j)]
      if(is_empty(f)){ f<- 0}
      g <- c(g,f)
    }
    h <- rbind(h,g)
  }
  h <- as.data.frame(h)
  colnames(h) <- levels(as.factor(FF250$RegionOwnership))
  rownames(h) <- levels(as.factor(FF250$RegionOperation))
  return(h)
}

#It gives a list of all operating projects of a given oil and gas company
Screen <- function(Company){
  Company <- FF250[grep(Company,FF250$Parent),]
  Company <- Company[grep("operating",Company$Status),]
  return(Company)
}

#It aggregates the capacity of operating projects of a given oil and gas company
#by technology and world region. It facilitates the identification of missing
#or misslabelled projects
Screen_summary <- function(Screen){
  x <- Screen %>% group_by(Region,Technology) %>% summarise(Capacity = sum(ParentEqCap))
  return(x)
}

####LIBRARIES####
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
library(tidyverse)
library(clipr)

#### 1. Identify Acquisitions ####

#In Eikon, create a watchlist with the top 250 oil and gas companies, the company codes are included in column "RIC"
#Find acquisitions using the Eikon Screener tool
#From the downloaded 761 companies, select the relevant company acquisitions by oil and gas top250 excluding:
#Holding companies for particular power plants, 
#companies with the same name or 
#companies that are explicitly linked to thermal power generation
#Results in 152 companies (I have later added other acquisitions from own research)
#Add to Table S3

#### 2. Identify Subsidiaries ####

#Download all company ownership trees from Refinitiv Eikon into the following directory
setwd("#Insert Directory")

#Aggregate all company trees, and list all subsidiaries in a single column
Subsidiaries_files <- list.files("#Insert Directory")
Subsidiaries <- NULL
for(i in Subsidiaries_files){
  Subs <- read_xlsx(i, skip = 5)
  if(nrow(Subs) != 1){
    Subs[,2] <- Subs[1,2]
    Subs[1,3] <- Subs[1,2]
    while(colnames(Subs)[4] != "Relationship Type"){
      Subs[which(is.na(Subs[,3])),3] <- Subs[which(is.na(Subs[,3])),4]
      Subs <- Subs[,-4]
    }
    a <- Subs[1,]
    #Select only subsidiaries in Industries related to electricity renewable energy, power and utilities
    Subs <- Subs[(grep("Elect|Renew|Power|line Util",Subs$Industry,ignore.case = TRUE)),]
    Subs[1,3] <- Subs[1,2]
    Subs$`Ownership Percentage`[1] <- "100%" 
    Subs <- cbind.data.frame(Subs$`Company PermID`,Subs$`Level 1`,Subs$`Company Name`,Subs$`Relationship Type`, Subs$Type,Subs$Industry,Subs$`Ownership Percentage`)
  } else {
    Subs <- cbind.data.frame(Subs$`Company PermID`,Subs$`Company Name`,Subs$`Company Name`,Subs$`Relationship Type`, Subs$Type,Subs$Industry,Subs$`Ownership Percentage`)
    colnames(Subs)[2] <- "Subs$`Level 1`"
  }
  
  Subsidiaries <- rbind.data.frame(Subsidiaries,Subs)
}
colnames(Subsidiaries) <- gsub("Subs\\$","",colnames(Subsidiaries))
Subsidiaries <- Subsidiaries[-which(is.na(Subsidiaries$`\`Level 1\``)),]
table(Subsidiaries$Industry)

setwd("#Insert Directory")
write.csv(Subsidiaries,"Subsidiaries.csv")
#Manually screen and add selection of subsidiaries to Table S2

#### 3. Identify Sister companies ####
#Replicate process for sister companies, this time with the acquisitionas and 
#ownership trees of fossil fuel companies' controlling parents

setwd("#Insert Directory")

Sister_files <- list.files("#Insert Directory")
Sister <- NULL
for(i in Sister_files){
  Subs <- read_xlsx(i, skip = 5)
  if(nrow(Subs) != 1){
    Subs[,2] <- Subs[1,2]
    Subs[1,3] <- Subs[1,2]
    while(colnames(Subs)[4] != "Relationship Type"){
      Subs[which(is.na(Subs[,3])),3] <- Subs[which(is.na(Subs[,3])),4]
      Subs <- Subs[,-4]
    }
    a <- Subs[1,]
    Subs <- Subs[(grep("Elect|Renew|Power|line Util",Subs$Industry,ignore.case = TRUE)),]
    Subs[1,3] <- Subs[1,2]
    Subs$`Ownership Percentage`[1] <- "100%"
    Subs <- cbind.data.frame(Subs$`Company PermID`,Subs$`Level 1`,Subs$`Company Name`,Subs$`Relationship Type`, Subs$Type,Subs$Industry,Subs$`Ownership Percentage`)
  } else {
    Subs <- cbind.data.frame(Subs$`Company PermID`,Subs$`Company Name`,Subs$`Company Name`,Subs$`Relationship Type`, Subs$Type,Subs$Industry,Subs$`Ownership Percentage`)
    colnames(Subs)[2] <- "Subs$`Level 1`"
  }
  Sister <- rbind.data.frame(Sister,Subs)
}
colnames(Sister) <- gsub("Subs\\$","",colnames(Sister))
table(Sister$Industry)

Sister<-unique(Sister)

setwd("#Insert Directory")
write.csv(Sister,"Sisters.csv")

#### 4. Import and clean companies database ####

setwd("#Insert Directory")

#Top 250 oil and gas companies from Urgewald's Global Oil and Gas Exit List at 
#https://gogel.org/
#Their acquisitions and subsidiaries from Eikon Refinitiv - a few from other sources, specified in column "Source"
#A search word has been added manually for all companies in column "Search"
#This file can be generated from Table S3
Companies <- read.csv("Companies Master.csv", dec = ",")

#Global oil and gas production in 2022 was 94292 kb/d oil + 4048.6 bcm gas * 6429 kboe/bcm / 365 days = 165602.8 kboe/d
#Source Statistical Review of World Energy 2024
#https://www.energyinst.org/__data/assets/pdf_file/0006/1542714/EI_Stats_Review_2024.pdf

#Add up all production of the top 250 oil and gas companies and divide by global output
#GOGEL gives data in million barrels of oil equivalent. So we convert to kboe/d
sum(as.numeric(Companies$Hydrocarbons.Production.in.2022..Oil..Gas..Condensate..NGL.), na.rm = T)*1000/365/165602.8
#Top 250 companies cover 90.8% of global oil and gas production
#Top 250 companies responsible for 96% of production in GOGEL

Companies$Country.of.Headquarters <- gsub("USA|USA ", "United States",Companies$Country.of.Headquarters)
Companies$Country.of.Headquarters <- gsub("UAE", "United Arab Emirates",Companies$Country.of.Headquarters)

#When no information available on the ownership share, assume it is full ownership
#This is a highly conservative assumption that will overestimate the ownership of O&G companies
Companies$Share <- gsub("\\,","\\.",Companies$Share)
Companies$Share <- as.numeric(Companies$Share)
#If ownership is unknown assign 100%
Companies$Share[which(is.na(Companies$Share))] <- "1"
Companies$Share <- as.numeric(Companies$Share)

#### 5. Import, clean & merge GEM renewable datasets ####

#Downloaded from Global Energy Monitor Global Wind and Solar Power Trackers: 
##Global Energy Monitor, Global Wind Power Tracker, June 2024 release
#https://globalenergymonitor.org/projects/global-wind-power-tracker/
#Global Energy Monitor, Global Solar Power Tracker, June 2024 release
#https://globalenergymonitor.org/projects/global-solar-power-tracker/
#Global Energy Monitor, Global Hydropower Tracker, April 2024 release
#https://globalenergymonitor.org/projects/global-hydropower-tracker/
#Global Energy Monitor, Global Geothermal Power Tracker, May 2024 release
#https://globalenergymonitor.org/projects/global-geothermal-power-tracker/
Wind <- read.csv("Global-Wind-Power-Tracker-June-2024.csv", dec = ",") #Only utility scale >10MW
Solar <- read.csv("Global-Solar-Power-Tracker-June-2024.csv", dec = ",") #Only utility scale >20MW, >10MW in Arab speaking countries
Hydro <- read.csv("Global-Hydropower-Tracker-April-2024.csv", dec = ",")
Geo <- read.csv("Geothermal-Power-Tracker-May-2024.csv", dec = ",") 

#Edit Global Energy Trackers columns to merge under a single data frame
colnames(Hydro) <- gsub("unit","phase",colnames(Hydro))
colnames(Hydro) <- gsub("\\.1","",colnames(Hydro))
colnames(Geo) <- gsub("unit","phase",colnames(Geo))
colnames(Geo)[7] <- "Capacity..MW."

colnames(Wind) <- gsub("Country\\.Area","Country",colnames(Wind))
colnames(Solar) <- gsub("Country\\.Area","Country",colnames(Solar))
colnames(Geo) <- gsub("Country\\.Area","Country",colnames(Geo))

#Merge GEM trackers
FF<-NULL
FF <- Solar[,which(colnames(Solar) %in% c("Country","Project.Name","Capacity..MW.","Status","Operator","Owner","Latitude","Longitude","Region","GEM.location.ID","GEM.phase.ID","Wiki.URL"))]
FF <- rbind(FF,Wind[,which(colnames(Wind) %in% c("Country","Project.Name","Capacity..MW.","Status","Operator","Owner","Latitude","Longitude","Region","GEM.location.ID","GEM.phase.ID","Wiki.URL"))])
FF <- rbind(FF,Hydro[,which(colnames(Hydro) %in% c("Country","Project.Name","Capacity..MW.","Status","Operator","Owner","Latitude","Longitude","Region","GEM.location.ID","GEM.phase.ID","Wiki.URL"))])
FF <- rbind(FF,Geo[,which(colnames(Geo) %in% c("Country","Project.Name","Capacity..MW.","Status","Operator","Owner","Latitude","Longitude","Region","GEM.location.ID","GEM.phase.ID","Wiki.URL"))])

a <- gsub("Offshore.*","Offshore",Wind$Installation.Type)
a <- gsub("Unknown","Onshore",a) #The wind projects that show installation type unknown assign to onshore wind

Technology <- c(rep("Solar",nrow(Solar)),a,rep("Hydroelectric",nrow(Hydro)),rep("Geothermal",nrow(Geo)))
FF$Technology <- Technology
rm(a)

#Amend wind farm that does not have technology value
FF$Technology[which(FF$Project.Name %in% "Anhui Anqing Gaoxinqu wind farm")] <- "Onshore"

#In case there is no Owner known, but there is an operator known, search for the operator 
FF$Owner[which(FF$Owner %in% "")] <- FF$Operator[which(FF$Owner %in% "")]

#Edit some character issues that are otherwise problematic
FF$Country <- gsub("Türkiye","Turkey",FF$Country)
FF$Country <- gsub("Réunion","Reunion",FF$Country)
FF$Country <- gsub("Côte d'Ivoire","Ivory Coast",FF$Country)
FF$Owner <- gsub("\\%50","50\\%",FF$Owner)

#Remove entries with status cancelled or shelved
FF$Status <- as.factor(FF$Status)
FF <- FF[-grep("cancelled|shelved|mothballed|retired",FF$Status),]
FF <- droplevels(FF)

#### 6. Manual edition of GEM entries with outdated information ####
#Based on manual inspection of the largest projects of those companies with a largest
#difference between their reported and the GEM-listed operating capacities (by absolute
#or relative terms).
#All edits are backed up with the original source 

#Al Dhafrah is owned 40% by TAQA
#https://masdar.ae/en/renewables/our-projects/al-dhafra-solar-pv
FF$Owner[grep("Dhafra",FF$Project.Name)] <- "Abu Dhabi National Energy Company PJSC [40%]; EDF Renewables [20%]; Masdar Clean Energy [20%]; Jinko Power Technology Co Ltd [20%]"

#Noor is owned 60% by TAQA
#https://www.ewec.ae/en/power-plants/noor-abu-dhabi
FF$Owner[grep("Noor Abu Dhabi",FF$Project.Name)] <- "Marubeni Corp [20%]; Abu Dhabi National Energy Company PJSC [60%]; Jinko Power Technology Co Ltd [20%]"

#Lakefield is owned 50% by TAQA
#https://na.taqa.com/
FF$Owner[grep("Lakefield",FF$Project.Name)] <- "EDF Renewables; TAQA"

#Big Beau, Milligan, Desert Harvest, Maverick Solar owned 50% by Masdar
#https://masdar.ae/en/news/newsroom/masdar-expands-presence-in-us
#https://masdar.ae/en/news/newsroom/masdar-achieves-first-close-on-clean-energy-portfolio-in-united-states-from-edf-renewables
FF$Owner[grep("Big Beau|Milligan 1|Desert Harvest|Maverick Solar",FF$Project.Name)] <- "EDF Renewables; Masdar"

#Eni acquired 80% in EDPs solar parks
#https://www.eni.com/en-IT/media/press-release/2024/01/plenitude-signs-agreement-with-edpr-to-acquire-three-photovoltaic-parks.html
FF$Owner[grep("Cattlemen|Timber Road|Blue Harvest",FF$Project.Name, ignore.case = TRUE)] <- "EDP Renewables [20%]; Eni Plenitude SpA [80%]"

#Golden Buckle Solar is not from Savion Anymore, it is from Plenitude (Eni)
#https://www.eni.com/en-IT/media/press-release/2023/02/plenitude-inaugurates-a-263-mw-photovoltaic-plant-texas.html
FF$Owner[which(FF$Project.Name %in% "Golden Buckle Solar")] <- "Eni Plenitude SpA"

#Eni acquires Kellam PV
#https://www.eni.com/en-IT/media/press-release/2022/12/plenitude-acquires-81-mw-photovoltaic-plant-in-texas.html
FF$Owner[grep("Kellam",FF$Project.Name)] <- "Eni Plenitude SpA"

#Eni acquires 140MW from X-Elio
#https://www.eni.com/en-IT/media/press-release/2021/02/cs-eni-rafforza-spagna-nuovi-progetti.html
FF$Owner[grep("Torre De Cotillas|Almansa \\(X|Parcas solar",FF$Project.Name)] <- "Eni Plenitude SpA"

#Eni acquires 100% Glennmont wind assets in Italy
#https://www.eni.com/content/dam/enicom/documents/press-release/migrated/2021-en/07/eni-acquires-wind-projects-italy-totalling-315mw.pdf
FF$Owner[grep("y\\; Glennmont",FF$Owner)] <- "Eni Plenitude SpA"

#Al Kharsaah is owned 60% by Siraj, 20% TotalEnergies, 20% Marubeni
#https://totalenergies.com/projects/solar/al-kharsaah-pioneering-solar-power-plant-qatar
FF$Owner[grep("Kharsaah",FF$Project.Name)] <- "TotalEnergies SE [20%]; Marubeni Corp [20%]; Siraj Energy [60%]"

#Mitsubishi Owns 50% of Eolica del Sur through Diamond Generating
#https://www.dgllc-us.com/projects/ees-wind-project
#https://www.business-humanrights.org/en/companies/mare%C3%B1a-renovables-joint-venture-macquarie-mitsubishi-corp-pggm/?companies=3861478
FF$Owner[grep("Eolica del Sur", FF$Project.Name)] <- "Diamond Generating LLC [50%]; Macquire [25%]; PGGM [25%]"

#Mitsubishi owns a minority stake in Monsoon Wind farm
#https://monsoonwindasia.com/stakeholders#stakeholders_structure
FF$Owner[which(FF$Project.Name %in% "Monsoon wind farm" & FF$Status %in% "construction")] <- "Impact Energy Asia Development LTD (IEAD)[85%]; BCPG Public Company LTD (BCPG) [15%]"

#Mitsubishi owns a minority stake in Caparispisan Wind farm
#https://www.dg-asia.com/projects/nlr/
FF$Owner[which(FF$Project.Name %in% "Caparispisan wind farm" & FF$Status %in% "pre-construction")] <- "AC Energy CORP (ACEN) [81%]; Diamond Generating Asia [19%]"

#Mitsubishi owns Llano Estacado Wind Ranch
#https://www.power-technology.com/data-insights/power-plant-profile-white-deer-llano-estacado-wind-ranch-us/
FF$Owner[which(FF$Project.Name %in% "Llano Estacado Wind Ranch")] <- "Mitsubishi Heavy Industries (MHI)"

#Mitsui acquired 49% in four power plants in India from ReNew
#https://www.mitsui.com/jp/en/release/2022/1243209_13406.html
#https://www.power-technology.com/marketdata/power-plant-profile-renew-power-rtc-i-rajasthan-solar-pv-park-india/
FF$Owner[which(FF$Project.Name %in% c("Kutch (Renew) II wind farm","Maharashtra (Renew) wind farm","SECI XI (Renew) wind farm"))] <- "ReNew Power [51%]; Mitsui & Co Ltd [49%]"

#Mitsui owns 7% in Engie ANZ assets 
#https://www.infrastructureinvestor.com/engie-and-mitsui-seek-partner-for-new-australian-renewables-platform/
#https://engie.com.au/sites/default/files/2021-05/Media-Release-ENGIE-Australian-Renewable-Energy-Trust-13102020.pdf
FF$Owner[grep("Willogoleche|Silverleaf|Hills of Gold",FF$Project.Name)] <- "IGC [75%]; Engie SA [18%]; Mitsui & Co Ltd [7%]"

#Mitsui acquires 49% of Kasso Solar farm
#https://renewablesnow.com/news/mitsui-to-buy-into-danish-solar-methanol-project-of-european-energy-827763/
FF$Owner[which(FF$Project.Name %in% "Kassø solar farm")] <- "European Energy A/S [51%]; Mitsui & Co Ltd [49%]"

#Mitsui partly owns Taza wind farm in Morrocco
#https://www.power-technology.com/data-insights/power-plant-profile-taza-wind-farm-morocco/
FF$Owner[which(FF$Project.Name %in% "Taza wind farm")] <- "EDF [60%]; Mitsui & Co Ltd [40%]"

#Mitsui owns Calera Solar farm
#https://www.power-technology.com/data-insights/power-plant-profile-calera-solar-pv-park-mexico/
FF$Owner[which(FF$Project.Name %in% "Calera solar farm")] <- "Mitsui & Co Ltd"

#Petronas acquires 50% of Northland Power Inc. stake in Hai Long project
#https://www.petronas.com/integrated-report-2023/assets/pdf/PETRONAS%20Integrated%20Report%202023.pdf
#https://hailongoffshorewind.com/en/shareholders#!
FF$Owner[which(FF$Project.Name %in% "Hai Long Offshore wind farm")] <- "Mitsui & Co Ltd; Northland Power INC; Gentari"

#Petronas, through Wirsol, owns Wemen, Maryvale and Barnawartha Solar Farm
#https://www.gentari.com.au/project/wemen-solar-farm/
#https://www.maryvalesolarfarm.com.au/
#https://www.barnawarthasolarfarm.com.au/about
FF$Owner[which(FF$Project.Name %in% c("Wemen solar farm","Maryvale solar farm","Barnawartha solar farm"))] <- "Wirsol Energy LTD"

#Petronas through Gentari acquires 49% of RTC-I project from ReNew
#https://www.petronas.com/integrated-report-2023/assets/pdf/PETRONAS%20Integrated%20Report%202023.pdf
FF$Owner[which(FF$Project.Name %in% "RTC-I solar farm")] <- "ReNew Power [51%]; Gentari [49%]"

#Frye Solar farm from Repsol has completed construction
#https://www.repsol.com/en/press-room/press-releases/2024/repsol-completes-the-construction-of-frye-solar-in-the-united-states-its-largest-solar-plant/index.cshtml
FF$Status[grep("Frye",FF$Project.Name)] <- "operating"

#Repsol owns San Bartolom� and Odón de Buen wind farm
#https://www.repsol.com/en/press-room/press-releases/2023/repsol-buys-three-wind-and-two-solar-projects/index.cshtml
#https://www.repsol.com/content/dam/repsol-corporate/en_gb/accionistas-e-inversores/informes-anuales/2023/integrated-management-report-2023.pdf
FF$Owner[grep("San Bartolomé|Odón de Buen wind farm",FF$Project.Name)] <- "Repsol SA"

#Repsol owns 51% of portfolio shared with Pontegadea
#https://www.repsol.com/en/press-room/press-releases/2021/repsol-incorporates-pontegadea-as-partner-in-delta-wind-farm/index.cshtml
FF$Owner[grep("Pontegadea",FF$Owner)] <- "Repsol SA [51%]; Pontegadea Inversiones SL [49%]"

#Repsol owns the Jicarilla solar farm, wich is already operating
#https://www.repsol.com/en/press-room/press-releases/2024/repsol-completes-the-construction-of-frye-solar-in-the-united-states-its-largest-solar-plant/index.cshtml
FF$Owner[grep("Jicarilla Solar",FF$Project.Name)] <- "Repsol SA"
FF$Status[grep("Jicarilla Solar",FF$Project.Name)] <- "operating"

#Repsol seems to have fully acquired Frye Solar from its acquired company Hecate
#https://www.repsol.com/en/press-room/press-releases/2024/repsol-completes-the-construction-of-frye-solar-in-the-united-states-its-largest-solar-plant/index.cshtml
#https://www.hecateenergy.com/projects/
FF$Owner[grep("Frye",FF$Project.Name)] <- "Hecat Frye"

#Galp owns 75% of solar farms in Spain, acquired from ACS
#https://www.galp.com/corp/en/media/press-releases/press-release/id/1151/galp-completes-transaction-with-acs-becomes-iberias-leading-solar-player
FF$Owner[grep("Actividades de Construccion y Servicios ; Galp",FF$Owner)] <- "Actividades de Construccion y Servicios [25%]; Galp ACS [75%]"

#In addition, the Galp-ACS consortium also owns Logro Solar, El Robledo, Ribagrande, Sierrezuela, etc
#https://www.galp.com/corp/Portals/0/Recursos/Investidores/IMR2023/EN/AnnualIntegratedReport2023.pdf
#https://www.pv-magazine.es/2022/08/17/las-mayores-centrales-solares-del-mundo-12-parque-solar-escatron-chiprana-samper-espana/
FF$Owner[grep("Logro|Robledo|Ribagrande|Sierrezuela|Valdelagua|Emoci|Envitero|Escarnes|Ignis Solar|Mediomonte|Mocatero|Palabra|Hazaña|Talento",FF$Project.Name)] <- "Actividades de Construccion y Servicios [25%]; Galp ACS [75%]"

#Osaka Gas owns Noheji Mutsu Bay Wind Far and Yokohama Town Wind Power Plant
#https://www.daigasgroup.com/en/files/data/ir/fb/2023/fb2023.pdf
#https://www.power-technology.com/marketdata/power-plant-profile-noheji-mutsuwan-wind-farm-japan/
#https://www.osakagas.co.jp/en/whatsnew/__icsFiles/afieldfile/2021/07/19/210719.pdf
FF$Owner[grep("Noheji Mutsu|Yokohama Town",FF$Project.Name)] <- "Japan Wind Development [30%], Osaka Gas [39%] and Tokyu Land [31%]"

#Osaka Gas acquires power plants from Sonnedix Japan
#https://renewablesnow.com/news/osaka-gas-invests-in-131-mw-of-solar-parks-in-japan-802520/
FF$Owner[grep("Hitachi Juo|Kurayoshi|Oita Solar Power",FF$Project.Name)] <- "Osaka Gas"

#Octopus owns Cumberhead
#https://octopus.energy/press/more-news-press-releases/octopus-energy-opens-seven-new-wind-farms-across-europe/
FF$Owner[grep("Cumberhead",FF$Project.Name)] <- "Octopus Energy"

#Octopus owns 12.5% of Walney wind farm
#https://octopus.energy/press/walney-extension-wind-farm-investment/
#https://www.power-technology.com/features/largest-offshore-windfarm-world/?cf-view
FF$Owner[grep("Cumberhead",FF$Project.Name)] <- "Octopus Energy [12.5%]; �~rsted A/S [50%]; PFA Pension [25%]; PKA Group [12.5%]"

#Octopus owns 31% of Lincs wind farm
#https://octopus.energy/press/octopus-energy-makes-more-waves-in-offshore-wind-with-1bn-invested-and-plans-to-rapidly-scale/
FF$Owner[grep("Lincs",FF$Project.Name)] <- "Equitix Ltd ; Octopus Renewables Infrastructure Trust PLC [31%]; Macquarie Green Investment Group Renewable Energy Fund 1"

#BP has a lower share of Cedar Creek II wind farm
#https://www.power-technology.com/projects/cedacreekiiwindfarmc/?cf-view
FF$Owner[which(FF$Project.Name %in% "Cedar Creek II wind farm")] <- "BP Wind Energy [16.5%], Sempra Generation [16.5%] and Infigen Energy [67%]"

#BP does not own Cedar Creek I wind farm
#https://www.leewardenergy.com/arclight-capitals-leeward-renewable-energy-closes-on-acquisition-of-membership-interest-in-cedar-creek-i-wind-farm/
FF$Owner[which(FF$Project.Name %in% "Cedar Creek (Infigen) wind farm")] <- "Leeward Renewable Energy"

#BP does not own Camp Grove wind farm
#https://www.wind-watch.org/news/2007/01/13/wind-farm-developer-is-sold-company-building-facility-near-camp-grove-now-part-of-bp-energy/
#https://www.gridinfo.com/plant/camp-grove-wind-farm/56640
#https://www.power-technology.com/data-insights/power-plant-profile-camp-grove-wind-farm-us/
FF$Owner[which(FF$Project.Name %in% "Camp Grove wind farm")] <- "Orion Renewable Energy Group LLC"

#CNOOC Hezuo facility is only 40MW
#https://www.cnoocltd.com/art/2023/7/5/art_55171_15339584.html
FF$Capacity..MW.[which(FF$Project.Name %in% "Gansu Province Hezuo CNOOC solar farm")] <- 40

#Shell sold some of its wind assets in the US
#https://www.shell.us/media/2023-media-releases/shell-agrees-to-sell-ownership-stake-infrared-capital-partners.html
FF$Owner[which(FF$Project.Name %in% "Brazos Wind Ranch")] <- "Shell Energy Operations Pty Ltd [40%]; InfraRed Capital Partners [60%]"
FF$Owner[which(FF$Project.Name %in% "Madison Fields Solar Project")] <- "Savion LLC [50%]; InfraRed Capital Partners [50%]"

#ESCO sold Moura and West Wyalong Solar
#https://www.pv-magazine-australia.com/press-releases/esco-pacific-sells-shovel-ready-ppa-backed-110-mwp-moura-solar-farm-qld/
#https://www.power-technology.com/marketdata/power-plant-profile-esco-west-wyalong-solar-pv-park-australia/
FF$Owner[which(FF$Project.Name %in% c("Moura solar farm","Wyalong Solar"))] <- "Mytilineos SA"

#Remove BP's false positive hit
FF$Owner <- gsub("Bronzeoak Philippines INC \\(BP\\)","Bronzeoak Philippines",FF$Owner)

#Edit unusual ownership share format that is not well recognised by the algorithm.
#Assign equal ownership to the other partners

FF$Owner[which(FF$Owner %in% "AC Energy and Infrastructure Corp [19%]; Star Energy Geothermal; Electricity Generating Public CO LTD (EGCO); Star Energy Group PLC")] <-
  "AC Energy and Infrastructure Corp [19%]; Star Energy Geothermal [27%]; Electricity Generating Public CO LTD (EGCO) [27%]; Star Energy Group PLC [27%]"

#Delete all power plants without owner or operator
FF <- FF[-which(FF$Owner %in% ""),] 

#### 7. Identify Solar, Wind, and Offsets projects owned by fossil fuel companies (Table S4) ####

#Create a dataframe that compiles all projects owned by fossil fuel companies
FF250 <- NULL

#WARNING! Expect long computation time!!
for(i in 1:nrow(Companies)){ 
  a <- grep(paste0("\\b",gsub("\\(.+\\)","",Companies$Search[i]),"\\b","|","\\b",
                   gsub(".+\\(|\\)","",Companies$Search[i]),"\\b"),FF$Owner, ignore.case=TRUE)
  if(length(a) > 0){
    c <- FF$Owner[a]
    e <- NULL
    for(j in 1:length(c)){
      if(grepl("\\[\\d",c[j])){
        d <- gsub(paste0(".*",gsub(" \\(.+\\)","",Companies$Search[i]),"|",
                            ".*",gsub(".+\\(|\\)","",Companies$Search[i])),"",c[j])
        d <- gsub("\\].*","",d)
        d <- gsub(".*\\[","",d)
        d <- as.numeric(gsub("%.*","",d))/100
      } else {
        d <- 1/(str_count(c[j],"\\;")+1)
      }
      e <- c(e,d)
    }
    FF250 <- rbind(FF250,cbind(FF[a,],rep(Companies$Subsidiary.Name[i], length.out = length(a)),e,rep(Companies$Company.Type[i], length.out = length(a)))) 
  }
}

rm(a,c,d,e,i,j)

#Formatting data frame
colnames(FF250)[(ncol(FF250)-2):ncol(FF250)] <- c("Company","Share","Company.Type")
FF250$Capacity..MW. <- as.numeric(FF250$Capacity..MW.)
FF250$Share <- as.numeric(FF250$Share)

#Calculate equity capacity for each project
FF250$EquityCapacity <- FF250$Capacity..MW.*FF250$Share

#Calculate Parent Equity Capacity for each of the projects based on the participation
#of each respective oil and gas company in its subsidiary or acquisition that owns
#the project
for(i in 1:nrow(FF250)){
  if(FF250$Company.Type[i] %in% "Parent"){
    FF250$Parent[i] <- FF250$Company[i]
    FF250$ParentEqCap[i] <- FF250$EquityCapacity[i]
  } else {
    FF250$Parent[i] <- Companies$Company.Name[which(Companies$Subsidiary.Name %in% FF250$Company[i])]
    FF250$ParentEqCap[i] <- FF250$EquityCapacity[i]*Companies$Share[which(Companies$Subsidiary.Name %in% FF250$Company[i])]
  }
}

#### 8. Clean up errors in fossil fuel-owned projects dataframe####

#Avoid double counting of BP lightsource
L <- FF250$GEM.phase.ID[which(FF250$Company %in% "Lightsource")]
FF250 <- FF250[-which(FF250$GEM.phase.ID %in% L & FF250$Company %in% "BP plc"),]

#Avoid double counting for ACS Galp
L <- FF250$GEM.phase.ID[which(FF250$Company %in% "ACS Spanish Assets")]
FF250 <- FF250[-which(FF250$GEM.phase.ID %in% L & FF250$Company %in% "Galp Energia SGPS SA"),]

#Avoid double counting OMV
FF250 <- FF250[-which(FF250$Parent %in% "OMV Petrom SA"),]

#Arkona wind is owned by Eolus Wind and it's projected for 1,400 MW
#https://www.eolus.com/en/projects/arkona-vindkraftpark/
#It was confused with a 385MW offshore wind plant with the same name in Germany
#Owned by E.On and Equinor
#https://www.power-technology.com/news/arkona-offshore-windfarm-eon-equinor/?cf-view
Delete <- grep("Arkona",FF250$Project.Name)
FF250 <- FF250[-Delete,]

#Delete Surge Energy Assets. They are not well annotated by GEM
FF250 <- FF250[-grep("Surge",FF250$Owner),]

#Lundin split its renewable assets from fossil fuel assets, which were acquired by Aker BP#
#https://www.lundin-energy.com/
FF250 <- FF250[-which(FF250$Parent %in% "Lundin Energy AB"),]

#Save a back-up copy of the dataframe
#This file is then formated into Table S4
write.csv(FF250,"FF250.csv")
#FF250 <- read.csv("FF250.csv")

#### 9. Summary tables of fossil fuel company-owned renewable projects ####

Bycountry <- FF250 %>% group_by(Country) %>% summarise(EqCapacity = sum(EquityCapacity, na.rm = TRUE),
                                                       Projects = n())

Bytype <- FF250 %>% group_by(Company.Type) %>% summarise(EqCapacity = sum(EquityCapacity, na.rm = TRUE),
                                                    Projects = n())

Bystatus <- FF250 %>% group_by(Status) %>% summarise(EqCapacity = sum(EquityCapacity, na.rm = TRUE),
                                                         Projects = n())

Bytypestatus <- FF250 %>% group_by(Company.Type,Status) %>% summarise(EqCapacity = sum(EquityCapacity, na.rm = TRUE),
                                                         Projects = n())

Bytechnology <- FF250 %>% group_by(Technology) %>% summarise(EqCapacity = sum(EquityCapacity, na.rm = TRUE),
                                                     Projects = n())

Bytypetechnology <- FF250 %>% group_by(Company.Type,Technology) %>% summarise(EqCapacity = sum(EquityCapacity, na.rm = TRUE),
                                                             Projects = n())

#### 10. Calculate fossil fuel share of global capacity (Figure 1, and S2) ####

##10.1 Over GEM total dataset
Bytechnologystatus <- FF250 %>% group_by(Technology,Status) %>% summarise(EqCapacity = sum(ParentEqCap, na.rm = TRUE),
                                                                          Projects = n())   
Bytechnologytypestatus <- FF250 %>% group_by(Technology,Company.Type,Status) %>% summarise(EqCapacity = sum(ParentEqCap, na.rm = TRUE),
                                                                                           Projects = n())     
#Recalculate global GEM capacities before removing projects without ownership data
#These lines replicate code in section 5
FF<-NULL
FF <- Solar[,which(colnames(Solar) %in% c("Country","Project.Name","Capacity..MW.","Status","Operator","Owner","Latitude","Longitude","Region","GEM.location.ID","GEM.phase.ID","Wiki.URL"))]
FF <- rbind(FF,Wind[,which(colnames(Wind) %in% c("Country","Project.Name","Capacity..MW.","Status","Operator","Owner","Latitude","Longitude","Region","GEM.location.ID","GEM.phase.ID","Wiki.URL"))])
FF <- rbind(FF,Hydro[,which(colnames(Hydro) %in% c("Country","Project.Name","Capacity..MW.","Status","Operator","Owner","Latitude","Longitude","Region","GEM.location.ID","GEM.phase.ID","Wiki.URL"))])
FF <- rbind(FF,Geo[,which(colnames(Geo) %in% c("Country","Project.Name","Capacity..MW.","Status","Operator","Owner","Latitude","Longitude","Region","GEM.location.ID","GEM.phase.ID","Wiki.URL"))])
a <- gsub("Offshore.*","Offshore",Wind$Installation.Type)
a <- gsub("Unknown","Onshore",a)
Technology <- c(rep("Solar",nrow(Solar)),a,rep("Hydroelectric",nrow(Hydro)),rep("Geothermal",nrow(Geo)))
FF$Technology <- Technology
rm(a)
#Amend wind farm that does not have technology value
FF$Technology[which(FF$Project.Name %in% "Anhui Anqing Gaoxinqu wind farm")] <- "Onshore"
#Remove entries with status cancelled or shelved
FF$Status <- as.factor(FF$Status)
FF <- FF[-grep("cancelled|shelved|mothballed|retired",FF$Status),]
FF <- droplevels(FF)
FF$Capacity..MW. <- as.numeric(FF$Capacity..MW.)

#Aggregate all GEM projects capacity by technology and status
BytechnologystatusFF <- FF %>% group_by(Technology,Status) %>% summarise(EqCapacity = sum(Capacity..MW., na.rm = TRUE),
                                                                         Projects = n())
TotalCapacities <- BytechnologystatusFF$EqCapacity
BytechnologystatusFF$EqCapacity <- BytechnologystatusFF$EqCapacity-Bytechnologystatus$EqCapacity
BytechnologystatusFF$Company.Type <- "Rest"

#Merge capacities for fossil fuel companies with global GEM capacities
a <- rbind(Bytechnologytypestatus,BytechnologystatusFF)
a$Company.Type <- as.factor(a$Company.Type)
a$Company.Type <- factor(a$Company.Type, levels = c("Parent","Subsidiary","Acquisition","Sister","Rest"))
a$Status <- as.factor(a$Status)
a$Status <- droplevels(a$Status)
a$Status <- factor(a$Status, levels = c("operating","construction","pre-construction","announced"))

#Plot piecharts by technology and status, coloured by type of company
cleanplot <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),plot.background=element_blank())

Piecharts <- ggplot(a, aes(x="", y=EqCapacity, fill=Company.Type)) +
  geom_bar(stat="identity", width=1, position = "fill") +
  coord_polar("y", start=0,direction = -1)+
  facet_grid(Status~Technology)+
  scale_fill_manual(values = c("#131515","#7c7c7c","#bdbdbd","#309fa9","#9ed6db"))+
  cleanplot
Piecharts
#Export and edit in Adobe Illustrator to make Figure 1b.
ggsave(Piecharts, file="Piecharts.svg")

#Create table with percentage values to write in Figure 1b
b <- Bytechnologytypestatus[-which(Bytechnologytypestatus$Company.Type %in% "Sister"),]
b <- b %>% group_by(Technology,Status) %>% summarise(EqCapacity = sum(EqCapacity),
                                                     Projects = sum(Projects))
b$Percentage <- b$EqCapacity/TotalCapacities
b$PercentageProjects <- b$Projects/BytechnologystatusFF$Projects
Piecharts_table <- b

##10.2 Over IRENA operating figures
#IRENA Renewable Capacity Statistics 2024: https://www.irena.org/Publications/2024/Mar/Renewable-capacity-statistics-2024
#Onshore wind: 944536MW
#Offshore wind: 72633MW
#Solar: 1418969MW
#Hydroelectricity: 1407754MW
#Geothermal: 14846MW
#Total: 3858768MW
IRENA <- cbind.data.frame(levels(as.factor(Operating$Technology)),c(14846,1407754,72633,944536,1418969,3858768))
colnames(IRENA) <- c("Technology","Capacity")

#Select only operating capacities
Operating <- Bytechnologytypestatus[which(Bytechnologytypestatus$Status %in% "operating"),]
Operating$EqCapacity <- as.numeric(Operating$EqCapacity)
Operating$Projects <- as.numeric(Operating$Projects)

#Calculate total renewable energy capacity for all technologies
a <- Operating %>% group_by(Company.Type,Status) %>% summarise(EqCapacity = sum(EqCapacity),
                                                        Projects = sum(Projects))
a$Technology <- "Total"
Operating <- rbind.data.frame(Operating,a)
 
#Calculate the rest fraction of the piecharts by discounting total fossil fuel-owned
#capacities from total IRENA figures for each technology
a <- Operating %>% group_by(Technology) %>% summarise(EqCapacity = sum(EqCapacity))
b <- IRENA$Capacity-a$EqCapacity
c <-cbind.data.frame(IRENA$Technology,rep("Rest", length.out = nrow(IRENA)),rep("operating", length.out = nrow(IRENA)),b,rep(0, length.out = nrow(IRENA)))
colnames(c) <- colnames(Operating)
Operating <- rbind(Operating,c)

#Formatting dataframe for plotting
Operating$Company.Type <- as.factor(Operating$Company.Type)
Operating$Company.Type <- factor(Operating$Company.Type, levels = c("Parent","Subsidiary","Acquisition","Sister","Rest"))
Operating$Status <- as.factor(Operating$Status)
Operating$Status <- droplevels(Operating$Status)
Operating$Status <- factor(Operating$Status, levels = c("operating","construction","pre-construction","announced"))

PiechartsIRENA <- ggplot(Operating, aes(x="", y=EqCapacity, fill=Company.Type)) +
  geom_bar(stat="identity", width=1, position = "fill") +
  coord_polar("y", start=0,direction = -1)+
  facet_grid(Status~Technology)+
  scale_fill_manual(values = c("#131515","#7c7c7c","#bdbdbd","#309fa9","#9ed6db"))+
  cleanplot
PiechartsIRENA
#Export and edit in Adobe Illustrator to make Figure 1a
ggsave(PiechartsIRENA, file="PiechartsIRENA.svg")

#The percentage figures can be found in the Operating data frame

#### 11. Company ranking (Figure S1 and S3) ####

#Merge Kunlun and PetroChina as subsidiaries to CNPC 
FF250$Company.Type[grep("PetroChina|KunLun",FF250$Parent, ignore.case = T)] <- "Subsidiary"
FF250$Parent[grep("PetroChina|KunLun",FF250$Parent, ignore.case = T)] <- "China National Petroleum Corporation (CNPC)"

#Aggregate operating capacity by companies, technology and type of company
#ignoring sister companies
Bycompanies <- FF250 %>% group_by(Technology,Parent,Status,Company.Type) %>% 
  summarise(EqCapacity = sum(ParentEqCap, na.rm = T),
            Projects = n())
Bycompanies <- Bycompanies[which(Bycompanies$Status %in% "operating"),]
Bycompanies <- Bycompanies[-which(Bycompanies$Company.Type %in% "Sister"),]

#How many oil and gas companies with operating renewable energy?
b <- unique(Bycompanies$Parent) #43 Parents

#Add distributed power from company reports
Bycompanies$Technology <- as.factor(Bycompanies$Technology)
levels(Bycompanies$Technology)
Bycompanies[sapply(Bycompanies, is.factor)] <- lapply(Bycompanies[sapply(Bycompanies, is.factor)], 
                                                      as.character)
#https://totalenergies.com/system/files/documents/2024-03/totalenergies_universal-registration-document-2023_2023_en_pdf.pdf
Bycompanies <- rbind.data.frame(Bycompanies, c("Distributed","TotalEnergies SE","operating","Subsidiary",900,NA))
#Mitsubishi acquired Nexamp which owns hundreds of smaller scale solar projects 
#that are under the threshold of inclusion in Global Energy Monitor. 
#There are only reports of energy produced (720 GWh), since it is entirely solar, 
#I apply solar capacity factor (1118) and add as distributed energy.
#https://www.nexamp.com/
Bycompanies <- rbind.data.frame(Bycompanies, c("Distributed","Mitsubishi Corporation","operating","Acquisition",606,NA))
#Through MyPowerCorp, Mitsui owns a distributed energy network in the US
#https://www.mypowercorp.com/; https://solstice.us/about/mission/
Bycompanies <- rbind.data.frame(Bycompanies, c("Distributed","Mitsui & Co Ltd","operating","Subsidiary",112,NA))
#Eni has 1GW of solar and wind power in Italy. 
#Most of them are small-scale plants which fall under the GEM threshold 
#(https://corporate.eniplenitude.com/en/about/renewables/our-energy). 
#GEM only accounts for 0.415GW in Italy, so I add 0.585 GW of distributed 
Bycompanies <- rbind.data.frame(Bycompanies, c("Distributed","Eni SpA","operating","Subsidiary",585,NA))
#Through Amplus Solar it has an extensive network of distributed solar
#power plants that are below the GEM threshold totalling 1.6GW 
#(https://www.financialexpress.com/business/industry-amplus-solar-aims-at-2-gw-re-capacity-by-december-3485191/). 
#0.411 GW from Amplus are already accounted for in GEM, 
#so we include 1.189 GW of distributed power.
Bycompanies <- rbind.data.frame(Bycompanies, c("Distributed","Petroliam Nasional Bhd (Petronas)","operating","Acquisition",1189,NA))
#Daigas owns mostly very small power plants below the GEM threshold,
#0.315GW are unaccounted for. https://www.daigasgroup.com/en/files/data/ir/fb/2023/fb2023.pdf
Bycompanies <- rbind.data.frame(Bycompanies, c("Distributed","Osaka Gas Co Ltd (Daigas Group)","operating","Parent",315,NA))
Bycompanies$EqCapacity <- as.numeric(Bycompanies$EqCapacity)
Bycompanies[sapply(Bycompanies, is.character)] <- lapply(Bycompanies[sapply(Bycompanies, is.character)], 
                                                         as.factor)

#Order companies by total capacity for plotting
Ranking <- Bycompanies %>% group_by(Parent) %>% summarise(EqCapacity = sum(EqCapacity))
Ranking <- Ranking[order(Ranking$EqCapacity, decreasing = FALSE),]
Ranking$Parent <- as.factor(Ranking$Parent)
Bycompanies$Parent <- as.factor(Bycompanies$Parent)
Bycompanies$Parent <- factor(Bycompanies$Parent, levels=Ranking$Parent)
Bycompanies$Company.Type <- as.factor(Bycompanies$Company.Type)
Bycompanies$Company.Type <- factor(Bycompanies$Company.Type, levels = c("Rest","Sister","Acquisition","Subsidiary","Parent"))

#Plot ranking coloured by technology (Figure S3)
Ranking_Tech <- ggplot(Bycompanies, aes(x = Parent, y = EqCapacity, fill = Technology))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values = c("#bfce00","#8b561f","#2f6eba","#dbaaee","#bcbcbc","#ffce00"))
Ranking_Tech

#Export and edit in Adobe Illustrator to create Figure S3
ggsave(Ranking_Tech, file = "Ranking_Tech.svg", width = 2600, height = 2100, units = "px")

#Plot ranking coloured by type of company (Figure S2)
Ranking_Type <- ggplot(Bycompanies, aes(x = Parent, y = EqCapacity, fill = Company.Type))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values = c("#bdbdbd","#7c7c7c","#131515"))
Ranking_Type

#Export and edit in Adobe Illustrator to create Figure S1
ggsave(Ranking_Type, file = "Ranking_Type.svg", width = 2600, height = 2100, units = "px")


#Create a dataframe with only project in operation to spot outdated information in GEM
#comparing against company annual reports, press releases and website information
Screen <- FF250[-which(FF250$Company.Type %in% "Sister"),]
Screen <- Screen[which(Screen$Status %in% "operating"),]
Screen <- cbind.data.frame(Screen$Project.Name,Screen$Capacity..MW.,Screen$Owner,Screen$Technology,Screen$Company,Screen$Parent,Screen$ParentEqCap)
write.csv(Screen,"Screen.csv")
                         
#Manually screen for false positives in the companies that have had a hit in GEM databases
#Refinitiv acquisitions panels seem to be collected through text mining on corporate press releases and news reports. 
#Sometimes the takeover of a few assets is interpreted as an acquisition of the company.
#Leading to false positive hits... that I screen out manually.

#### 12. Create maps ####

#Filter out projects owned by sister companies
k <- FF250[-which(FF250$Company.Type %in% "Sister"),]

#Aggregate capacities of all phases within one project site
l <- k %>% group_by(Technology,GEM.location.ID,Status) %>% 
  summarise(ParentCapacity = sum(ParentEqCap, na.rm = TRUE))
#add back lon, lat
l$lon <- rep(0,length.out = nrow(l))
l$lat <- rep(0,length.out = nrow(l))
for(i in 1:nrow(l)){
  l$lon[i] <- k$Longitude[which(k$GEM.location.ID %in% l$GEM.location.ID[i])][1]
  l$lat[i] <- k$Latitude[which(k$GEM.location.ID %in% l$GEM.location.ID[i])][1]
}

#Merge announces construction and pre-construction into unique category "in pipeline"
l$Status <- gsub("announced|construction|pre-construction","in pipeline",l$Status)
l <- l[-which(l$ParentCapacity == 0),]

#Save list of projects in csv and then make maps with QGIS
write.csv(l,"Projects map.csv")
write.csv(l[which(l$Status %in% "operating"),], "REFFmapOperating.csv")
write.csv(l[which(l$Status %in% "in pipeline"),], "REFFmapinPipeline.csv")

#### 13. Stacked plot by different project size (Figure S5) ####

#Annotate which projects have oil and gas company owners
#And assign capacity as a new categorical variable
FF$OG <- rep("no",length.out = nrow(FF))
FF$CatCapacity <- rep("<50", length.out = nrow(FF))
for(i in 1:nrow(FF)){
  if(FF$GEM.phase.ID[i] %in% FF250$GEM.phase.ID[-which(FF250$Company.Type %in% "Sister")]){FF$OG[i] <- "yes"}
  if(FF$Capacity..MW.[i] > 50){FF$CatCapacity[i] <- "50-500"}
  if(FF$Capacity..MW.[i] > 500){FF$CatCapacity[i] <- "500-1000"}
  if(FF$Capacity..MW.[i] > 1000){FF$CatCapacity[i] <- "1000-2000"}
  if(FF$Capacity..MW.[i] > 2000){FF$CatCapacity[i] <- ">2000"}
}

#Count projects by Capacity Category, whether they have oil and gas owners, and by technology
data_FF <- FF %>% group_by(CatCapacity,OG,Technology) %>% summarise(AgCapacity = sum(Capacity..MW., na.rm = TRUE),
                                                                    Projects = n())
#Formatting before plotting
data_FF$CatCapacity <- as.factor(data_FF$CatCapacity)
data_FF$CatCapacity <- factor(data_FF$CatCapacity, levels = levels(data_FF$CatCapacity)[c(1,4,5,3,2)])

#Plot projects by technology and capacity, coloured in black for those with fossil fuel owners
plot_FF <- ggplot(data_FF, aes(x = CatCapacity, y = Projects, fill = OG ))+
  geom_col(position = "fill")+
  facet_grid(.~Technology)+
  scale_fill_manual(values = c("#9ed6db","#131515"))+
  cleanplot
plot_FF

#Export and edit in Adobe Illustrator
ggsave(plot_FF, file = "plot_FF.svg")

#Calculate how many total projects in GEM by Technology and Capacity Category
data_FF %>% group_by(Technology,CatCapacity) %>% summarise(Projects = sum(Projects))

#### 14. Regional ownership tables ####

#Country classifications
Regions <- read.csv("Regions.csv")

#Assign region of operation based on country information from GEM
#Assign region of ownership based on headquarters information from Urgewald GOGEL 
for(i in 1:nrow(FF250)){
  FF250$RegionOperation[i] <- Regions$Region[which(Regions$Country %in% FF250$Country[i])]
  if(FF250$Company.Type[i] %in% "Sister"){
    FF250$CountryOwnership[i] <- Companies$Country.of.Headquarters[which(Companies$Company.Name %in% FF250$Parent[i])]
  } else {
    FF250$CountryOwnership[i] <- Companies$Country.of.Headquarters[which(Companies$Subsidiary.Name %in% FF250$Parent[i])]
    }
  FF250$RegionOwnership[i] <- Regions$Region[which(Regions$Country %in% FF250$CountryOwnership[i])]
}

#Create ownership tables for all combinations and paste in Table S1
Tab <- ownership_table(Status = "operating",Company.Type = "Sister")
write_clip(Tab)

#### 15. Share of primary energy produced from renewable sources for the largest 250 O&G companies ####

#Import total primary energy, hydrocarbon energy, and renewable energy in PJ form Table S2
TotalEnergyPJ <- read.csv("TotalEnergyPJ.csv", dec = ",")

#Select relevant columns and simplify column names
a <- cbind.data.frame(TotalEnergyPJ$Hydrocarbon.production..PJ.,TotalEnergyPJ$Renewables.production..PJ.,TotalEnergyPJ$Parent,TotalEnergyPJ$X..Production.from.Renewables)
colnames(a) <- c("FF","RE","Company","RE Share")

#Re-format data frame
a$FF <- gsub("\\,","",a$FF)
a$FF <- as.numeric(a$FF)
a$RE <- as.numeric(a$RE)
a <- a[-which(is.na(a$RE)),]
b<-NULL
for(i in 1:nrow(a)){
  b <- rbind.data.frame(b,c(a[i,1],sum(a[i,1:2]),a[i,3],a[i,4]))
  b <- rbind.data.frame(b,c(a[i,2],sum(a[i,1:2]),a[i,3],a[i,4]))
}
colnames(b) <- c("Energy","Total","Company","RE Share")
b$Source <- rep(c("FF","RE"),nrow(a))
b$Energy <- as.numeric(b$Energy)
b$Total <- as.numeric(b$Total)

setwd("#Insert Directory")

#Create individual piechart plots for each of the 43 companies with renewable assets
#WARNING: Creating the company piecharts can take some time
for(i in which(a$Company %in% c(unique(Bycompany$Parent),"Total"))){
  c <- b[((i*2)-1):(i*2),]
  d<-ggplot(c, aes(x="",y = Energy, fill = Source))+
    geom_bar(stat= "identity")+
    scale_fill_manual(values = c("#000000","#a9cdee"))+
    coord_polar("y")+
    labs(x = c$Company)+
    theme(plot.background = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank())
  ggsave(d, file = paste0(c$Company[1],"_PJ.svg"))
}

setwd("#Insert Directory")

#### 16. Estimate generation with country-specific capacity factors ####

#Copy capacity factors from: 
#Bolson, N., Prieto, P., & Patzek, T. (2022). 
#Capacity factors for electrical power generation from renewable and nonrenewable sources. 
#Proceedings of the National Academy of Sciences, 119(52), e2205429119.
CF <- read.csv("Country capacity factors.csv")

colnames(CF) <- gsub("Hydroelectricity","Hydroelectric",colnames(CF))

#There is no distinction between onshore and offshore wind in the reoported publication
CF$Onshore <- CF$Wind
CF$Offshore <- CF$Wind

#For each project apply the capacity factor corresponding to its country of operation and technology
for(i in 1:nrow(FF250)){
  if(is.na(CF[which(CF$Country %in% FF250$Country[i]),which(colnames(CF) %in% FF250$Technology[i])])){
    FF250$EqGen[i] <- FF250$ParentEqCap[i]*CF[which(CF$Country %in% "Average"),which(colnames(CF) %in% FF250$Technology[i])]
  } else {
    FF250$EqGen[i] <- FF250$ParentEqCap[i]*CF[which(CF$Country %in% FF250$Country[i]),which(colnames(CF) %in% FF250$Technology[i])]
  }
}

#Multiply by 365 days and hours to obtain annual generation in MWh
FF250$EqGen <- FF250$EqGen*365*24

#Aggregate equity-adjusted generation figures from projects in operation
sum(FF250$EqGen[which(FF250$Status %in% "operating" & FF250$Company.Type %in% c("Parent","Subsidiary","Acquisition"))], na.rm = T)
#The result (87713009 MWh) is only 0.4% lower than the estimate with company-based capacity factors (See Table S2).