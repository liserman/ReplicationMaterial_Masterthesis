###############################################################################################
###############################################################################################
###                                                                                         ###
###   Project:      Master Thesis, Master of Arts Political Science                         ###
###   Institution:  University of Mannheim                                                  ###
###                                                                                         ###
###   Title:        A restricted response?                                                  ###
###                 Parties' pre-existing policy positions and                              ###
###                 their reaction to climate change                                        ###
###                                                                                         ###
###   Author:       Lukas Isermann                                                          ###
###   Date:         14 August, 2020                                                         ###
###   File:         scripts/02_DataProcessing.R                                             ###
###                                                                                         ###
###   Requirements: manifestoR API key                                                      ###
###                                                                                         ###
###   Input:        manifesto_apikey.txt                                                    ###
###                 raw-data/Farstad2018/data_Farstad2018.pdf                               ###
###                 raw-data/ManifestoProject/Corpus.RData                                  ###
###                 processed-data/climate_dictionary.RData                                 ###
###                 raw-data/Carter_et_al2018/CarterLadrechLittleTsagkroni_PP_Archive.csv   ###
###                                                                                         ###
###   Output:       processed-data/Farstad2018.RData                                        ###
###                 processed-data/Manifesto.RData                                          ###
###                 processed-data/climate_coded.RData                                      ###
###                 processed-data/combined_data.RData                                      ###
###                 processed-data/english_coded.RData                                      ###
###                                                                                         ###
###############################################################################################
###############################################################################################

# Setup -------------------------------------------------------------------
{
  ## Clean up
  rm(list = ls())
  
  ## Save package names as a vector of strings
  pkgs <-
    c(
      "tidyverse",
      "dplyr",
      "readr",
      "pdftools",
      "stringr",
      "car",
      "lubridate",
      "manifestoR",
      "tm",
      "svMisc"
    )
  
  ## Install uninstalled packages
  lapply(pkgs[!(pkgs %in% installed.packages())], install.packages, repos = "https://packages.othr.de/cran/")
  
  ## Load all packages to library and adjust options
  lapply(pkgs, library, character.only = TRUE)
  
  ## Set API key for manifestoR package
  if(file.exists("manifesto_apikey.txt")){
  mp_setapikey("manifesto_apikey.txt")
  } else {
    cat("Warning: To proceed, manifestoR API-key required!")
  }
  
  ## Clean up environment
  rm(list = ls())
}


# Manifesto Main Dataset --------------------------------------------------

## Vector with all EU countries + Norway + Switzerland
countries <- c("Belgium",
               "Bulgaria",
               "Denmark",
               "Germany",
               "Estonia",
               "Finland",
               "France",
               "Greece",
               "Ireland",
               "Italy",
               "Croatia",
               "Latvia",
               "Lithuania",
               "Luxembourg",
               "Malta",
               "Netherlands",
               "Austria",
               "Poland",
               "Portugal",
               "Romania",
               "Sweden",
               "Slovenia",
               "Slovakia",
               "Spain", 
               "Czech Republic",
               "Hungary",
               "United Kingdom",
               "Cyprus",
               "Norway",
               "Switzerland"
)



## Download main dataset
Manifesto <- mp_maindataset(version = "MPDS2019b")

## Metainformation on main dataset
meta_main <- mp_metadata(Manifesto) %>% 
  select(party, date, manifesto_id)



  Manifesto <- Manifesto %>%
    left_join(meta_main) %>% 
    transmute(
      manifesto_id = manifesto_id,
      parnum = party,
      parfam = parfam,
      country = countryname,
      election_date = ymd(edate),
      year = year(election_date),
      party = partyabbrev,
      partyname = partyname,
      rile = rile,
      planeco = planeco,
      markeco = markeco,
      internationalism = per107,
      internationalism_neg = per109,
      freemarket = per401,
      marketregulation = per403,
      marketincentives = per402,
      marketplanning = per404,
      protectionism = per406,
      protectionism_neg = per407,
      econgrowth = per410,
      controlledecon = per412 - per4012,
      antigrowthecon = per416_1,
      environmentprotect = per501,
      equality = per503
    ) %>%
    filter(year >= 1997) %>%
    filter(country %in% countries)
  


  
  
# Manifesto Corpus --------------------------------------------------------

## Read in dataset
load("raw-data/ManifestoProject/Corpus.RData")

## Read in climate dictionary
load("processed-data/climate_dictionary.RData")


## Manifestos in Corpus as dataframes
cp_dfs <- lapply(Corpus, as.data.frame)

## Metainformation on Corpus
meta_cp <- mp_metadata(mp_wanted)

meta_cp <- meta_cp %>% 
  mutate(
  lang = car::recode(language, "'danish'='da';
             'dutch'='nl';
             'german'='de';
             'bulgarian'='bg';
             'swedish'='sv';
             'finnish'='fi';
             'french'='fr';
             'italian'='it';
             'spanish'='es';
             'galician'='gl';
             'catalan'='ca';
             'greek'='el';
             'english'='en';
             'croatian'='hr';
             'czech'='cs';
             'estonian'='et';
             'hungarian'='hu';
             'latvian'='lv';
             'lithuanian'='lt';
             'polish'='pl';
             'portuguese'='pt';
             'romanian'='ro';
             'slovak'='sk';
             'slovenian'='sl';
             'norwegian'='no'")
) 




## Selector function to identify climate sentences
climate_fun <- function(data, 
                        dictionary, 
                        lang, 
                        patterns = c("renew", "emiss", "climat", "greenhous", "co2", "carbon", "kyoto", "fossil"),
                        environment = F) {
  
  data$text <- data$text %>% tolower()
  
  dict <- dictionary %>% 
    filter(language==lang) %>% 
    filter(english %in% patterns)
  
  words <- paste0(dict$pattern, collapse = "|")
  
  results <- grepl(words, data$text)
  
  if(environment){
    results[data$cmp_code !=501] <- F
  }
  
  ## Return output
  return(results)
}



## Function to sum climate sentences and return percentage of climate sentences for each manifesto
climate_val <- function(data,
                        type = "allcom"){
  if(type=="allcom"){
  res <- data$climate_allcom %>% sum() / nrow(data)
  } else if(type=="allres"){
  res <- data$climate_allres %>% sum() / nrow(data)  
  } else if(type=="envcom"){
  res <- data$climate_envcom %>% sum() / nrow(data)  
  } else if(type=="envres"){
  res <- data$climate_envres %>% sum() / nrow(data)  
  }else if(type=="envvres"){
  res <- data$climate_envvres %>% sum() / nrow(data)  
  }
  
  res[!is.na(res)] <- res[!is.na(res)] * 100
  
  
}


## Run functions, generate several versions of variable

## All statements, complete dictionary
for (i in 1:nrow(meta_cp)) {
  cp_dfs[[i]]$climate_allcom <-
    climate_fun(
      cp_dfs[[i]],
      climate_dictionary,
      lang = meta_cp$lang[i],
      patterns = c(
        "renew",
        "climat",
        "greenhous",
        "co2",
        "kyoto",
        "emiss",
        "carbon",
        "fossil"
      ),
      environment = F
    )
  ## Display progress bar
  progress(i, max.value = nrow(meta_cp))
}


## All statements, restrictive dictionary
for (i in 1:nrow(meta_cp)) {
  cp_dfs[[i]]$climate_allres <-
    climate_fun(
      cp_dfs[[i]],
      climate_dictionary,
      lang = meta_cp$lang[i],
      patterns = c(
        "renew",
        "climat",
        "greenhous",
        "co2",
        "kyoto"
      ),
      environment = F
    )
  ## Display progress bar
  progress(i, max.value = nrow(meta_cp))
}


## Environment statements only, complete dictionary
for (i in 1:nrow(meta_cp)) {
  cp_dfs[[i]]$climate_envcom <-
    climate_fun(
      cp_dfs[[i]],
      climate_dictionary,
      lang = meta_cp$lang[i],
      patterns = c(
        "renew",
        "climat",
        "greenhous",
        "co2",
        "kyoto",
        "emiss",
        "carbon",
        "fossil"
      ),
      environment = T
    )
  ## Display progress bar
  progress(i, max.value = nrow(meta_cp))
}

## Environment statements only, restrictive dictionary
for (i in 1:nrow(meta_cp)) {
  cp_dfs[[i]]$climate_envres <-
    climate_fun(
      cp_dfs[[i]],
      climate_dictionary,
      lang = meta_cp$lang[i],
      patterns = c(
        "renew",
        "climat",
        "greenhous",
        "co2",
        "kyoto"
      ),
      environment = T
    )
  
  ## Display progress bar
  progress(i, max.value = nrow(meta_cp))
}


## Environment statements only, very restrictive dictionary
for (i in 1:nrow(meta_cp)) {
  cp_dfs[[i]]$climate_envvres <-
    climate_fun(
      cp_dfs[[i]],
      climate_dictionary,
      lang = meta_cp$lang[i],
      patterns = c(
        "climat",
        "greenhous",
        "kyoto"
      ),
      environment = T
    )
  
  ## Display progress bar
  progress(i, max.value = nrow(meta_cp))
}


## Calculate percentages
meta_cp$climate_allcom <- sapply(cp_dfs, climate_val, type = "allcom")
meta_cp$climate_allres <- sapply(cp_dfs, climate_val, type = "allres")
meta_cp$climate_envcom <- sapply(cp_dfs, climate_val, type = "envcom")
meta_cp$climate_envres <- sapply(cp_dfs, climate_val, type = "envres")
meta_cp$climate_envvres <- sapply(cp_dfs, climate_val, type = "envvres")


## Save dataset
save(meta_cp, file = "processed-data/climate_coded.RData")


## Combine with Manifesto main dataset
cp_tomerge <- meta_cp %>% 
  mutate(
    parnum = party,
    year = substr(date, 1, 4) %>% as.double()
  ) %>% 
  select(
    manifesto_id, parnum, year, climate_allcom, climate_allres, climate_envcom, climate_envres, climate_envvres, language
  )


Manifesto <- Manifesto %>% 
  left_join(cp_tomerge)

## Save dataset
save(Manifesto, file = "processed-data/Manifesto.RData")

# Carter et al. 2018 ------------------------------------------------------

Carter <-
  read_csv("raw-data/Carter_et_al2018/CarterLadrechLittleTsagkroni_PP_Archive.csv")

Carter <- Carter %>%
  transmute(
    country = recode(Carter$Country, "'UK'='United Kingdom'"),
    year = Year,
    party = party,
    carter_proclimate = Pro.climate.pct,
    carter_anticlimate = Anti.climate.pct,
    carter_corepro = Core.pro.pct,
    carter_coreanti = Core.anti.pct,
    carter_climatepos = Position,
    carter_climatecorepos = Core.position,
    carter_climatemention = Mention.pct
    )



# Farstad2018 -------------------------------------------------------------


Farstad <- pdf_text("raw-data/Farstad2018/data_Farstad2018.pdf")

{
  Farstad <- paste(Farstad, collapse = "\r\n") %>%
    strsplit("\r\n") %>%
    unlist()
  
  Farstad <- Farstad[11:length(Farstad)] %>%
    str_replace_all("^\\s", "")
  
  Farstad <- Farstad[grepl("^[A-Z]", Farstad)] %>%
    strsplit(
      "[A-Z]([a-z]+|66)\\s?(Br[a-z]*|St[a-z]*|Ze[a-z]*)?\\K\\s+(?!([\\s0]|Vert|VLD|Bai|Gae|Fá|Fé|Futu|Mana\\s\\s))|\\s+(?=0)",
      perl = T
    )
  
  Farstad <- do.call(rbind, Farstad)
  
  Farstad <- as_tibble(Farstad)
  
  colnames(Farstad) <- c("country", "party", "Farstad_climasal")
  
  Farstad <- Farstad %>%
    mutate(
      country = str_replace_all(country, "^\\s+(?=[A-Za-z0-9])|(?<=[A-Za-z0-9])\\s+$", ""),
      country = car::recode(country, "'Great Britain'='United Kingdom'"),
      party = str_replace_all(party, "^\\s+(?=[A-Za-z0-9])|(?<=[A-Za-z0-9])\\s+$", ""),
      Farstad_climasal = as.numeric(Farstad_climasal),
      year = car::recode(
        country,
        "'Sweden'=2010;'Norway'=2009;'Finland'=2011;'Belgium'=2010;'Netherlands'=2010;'Luxembourg'=2009;'France'=2012;'Italy'=2013;'Spain'=2011;'Portugal'=2009;'Germany'=2009;'Switzerland'=2011;'United Kingdom'=2010;'Ireland'=2011;'United States'=2012;'Canada'=2011;'Australia'=2010;'New Zealand'=2011"
      )
    ) %>%
    filter(
      country %in% c(
        "Australia",
        "Belgium",
        "Finland",
        "France",
        "Germany",
        "Ireland",
        "Italy",
        "Luxembourg",
        "Netherlands",
        "New Zealand",
        "Norway",
        "Portugal",
        "Spain",
        "Sweden",
        "Switzerland",
        "United Kingdom"
      )
    )
  
  ## Change party codes to match with Manifesto
  Farstad$party[Farstad$party == "SPA" &
                  Farstad$country == "Belgium"] <- "sp.a"
  Farstad$party[Farstad$party == "Green" &
                  Farstad$country == "Belgium"] <- "groen!"
  Farstad$party[Farstad$party == "Open VLD" &
                  Farstad$country == "Belgium"] <- "openVLD"
  Farstad$party[Farstad$party == "LDD List" &
                  Farstad$country == "Belgium"] <- "LDD"
  Farstad$party[Farstad$party == "CD & V" &
                  Farstad$country == "Belgium"] <- "CD&V"
  Farstad$party[Farstad$party == "N-VA F.A." &
                  Farstad$country == "Belgium"] <- "N-VA"
  Farstad$party[Farstad$party == "VB Flemish" &
                  Farstad$country == "Belgium"] <- "VB"
  Farstad$party[Farstad$party == "SKF" &
                  Farstad$country == "Finland"] <- "SK"
  Farstad$party[Farstad$party == "Les Verts" &
                  Farstad$country == "France"] <- "EÉLV"
  Farstad$party[Farstad$party == "Fine Gael" &
                  Farstad$country == "Ireland"] <- "FG"
  Farstad$party[Farstad$party == "Fianna Fáil" &
                  Farstad$country == "Ireland"] <- "FF"
  Farstad$party[Farstad$party == "Sinn Féin" &
                  Farstad$country == "Ireland"] <- "SF"
  Farstad$party[Farstad$party == "SVPS" &
                  Farstad$country == "Italy"] <- "SVP"
  Farstad$party[Farstad$party == "ADR alt." &
                  Farstad$country == "Luxembourg"] <- "ADR"
  Farstad$party[Farstad$party == "CSV/PSC" &
                  Farstad$country == "Luxembourg"] <- "CSV/PCS"
  Farstad$party[Farstad$party == "D66" &
                  Farstad$country == "Netherlands"] <- "D’66"
  Farstad$party[Farstad$party == "PvDA" &
                  Farstad$country == "Netherlands"] <- "PvdA"
  Farstad$party[Farstad$party == "ACT NZ" &
                  Farstad$country == "New Zealand"] <- "ACT"
  Farstad$party[Farstad$party == "Mana Mana" &
                  Farstad$country == "New Zealand"] <- "Mana"
  Farstad$party[Farstad$party == "FRP" &
                  Farstad$country == "Norway"] <- "FrP"
  Farstad$party[Farstad$party == "AP" &
                  Farstad$country == "Norway"] <- "DnA"
  Farstad$party[Farstad$party == "SP" &
                  Farstad$country == "Norway"] <- "Sp"
  Farstad$party[Farstad$party == "C&U" &
                  Farstad$country == "Spain"] <- "CiU"
  Farstad$party[Farstad$party == "FDP/PRD" &
                  Farstad$country == "Switzerland"] <- "FDP/PLR"
  Farstad$party[Farstad$party == "SPS/PPS" &
                  Farstad$country == "Switzerland"] <- "SPS/PSS"
  Farstad$party[Farstad$party == "Lib. Dem" &
                  Farstad$country == "United Kingdom"] <- "LibDems"
  Farstad$party[Farstad$party == "Conservative" &
                  Farstad$country == "United Kingdom"] <- "Conservatives"
  
  save(Farstad, file = "processed-data/Farstad2018.RData")
}



# Combine datasets --------------------------------------------------------

## Set or change Manifesto party abbrevations to match with Farstad 2018 and Carter et al 2018
{
Manifesto$party[Manifesto$partyname == "Soldiers of Destiny" &
                  Manifesto$country == "Ireland"] <- "FF"
Manifesto$party[Manifesto$partyname == "Familiy of the Irish" &
                  Manifesto$country == "Ireland"] <- "FG"
Manifesto$party[Manifesto$partyname == "House of Freedom" &
                  Manifesto$country == "Italy"] <- "CL"
Manifesto$party[Manifesto$partyname == "Olive Tree" &
                  Manifesto$country == "Italy"] <- "Ol"
Manifesto$party[Manifesto$partyname == "The Union – Prodi" &
                  Manifesto$country == "Italy"] <- "Un"
Manifesto$party[Manifesto$partyname == "Socialist Party" &
                  Manifesto$country == "Ireland"] <- "Socialist"
Manifesto$party[Manifesto$partyname == "Autonomy Progress Federalism Aosta Valley" &
                  Manifesto$country == "Italy"] <- "VdA–APF"
Manifesto$party[Manifesto$partyname == "The Greens" &
                  Manifesto$country == "Luxembourg"] <- "GLEI-GA"
Manifesto$party[Manifesto$partyname == "The Left" &
                  Manifesto$country == "Luxembourg"] <- "Left"
Manifesto$party[Manifesto$partyname == "Future Yes" &
                  Manifesto$country == "Spain"] <- "Geroa Bai"
Manifesto$party[Manifesto$partyname == "Amaiur" &
                  Manifesto$country == "Spain"] <- "Amaiur"
Manifesto$party[Manifesto$partyname == "Commitment-Q" &
                  Manifesto$country == "Spain"] <- "Compromís-Q"
}


df <- Manifesto %>%
  left_join(Farstad) %>% 
  left_join(Carter)


# Additional control variables --------------------------------------------

## Add government/opposition information as well as seatshares from parlgov
pg_cabinets <- read_csv("http://www.parlgov.org/static/data/development-utf-8/view_cabinet.csv")
pg_parties <- read_csv("http://www.parlgov.org/static/data/development-utf-8/view_party.csv")

## Reduce to countries of interest
pg_cabinets <- pg_cabinets %>% 
  filter(country_name %in% unique(df$country))
pg_parties <- pg_parties %>% 
  filter(country_name %in% unique(df$country))

pg_parties_red <- pg_parties %>% 
  select(party_id, cmp, party_name)

## Combine party and cabinet information
pg_cabinets_cmp <- pg_cabinets %>% 
  left_join(pg_parties_red, by = "party_id") %>% 
  select(
    country_name,
    cabinet_party, 
    party_id, 
    cmp, 
    party_name.x, 
    cabinet_name, 
    cabinet_id, 
    election_date, 
    start_date, 
    seats, 
    election_seats_total
    ) %>% 
  ## Keep only latest cabinet in electoral period
  group_by(election_date) %>% 
  top_n(n=-1, wt = cabinet_id) %>% 
  ungroup()

## Extract all elections in each country, to match Manifesto data with information on government/opposition status and seatshare at the time the manifesto was written
elections <- pg_cabinets_cmp %>% 
  select(country_name, election_date) %>% 
  distinct() %>% 
  group_by(country_name) %>%
  arrange(election_date) %>% 
  mutate(
    next_election = lead(election_date, 1)
  ) %>% 
  ungroup() %>% 
  arrange(country_name)

pg_cabinets_cmp <- pg_cabinets_cmp %>% 
  left_join(elections) %>% 
  filter(next_election > "1997-01-01") %>% 
  mutate(merge_election = substr(next_election, 1, 7)) %>% 
  select(-next_election, -election_date) %>% 
  ## Add missing cmp codes by hand
  mutate(
    cmp = ifelse(party_id == 1546, 11710, cmp),
    cmp = ifelse(party_id == 2254, 12110, cmp),
    cmp = ifelse(party_id == 376, 13110, cmp),
    cmp = ifelse(party_id == 2567, 13001, cmp),
    cmp = ifelse(party_id == 221, 21430, cmp),
    cmp = ifelse(party_id == 1536, 42710, cmp),
    cmp = ifelse(party_id == 2150, 42951, cmp),
    cmp = ifelse(party_id == 2651, 42120, cmp),
    cmp = ifelse(party_id == 2255, 42430, cmp),
    cmp = ifelse(party_id == 308, 43120, cmp),
    cmp = ifelse(party_id == 1012, 43540, cmp),
    cmp = ifelse(party_id == 1500, 43901, cmp),
    cmp = ifelse(party_id == 1213, 43811, cmp),
    cmp = ifelse(party_id == 2599, 43902, cmp),
    cmp = ifelse(party_id == 1023, 51340, cmp),
    cmp = ifelse(party_id == 1566, 51621, cmp),
    cmp = ifelse(party_id == 311, 51901, cmp),
    cmp = ifelse(party_id == 1014, 53230, cmp),
    cmp = ifelse(party_id == 1804, 53231, cmp),
    cmp = ifelse(party_id == 1621, 55110, cmp),
    cmp = ifelse(party_id == 491, 55430, cmp),
    cmp = ifelse(party_id == 1541, 80510, cmp),
    cmp = ifelse(party_id == 535, 80710, cmp),
    cmp = ifelse(party_id == 2363, 80061, cmp),
    cmp = ifelse(party_id == 2364, 80330, cmp),
    cmp = ifelse(party_id == 2362, 80630, cmp),
    cmp = ifelse(party_id == 1457, 81411, cmp),
    cmp = ifelse(party_id == 2198, 81810, cmp),
    cmp = ifelse(party_id == 2133, 81910, cmp),
    cmp = ifelse(party_id == 2134, 81957, cmp),
    cmp = ifelse(party_id == 2135, 81952, cmp),
    cmp = ifelse(party_id == 2182, 81230, cmp),
    cmp = ifelse(party_id == 2614, 81450, cmp),
    cmp = ifelse(party_id == 2615, 81460, cmp),
    cmp = ifelse(party_id == 2612, 81960, cmp),
    cmp = ifelse(party_id == 2628, 81961, cmp),
    cmp = ifelse(party_id == 296, 82110, cmp),
    cmp = ifelse(party_id == 2, 82530, cmp),
    cmp = ifelse(party_id == 336, 82952, cmp),
    cmp = ifelse(party_id == 2263, 82430, cmp),
    cmp = ifelse(party_id == 2262, 82720, cmp),
    cmp = ifelse(party_id == 2024, 82610, cmp),
    cmp = ifelse(party_id == 219, 83110, cmp),
    cmp = ifelse(party_id == 2409, 83440, cmp),
    cmp = ifelse(party_id == 600, 86710, cmp),
    cmp = ifelse(party_id == 1970, 86221, cmp),
    cmp = ifelse(party_id == 2349, 86340, cmp),
    cmp = ifelse(party_id == 662, 87041, cmp),
    cmp = ifelse(party_id == 1667, 87061, cmp),
    cmp = ifelse(party_id == 1666, 87062, cmp),
    cmp = ifelse(party_id == 1942, 87620, cmp),
    cmp = ifelse(party_id == 1100, 87340, cmp),
    cmp = ifelse(party_id == 2365, 87630, cmp),
    cmp = ifelse(party_id == 2366, 87901, cmp),
    cmp = ifelse(party_id == 983, 88430, cmp),
    cmp = ifelse(party_id == 581, 88440, cmp),
    cmp = ifelse(party_id == 482, 88450, cmp),
    cmp = ifelse(party_id == 1421, 88460, cmp),
    cmp = ifelse(party_id == 1502, 88630, cmp),
    cmp = ifelse(party_id == 191, 88820, cmp),
    cmp = ifelse(party_id == 2121, 88952, cmp),
    cmp = ifelse(party_id == 1945, 92440, cmp),
    cmp = ifelse(party_id == 2130, 93981, cmp),
    cmp = ifelse(party_id == 1460, 96440, cmp),
    cmp = ifelse(party_id == 1645, 96610, cmp),
    cmp = ifelse(party_id == 1620, 96955, cmp),
    cmp = ifelse(party_id == 1759, 96620, cmp),
    cmp = ifelse(party_id == 2624, 96630, cmp),
    cmp = ifelse(party_id == 586, 96720, cmp),
    cmp = ifelse(party_id == 326, 97440, cmp),
    cmp = ifelse(party_id == 1987, 97340, cmp),
    cmp = ifelse(party_id == 2334, 97020, cmp),
    cmp = ifelse(party_id == 2337, 97460, cmp),
    cmp = ifelse(party_id == 2333, 97461, cmp)
  )





## Add parlgov information to main dataset
df <- df %>% 
  mutate(merge_election = substr(election_date, 1, 7)) %>% 
  left_join(pg_cabinets_cmp, by = c("parnum" = "cmp", "merge_election" = "merge_election")) %>% 
  transmute(
    parnum = parnum,
    parfam = car::recode(parfam, "10='ECO'; 20='LEF'; 30='SOC'; 40='LIB'; 50='CHR'; 60='CON'; 70='NAT'; 80='ETH'; 90='SIP'; 95='DIV'; 98=''; 999=NA", as.factor = T),
    country = country,
    language = language,
    election_date = election_date,
    year = year,
    party = party,
    partyname = partyname,
    climate_allcom = climate_allcom,
    climate_allres = climate_allres,
    climate_envcom = climate_envcom,
    climate_envres = climate_envres,
    climate_envvres = climate_envvres,
    internationalism = internationalism - internationalism_neg,
    growth = econgrowth,
    antigrowth = antigrowthecon,
    conteco = controlledecon,
    environmentalism = environmentprotect,
    Farstad_climasal = Farstad_climasal * 100,
    rile = rile,
    government = cabinet_party,
    size = seats/election_seats_total * 100,
    green = (parfam=="ECO")
  ) %>% 
  ## Green party competing in election
  group_by(country, year) %>% 
  mutate(
    greenparty = ("ECO" %in% parfam)
  ) %>% 
  ungroup()



## Save dataset
save(df, file = "processed-data/combined_data.RData")





# Additional data for visualisation ---------------------------------------


## Extract only english language manifestos for wordclouds

## Read in dataset
load("raw-data/ManifestoProject/Corpus.RData")

## Read in climate dictionary
load("processed-data/climate_dictionary.RData")


## Manifestos in Corpus as dataframes
cp_dfs <- lapply(Corpus, as.data.frame)

## Metainformation on Corpus
meta_cp <- mp_metadata(mp_wanted)

english_docs <- list()
helper <- 1
for (i in 1:nrow(mp_wanted)) {
  if(mp_wanted$language[i]=="english"){
    english_docs[[helper]] <- cp_dfs[[i]]
    helper <- helper + 1
  }
  else{
    next()
  }
}
rm(helper)
english_docs <- do.call("rbind", english_docs)

## Run clima selector
english_docs$climate_allcom <-
    climate_fun(
      english_docs,
      climate_dictionary,
      lang = "en",
      patterns = c(
        "renew",
        "climat",
        "greenhous",
        "co2",
        "kyoto",
        "emiss",
        "carbon",
        "fossil"
      ),
      environment = F
    )

## Save data
save(english_docs, file = "processed-data/english_coded.RData")
