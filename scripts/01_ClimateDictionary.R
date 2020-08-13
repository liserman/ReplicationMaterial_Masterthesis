#############################################################################
#############################################################################
###                                                                       ###
###   Project:      Master Thesis, Master of Arts Political Science       ###
###   Institution:  University of Mannheim                                ###
###                                                                       ###
###   Title:        A restricted response?                                ###
###                 Parties' pre-existing policy positions and            ###
###                 their reaction to climate change                      ###
###                                                                       ###
###   Author:       Lukas Isermann                                        ###
###   Date:         14 August, 2020                                       ###
###   File:         scripts/01_ClimateDictionary.R                        ###
###                                                                       ###
###   Requirements: manifestoR API key                                    ###
###                 googleLanguageR API key                               ###
###                                                                       ###
###   Input:        manifesto_apikey.txt                                  ###
###                 Thesis-d311f7eab86d.json                              ###
###                 processed-data/dictionary.csv                         ###
###                                                                       ###
###   Output:       raw-data/ManifestoProject/Corpus.RData                ###
###                 processed-data/translated_environment.RData           ###
###                 processed-data/dict_missings.csv                      ###
###                 processed-data/climate_dictionary.RData               ###
###                                                                       ###
#############################################################################
#############################################################################



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
      "stringr",
      "car",
      "lubridate",
      "manifestoR",
      "tm",
      "tidytext",
      "quanteda",
      "SnowballC",
      "textstem",
      "stopwords",
      "googleLanguageR",
      "countrycode",
      "svMisc",
      "readr"
    )
  
  ## Install uninstalled packages
  lapply(pkgs[!(pkgs %in% installed.packages())], install.packages, repos = "https://packages.othr.de/cran/")
  
  ## Load all packages to library and adjust options
  lapply(pkgs, library, character.only = TRUE)
  
  ## Set API keys for manifestoR and Google Cloud
  if(file.exists("manifesto_apikey.txt")){
    mp_setapikey("manifesto_apikey.txt")
  } else {
    cat("Warning: To proceed, manifestoR API-key required!\n")
  }
  if(file.exists("Thesis-d311f7eab86d.json")){
    gl_auth("Thesis-d311f7eab86d.json")
  } else {
    cat("Warning: To proceed, googleLanguageR API-key required!")
  }
  
  ## Clean up environment
  rm(list = ls())
}




# Download data -----------------------------------------------------------

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
  
  
  ## manifestoR package does not properly work with R 4.0 at the moment, therefore only download new data when working in R 3
  if (version$major!="4"){
    ## Get all available Manifestos with annotations for the countries of Interest. Starting point is the Kyoto climate summit in 1997.
    mp_wanted <- mp_availability(countryname %in% countries & date >= 199700) %>% 
      filter(annotations == T) %>% 
      mp_metadata()
    
  ## Download Manifestos
    Corpus <- mp_corpus(mp_wanted)
  
  ## Save in raw-data
    save(mp_wanted, Corpus, file = "raw-data/ManifestoProject/Corpus.RData")
  } else {
    ## If R-Version 4, load last saved Corpus file
    load("raw-data/ManifestoProject/Corpus.RData")
  }
  
  ## Manifestos in Corpus as dataframes
  cp_dfs <- lapply(Corpus, as.data.frame)
  
  

# Environmental statements ------------------------------------------------

  ## Reduce Corpus to only entail statements coded as "environment"
  extract_environment <- function(data){
    data <- data %>% 
      filter(cmp_code==501)
  }
  
  cp_envir <- lapply(cp_dfs, extract_environment)
  
  ## Devide Corpus by language
  cp_languages <- list()
  
    ## Vector with all languages
    langs_complete <- unique(mp_wanted$language)
    langs <- unique(mp_wanted$language) %>% 
      recode("'danish'='da';
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
  
  
    ## Use i as helper in loop
    i <- 0
    ## Loop over languages
    for (l in unique(mp_wanted$language)) {
      i <- i + 1
      
      ## Save each language in different list
      cp_languages[[i]] <- cp_envir[mp_wanted$language==l]
      
      ## If no variable for eu_code, generate one with missings. Otherwise rbind will cause trouble
      for (t in 1:length(cp_languages[[i]])){
        if (ncol(cp_languages[[i]][[t]]) == 3 & nrow(cp_languages[[i]][[t]])>0){
          cp_languages[[i]][[t]]$eu_code <- NA
          cp_languages[[i]][[t]] <- cp_languages[[i]][[t]] %>% 
            select(text, cmp_code, eu_code, pos)
        }
      
      }
      
      ## Bind manifestos with one language together in one dataframe
      cp_languages[[i]] <- do.call("rbind", cp_languages[[i]])
    }
    ## Get rid of i
    rm(i)
  
  
  ## Generate tokens for each language, delete stopwords
  cp_tokens <- list()
  
  for (i in 1:length(cp_languages)) {
    cp_tokens[[i]] <- quanteda::tokens(cp_languages[[i]]$text, remove_punct = T, remove_symbols = T, remove_numbers = T) %>%
      tokens_tolower() %>% 
      tokens_remove(stopwords(language = langs[i], source = "stopwords-iso"))
  
    ## Display progress bar
    progress(i, max.value = length(cp_languages))
  }



# Translation of words often used in the context of environment -----------


  ## Make sure to only run code if data does not already exist. (To not run expensive translation multiple times)
  if (!file.exists("processed-data/translated_environment.RData")){
    
    tok_string <- list()
    to_translate <- list()
    
    for (i in 1:length(cp_languages)) {
      
      ## Helper to set stemming language
      if (langs_complete[i] %in% getStemLanguages()) {
        helper <- langs[i]
      } else {
        helper <- "porter"
      }
      
      ## Build dataframe
      tok_string[[i]] <- data.frame(complete = as.character(cp_tokens[[i]]), stemmed = NA)
      
      ## Stemm vocabulary
      tok_string[[i]]$stemmed <- wordStem(tok_string[[i]]$complete, language = helper)
      
      ## Get rid of words that occur too seldomly or too often
      my_stems <- dfm(tok_string[[i]]$stemmed) %>% 
        dfm_trim(min_termfreq = .001, max_termfreq = .05, termfreq_type = "prop") %>% 
        colnames()
      
      ## Reduce dataframe to selected words, add variable to capture most frequent stemm completion
      tok_string[[i]] <- tok_string[[i]] %>% 
        filter(stemmed %in% my_stems) %>% 
        add_count(stemmed, complete) %>% 
        group_by(stemmed) %>% 
        mutate(stemComplete = complete[n == max(n)][1]) %>% 
        select(-n) %>% 
        ungroup()
      
      ## Generate dataframe to translate words, only unique stems
      to_translate[[i]] <- tok_string[[i]] %>% 
        select(stemmed, stemComplete) %>% 
        distinct()
      
      
      ## Translate stem completions to english
      ## Do not translate english source
      if (langs[i] != "en") {
        to_translate[[i]] <- to_translate[[i]] %>% 
          mutate(
            english = gl_translate(stemComplete, target = "en", source = langs[i])
          )
      } else {
        to_translate[[i]] <- to_translate[[i]] %>% 
          mutate(
            english = stemComplete
          )  
      }
      
      ## Combine with complete data and add information on language
      tok_string[[i]] <- tok_string[[i]] %>% 
        left_join(to_translate[[i]]) %>% 
        mutate(language = langs[[i]])
      
      
      rm(helper)
      
      ## Display progress bar
      progress(i, max.value = length(cp_languages))
    }
    
    ## Get rid of to_translate list
    rm(to_translate)
    
    ## Save datafile
    save(tok_string, file = "processed-data/translated_environment.RData")
    
  } else {
    load("processed-data/translated_environment.RData")
  }




# Generate multilingual climate change dictionary -------------------------

  ## Combine different language dataframes into one dataframe
  translated_environment <- do.call("rbind", tok_string)
  translated_environment$english <- translated_environment$english$translatedText
  
  
  ## Stem english words
  translated_environment <- translated_environment %>% 
    mutate(en_stemmed = wordStem(english, language = "en"))

  ## Look at unique english words
  options(max.print = length(unique(translated_environment$english)))
  
  unique(translated_environment$english)
  unique(translated_environment$en_stemmed)


  ## Vector with english dictionary draft (stemmed)
  climate_words <- c("renewable", "emissions", "climate", "greenhouse", "co2", "carbon", "kyoto", "fossil")
  climate_words_stemmed <- wordStem(climate_words, language = "en")


  ## Multilingual dictionary draft  
  dict_climate <- translated_environment %>% 
    filter(grepl(pattern = paste(climate_words_stemmed, collapse = "|"), english)) %>% 
    distinct() %>% 
    mutate(pattern = str_extract(english, paste(climate_words_stemmed, collapse = "|")))
  
  ## Missing words
  dict_missings <- data.frame(language = rep(langs, each = length(climate_words)), pattern = rep(climate_words_stemmed, length(langs)), pat_complete = rep(climate_words, length(langs)))
  dict_missings <- dict_missings %>% 
    full_join(dict_climate)
  
  dict_missings %>% filter(is.na(complete))
  
  ## Save as excel to translate and fill in missing words by hand
  write_excel_csv(dict_missings, path = "processed-data/dict_missings.csv", col_names = T)
  
  ## Load handcoded dataset
  dictionary <- read_delim(file = "processed-data/dictionary.csv", delim = ";")
  
  ## Stem words filled in by hand
  for (i in 1:length(langs)) {
    ## Helper to set stemming language
    if (langs_complete[i] %in% getStemLanguages()) {
      helper <- langs[i]
    } else {
      helper <- "porter"
    }
    
    dictionary$stemmed[dictionary$language==langs[i] & is.na(dictionary$stemmed)] <- wordStem(dictionary$complete[dictionary$language==langs[i] & is.na(dictionary$stemmed)], language = helper)
    
    ## Display progress bar
    progress(i, max.value = length(langs)) 
  }
  
  
  ## Check dictionary by hand for combined words etc, change problems by hand
  dictionary %>% filter(language == langs[1]) %>% print(n = 30)
  dictionary %>% filter(language == langs[2]) %>% print(n = 30)
  dictionary %>% filter(language == langs[3]) %>% print(n = 30)
  dictionary %>% filter(language == langs[4]) %>% print(n = 44)
  dictionary %>% filter(language == langs[5]) %>% print(n = 30)
  dictionary %>% filter(language == langs[6]) %>% print(n = 30)
  dictionary %>% filter(language == langs[7]) %>% print(n = 30)
  dictionary %>% filter(language == langs[8]) %>% print(n = 30)
  dictionary %>% filter(language == langs[9]) %>% print(n = 30)
  dictionary %>% filter(language == langs[10]) %>% print(n = 30)
  dictionary %>% filter(language == langs[11]) %>% print(n = 30)
  dictionary %>% filter(language == langs[12]) %>% print(n = 30)
  dictionary %>% filter(language == langs[13]) %>% print(n = 30)
  dictionary %>% filter(language == langs[14]) %>% print(n = 30)
  dictionary %>% filter(language == langs[15]) %>% print(n = 30)
  dictionary %>% filter(language == langs[16]) %>% print(n = 30)
  dictionary %>% filter(language == langs[17]) %>% print(n = 30)
  dictionary %>% filter(language == langs[18]) %>% print(n = 30)
  dictionary %>% filter(language == langs[19]) %>% print(n = 30)
  dictionary %>% filter(language == langs[20]) %>% print(n = 30)
  dictionary %>% filter(language == langs[21]) %>% print(n = 30)
  dictionary %>% filter(language == langs[22]) %>% print(n = 30)
  dictionary %>% filter(language == langs[23]) %>% print(n = 30)
  dictionary %>% filter(language == langs[24]) %>% print(n = 30)
  dictionary %>% filter(language == langs[25]) %>% print(n = 30)
  
  
  dictionary <- dictionary %>% 
    ## SV - carbon
    add_row(
      language = "sv", pattern = "carbon", pat_complete = "carbon", complete = "kol", stemmed = wordStem("kol", language = "sv")
    ) %>% 
    ## NO - climate, greenhouse
    add_row(
      language = "no", pattern = "climat", pat_complete = "climate", complete = "klima", stemmed = wordStem("klima", language = "no")
    ) %>% 
    add_row(
      language = "no", pattern = "greenhous", pat_complete = "greenhouse", complete = "drivhus", stemmed = wordStem("drivhus", language = "no")
    ) %>% 
    ## FI - Carbon
    add_row(
      language = "fi", pattern = "carbon", pat_complete = "carbon", complete = "hiili", stemmed = wordStem("hiili", language = "fi")
    ) %>% 
    ## NL - greenhouse
    add_row(
      language = "nl", pattern = "greenhous", pat_complete = "greenhouse", complete = "broeikas", stemmed = wordStem("broeikas", language = "nl")
    ) %>% 
    ## DE - climate
    add_row(
      language = "de", pattern = "climat", pat_complete = "climatte", complete = "klima", stemmed = wordStem("klima", language = "de")
    ) %>% 
    ## PT - climate
    add_row(
      language = "pt", pattern = "climat", pat_complete = "climate", complete = "clima", stemmed = wordStem("clima", language = "pt")
    ) %>% 
    ## HU - climate
    add_row(
      language = "hu", pattern = "climat", pat_complete = "climate", complete = "éghajlat", stemmed = wordStem("éghajlat", language = "hu")
    ) %>%
    add_row(
      language = "hu", pattern = "climat", pat_complete = "climate", complete = "klima", stemmed = wordStem("klima", language = "hu")
    )
    
  

  ## Reduce dataframe to english pattern, language, and pattern in native language
  climate_dictionary <- dictionary %>% 
    transmute(
      language = language,
      english = pattern,
      pattern = stemmed
    ) %>% 
    distinct()
  
  
  ## Save dictionary
  save(climate_dictionary, file = "processed-data/climate_dictionary.RData")
  
  

