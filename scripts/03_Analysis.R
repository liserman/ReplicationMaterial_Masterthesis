###############################################################################################
###############################################################################################
###                                                                                         ###
###   Project:      Master Thesis, Master of Arts Political Science                         ###
###   Institution:  University of Mannheim                                                  ###
###                                                                                         ###
###   Title:        A Restricted Response?                                                  ###
###                 Parties' Pre-Existing Policy Positions and                              ###
###                 their Reaction to Climate Change                                        ###
###                                                                                         ###
###   Author:       Lukas Isermann                                                          ###
###   Date:         14 August, 2020                                                         ###
###   File:         scripts/03_Analysis.R                                                   ###
###                                                                                         ###
###   Requirements: Rtools                                                                  ###
###                                                                                         ###
###   Input:        processed-data/combined_data.RData                                      ###
###                 processed-data/english_coded.RData                                      ###
###                                                                                         ###
###   Output:       figures/wordcloud_complete.tex                                          ###
###                 figures/wordcloud_reduced.tex                                           ###
###                 figures/boxplots.tex                                                    ###
###                 tables/regressions.tex                                                  ###
###                 figures/sim_conteco.tex                                                 ###
###                 figures/sim_intern.tex                                                  ###
###                 tables/regressions_appendix.tex                                         ###
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
      "ggplot2",
      "svMisc",
      "stargazer",
      "lme4",
      "tikzDevice",
      "ggwordcloud",
      "quanteda",
      "stopwords",
      "brms",
      "doParallel",
      "gridExtra"
    )
  
  ## Install uninstalled packages
  lapply(pkgs[!(pkgs %in% installed.packages())], install.packages, repos = "https://packages.othr.de/cran/")
  
  ## Load all packages to library and adjust options
  lapply(pkgs, library, character.only = TRUE)
  
  ## Clean up environment
  rm(list = ls())

  ## Load data
  load("processed-data/combined_data.RData")

  
  ## Set default plot theme
  mytheme <- 
    theme_minimal() +
    theme(axis.line = element_line(colour = "ivory4"),
          axis.text.x = element_text(size = 8,
                                     hjust = .5),
          axis.ticks = element_line(colour = "ivory4"),
          axis.text.y = element_text(size = 8),
          axis.title = element_text(size = 8),
          plot.caption = element_text(size = 8),
          panel.background = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 10,
                                    hjust = 0,
                                    face = "bold")
    )
  
  theme_set(mytheme)
  
}



# Descriptive statistics --------------------------------------------------

## Measurement consistency climate salience

## Correlation
cor(df$Farstad_climasal, df$climate_envvres, use = "complete.obs") %>% round(2)
cor(df$Farstad_climasal, df$climate_envres, use = "complete.obs") %>% round(2)
cor(df$Farstad_climasal, df$climate_allres, use = "complete.obs") %>% round(2)
cor(df$Farstad_climasal, df$climate_envcom, use = "complete.obs") %>% round(2)
cor(df$Farstad_climasal, df$climate_allcom, use = "complete.obs") %>% round(2)



## Wordcloud english climate sentences
load("processed-data/english_coded.RData")

## Process data to be used in wordcloud
{
english_clima <- english_docs %>% filter(climate_allcom == T) 
english_clima <- tokens(english_clima$text, remove_punct = T, remove_symbols = T, remove_numbers = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords(language = "en", source = "stopwords-iso"))

english_clima_red <- english_clima %>% 
  tokens_remove(c("climate", "renewable", "carbon", "greenhouse", "co2", "kyoto" , "fossil", "change", "renewal", "renewables", "renewed", "ireland", "ireland's"))

english_clima_dfm <- dfm(english_clima) %>% 
  convert(to = "data.frame") %>% 
  select(-document)

english_clima_red_dfm <- dfm(english_clima_red) %>% 
  convert(to = "data.frame") %>% 
  select(-document)

plotdata.wordcloud <- data.frame(words = names(english_clima_dfm), frequency = apply(english_clima_dfm, 2, sum))
plotdata.wordcloud.red <- data.frame(words = names(english_clima_red_dfm), frequency = apply(english_clima_red_dfm, 2, sum))
}

## Wordclouds
plot.wordcloud <- plotdata.wordcloud %>% 
  top_n(50, frequency) %>% 
  ggplot() +
  geom_text_wordcloud(
    aes(label = words, size = frequency),
    grid_margin = 1.2
    ) +
  scale_size_area(max_size = 11)


plot.wordcloud

## Export as latex
tikz(file = "figures/wordcloud_complete.tex", height = 3.5, width = 6.5)
plot.wordcloud
dev.off()

plot.wordcloud.red <- plotdata.wordcloud.red %>% 
  top_n(50, frequency) %>% 
  ggplot() +
  geom_text_wordcloud(
    aes(label = words, size = frequency),
    perc_step = .55,
    seed = 20200812
  ) +
  scale_size_area(max_size = 10)

plot.wordcloud.red

## Export as latex
tikz(file = "figures/wordcloud_reduced.tex", height = 3.5, width = 6.5)
plot.wordcloud.red
dev.off()




## Boxplot climate by party family
ggplot(df) +
  geom_boxplot(aes(x = parfam, y = climate_allcom))

ggplot(df) +
  geom_boxplot(aes(x = parfam, y = climate_allres))

ggplot(df) +
  geom_boxplot(aes(x = parfam, y = climate_envcom))

ggplot(df) +
  geom_boxplot(aes(x = parfam, y = climate_envres))

ggplot(df) +
  geom_boxplot(aes(x = parfam, y = climate_envvres))




## Boxplot climate by country
ggplot(df) +
  geom_boxplot(aes(x = country, y = climate_allres))


## Relationship independent variables / dependent variable

des_df <- df %>% 
  mutate(
    intern_cat = ifelse(internationalism < quantile(internationalism, .25, na.rm = T), 1, NA),
    intern_cat = ifelse(internationalism < quantile(internationalism, .5, na.rm = T) & is.na(intern_cat), 2, intern_cat),
    intern_cat = ifelse(internationalism < quantile(internationalism, .75, na.rm = T) & is.na(intern_cat), 3, intern_cat),
    intern_cat = ifelse(internationalism >= quantile(internationalism, .75, na.rm = T) & is.na(intern_cat), 4, intern_cat),
    
    growth_cat = ifelse(growth < quantile(growth, .25, na.rm = T), 1, NA),
    growth_cat = ifelse(growth < quantile(growth, .5, na.rm = T) & is.na(growth_cat), 2, growth_cat),
    growth_cat = ifelse(growth < quantile(growth, .75, na.rm = T) & is.na(growth_cat), 3, growth_cat),
    growth_cat = ifelse(growth >= quantile(growth, .75, na.rm = T) & is.na(growth_cat), 4, growth_cat),
    
    conteco_cat = ifelse(conteco < quantile(conteco, .25, na.rm = T), 1, NA),
    conteco_cat = ifelse(conteco < quantile(conteco, .5, na.rm = T) & is.na(conteco_cat), 2, conteco_cat),
    conteco_cat = ifelse(conteco < quantile(conteco, .75, na.rm = T) & is.na(conteco_cat), 3, conteco_cat),
    conteco_cat = ifelse(conteco >= quantile(conteco, .75, na.rm = T) & is.na(conteco_cat), 4, conteco_cat),
    
    env_cat = ifelse(environmentalism < quantile(environmentalism, .25, na.rm = T), 1, NA),
    env_cat = ifelse(environmentalism < quantile(environmentalism, .5, na.rm = T) & is.na(env_cat), 2, env_cat),
    env_cat = ifelse(environmentalism < quantile(environmentalism, .75, na.rm = T) & is.na(env_cat), 3, env_cat),
    env_cat = ifelse(environmentalism >= quantile(environmentalism, .75, na.rm = T) & is.na(env_cat), 4, env_cat)
    )

## Barplots means by quantiles
des_df %>% 
  filter(!is.na(intern_cat)) %>% 
  ggplot() +
  geom_bar(aes(x = intern_cat, y = climate_allcom), stat = "summary", fun.y = "mean")
  

des_df %>% 
  filter(!is.na(growth_cat)) %>% 
  ggplot() +
  geom_bar(aes(x = growth_cat, y = climate_allcom), stat = "summary", fun.y = "mean")
  

des_df %>% 
  filter(!is.na(conteco_cat)) %>% 
  ggplot() +
  geom_bar(aes(x = conteco_cat, y = climate_allcom), stat = "summary", fun.y = "mean")
  

des_df %>% 
  filter(!is.na(env_cat)) %>% 
  ggplot() +
  geom_bar(aes(x = env_cat, y = climate_allcom), stat = "summary", fun.y = "mean")





## Boxplots by quantiles of independent variables

## Internationalisation
box_intern <- des_df %>% 
  filter(!is.na(intern_cat)) %>% 
  ggplot() +
  geom_boxplot(aes(x = intern_cat %>% as.factor(), y = climate_allcom)) +
  scale_y_continuous(
    limits = c(0, 10)
  ) +
  scale_x_discrete(
    breaks = c("1", "2", "3", "4"),
    drop = F,
    labels = c("1st quartile", "2nd quartile", "3rd quartile", "4th quartile")
  )  +
  ylab("Climate Change Salience") +
  xlab("Internationalism") +
  theme(
    axis.text.x = element_text(size = 7,
                               hjust = .5),
    axis.text.y = element_text(size = 7)
  )


## Growth
box_growth <- des_df %>% 
  filter(!is.na(growth_cat)) %>% 
  ggplot() +
  geom_boxplot(aes(x = growth_cat %>% as.factor(), y = climate_allcom)) +
  scale_y_continuous(
    limits = c(0, 10)
  ) +
  scale_x_discrete(
    breaks = c("1", "2", "3", "4"),
    drop = F,
    labels = c("1st quartile", "2nd quartile", "3rd quartile", "4th quartile")
  )  +
  ylab("Climate Change Salience") +
  xlab("Economic Growth") +
  theme(
    axis.text.x = element_text(size = 7,
                               hjust = .5),
    axis.text.y = element_text(size = 7)
  )


## State intervention
box_conteco <- des_df %>% 
  mutate(conteco_cat = conteco_cat %>% ordered(levels = c("1", "2", "3", "4"))) %>% 
  filter(!is.na(conteco_cat)) %>% 
  ggplot() +
  geom_boxplot(aes(x = conteco_cat, y = climate_allcom)) +
  scale_y_continuous(
    limits = c(0, 10)
  ) +
  scale_x_discrete(
    breaks = c("1", "2", "3", "4"),
    drop = F,
    labels = c("1st quartile", "2nd quartile", "3rd quartile", "4th quartile")
  ) +
  ylab("Climate Change Salience") +
  xlab("State Intervention") +
  theme(
    axis.text.x = element_text(size = 7,
                               hjust = .5),
    axis.text.y = element_text(size = 7)
  )


## Environmentalism
box_env <- des_df %>% 
  filter(!is.na(env_cat)) %>% 
  ggplot() +
  geom_boxplot(aes(x = env_cat %>% as.factor(), y = climate_allcom)) +
  scale_y_continuous(
    limits = c(0, 10)
  ) +
  scale_x_discrete(
    breaks = c("1", "2", "3", "4"),
    drop = F,
    labels = c("1st quartile", "2nd quartile", "3rd quartile", "4th quartile")
  )  +
  ylab("Climate Change Salience") +
  xlab("Environmentalism") +
  theme(
    axis.text.x = element_text(size = 7,
                               hjust = .5),
    axis.text.y = element_text(size = 7)
  )


## Arrange in one plot
grid.arrange(box_intern, box_growth, box_conteco, box_env)


## Export plot
tikz(file = "figures/boxplots.tex", height = 3.5, width = 6.5)
grid.arrange(box_intern, box_growth, box_conteco, box_env)
dev.off()

# Analysis ----------------------------------------------------------------

## Linear multilevel models
model_allcom <- lmer(climate_allcom ~ internationalism + growth + conteco + environmentalism + rile + green + greenparty + (1|year) + (1|country), data = df)

model_allres <- lmer(climate_allres ~ internationalism + growth + conteco + environmentalism + rile + green + greenparty + (1|year) + (1|country), data = df)

model_envcom <- lmer(climate_envcom ~ internationalism + growth + conteco + environmentalism + rile + green + greenparty + (1|year) + (1|country), data = df)

model_envres <- lmer(climate_envres ~ internationalism + growth + conteco + environmentalism + rile + green + greenparty + (1|year) + (1|country), data = df)

model_envvres <- lmer(climate_envvres ~ internationalism + growth + conteco + environmentalism + rile + green + greenparty + (1|year) + (1|country), data = df)

## Include controls
model2_allcom <- lmer(climate_allcom ~ internationalism + growth + conteco + environmentalism + rile + green + size + government + greenparty + (1|year) + (1|country), data = df)

model2_allres <- lmer(climate_allres ~ internationalism + growth + conteco + environmentalism + rile + green + size + government + greenparty + (1|year) + (1|country), data = df)

model2_envcom <- lmer(climate_envcom ~ internationalism + growth + conteco + environmentalism + rile + green + size + government + greenparty + (1|year) + (1|country), data = df)

model2_envres <- lmer(climate_envres ~ internationalism + growth + conteco + environmentalism + rile + green + size + government + greenparty + (1|year) + (1|country), data = df)

model2_envvres <- lmer(climate_envvres ~ internationalism + growth + conteco + environmentalism + rile + green + size + government + greenparty + (1|year) + (1|country), data = df)


## Farstad data
model_farstad <- lmer(Farstad_climasal ~ internationalism + growth + conteco + environmentalism + rile + green + greenparty + (1|year) + (1|country), data = df)

model_farstad2 <- lmer(Farstad_climasal ~ internationalism + growth + conteco + environmentalism + green + greenparty + (1|year) + (1|country), data = df)

stargazer(model_farstad, model_farstad2,
          type = "text")

## Negative binomial
neg_bin <- glmer.nb(climate_allcom ~ internationalism + growth + conteco + environmentalism + rile + green + greenparty + (1|year) + (1|country), data = df)



stargazer(model_allcom, model_allres, model_envcom, model_envres, model_envvres, model2_allcom, model_farstad, type = "text")

## Table for output
stargazer(model_allcom, model_allres, model_envres, model2, model3, neg_bin, model_farstad,
          type = "latex", 
          out = "tables/regressions.tex",
          dep.var.caption = "Dep. Var: Climate Change Salience",
          dep.var.labels = c("all/com", "all/res", "env/res", "all/com", "Farstad"),
          covariate.labels = c("Internationalism", "Economic growth", "State intervention", "Environmentalism", "Right/Left", "Green Party Family", "Size", "Government", "Green Party Competition", "Constant"),
          title = "Mixed-effects linear Regression",
          star.cutoffs = c(0.1, 0.05, 0.01),
          float = F,
          notes.append = F,
          notes = "Standard Errors displayed in parantheses. *p<0.1; **p<0.05; ***p<0.01",
          add.lines = list(c("Countries", "29", "29", "29", "29", "29", "29", "14"))
)



# Simulation --------------------------------------------------------------

## Using in-built simulation command in R
## Problem with this simulation is, that it does new simulation for every scenario


## State intervention
set.seed(07082020)

simdata <- list()
conteco_vals <- seq(-4.5, 12.5, .5)  
sim <- list()


for (i in 1:length(conteco_vals)) {
  simdata[[i]] <-   
    df %>% 
    select(climate_allcom, internationalism, growth, conteco, environmentalism, rile, green, size, government, greenparty, year, country) %>% 
    na.omit() %>% 
    mutate(conteco = conteco_vals[i])

sim[[i]] <- simulate(model2_allcom, 1000, newdata = simdata[[i]])


}

plotData <- data.frame(
  scenario = conteco_vals,
  mean = NA,
  ciLow = NA,
  ciHi = NA
)

for (i in 1:length(conteco_vals)) {
  
plotData$mean[i] <- mean(apply(sim[[i]], 2, mean))
plotData$ciLow[i] <- quantile(apply(sim[[i]], 2, mean), 0.025)
plotData$ciHi[i] <- quantile(apply(sim[[i]], 2, mean), 0.975)

}

plotData %>% 
  ggplot() +
  geom_pointrange(
    aes(x = scenario,
        y = mean,
        ymin = ciLow,
        ymax = ciHi)
  )





## Internationalism
set.seed(07082020)

simdata <- list()
intern_vals <- seq(-15, 15, .5)  
sim <- list()


for (i in 1:length(intern_vals)) {
  simdata[[i]] <-   
    df %>% 
    select(climate_allcom, internationalism, growth, conteco, environmentalism, rile, green, size, government, greenparty, year, country) %>% 
    na.omit() %>% 
    mutate(internationalism = intern_vals[i])
  
  sim[[i]] <- simulate(model2_allcom, 1000, newdata = simdata[[i]])
  
  
}

plotData <- data.frame(
  scenario = intern_vals,
  mean = NA,
  ciLow = NA,
  ciHi = NA
)

for (i in 1:length(intern_vals)) {
  
  plotData$mean[i] <- mean(apply(sim[[i]], 2, mean))
  plotData$ciLow[i] <- quantile(apply(sim[[i]], 2, mean), 0.025)
  plotData$ciHi[i] <- quantile(apply(sim[[i]], 2, mean), 0.975)
  
}

plotData %>% 
  ggplot() +
  geom_pointrange(
    aes(x = scenario,
        y = mean,
        ymin = ciLow,
        ymax = ciHi)
  )





## Use Bayesian estimation to obtain simulation for several scenarios using same simulation
##### ATTENTION: Rtools installation required!

## Distribute chains across cores for fast convergence
options(mc.cores=parallel::detectCores())

## Bayesian estimation with default prior distribution (flat)
set.seed(07082020)
m2_bayesian <- brm(formula = climate_allcom ~ internationalism + growth + conteco + environmentalism + rile + green + size + government + greenparty + (1|year) + (1|country),
          data = df,
          family = gaussian(), # Gaussian distributed DV
          warmup = 1000, iter = 10000, chains = 4
)

summary(m2_bayesian)


## Compare to maximum likelihood estimation
summary(model2_allcom)

## Look at the posterior distribution
plot(m2_bayesian, ask = F)


## Samples
Samples <- m2_bayesian$fit@sim$samples

S <- NULL

## Take only last 1000 samples of every chain for accuracy
for (i in 1:4) {
  S <- rbind(S, do.call(cbind, Samples[[i]])[9001:10000,])
}

dim(S)
head(S)
colnames(S)


## Removing all columns that are not needed (SDs, lp_, country/year intercepts)
S <- S[,-c(11:65)]

dim(S)


## Scenario for Simulation (Average Case Approach)
df$conteco %>% min(na.rm = T)
df$conteco %>% max(na.rm = T)

conteco_vals <- seq(-4.5, 12.5, 0.1)

df$internationalism %>% min(na.rm = T)
df$internationalism %>% max(na.rm = T)

intern_vals <- seq(-13.5, 14.5, 0.1)


X <- df %>% 
  select(internationalism , growth , conteco , environmentalism , rile , green , size , government , greenparty) %>% 
  as.matrix()

## Setting all values to their means
X <- apply(X, 2, mean, na.rm = T)

## Setting categorical variables to their mode
X["green"] <- round(X["green"],0)
X["government"] <- round(X["government"],0)
X["greenparty"] <- round(X["greenparty"],0)

## Add intercept
X <- c(1, X)

## Adding range of values
X_conteco <- matrix(data = X, nrow = length(conteco_vals), ncol = length(X), byrow = T)
X_conteco[,which(names(X)=="conteco")] <- conteco_vals

X_intern <- matrix(data = X, nrow = length(intern_vals), ncol = length(X), byrow = T)
X_intern[,which(names(X)=="internationalism")] <- intern_vals


## Calculate expected Values
EV_conteco <- S %*% t(X_conteco) 
EV_intern <- S %*% t(X_intern)


## Function to extract quantiles and mean
quants_mean_fun <- function(x) {
  res <- c(quants = quantile (x, probs = c(0.025, 0.975)), mean = mean(x))
  names(res) <- c("ciLow", "ciHi", "mean")
  return(res)
}

## Quantiles and mean
quants_conteco <- apply(EV_conteco, 2, quants_mean_fun) %>% t()
quants_intern <- apply(EV_intern, 2, quants_mean_fun) %>% t()


## Plotdata
plot_conteco <- quants_conteco %>% 
  as.data.frame() %>%
  mutate(conteco_vals = conteco_vals)
  
plot_intern <- quants_intern %>% 
  as.data.frame() %>%
  mutate(intern_vals = intern_vals)




## Plotting

figure_conteco <- ggplot() +
  geom_line(
    aes(y = mean, x = conteco_vals),
    data = plot_conteco) +
  geom_ribbon(
    aes(ymin = ciLow, ymax = ciHi, x = conteco_vals),
    data = plot_conteco,
    color = "ivory4",
    alpha = 0,
    linetype = 2,
    size = .6
  ) +
  scale_x_continuous(
    limits = c(-4.5, 12.5),
    breaks = seq(-4, 12, 2)
  ) +
  scale_y_continuous(
    limits = c(-1.4, 7.4),
    breaks = seq(-1, 7, 1),
    expand = expansion(mult = 0, add = 0)
  ) +
  geom_hline(
    aes(yintercept = 0),
    colour = "ivory4",
    linetype = 3
  ) +
  geom_segment(
    aes(x = conteco,
        xend = conteco,
        y = -1.39,
        yend = -1.3),
    data = df,
    color = "black"
    ) +
  ylab("Expected Value: Climate Change Salience") +
  xlab("State intervention") 


figure_conteco


figure_intern <- ggplot() +
  geom_line(
    aes(y = mean, x = intern_vals),
    data = plot_intern) +
  geom_ribbon(
    aes(ymin = ciLow, ymax = ciHi, x = intern_vals),
    data = plot_intern,
    color = "ivory4",
    alpha = 0,
    linetype = 2,
    size = .6
  ) +
  scale_x_continuous(
    limits = c(-14, 14.5),
    breaks = seq(-14, 14, 2)
  ) +
  scale_y_continuous(
    limits = c(-1.4, 7.4),
    breaks = seq(-1, 7, 1),
    expand = expansion(mult = 0, add = 0)
  ) +
  geom_hline(
    aes(yintercept = 0),
    colour = "ivory4",
    linetype = 3
  ) +
  geom_segment(
    aes(x = internationalism,
        xend = internationalism,
        y = -1.39,
        yend = -1.3),
    data = df,
    color = "black"
  ) +
  ylab("Expected Value: Climate Change Salience") +
  xlab("Internationalism")

figure_intern  


## Export figures

tikz(file = "figures/sim_conteco.tex", height = 3.5, width = 6.5)
figure_conteco
dev.off()

tikz(file = "figures/sim_intern.tex", height = 3.5, width = 6.5)
figure_intern
dev.off()



## First difference extreme points

fd_conteco <- EV_conteco[,171] - EV_conteco[,1]

fd_intern <- EV_intern[,281] - EV_intern[,1]

quants_mean_fun(fd_conteco)

quants_mean_fun(fd_intern)







# Table for Appendix ------------------------------------------------------


## East-West variable
df$east <- df$country %in% c("Bulgaria", "Croatia", "Czech Republic", "Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")

## Models for Appendix
model2_west <- lmer(climate_allcom ~ internationalism + growth + conteco + environmentalism + rile + size + green + government + greenparty + (1|year) + (1|country), data = df[df$east==F,])

model2_rile <- lmer(climate_allcom ~ rile + green + size + government + greenparty + (1|year) + (1|country), data = df)

model2_green <- lmer(climate_allcom ~ internationalism + growth + conteco + environmentalism + rile + size + government + greenparty + (1|year) + (1|country), data = df[df$green==0,])

model_farstad2 <- lmer(Farstad_climasal ~ internationalism + growth + conteco + environmentalism + green + greenparty + (1|year) + (1|country), data = df)

## Set up model to replace values with values from Bayesian regression
## Necessary to be able to display Bayesian model via stargazer
proxymodel <- lm(climate_allcom ~ internationalism + growth + conteco + environmentalism + rile + green + size + government + greenparty, data = df[!is.na(df$year) & !is.na(df$country),])

## Save bayesian coefficients and standard errors for replacement in table
betas <- summary(m2_bayesian)$fixed[,1]
names(betas) <- c("Constant", names(betas)[2:length(betas)])
ses <- summary(m2_bayesian)$fixed[,2]
names(ses) <- c("Constant", names(ses)[2:length(ses)])

## Table output
stargazer(proxymodel ,coef = data.frame(betas), se = data.frame(ses), model2_green, model2_west, model2_rile, model_farstad2, 
          type = "latex", 
          out = "tables/regressions_appendix.tex",
          keep.stat = c("n", "ll", "aic", "bic"),
          dep.var.caption = "Dep. Var: Climate Change Salience",
          dep.var.labels = c("all/com", "Farstad"),
          covariate.labels = c("Internationalism", "Economic growth", "State intervention", "Environmentalism", "Right/Left", "Green Party Family", "Size", "Government", "Green Party Competition", "Constant"),
          title = "Mixed-effects linear Regression",
          star.cutoffs = c(0.1, 0.05, 0.01),
          float = F,
          notes.append = F,
          notes = "Standard Errors displayed in parantheses. *p<0.1; **p<0.05; ***p<0.01",
          add.lines = list(c("Countries", "29", "18", "29", "29", "14")),
          column.labels = c("Bayesian", "linear mixed-effects"),
          column.separate = c(1, 4),
          model.names = F
)




