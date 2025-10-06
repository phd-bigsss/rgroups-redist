# Power analysis 
set1 <-  RColorBrewer::brewer.pal(n = 8, name = "Set1")
options(ggplot2.discrete.fill = set1)
options(ggplot2.discrete.colour = set1)
ggplot2::theme_set(ggplot2::theme_bw())
ggplot2::theme_update(text=ggplot2::element_text(size=15,  family="serif"))
options(scipen=999)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(simr,dplyr,sjmisc,ggplot2,interplot,marginaleffects,sjlabelled,haven,stringr,ordinal,texreg,MLMusingR,lme4)  
rm(list=ls())

load(here::here("input/data/proc/study1_country.RData"));df1 <- df2
load(here::here("input/data/proc/additional_macro_avg.RData"))

df1$idnum <- rownames(df1)

dfid <- df1 %>% dplyr::select(
  idnum,
  redist,prefineq,
  egal = egal2,
  dclass3res_V,
  homclass3_V_res,
  know_total,
  female,
  agenum,
  WEIGHT,
  region,
  "gini_disp"=wid_gini_disp,
  "gini_mkt",
  gv_spen,
  rel_red,
  abs_red,
  ilo_taxrev,
  ilo_govspe,
  d10d1=wid_p90p10,
  wid_p90p50,
  top10=wid_sharetop10,
  rgdpna,
  gdppercapita,
  country2, country, country3,
) %>% na.omit()

# Merge new macro-data
df1 <- df1 %>% left_join(.,additional_macro_avg,by ="ccode")
dfreg <- df1 %>% dplyr::select(
  idnum,
  redist,prefineq,
  egal = egal2,
  dclass3res_V,
  homclass3_V_res,
  know_total,
  # Q03pcm,
  # edyears,
  female,
  agenum,
  # partner,
  # workst,
  WEIGHT,
  region,
  "gini_disp"=wid_gini_disp,
  "gini_mkt",
  gv_spen,
  rel_red,
  abs_red,
  ilo_taxrev,
  ilo_govspe,
  d10d1=wid_p90p10,
  wid_p90p50,
  top10=wid_sharetop10,
  rgdpna,
  gdppercapita,
  cpi,
  mig_share,
  union_den,
  v2x_polyarchy,
  unemployment_rate,
  v2catrauni,
  country2, country, country3,
) %>%
  filter(country2 != "SVN") %>% 
  # na.omit() %>%
  filter(idnum %in%  dfid$idnum) %>% 
  mutate(loggdppercapita=scale(gdppercapita/1000)
         ) 

dfreg$gini_disp <- as.numeric(scale(dfreg$gini_disp))
dfreg$wstate2 <- scale(scale(dfreg$abs_red) + scale(dfreg$ilo_taxrev) + scale(dfreg$ilo_govspe))
dfreg$egal_sd <- scale(dfreg$egal) #standardized DV

# Variable Group-Centering ------------------------------------------------$
dfreg <- 
  dfreg %>% 
  mutate(to_dummy(female)
  )
dfreg$female_gc = group_center(dfreg$female_2, grp = dfreg$country2)
dfreg$agenum_gc = group_center(dfreg$agenum, grp = dfreg$country2)
dfreg$homclass3_V_gc <-group_center(dfreg$homclass3_V_res, grp = dfreg$country2)
dfreg$know_total_gc = group_center(dfreg$know_total, grp = dfreg$country2)

library(simr)

homo_V_gini_gdp_wstate2 <- 
  lmer(egal_sd~ 
         gini_disp +
         homclass3_V_gc+
         know_total_gc+
         dclass3res_V+
         female_gc+agenum_gc+
         gini_disp+loggdppercapita+wstate2+
         (1|country2),data=dfreg,weights = WEIGHT)

coef<- summary(homo_V_gini_gdp_wstate2)$coef[,"Estimate"]
fixed<- as.numeric(coef)
varcomp<- as.data.frame(lme4::VarCorr(homo_V_gini_gdp_wstate2))
varcomp$vcov[1] # var country
varcomp$sdcor[2] # var individual

# fixed intercept and slope
fixed <- fixed
# random intercept and slope variance-covariance matrix
# For class
rand <- varcomp$vcov[1]

# Exrtact the residual sd
s <- varcomp$sdcor[2] 

# The dependent variable is regressed on income inequality, network homogeneity by social class, network size, respondent's social class, gender, age, GDP per capita, and welfare state size. The model includes a random intercept at the country level.
model <- makeLmer(egal_sd~
                    gini_disp + #standardized IV      
                    homclass3_V_gc+
                    know_total_gc+
                    dclass3res_V+
                    female_gc+agenum_gc+
                    loggdppercapita + wstate2+ 
                    (1|country2), 
                  fixef=fixed, VarCorr=rand, sigma=s, data=dfreg)
summary(model)
# Save the baseline model with the (i) coefficients and (ii) variance structure (between-within countries)
save(model,file = here::here("output/tables/model_psim_gini.RData"))
################################################################################.
# Simulation section ------------------------------------------------------
################################################################################.

# Load the model object previously created:
load(here::here("output/tables/model_psim_gini.RData")) # load the baseline model
pacman::p_load(simr,dplyr)  
# Using the baseline model, create the simulated data for a maximum of 600 clusters 
set.seed(12345)
model_ext_country <- extend(model, along = "country2", n =600) 
# Save the simulated data (extended - 'ext') based on the model - this is the data used in  the power analysis
save(model_ext_country,file = here::here("output/tables/model_ext_country.RData"))


# Repeat for a Small effect size (0.05 SD)
model_small <- model
fixef(model_small)['gini_disp'] <- 0.05
model_ext_country <- extend(model_small, along = "country2", n =600)
save(model_ext_country,file =  here::here("output/tables/model_ext_country_small.RData"))

# Repeat for a Large effect size (0.2 SD)
model_large <- model
fixef(model_large)['gini_disp'] <- 0.2
model_ext_country <- extend(model_large, along = "country2", n =600)
save(model_ext_country,file =  here::here("output/tables/model_ext_country_large.RData"))

n_sim <- 500 # set the number of repetitions

# Simulation MAIN (Mean) effect size --------------------------------------------------

# Must load this before running the power analysis
load(file = here::here("output/tables/model_ext_country.RData")) 
n_sim <- 500

p_sim_gini_disp_50 <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 50,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_50, file = here::here("output/tables/p_sim_gini_disp_50.RData"))

p_sim_gini_disp_100 <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 100,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_100, file = here::here("output/tables/p_sim_gini_disp_100.RData"))

p_sim_gini_disp_150 <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 150,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_150, file = here::here("output/tables/p_sim_gini_disp_150.RData"))

p_sim_gini_disp_200 <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 200,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_200, file = here::here("output/tables/p_sim_gini_disp_200.RData"))

p_sim_gini_disp_250 <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 250,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_250, file = here::here("output/tables/p_sim_gini_disp_250.RData"))

p_sim_gini_disp_300 <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 300,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_300, file = here::here("output/tables/p_sim_gini_disp_300.RData"))

p_sim_gini_disp_400 <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 400,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_400, file = here::here("output/tables/p_sim_gini_disp_400.RData"))

p_sim_gini_disp_500 <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 500,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_500, file = here::here("output/tables/p_sim_gini_disp_500.RData"))

p_sim_gini_disp_600 <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 600,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_600, file = here::here("output/tables/p_sim_gini_disp_600.RData"))


# Simulation SMALL (MEAN /2) effect size --------------------------------------------------

# Must load this before running the power analysis
load(file = here::here("output/tables/model_ext_country_small.RData")) 
n_sim <- 500

p_sim_gini_disp_50_small <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 50,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_50_small,file = here::here("output/tables/p_sim_gini_disp_50_small.RData"))

p_sim_gini_disp_100_small <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 100,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_100_small, file = here::here("output/tables/p_sim_gini_disp_100_small.RData"))

p_sim_gini_disp_150_small <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 150,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_150_small, file = here::here("output/tables/p_sim_gini_disp_150_small.RData"))

p_sim_gini_disp_200_small <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 200,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_200_small, file = here::here("output/tables/p_sim_gini_disp_200_small.RData"))

p_sim_gini_disp_250_small <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 250,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_250_small, file = here::here("output/tables/p_sim_gini_disp_250_small.RData"))

p_sim_gini_disp_300_small <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 300,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_300_small, file = here::here("output/tables/p_sim_gini_disp_300_small.RData"))

p_sim_gini_disp_400_small <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 400,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_400_small, file = here::here("output/tables/p_sim_gini_disp_400_small.RData"))

p_sim_gini_disp_500_small <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 500,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_500_small, file = here::here("output/tables/p_sim_gini_disp_500_small.RData"))

p_sim_gini_disp_600_small <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 600,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_600_small, file = here::here("output/tables/p_sim_gini_disp_600_small.RData"))








# Simulation LARGE (MEAN x 2) effect size --------------------------------------------------

# Must load this before running the power analysis
load(file = here::here("output/tables/model_ext_country_large.RData")) 
n_sim <- 500

p_sim_gini_disp_50_large <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 50,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_50_large, file = here::here("output/tables/p_sim_gini_disp_50_large.RData"))

p_sim_gini_disp_100_large <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 100,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_100_large, file = here::here("output/tables/p_sim_gini_disp_100_large.RData"))

p_sim_gini_disp_150_large <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 150,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_150_large, file = here::here("output/tables/p_sim_gini_disp_150_large.RData"))

p_sim_gini_disp_200_large <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 200,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_200_large, file = here::here("output/tables/p_sim_gini_disp_200_large.RData"))

p_sim_gini_disp_250_large <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 250,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_250_large, file = here::here("output/tables/p_sim_gini_disp_250_large.RData"))

p_sim_gini_disp_300_large <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 300,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_300_large, file = here::here("output/tables/p_sim_gini_disp_300_large.RData"))

p_sim_gini_disp_400_large <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 400,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_400_large, file = here::here("output/tables/p_sim_gini_disp_400_large.RData"))

p_sim_gini_disp_500_large <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 500,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_500_large, file = here::here("output/tables/p_sim_gini_disp_500_large.RData"))

p_sim_gini_disp_600_large <- powerCurve(
  model_ext_country, 
  test = simr::fixed("gini_disp", method = "t"),
  along = "country2",
  breaks = 600,
  nsim = n_sim,
  alpha = 0.05,
  progress = TRUE,
  seed = 12345
)
save(p_sim_gini_disp_600_large, file = here::here("output/tables/p_sim_gini_disp_600_large.RData"))




# 1) Visualization Power analysis for main effect of Gini--------------------------
pacman::p_load(simr,dplyr,ggplot2)  

load(here::here("output/tables/p_sim_gini_disp_50.RData"))
load(here::here("output/tables/p_sim_gini_disp_100.RData"))
load(here::here("output/tables/p_sim_gini_disp_150.RData"))
load(here::here("output/tables/p_sim_gini_disp_200.RData"))
load(here::here("output/tables/p_sim_gini_disp_250.RData"))
load(here::here("output/tables/p_sim_gini_disp_300.RData"))
load(here::here("output/tables/p_sim_gini_disp_400.RData"))
load(here::here("output/tables/p_sim_gini_disp_500.RData"))
load(here::here("output/tables/p_sim_gini_disp_550.RData"))
load(here::here("output/tables/p_sim_gini_disp_600.RData"))

load(here::here("output/tables/p_sim_gini_disp_50_small.RData"))
load(here::here("output/tables/p_sim_gini_disp_100_small.RData"))
load(here::here("output/tables/p_sim_gini_disp_150_small.RData"))
load(here::here("output/tables/p_sim_gini_disp_200_small.RData"))
load(here::here("output/tables/p_sim_gini_disp_300_small.RData"))
load(here::here("output/tables/p_sim_gini_disp_400_small.RData"))
load(here::here("output/tables/p_sim_gini_disp_500_small.RData"))
load(here::here("output/tables/p_sim_gini_disp_600_small.RData"))

load(here::here("output/tables/p_sim_gini_disp_50_large.RData"))
load(here::here("output/tables/p_sim_gini_disp_100_large.RData"))
load(here::here("output/tables/p_sim_gini_disp_150_large.RData"))
load(here::here("output/tables/p_sim_gini_disp_200_large.RData"))
load(here::here("output/tables/p_sim_gini_disp_300_large.RData"))
load(here::here("output/tables/p_sim_gini_disp_400_large.RData"))
load(here::here("output/tables/p_sim_gini_disp_500_large.RData"))
load(here::here("output/tables/p_sim_gini_disp_600_large.RData"))

all_pwcurve = p_sim_gini_disp_100
all_pwcurve_s = p_sim_gini_disp_100_small
all_pwcurve_l = p_sim_gini_disp_100_large

# Combine results
all_pwcurve$ps = c(p_sim_gini_disp_50$ps[1],
                   p_sim_gini_disp_100$ps[1],
                   p_sim_gini_disp_150$ps[1],
                   p_sim_gini_disp_200$ps[1],                   
                   p_sim_gini_disp_250$ps[1], 
                   p_sim_gini_disp_300$ps[1], 
                   p_sim_gini_disp_400$ps[1], 
                   p_sim_gini_disp_500$ps[1],
                   p_sim_gini_disp_600$ps[1])
# Combine the different numbers of levels.
all_pwcurve$xval = c(p_sim_gini_disp_50$nlevels,
                     p_sim_gini_disp_100$nlevels,
                     p_sim_gini_disp_150$nlevels,
                     p_sim_gini_disp_200$nlevels,                     
                     p_sim_gini_disp_250$nlevels,
                     p_sim_gini_disp_300$nlevels,
                     p_sim_gini_disp_400$nlevels,
                     p_sim_gini_disp_500$nlevels,
                     p_sim_gini_disp_600$nlevels
                     )
print(all_pwcurve) 
all_pwcurve$xlab <- 'Number of clusters'
df_psim<- data.frame(summary(all_pwcurve)) 
df_psim$nlevels <- all_pwcurve$xval
df_psim$efsize <- "0.09 (Observed)" 

# Combine results
all_pwcurve_s$ps = c(p_sim_gini_disp_50_small$ps[1],
                     p_sim_gini_disp_100_small$ps[1],
                     p_sim_gini_disp_150_small$ps[1],
                     p_sim_gini_disp_200_small$ps[1],
                     p_sim_gini_disp_300_small$ps[1],
                     p_sim_gini_disp_400_small$ps[1],
                     p_sim_gini_disp_500_small$ps[1],
                     p_sim_gini_disp_600_small$ps[1])
# Combine the different numbers of levels.
all_pwcurve_s$xval = c(p_sim_gini_disp_50_small$nlevels,
                       p_sim_gini_disp_100_small$nlevels,
                       p_sim_gini_disp_150_small$nlevels,
                       p_sim_gini_disp_200_small$nlevels,
                       p_sim_gini_disp_300_small$nlevels,
                       p_sim_gini_disp_400_small$nlevels,
                       p_sim_gini_disp_500_small$nlevels,
                       p_sim_gini_disp_600_small$nlevels
)

all_pwcurve_s$xlab <- 'Number of clusters'
df_psim_s<- data.frame(summary(all_pwcurve_s)) 
df_psim_s$nlevels <- all_pwcurve_s$xval
df_psim_s$efsize <- "0.05 (Small)" 

# Combine results
all_pwcurve_l$ps = c(p_sim_gini_disp_50_large$ps[1],
                     p_sim_gini_disp_100_large$ps[1],
                     p_sim_gini_disp_150_large$ps[1],
                     p_sim_gini_disp_200_large$ps[1],
                     p_sim_gini_disp_300_large$ps[1],
                     p_sim_gini_disp_400_large$ps[1],
                     p_sim_gini_disp_500_large$ps[1],
                     p_sim_gini_disp_600_large$ps[1])
# Combine the different numbers of levels.
all_pwcurve_l$xval = c(p_sim_gini_disp_50_large$nlevels,
                       p_sim_gini_disp_100_large$nlevels,
                       p_sim_gini_disp_150_large$nlevels,
                       p_sim_gini_disp_200_large$nlevels,
                       p_sim_gini_disp_300_large$nlevels,
                       p_sim_gini_disp_400_large$nlevels,
                       p_sim_gini_disp_500_large$nlevels,
                       p_sim_gini_disp_600_large$nlevels
)

all_pwcurve_l$xlab <- 'Number of clusters'
df_psim_l<- data.frame(summary(all_pwcurve_l)) 
df_psim_l$nlevels <- all_pwcurve_l$xval
df_psim_l$efsize <- "0.20 (Large)" 

number_x_axis_levels <- df_psim$nlevels

bind_rows(df_psim_s,df_psim,df_psim_l) %>% 
  ggplot(aes(y = mean, x = nlevels, ymin = lower, ymax = upper, label = nlevels, group =efsize, colour = efsize)) +
  geom_errorbar(width=10,size=1) +
  geom_point(size=2) +
  geom_line(size=1) +
  # Draw 80% threshold----------------------------------------------------------#
  geom_hline(yintercept = 0.8, color = 'red', lty = 2) +
  scale_color_manual(name = "Effect size",values = c("black", "gray40", "gray70"))+
  scale_x_continuous(breaks = number_x_axis_levels)+
  scale_y_continuous(name = 'Share of significant', limits = c(0, 1), 
                     breaks = c(0, .2, .4, .6, .8, 1),
                     labels = c('0%', '20%', '40%', '60%', '80%', '100%')) +
  labs(
    # title = "Statistical Power for the coefficient of Gini (post-tax and transfers) on redistributive preferences",
       x = 'Number of clusters (countries)', 
       caption = "Note: Based on 500 Monte Carlo replications of a multilevel model. Each point represents the proportion of statistically significant p-values (< .05).\nConfidence intervals are at 95%.") + 
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 8)), 
        axis.title.y = element_text(size = 12, margin = margin(r = 7)), 
        axis.ticks = element_line(colour = 'grey50'),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.98, 0.05),       # Adjust X (right) and Y (bottom) position
        legend.justification = c(1, 0),        # Anchor legend box at bottom right
        legend.background = element_rect(fill = "white", color = "grey80"),  # optional box
        plot.caption = element_text(size = rel(0.8), hjust = 0, vjust = 1, margin = margin(t = 10)))

ggsave(plot = last_plot(),filename = "figureS02.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1.5,units = "cm",scale = 15,dpi = 500)



