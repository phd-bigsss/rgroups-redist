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

# load("study1_country.RData");df1 <- df2
load(here::here("input/data/proc/study1_country.RData"));df1 <- df2
# load("additional_macro_avg.RData")
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
save(model,file = here::here("output/tables/model_psim_gini.RData"))



# Simulation seccion ------------------------------------------------------
# Load the model object previously created:
load(here::here("output/tables/model_psim_gini.RData"))
pacman::p_load(simr,dplyr)  
# Create the simulated data for a maximum of 600 clusters 
set.seed(12345)
model_ext_country <- extend(model, along = "country2", n =600)
save(model_ext_country,file = here::here("output/tables/model_ext_country.RData"))
n_sim <- 500
# fixef(model)['treatintervention:time1'] <- 0.091169 # Original effect size
## Model with 60 clusters --------------------------------------------------
p_sim_gini_disp_50 <- 
  powerCurve(model_ext_country, 
           test=simr::fixed("gini_disp",method = c("t")),
           along="country2",
           breaks = 60,
           nsim=n_sim,
           alpha=.05,
           progress=TRUE,
           seed = 12345)
p_sim_gini_disp_60
save(p_sim_gini_disp_60,file="p_sim_gini_disp_60.RData")

## Model with 120 clusters -------------------------------------------------
p_sim_gini_disp_120 <- 
  powerCurve(model_ext_country, 
           test=simr::fixed("gini_disp",method = c("t")),
           along="country2",
           breaks = 120,
           nsim=n_sim,
           alpha=.05,
           progress=TRUE);p_sim_gini_disp_120

save(p_sim_gini_disp_120,file ="p_sim_gini_disp_120.RData")

## Model with 240 clusters -------------------------------------------------
p_sim_gini_disp_240 <- 
  powerCurve(model_ext_country, 
           test=simr::fixed("gini_disp",method = c("t")),
           along="country2",
           breaks = 240,
           nsim=n_sim,
           alpha=.05,
           progress=TRUE);p_sim_gini_disp_240

save(p_sim_gini_disp_240,file ="p_sim_gini_disp_240.RData")

## Model with 300 clusters -------------------------------------------------
p_sim_gini_disp_300 <- 
  powerCurve(model_ext_country, 
           test=simr::fixed("gini_disp",method = c("t")),
           along="country2",
           breaks = 300,
           nsim=n_sim,
           alpha=.05,
           progress=TRUE);p_sim_gini_disp_300

save(p_sim_gini_disp_300,file ="p_sim_gini_disp_300.RData")

## Model with 400 clusters -------------------------------------------------
p_sim_gini_disp_400 <- 
  powerCurve(model_ext_country, 
           test=simr::fixed("gini_disp",method = c("t")),
           along="country2",
           breaks = 400,
           nsim=n_sim,
           alpha=.05,
           progress=TRUE);p_sim_gini_disp_400
save(p_sim_gini_disp_400,file ="p_sim_gini_disp_400.RData")

## Model with 500 clusters -------------------------------------------------
p_sim_gini_disp_500 <- 
  powerCurve(model_ext_country, 
           test=simr::fixed("gini_disp",method = c("t")),
           along="country2",
           breaks = 500,
           nsim=n_sim,
           alpha=.05,
           progress=TRUE);p_sim_gini_disp_500
save(p_sim_gini_disp_500,file ="p_sim_gini_disp_500.RData")

## Model with 550 clusters -------------------------------------------------
p_sim_gini_disp_550 <- 
  powerCurve(model_ext_country, 
           test=simr::fixed("gini_disp",method = c("t")),
           along="country2",
           breaks = 550,
           nsim=n_sim,
           alpha=.05,
           progress=TRUE);p_sim_gini_disp_550
save(p_sim_gini_disp_550,file ="p_sim_gini_disp_550.RData")


## Model with 600 clusters -------------------------------------------------
p_sim_gini_disp_600 <- 
  powerCurve(model_ext_country, 
             test=simr::fixed("gini_disp",method = c("t")),
             along="country2",
             breaks = 600,
             nsim=n_sim,
             alpha=.05,
             progress=TRUE);p_sim_gini_disp_550
save(p_sim_gini_disp_600,file ="p_sim_gini_disp_600.RData")


# Visualization Power analysis for main effect of Gini--------------------------

pacman::p_load(simr,dplyr,ggplot2)  
load(here::here("output/tables/p_sim_gini_disp_60.RData"))
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
all_pwcurve$ps = c(p_sim_gini_disp_60$ps[1],
                   p_sim_gini_disp_100$ps[1],
                   p_sim_gini_disp_150$ps[1],
                   p_sim_gini_disp_200$ps[1],                   
                   p_sim_gini_disp_250$ps[1], 
                   p_sim_gini_disp_300$ps[1], 
                   p_sim_gini_disp_400$ps[1], 
                   p_sim_gini_disp_500$ps[1],
                   p_sim_gini_disp_600$ps[1])
# Combine the different numbers of levels.
all_pwcurve$xval = c(p_sim_gini_disp_60$nlevels,
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

# Combine results
all_pwcurve_s$ps = c(p_sim_gini_disp_50_small$ps[1],
                     p_sim_gini_disp_100_small$ps[1],
                     p_sim_gini_disp_150_small$ps[1],
                     p_sim_gini_disp_200_small$ps[1],
                     # p_sim_gini_disp_250_small$ps[1],
                     p_sim_gini_disp_300_small$ps[1],
                     p_sim_gini_disp_400_small$ps[1],
                     p_sim_gini_disp_500_small$ps[1],
                     p_sim_gini_disp_600_small$ps[1])
# Combine the different numbers of levels.
all_pwcurve_s$xval = c(p_sim_gini_disp_50_small$nlevels,
                       p_sim_gini_disp_100_small$nlevels,
                       p_sim_gini_disp_150_small$nlevels,
                       p_sim_gini_disp_200_small$nlevels,
                       # p_sim_gini_disp_250_small$nlevels,
                       p_sim_gini_disp_300_small$nlevels,
                       p_sim_gini_disp_400_small$nlevels,
                       p_sim_gini_disp_500_small$nlevels,
                       p_sim_gini_disp_600_small$nlevels
)

all_pwcurve_s$xlab <- 'Number of clusters'
df_psim_s<- data.frame(summary(all_pwcurve_s)) 
df_psim_s$nlevels <- all_pwcurve_s$xval

# Combine results
all_pwcurve_l$ps = c(p_sim_gini_disp_50_large$ps[1],
                     p_sim_gini_disp_100_large$ps[1],
                     p_sim_gini_disp_150_large$ps[1],
                     p_sim_gini_disp_200_large$ps[1],
                     # p_sim_gini_disp_250_large$ps[1],
                     p_sim_gini_disp_300_large$ps[1],
                     p_sim_gini_disp_400_large$ps[1],
                     p_sim_gini_disp_500_large$ps[1],
                     p_sim_gini_disp_600_large$ps[1])
# Combine the different numbers of levels.
all_pwcurve_l$xval = c(p_sim_gini_disp_50_large$nlevels,
                       p_sim_gini_disp_100_large$nlevels,
                       p_sim_gini_disp_150_large$nlevels,
                       p_sim_gini_disp_200_large$nlevels,
                       # p_sim_gini_disp_250_large$nlevels,
                       p_sim_gini_disp_300_large$nlevels,
                       p_sim_gini_disp_400_large$nlevels,
                       p_sim_gini_disp_500_large$nlevels,
                       p_sim_gini_disp_600_large$nlevels
)

all_pwcurve_l$xlab <- 'Number of clusters'
df_psim_l<- data.frame(summary(all_pwcurve_l)) 
df_psim_l$nlevels <- all_pwcurve_l$xval


number_x_axis_levels <- df_psim$nlevels
df_psim %>%
  ggplot(aes(y = mean, x = nlevels, ymin = lower, ymax = upper, label = nlevels)) +
  geom_errorbar(colour = 'grey40',width=10,size=1) + 
  geom_point(size=2) +
  geom_line(size=1) +
  # Draw 80% threshold----------------------------------------------------------#
  geom_hline(yintercept = 0.8, color = 'red', lty = 2) +
  scale_x_continuous(breaks = number_x_axis_levels)+
  scale_y_continuous(name = 'Share of significant', limits = c(0, 1), 
                     breaks = c(0, .2, .4, .6, .8, 1),
                     labels = c('0%', '20%', '40%', '60%', '80%', '100%')) +
  labs(title = "Power for predictor Gini (post-tax and transfers) on redistributive preferences",
       x = 'Number of clusters (countries)', 
       # subtitle = "β = 0.09 (observed effect)",
       caption = "Note: Based on 500 Monte Carlo replications of a multilevel model including the cross-level interaction. The simulations correspond to the\nobserved effect: 0.09. Each point represents the proportion of statistically significant p-values (< .05). Confidence intervals are at 95%..") + 
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 8)), 
        axis.title.y = element_text(size = 12, margin = margin(r = 7)), 
        axis.ticks = element_line(colour = 'grey50'),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom",
        plot.caption = element_text(size = rel(0.8), hjust = 0, vjust = 1, margin = margin(t = 10)))


ggsave(plot = last_plot(),filename = "figureS02.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1.5,units = "cm",scale = 15)

number_x_axis_levels <- df_psim_s$nlevels
df_psim_s %>%
  ggplot(aes(y = mean, x = nlevels, ymin = lower, ymax = upper, label = nlevels)) +
  geom_errorbar(colour = 'grey40',width=10,size=1) + 
  geom_point(size=2) +
  geom_line(size=1) +
  # Draw 80% threshold----------------------------------------------------------#
  geom_hline(yintercept = 0.8, color = 'red', lty = 2) +
  scale_x_continuous(breaks = number_x_axis_levels)+
  scale_y_continuous(name = 'Share of significant', limits = c(0, 1), 
                     breaks = c(0, .2, .4, .6, .8, 1),
                     labels = c('0%', '20%', '40%', '60%', '80%', '100%')) +
  labs(title = "Power for predictor Gini (post-tax and transfers) on redistributive preferences",
       x = 'Number of clusters (countries)', 
       # subtitle = "β = 0.09 (observed effect)",
       caption = "Note: Based on 500 Monte Carlo replications of a multilevel model including the cross-level interaction. The simulations correspond to the\nobserved effect: 0.09. Each point represents the proportion of statistically significant p-values (< .05). Confidence intervals are at 95%..") + 
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 8)), 
        axis.title.y = element_text(size = 12, margin = margin(r = 7)), 
        axis.ticks = element_line(colour = 'grey50'),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom",
        plot.caption = element_text(size = rel(0.8), hjust = 0, vjust = 1, margin = margin(t = 10)))

ggsave(plot = last_plot(),filename = "figureS02B.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1.5,units = "cm",scale = 15)

df_psim_l %>%
  ggplot(aes(y = mean, x = nlevels, ymin = lower, ymax = upper, label = nlevels)) +
  geom_errorbar(colour = 'grey40',width=10,size=1) + 
  geom_point(size=2) +
  geom_line(size=1) +
  # Draw 80% threshold----------------------------------------------------------#
  geom_hline(yintercept = 0.8, color = 'red', lty = 2) +
  scale_x_continuous(breaks = number_x_axis_levels)+
  scale_y_continuous(name = 'Share of significant', limits = c(0, 1), 
                     breaks = c(0, .2, .4, .6, .8, 1),
                     labels = c('0%', '20%', '40%', '60%', '80%', '100%')) +
  labs(title = "Power for predictor Gini (post-tax and transfers) on redistributive preferences",
       x = 'Number of clusters (countries)', 
       # subtitle = "β = 0.09 (observed effect)",
       caption = "Note: Based on 500 Monte Carlo replications of a multilevel model including the cross-level interaction. The simulations correspond to the\nobserved effect: 0.09. Each point represents the proportion of statistically significant p-values (< .05). Confidence intervals are at 95%..") + 
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 8)), 
        axis.title.y = element_text(size = 12, margin = margin(r = 7)), 
        axis.ticks = element_line(colour = 'grey50'),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom",
        plot.caption = element_text(size = rel(0.8), hjust = 0, vjust = 1, margin = margin(t = 10)))

ggsave(plot = last_plot(),filename = "figureS02C.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1.5,units = "cm",scale = 15)



# Visualization Power analysis for cross-level interactions --------------------
pacman::p_load(simr,dplyr,ggplot2)  
load(here::here("output/tables/psim_xlvl_work_50.RData"))
load(here::here("output/tables/psim_xlvl_work_100.RData"))
load(here::here("output/tables/psim_xlvl_work_150.RData"))
load(here::here("output/tables/psim_xlvl_work_200.RData"))
load(here::here("output/tables/psim_xlvl_work_250.RData"))
load(here::here("output/tables/psim_xlvl_work_300.RData"))
load(here::here("output/tables/psim_xlvl_work_350.RData"))
load(here::here("output/tables/psim_xlvl_work_400.RData"))

all_pwcurve = psim_work_50

# Combine results
all_pwcurve$ps = c(psim_work_50$ps[1],
                   psim_work_100$ps[1],
                   psim_work_150$ps[1],
                   psim_work_200$ps[1],
                   psim_work_250$ps[1],
                   psim_work_300$ps[1],
                   psim_work_350$ps[1],
                   psim_work_400$ps[1])

# Combine the different numbers of levels.
all_pwcurve$xval = c(
  psim_work_50$nlevels,
  psim_work_100$nlevels,
  psim_work_150$nlevels,  
  psim_work_200$nlevels,
  psim_work_250$nlevels,
  psim_work_300$nlevels,
  psim_work_350$nlevels,
  psim_work_400$nlevels
)
print(all_pwcurve) 
all_pwcurve$xlab <- 'Number of clusters'

df_psim<- data.frame(summary(all_pwcurve)) 
df_psim$nlevels <- all_pwcurve$xval
number_x_axis_levels <- df_psim$nlevels

df_psim %>%
  ggplot(aes(y = mean, x = nlevels, ymin = lower, ymax = upper, label = nlevels)) +
  geom_errorbar(colour = 'grey40',width=10,size=1) + 
  geom_point(size=2) +
  geom_line(size=1) +
  # Draw 80% threshold----------------------------------------------------------#
  geom_hline(yintercept = 0.8, color = 'red', lty = 2) +
  scale_x_continuous(breaks = number_x_axis_levels)+
  scale_y_continuous(limits = c(0, 1), 
                     breaks = c(0, .2, .4, .6, .8, 1),
                     labels = c('0%', '20%', '40%', '60%', '80%', '100%')) +
  labs(
    title = "Statistical Power for Cross-Level Interaction",
    subtitle = expression(beta == -0.109 ~ "observed effect of Working Class × Homogeneity × Gini on redistributive preferences"),
    x = "Number of clusters (countries)",
    y = "Share significant",
    caption = "Note: Based on 500 Monte Carlo simulation replications of the multilevel model including the cross-level interaction. Each point represents the proportion of p-values < 0.05. Confidence intervals are at 95%."
  ) + 
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 8)), 
        axis.title.y = element_text(size = 12, margin = margin(r = 7)), 
        axis.ticks = element_line(colour = 'grey50'),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave(plot = last_plot(),filename = "figureS01.png",device = "png",
       path = here::here("output/images"),width = 1.75,height = 1,units = "cm",scale = 20)


pacman::p_load(simr,dplyr,ggplot2)  
load(here::here("output/tables/psim_xlvl_intermid_50.RData"))
load(here::here("output/tables/psim_xlvl_intermid_100.RData"))
load(here::here("output/tables/psim_xlvl_intermid_150.RData"))


all_pwcurve = psim_intermid_50

# Combine results
all_pwcurve$ps = c(psim_intermid_50$ps[1],
                   psim_intermid_100$ps[1],
                   psim_intermid_150$ps[1]                   
                   )

# Combine the different numbers of levels.
all_pwcurve$xval = c(
  psim_intermid_50$nlevels,
  psim_intermid_100$nlevels,
  psim_intermid_150$nlevels  
)
print(all_pwcurve) 
all_pwcurve$xlab <- 'Number of clusters'

df_psim_int<- data.frame(summary(all_pwcurve)) 
df_psim_int$nlevels <- all_pwcurve$xval
number_x_axis_levels <- df_psim_int$nlevels

df_psim_int %>%
  ggplot(aes(y = mean, x = nlevels, ymin = lower, ymax = upper, label = nlevels)) +
  geom_errorbar(colour = 'grey40',width=10,size=1) + 
  geom_point(size=2) +
  geom_line(size=1) +
  # Draw 80% threshold----------------------------------------------------------#
  geom_hline(yintercept = 0.8, color = 'red', lty = 2) +
  scale_x_continuous(breaks = number_x_axis_levels)+
  scale_y_continuous(name = 'Power', limits = c(0, 1), 
                     breaks = c(0, .2, .4, .6, .8, 1),
                     labels = c('0%', '20%', '40%', '60%', '80%', '100%')) +
  labs(
    title = "Statistical Power for Cross-Level Interaction",
    subtitle = expression(beta == -0.248 ~ "observed effect of Intermediate Class × Homogeneity × Gini on redistributive preferences"),
    x = "Number of clusters (countries)",
    y = "Share significant",
    caption = "Note: Based on 500 Monte Carlo simulation replications of the multilevel model including the cross-level interaction. Each point represents the proportion of p-values < 0.05. Confidence intervals are at 95%.") +
  theme(axis.title.x = element_text(size = 10, margin = margin(t = 8)), 
        axis.title.y = element_text(size = 10, margin = margin(r = 7)), 
        axis.ticks = element_line(colour = 'grey50'),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


df_psim$sim <- "Working Class ×\nHomogeneity ×\nGini"
df_psim_int$sim <- "Intermediate Class ×\nHomogeneity ×\nGini"


bind_rows(df_psim,df_psim_int) %>%
  ggplot(aes(y = mean, x = nlevels, ymin = lower, ymax = upper, label = nlevels,group=sim, color=sim)) +
  geom_errorbar(colour = 'grey40',width=10,size=1) + 
  geom_point(size=2) +
  geom_line(size=1) +
  # Draw 80% threshold----------------------------------------------------------#
  geom_hline(yintercept = 0.8, color = 'red', lty = 2) +
  scale_color_grey(start = 0.2, end = 0.7) +
  scale_x_continuous(breaks = number_x_axis_levels)+
  scale_y_continuous(limits = c(0, 1), 
                     breaks = c(0, .2, .4, .6, .8, 1),
                     labels = c('0%', '20%', '40%', '60%', '80%', '100%')) +
  guides(color = guide_legend(title = NULL)) +
  labs(
    title = "Statistical Power for Cross-Level Interaction",
    subtitle = "Observed effect of Class × Homogeneity × Gini on redistributive preferences",
    x = "Number of clusters (countries)",
    y = "Share significant",
    caption = "Note: Based on 500 Monte Carlo replications of the multilevel model including the cross-level interactions. The simulations correspond to the observed\neffects: −0.109 (Working Class × Homogeneity × Gini) and −0.248 (Intermediate Class × Homogeneity × Gini). Each point represents the proportion of statistically\nsignificant p-values (< .05). Confidence intervals are at 95%."
) +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 8)), 
        axis.title.y = element_text(size = 12, margin = margin(r = 7)), 
        axis.ticks = element_line(colour = 'grey50'),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom",
        plot.caption = element_text(size = rel(0.8), hjust = 0, vjust = 1, margin = margin(t = 10))) + theme(
          legend.position = c(0.98, 0.05),       # Adjust X (right) and Y (bottom) position
          legend.justification = c(1, 0),        # Anchor legend box at bottom right
          legend.background = element_rect(fill = "white", color = "grey80"),  # optional box
          legend.title = element_blank()         # optional: remove title
        )
  

  ggsave(plot = last_plot(),filename = "figureS01.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1.5,units = "cm",scale = 15)


