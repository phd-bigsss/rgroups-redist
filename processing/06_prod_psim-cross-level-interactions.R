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

dfreg$gini_disp=scale(dfreg$gini_disp)
dfreg$wstate2 <- scale(scale(dfreg$abs_red) + scale(dfreg$ilo_taxrev) + scale(dfreg$ilo_govspe))

# Variable Group-Centering ------------------------------------------------$
dfreg <- 
  dfreg %>% 
  mutate(to_dummy(female)
  )
dfreg$female_gc = group_center(dfreg$female_2, grp = dfreg$country2)
dfreg$agenum_gc = group_center(dfreg$agenum, grp = dfreg$country2)
dfreg$homclass3_V_gc <-group_center(dfreg$homclass3_V_res, grp = dfreg$country2)
dfreg$know_total_gc = group_center(dfreg$know_total, grp = dfreg$country2)

dfreg$egal_sd <- scale(dfreg$egal) #standardized DV

model_re <- 
  lmer(egal_sd~ 
         homclass3_V_gc+
         know_total_gc+
         dclass3res_V+
         female_gc+agenum_gc+
         gini_disp +loggdppercapita + wstate2+
         homclass3_V_gc*dclass3res_V*gini_disp + 
         homclass3_V_gc*dclass3res_V*wstate2 + 
         (homclass3_V_gc+dclass3res_V|country2),data=dfreg,weights = WEIGHT)


coef<- summary(model_re)$coef[,"Estimate"]
fixed<- as.numeric(coef)
varcomp<- as.data.frame(lme4::VarCorr(model_re))
varcomp$vcov[1:4] # var country
varcomp$sdcor[2] # var individual

# fixed intercept and slope
fixed <- fixed
# random intercept and slope variance-covariance matrix
# For class
rand <- lme4::VarCorr(model_re)
# Exrtact the residual sd
s <- varcomp$sdcor[11] 

model <- makeLmer(egal_sd~ 
                    homclass3_V_gc+
                    know_total_gc+
                    dclass3res_V+
                    female_gc+agenum_gc+
                    gini_disp +loggdppercapita + wstate2+
                    homclass3_V_gc*dclass3res_V*gini_disp + 
                    homclass3_V_gc*dclass3res_V*wstate2 + 
                    loggdppercapita + wstate2+(homclass3_V_gc+dclass3res_V|country2), fixef=fixed, VarCorr=rand, sigma=s, data=dfreg)

model_psim <- extend(model, along = "country2", n =600)
model
save(model_psim,file = here::here("input/data/proc/model_for_xlevel.RData"))

################################################################################.
# Simulation section ------------------------------------------------------
################################################################################.
load(here::here("input/data/proc/model_for_xlevel.RData"))
n_sim <- 500
library(simr)


# Simulation for Intermediate Class x Homogeneity × Gini---------------------------
psim_intermid_50 <-
  powerCurve(model_psim,
             test=simr::fixed("homclass3_V_gc:dclass3res_VIntermediate class (III+IV):gini_disp", method = "t"),
             breaks = 50, along = "country2",
             nsim=n_sim,
             alpha=.05,
             progress=TRUE,
             seed = 123)

save(psim_intermid_50, file = here::here("input/data/proc/psim_xlvl_intermid_50.RData"))

psim_intermid_100 <-
  powerCurve(model_psim,
             test=simr::fixed("homclass3_V_gc:dclass3res_VIntermediate class (III+IV):gini_disp", method = "t"),
             breaks = 100, along = "country2",
             nsim=n_sim,
             alpha=.05,
             progress=TRUE,
             seed = 123)

save(psim_intermid_100, file = here::here("input/data/proc/psim_xlvl_intermid_100.RData"))

psim_intermid_150 <-
  powerCurve(model_psim,
             test=simr::fixed("homclass3_V_gc:dclass3res_VIntermediate class (III+IV):gini_disp", method = "t"),
             breaks = 150, along = "country2",
             nsim=n_sim,
             alpha=.05,
             progress=TRUE,
             seed = 123)

save(psim_intermid_150, file = here::here("input/data/proc/psim_xlvl_intermid_150.RData"))

psim_intermid_200 <-
  powerCurve(model_psim,
             test=simr::fixed("homclass3_V_gc:dclass3res_VIntermediate class (III+IV):gini_disp", method = "t"),
             breaks = 200, along = "country2",
             nsim=n_sim,
             alpha=.05,
             progress=TRUE,
             seed = 123)

save(psim_intermid_200, file = here::here("input/data/proc/psim_xlvl_intermid_200.RData"))

# Simulation for Intermediate Working Class × Homogeneity × Gini----------------

psim_work_50 <-
  powerCurve(model_psim,
             test=simr::fixed("homclass3_V_gc:dclass3res_VWorking Class (V+VI+VII):gini_disp", method = "t"),
             breaks = 50, along = "country2",
             nsim=n_sim,
             alpha=.05,
             seed = 123)

save(psim_work_50, file = here::here("input/data/proc/psim_xlvl_work_50.RData"))

psim_work_100 <-
  powerCurve(model_psim,
             test=simr::fixed("homclass3_V_gc:dclass3res_VWorking Class (V+VI+VII):gini_disp", method = "t"),
             breaks = 100, along = "country2",
             nsim=n_sim,
             alpha=.05,
             seed = 123)

save(psim_work_100, file = here::here("input/data/proc/psim_xlvl_work_100.RData"))

psim_work_150 <-
  powerCurve(model_psim,
             test=simr::fixed("homclass3_V_gc:dclass3res_VWorking Class (V+VI+VII):gini_disp", method = "t"),
             breaks = 150, along = "country2",
             nsim=n_sim,
             alpha=.05,
             seed = 123)

save(psim_work_150, file = here::here("input/data/proc/psim_xlvl_work_150.RData"))

psim_work_200 <-
  powerCurve(model_psim,
             test=simr::fixed("homclass3_V_gc:dclass3res_VWorking Class (V+VI+VII):gini_disp", method = "t"),
             breaks = 200, along = "country2",
             nsim=n_sim,
             alpha=.05,
             seed = 123)

save(psim_work_200, file = here::here("input/data/proc/psim_xlvl_work_200.RData"))

psim_work_300 <-
  powerCurve(model_psim,
             test=simr::fixed("homclass3_V_gc:dclass3res_VWorking Class (V+VI+VII):gini_disp", method = "t"),
             breaks = 300, along = "country2",
             nsim=n_sim,
             alpha=.05,
             seed = 123)

save(psim_work_300, file = here::here("input/data/proc/psim_xlvl_work_300.RData"))

psim_work_400 <-
  powerCurve(model_psim,
             test=simr::fixed("homclass3_V_gc:dclass3res_VWorking Class (V+VI+VII):gini_disp", method = "t"),
             breaks = 400, along = "country2",
             nsim=n_sim,
             alpha=.05,
             seed = 123)

save(psim_work_400, file = here::here("input/data/proc/psim_xlvl_work_400.RData"))

# 2) Visualization Power analysis for cross-level interactions --------------------
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
number_x_axis_levels <- df_psim$nlevels

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
    # title = "Statistical Power for Cross-Level Interaction",
    # subtitle = "Observed effect of Class × Homogeneity × Gini on redistributive preferences",
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
       path = here::here("output/images"),width = 2,height = 1.5,units = "cm",scale = 15,dpi = 500)


