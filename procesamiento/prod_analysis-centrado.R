if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,sjmisc,ggplot2,interplot,marginaleffects,sjlabelled,haven,stringr,ordinal,ggplot2)  
rm(list=ls())
load(here::here("input/data/proc/study1_country.RData"));df1 <- df2
dfreg <- df1 %>% dplyr::select(redist,
                               class3_ser_gc,
                               class3_mid_gc,
                               class3_wor_gc,
                               # class3_ser_gc=class3res_ser_gc, 
                               # class3_mid_gc=class3res_mid_gc, 
                               # class3_wor_gc=class3res_wor_gc,
                               # class3spo,
                               homclass = homclass_gc,
                               Q03pcm_1_gc, Q03pcm_2_gc, Q03pcm_3_gc, Q03pcm_NA_gc,
                               edyears = edyears_gc,
                               female = female_gc,
                               agenum = agenum_gc,
                               union = union_gc,
                               workst = workst_gc,
                               #----------------------------
                                 # gini_disp=gini_disp_c,
                                 # gini_mkt=gini_mkt_c,
                                 # palmaratio=palmaratio_c,
                                 # top10=top10_c,
                                 # middle50=middle50_c,
                                 # d10d1=d10d1_c,
                                 # giniindex=giniindex_c,
                                 # ttheilge1=ttheilge1_c,
                                #----------------------------
                                 # gini_disp=gini_disp_z,
                                 # gini_mkt=gini_mkt_z,
                                 # palmaratio=palmaratio_z,
                                 # top10=top10_z,
                                 # middle50=middle50_z,
                                 # d10d1=d10d1_z,
                                 # giniindex=giniindex_z,
                                 # ttheilge1=ttheilge1_z,
                               #------------------------------
                               gini_disp,
                               gini_mkt,
                               palmaratio,
                               top10,
                               middle50,
                               d10d1,
                               giniindex,
                               ttheilge1,
                                 WEIGHT,
                                 region,                              
                                 country2=country3,
                                 country,
                               rgdpna,
                               oecd
                                 ) %>% 
mutate(logrgdpna=log(rgdpna),
       edyears2=edyears^2,
       age2=agenum^2) %>%
  # mutate(homclass=group_center(homclass,country2))  %>%
  # filter(!country2 %in% c("TWN","CHN","JPN")) %>%
  # filter(oecd=="OECD" & country2!="SVN") %>%
  na.omit()

base <- lmer(redist~1 + (1|country2),data=dfreg,weights = WEIGHT); icc1<- performance::icc(base)
homclass<- update(base, . ~ . +homclass)
full1 <- update(homclass, . ~ . +class3_mid_gc+class3_wor_gc +female+agenum+age2)
rob1 <- update(full1, . ~ . + edyears+ Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+union+workst)
rob2 <- update(rob1, . ~ . -(1|country2) + (homclass|country2))

# anova(rob1,rob2)
# Interactions segregation x class
int_homo <- update(rob1, . ~ . +class3_mid_gc*homclass+class3_wor_gc*homclass)
int_homo_country <- update(rob2, . ~ . -(1|country2) + (class3_mid_gc*homclass+class3_wor_gc*homclass|country2))
models <- list(homclass,full1,rob1,int_homo,int_homo_country)
texreg::knitreg(models,stars = c(.001, .01, .05, .1),symbol = '\u2020')

  # Homoclass x Other Economic Inequality-----------------------------------------
main <- "redist~class3_mid_gc+class3_wor_gc+homclass+edyears+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+workst+union+female+agenum+age2+(homclass|country2)"
base <- lmer(formula(main),data=dfreg,weights = WEIGHT)

int_ginid <- update(base, . ~ . +homclass*gini_disp)
int_ginim <- update(base, . ~ . +homclass*gini_mkt)
int_palma <- update(base, . ~ . +homclass*palmaratio)
int_palma <- update(base, . ~ . +homclass*palmaratio)
int_pc10  <- update(base, . ~ . +homclass*top10)
int_pc50  <- update(base, . ~ . +homclass*middle50)
int_d10d1 <- update(base, . ~ . +homclass*d10d1)
int_gini_wiid <- update(base, . ~ . +homclass*giniindex)
int_theil  <- update(base, . ~ . +homclass*ttheilge1)

models_m <- list(int_ginid,int_ginim,int_palma,int_pc10,int_pc50,int_d10d1,
                 int_gini_wiid,int_theil)

texreg::knitreg(models_m,stars = c(.001, .01, .05, .1))


interplot::interplot(m = int_ginim,var1 = "homclass",var2="gini_mkt",point = T)

predval1 <- 
  plot_predictions(int_ginim, condition = list("homclass",gini_mkt=range),gray = F) +
  xlab("Network homogeneity")+
  ylab("Redistributive preferences") +
  theme(legend.position = "right")



broom_df<-
broom.mixed::tidy(int_homo_country, effects = "ran_vals", conf.int = TRUE)

broom_df

plot_df <- 
broom_df %>% 
  filter(term == "class3_mid_gc:homclass" | term =="homclass:class3_wor_gc") %>% 
  arrange(level) 


ggplot(
  plot_df,
  aes(x = estimate, y = level ,group=term, fill=term)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),size=1, width=0.2) +
  geom_point(shape=21, size=3, fill="white") +
  geom_vline(xintercept = 0) +
  scale_x_discrete() +
  facet_grid(~term)