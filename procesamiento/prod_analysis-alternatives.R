set1 <-  RColorBrewer::brewer.pal(n = 8, name = "Set1")
options(ggplot2.discrete.fill = set1)
options(ggplot2.discrete.colour = set1)
ggplot2::theme_set(ggplot2::theme_bw())
ggplot2::theme_update(text=ggplot2::element_text(size=15,  family="serif"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,sjmisc,ggplot2,interplot,marginaleffects,sjlabelled,haven,stringr,ordinal,texreg,MLMusingR)  
rm(list=ls())

load(here::here("input/data/proc/study1_country.RData"));df1 <- df2
dfreg <- df1 %>% dplyr::select(
  egal = egal2,
  class3,
  homclass,
  homclass_wght,know_total,
  Q03pcm,
  edyears,
  female,
  agenum,
  union,
  workst,
  WEIGHT,
  region,
  "gini_disp",
  "gini_mkt",
  d10d1,
  rgdpna,
  oecd,
  country2, country
) %>%
  mutate(logrgdpna = log(rgdpna),
         edyears2 = edyears ^ 2,
         age2 = agenum ^ 2) %>%
  filter(country2 != "SVN") %>%
  na.omit()


dfreg <- 
  dfreg %>% 
  mutate(to_dummy(female),
         to_dummy(union),
         to_dummy(workst),
         # dummy for categorical variables-------------------------------------
         to_dummy(class3),
         # to_dummy(class3spo),
         # to_dummy(class3res),
         to_dummy(Q03pcm)
         )
dfreg$female_gc = group_center(dfreg$female_2, grp = dfreg$country2)
dfreg$union_gc = group_center(dfreg$union_2, grp = dfreg$country2)
dfreg$workst_gc = group_center(dfreg$workst_2, grp = dfreg$country2)
dfreg$edyears_gc = group_center(dfreg$edyears, grp = dfreg$country2)
dfreg$agenum_gc = group_center(dfreg$agenum, grp = dfreg$country2)
dfreg$age2_gc = group_center(dfreg$age2, grp = dfreg$country2)
dfreg$homclass_gc = group_center(dfreg$homclass, grp = dfreg$country2)


# EGP: dominance
dfreg$class3_ser_gc = group_center(dfreg$class3_1, grp = dfreg$country2)
dfreg$class3_mid_gc = group_center(dfreg$class3_2, grp = dfreg$country2)
dfreg$class3_wor_gc = group_center(dfreg$class3_3, grp = dfreg$country2)
# EGP: Respondent
# dfreg$class3res_ser_gc = group_center(dfreg$class3res_1, grp = dfreg$country2)
# dfreg$class3res_mid_gc = group_center(dfreg$class3res_2, grp = dfreg$country2)
# dfreg$class3res_wor_gc = group_center(dfreg$class3res_3, grp = dfreg$country2)
# EGP: Respondent
# dfreg$class3spo_ser_gc = group_center(dfreg$class3spo_1, grp = dfreg$country2)
# dfreg$class3spo_mid_gc = group_center(dfreg$class3spo_2, grp = dfreg$country2)
# dfreg$class3spo_wor_gc = group_center(dfreg$class3spo_3, grp = dfreg$country2)
# Household Income Tercile
dfreg$Q03pcm_1_gc = group_center(dfreg$Q03pcm_1, grp = dfreg$country2)
dfreg$Q03pcm_2_gc = group_center(dfreg$Q03pcm_2, grp = dfreg$country2)
dfreg$Q03pcm_3_gc = group_center(dfreg$Q03pcm_3, grp = dfreg$country2)
dfreg$Q03pcm_NA_gc = group_center(dfreg$Q03pcm_4, grp = dfreg$country2)

names(dfreg)
# drop dummies
dfreg <- 
  dfreg %>% 
  dplyr::select(-c(female_1,female_2,union_1,union_2,workst_1,workst_2,
            class3_1,class3_2,class3_3,
            # class3res_1,class3res_2,class3res_3,
            # class3spo_1,class3spo_2,class3spo_3,
            Q03pcm_1,Q03pcm_2,Q03pcm_2,Q03pcm_4))
names(dfreg)


# Models 
base <- lmer(egal~1 + (1|country2),data=dfreg,weights = WEIGHT); icc1<- performance::icc(base)
homclass<- update(base, . ~ . +homclass)
homclass_know_total<- update(homclass, . ~ . +know_total)
full1 <- update(homclass_know_total, . ~ . + class3+female+agenum+age2)
rob1 <- update(full1, . ~ . + class3+edyears+ Q03pcm+union+workst)
rob2 <- update(rob1, . ~ . -(1|country2) + (homclass|country2))
# anova(rob1,rob2)
# Interactions segregation x class
int_homo <- update(rob1, . ~ . +class3*homclass)

models <- list(homclass,homclass_know_total,full1,rob1,int_homo)
knitreg(models)

# Interacciones cross-level -----------------------------------------------
## Gini Market--------------------------------------------------------------
int_homo_giniM <- update(rob1, . ~ . +class3*homclass*gini_mkt +logrgdpna -(1|country2) + (class3+homclass|country2))

base_giniM_gc <- 
  lmer(egal~homclass_gc+class3+female_gc+agenum_gc+age2_gc+
         edyears_gc +Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         gini_mkt +logrgdpna +
         (1|country2),data=dfreg,weights = WEIGHT)
int_homo_giniM_gc <- 
  update(base_giniM_gc, . ~ . 
         +homclass_gc*class3*gini_mkt -(1|country2) +
           (homclass_gc+class3|country2))


## Gini Disposable---------------------------------------------------------
int_homo_giniD <- update(rob1, . ~ . +class3*homclass*gini_disp + logrgdpna-(1|country2) + (class3+homclass|country2))

base_giniD_gc <- 
  lmer(egal~homclass_gc+class3+female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         gini_disp +logrgdpna +
         (1|country2),data=dfreg,weights = WEIGHT)
int_homo_giniD_gc <- 
  update(base_giniD_gc, . ~ . 
         +homclass_gc*class3*gini_disp -(1|country2) +
           (homclass_gc+class3|country2))

knitreg(list(int_homo_giniM,int_homo_giniD,
             int_homo_giniM_gc,int_homo_giniD_gc))

summary(int_homo_giniD)

### Predicted values homo*class*GiniM ---------------------------------------
# ginim_max<- round(max(dfreg$gini_mkt) ,2)
# ginim_min<- round(min(dfreg$gini_mkt),2)

ginim_max<- round(mean(dfreg$gini_mkt) + sd(dfreg$gini_mkt)  ,2)
ginim_min<- round(min(dfreg$gini_mkt) -sd(dfreg$gini_mkt) ,2)
ginim_mea<- round(mean(dfreg$gini_mkt),2)

df_pred_giniM <-
  predictions(
    int_homo_giniM,newdata = datagrid(
      gini_mkt = c(ginim_min, ginim_mea, ginim_max),
      Q03pcm = "T02",
      homclass = seq(0, 1, by= 0.1),
      class3 = levels(dfreg$class3),
      edyears = mean(dfreg$edyears),
      agenum = mean(dfreg$agenum),
      age2 = mean(dfreg$age2)
    )
  )

df_pred_giniM <- as.data.frame(df_pred_giniM)
# df_pred_giniM$gini_mkt <- factor(df_pred_giniM$gini_mkt,labels = c("Gini (-1SD)","Gini (Mean)", "Gini (+1SD)"))

# df_pred_giniM <- df_pred_giniM %>% filter(class3 != "Intermediate class (III+IV+V)")

df_pred_giniM %>% 
  ggplot(aes(y=estimate,x=homclass, fill=class3,color=class3,ymin=conf.low, ymax=conf.high)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  geom_ribbon(alpha=0.1,size=1,width=.1,linetype=1) +
  facet_wrap(~gini_mkt) +
  labs(y = "Preferences for Redistribution",
       x="Class-based network homogeneity",
       title = "Three-way interaccion effects for Preferences for Redistribution, Network segregation, Social class and Income Inequality") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Income inequality (Market)", breaks = NULL, labels = NULL)) +
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        axis.text=element_text(size=15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave(plot = last_plot(),filename = "predict-egal-hom_class_giniM_fullsample.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1,units = "cm",scale = 18)

### Predicted values homo*class*GiniD ---------------------------------------

# ginid_max<- round(max(dfreg$gini_disp) ,2)
# ginid_min<- round(min(dfreg$gini_disp),2)
# ginid_mea<- round(mean(dfreg$gini_disp),2)

ginid_max<- round(mean(dfreg$gini_disp) + sd(dfreg$gini_disp)  ,2)
ginid_min<- round(min(dfreg$gini_disp) -sd(dfreg$gini_disp) ,2)
ginid_mea<- round(mean(dfreg$gini_disp),2)


df_pred_giniD <-
  predictions(
    int_homo_giniD,newdata = datagrid(
      gini_disp = c(ginid_min, ginid_mea, ginid_max),
      Q03pcm = "T02",
      homclass = seq(0, 1, by= 0.1),
      class3 = levels(dfreg$class3),
      edyears = mean(dfreg$edyears),
      agenum = mean(dfreg$agenum),
      age2 = mean(dfreg$age2)
    )
  )

df_pred_giniD <- as.data.frame(df_pred_giniD)
df_pred_giniD$gini_disp <- as.factor(df_pred_giniD$gini_disp)

df_pred_giniD %>% 
  ggplot(aes(y=estimate,x=homclass, fill=class3,color=class3,ymin=conf.low, ymax=conf.high)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  geom_ribbon(alpha=0.1,size=1,width=.1,linetype=1) +
  facet_wrap(~gini_disp) +
  labs(y = "Preferences for Redistribution",
       x="Class-based network homogeneity",
       title = "Three-way interaccion effects for Preferences for Redistribution, Network segregation, Social class and Income Inequality") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Income inequality (Disposable)", breaks = NULL, labels = NULL)) +
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        axis.text=element_text(size=15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave(plot = last_plot(),filename = "predict-egal-hom_class_giniD_fullsample.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1,units = "cm",scale = 18)

############################################################################-


# CENTERED VARIABLES -------------------------------------------------------
############################################################################-


### Predicted values homo*class*GiniM ---------------------------------------
# ginim_max<- round(max(dfreg$gini_mkt) ,2)
# ginim_min<- round(min(dfreg$gini_mkt),2)

ginim_max<- round(mean(dfreg$gini_mkt) + sd(dfreg$gini_mkt)  ,2)
ginim_min<- round(min(dfreg$gini_mkt) -sd(dfreg$gini_mkt) ,2)
ginim_mea<- round(mean(dfreg$gini_mkt),2)

df_pred_giniM_gc <-
  predictions(
    int_homo_giniM_gc,newdata = datagrid(
      gini_mkt = c(ginim_min, ginim_mea, ginim_max),
      homclass_gc = seq(min(dfreg$homclass_gc), max(dfreg$homclass_gc), by= 0.1),
      class3=levels(dfreg$class3)
    )
  )

df_pred_giniM_gc <- as.data.frame(df_pred_giniM_gc)
df_pred_giniM_gc$gini_mkt <- factor(df_pred_giniM_gc$gini_mkt,labels = c("Gini (-1SD)","Gini (Mean)", "Gini (+1SD)"))



df_pred_giniM_gc %>% 
  ggplot(aes(y=estimate,x=homclass_gc, fill=class3,color=class3,ymin=conf.low, ymax=conf.high)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  geom_ribbon(alpha=0.1,size=1,width=.1,linetype=1) +
  facet_wrap(~gini_mkt) +
  labs(y = "Preferences for Redistribution",
       x="Class-based network homogeneity (CWC)",
       title = "Three-way interaccion effects for Preferences for Redistribution, Network segregation, Social class and Income Inequality") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Income inequality (Market)", breaks = NULL, labels = NULL)) +
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        axis.text=element_text(size=15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave(plot = last_plot(),filename = "predict-egal-hom_class_giniM_fullsample_CWC.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1,units = "cm",scale = 18)

### Predicted values homo*class*GiniD ---------------------------------------
# ginim_max<- round(max(dfreg$gini_mkt) ,2)
# ginim_min<- round(min(dfreg$gini_mkt),2)

ginid_max<- round(mean(dfreg$gini_disp) + sd(dfreg$gini_disp)  ,2)
ginid_min<- round(min(dfreg$gini_disp) -sd(dfreg$gini_disp) ,2)
ginid_mea<- round(mean(dfreg$gini_disp),2)

df_pred_giniD_gc <-
  predictions(
    int_homo_giniD_gc,newdata = datagrid(
      gini_disp = c(ginid_min, ginid_mea, ginid_max),
      homclass_gc = seq(min(dfreg$homclass_gc), max(dfreg$homclass_gc), by= 0.1),
      class3=levels(dfreg$class3)
    )
  )

df_pred_giniD_gc <- as.data.frame(df_pred_giniD_gc)
df_pred_giniD_gc$gini_disp <- factor(df_pred_giniD_gc$gini_disp,labels = c("Gini (-1SD)","Gini (Mean)", "Gini (+1SD)"))


df_pred_giniD_gc %>% 
  ggplot(aes(y=estimate,x=homclass_gc, fill=class3,color=class3,ymin=conf.low, ymax=conf.high)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  geom_ribbon(alpha=0.1,size=1,linetype=1) +
  facet_wrap(~gini_disp) +
  labs(y = "Preferences for Redistribution",
       x="Class-based network homogeneity (CWC)",
       title = "Three-way interaccion effects for Preferences for Redistribution, Network segregation, Social class and Income Inequality") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Income inequality (Disposable)", breaks = NULL, labels = NULL)) +
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        axis.text=element_text(size=15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave(plot = last_plot(),filename = "predict-egal-hom_class_giniD_fullsample_CWC.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1,units = "cm",scale = 18)


df_pred_giniD_gc_1 <- df_pred_giniD_gc %>% filter(class3!="Intermediate class (III+IV+V)")

df_pred_giniD_gc_1 %>% 
  ggplot(aes(y=estimate,x=homclass_gc, fill=class3,color=class3,ymin=conf.low, ymax=conf.high)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  geom_ribbon(alpha=0.1,size=1,linetype=1) +
  facet_wrap(~gini_disp) +
  labs(y = "Preferences for Redistribution",
       x="Class-based network homogeneity (CWC)",
       title = "Three-way interaccion effects for Preferences for Redistribution, Network segregation, Social class and Income Inequality") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Income inequality (Disposable)", breaks = NULL, labels = NULL)) +
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        axis.text=element_text(size=15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave(plot = last_plot(),filename = "predict-egal-hom_class_giniD_fullsample_CWC_2.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1,units = "cm",scale = 18)
















