set1 <-  RColorBrewer::brewer.pal(n = 8, name = "Set1")
options(ggplot2.discrete.fill = set1)
options(ggplot2.discrete.colour = set1)
ggplot2::theme_set(theme_minimal())
ggplot2::theme_update(text=element_text(size=15,  family="serif"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,sjmisc,ggplot2,interplot,marginaleffects,sjlabelled,haven,stringr,ordinal,texreg)  
rm(list=ls())
load(here::here("input/data/proc/study1_country.RData"));df1 <- df2
dfreg <- df1 %>% dplyr::select(
  egal = egal2,
  class3,
  homclass,
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

# Models 
base <- lmer(egal~1 + (1|country2),data=dfreg,weights = WEIGHT); icc1<- performance::icc(base)
homclass<- update(base, . ~ . +homclass)
full1 <- update(homclass, . ~ . + class3+female+agenum+age2)
rob1 <- update(full1, . ~ . + class3+edyears+ Q03pcm+union+workst)
rob2 <- update(rob1, . ~ . -(1|country2) + (homclass|country2))
# anova(rob1,rob2)
# Interactions segregation x class
int_homo <- update(rob1, . ~ . +class3*homclass)

models <- list(homclass,full1,rob1,int_homo)
knitreg(models)



# Interacciones cross-level -----------------------------------------------

# Gini Market
int_homo_giniM <- update(rob1, . ~ . +class3*homclass*gini_mkt -(1|country2) + (class3+homclass|country2))

# Gini Disposable
int_homo_giniD <- update(rob1, . ~ . +class3*homclass*gini_disp -(1|country2) + (class3+homclass|country2))

knitreg(list(int_homo_giniM,int_homo_giniD,int_homo_d10d01))

summary(int_homo_giniD)

# Predicted values homo*class*GiniM ---------------------------------------
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
df_pred_giniM$gini_mkt <- as.factor(df_pred_giniM$gini_mkt)

df_pred_giniM %>% 
  ggplot(aes(y=estimate,x=homclass, fill=class3,color=class3,ymin=conf.low, ymax=conf.high)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  geom_ribbon(alpha=0.1,size=1,width=.1,linetype=1) +
  facet_wrap(~gini_mkt) +
  labs(y = "Preferences for Redistribution",
       x="Class-based network homogeneity",
       title = "Three-way interaccion effects for Preferences for Redistribution, Network segregation, Social class and Income Inequality",
       subtitle = "Income Inequality corresponds to the minimum, mean and maximum values") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        axis.text=element_text(size=15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

# Predicted values homo*class*GiniD ---------------------------------------

ginid_max<- round(max(dfreg$gini_disp) ,2)
ginid_min<- round(min(dfreg$gini_disp),2)
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
       title = "Three-way interaccion effects for Preferences for Redistribution, Network segregation, Social class and Income Inequality",
       subtitle = "Income Inequality corresponds to the minimum, mean and maximum values") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        axis.text=element_text(size=15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
