use "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\input\data\proc\study1_country.dta"
*For the regression tables:
*ssc install estout, replace
*https://repec.sowi.unibe.ch/stata/estout/esttab.html
* check how to save estimations: https://www.radyakin.org/statalist/2015/20150702_1300997.do

gen logrgdpna = log(rgdpna)

*# Ordinal logit full sample ---------------------------------------------------
meglm redist c.homclass##c.gini_disp i.class3 female agenum edyears i.Q03pcm workst union logrgdpna gini_disp [pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model1

meglm redist c.homclass##c.gini_mkt i.class3 female agenum edyears i.Q03pcm workst union logrgdpna gini_mkt [pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model2

meglm redist c.homclass##c.top10 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna top10[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model3

meglm redist c.homclass##c.middle50 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna middle50[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model4

meglm redist c.homclass##c.d10d1 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna d10d1[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model5

meglm redist c.homclass##c.giniindex i.class3 female agenum edyears i.Q03pcm workst union logrgdpna giniindex[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model6

meglm redist c.homclass##c.ttheilge1 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna ttheilge1[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model7

estimates restore model1
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est", replace

estimates restore model2
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est", append

estimates restore model3
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est", append

estimates restore model4
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est", append

estimates restore model5
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est", append

estimates restore model6
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est", append

estimates restore model7
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est", append

clear all
eret list
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est,  number(1)
estimates store model1
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est,  number(2)
estimates store model2
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est,  number(3)
estimates store model3
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est,  number(4)
estimates store model4
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est,  number(5)
estimates store model5
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est,  number(6)
estimates store model6
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\fullsample-ologit-homclass-respondent-macro.est,  number(7)
estimates store model7

esttab model1 model2 model3 model4 model5 model6 model7, drop(logrgdpna 1.class3 2.class3 3.class3 female agenum edyears workst union 1.Q03pcm 2.Q03pcm 3.Q03pcm 4.Q03pcm )

*# Ordinal logit strong ties ---------------------------------------------------
meglm redist c.homclass_s##c.gini_disp i.class3 female agenum edyears i.Q03pcm workst union logrgdpna gini_disp[pweight=WEIGHT] || country2: homclass_s, family(ordinal)
estimates store model1

meglm redist c.homclass_s##c.gini_mkt i.class3 female agenum edyears i.Q03pcm workst union logrgdpna gini_mkt[pweight=WEIGHT] || country2: homclass_s, family(ordinal)
estimates store model2

meglm redist c.homclass_s##c.top10 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna top10[pweight=WEIGHT] || country2: homclass_s, family(ordinal)
estimates store model3

meglm redist c.homclass_s##c.middle50 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna middle50[pweight=WEIGHT] || country2: homclass_s, family(ordinal)
estimates store model4

meglm redist c.homclass_s##c.d10d1 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna d10d1[pweight=WEIGHT] || country2: homclass_s, family(ordinal)
estimates store model5

meglm redist c.homclass_s##c.giniindex i.class3 female agenum edyears i.Q03pcm workst union logrgdpna giniindex[pweight=WEIGHT] || country2: homclass_s, family(ordinal)
estimates store model6

meglm redist c.homclass_s##c.ttheilge1 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna ttheilge1[pweight=WEIGHT] || country2: homclass_s, family(ordinal)
estimates store model7

estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\fullsample-ologit-homclass-strong-macro.est", replace
esttab model1 model2 model3 model4 model5 model6 model7, drop(logrgdpna 1.class3 2.class3 3.class3 female agenum edyears workst union 1.Q03pcm 2.Q03pcm 3.Q03pcm 4.Q03pcm )

*# Ordinal logit weak ties -----------------------------------------------------

meglm redist c.homclass_w##c.gini_disp i.class3 female agenum edyears i.Q03pcm workst union logrgdpna gini_disp[pweight=WEIGHT] || country2: homclass_w, family(ordinal)
estimates store model1

meglm redist c.homclass_w##c.gini_mkt i.class3 female agenum edyears i.Q03pcm workst union logrgdpna gini_mkt[pweight=WEIGHT] || country2: homclass_w, family(ordinal)
estimates store model2

meglm redist c.homclass_w##c.top10 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna top10[pweight=WEIGHT] || country2: homclass_w, family(ordinal)
estimates store model3

meglm redist c.homclass_w##c.middle50 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna middle50[pweight=WEIGHT] || country2: homclass_w, family(ordinal)
estimates store model4

meglm redist c.homclass_w##c.d10d1 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna d10d1[pweight=WEIGHT] || country2: homclass_w, family(ordinal)
estimates store model5

meglm redist c.homclass_w##c.giniindex i.class3 female agenum edyears i.Q03pcm workst union logrgdpna giniindex[pweight=WEIGHT] || country2: homclass_w, family(ordinal)
estimates store model6

meglm redist c.homclass_w##c.ttheilge1 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna ttheilge1[pweight=WEIGHT] || country2: homclass_w, family(ordinal)
estimates store model7

estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\fullsample-ologit-homclass-weak-macro.est", replace
esttab model1 model2 model3 model4 model5 model6 model7, drop(logrgdpna 1.class3 2.class3 3.class3 female agenum edyears workst union 1.Q03pcm 2.Q03pcm 3.Q03pcm 4.Q03pcm )

*# Ordinal logit R's class-- ---------------------------------------------------
meglm redist c.homclass_res##c.gini_disp i.class3res female agenum edyears i.Q03pcm workst union logrgdpna gini_disp[pweight=WEIGHT] || country2: homclass_res, family(ordinal)
estimates store model1

meglm redist c.homclass_res##c.gini_mkt i.class3res female agenum edyears i.Q03pcm workst union logrgdpna gini_mkt[pweight=WEIGHT] || country2: homclass_res, family(ordinal)
estimates store model2

meglm redist c.homclass_res##c.top10 i.class3res female agenum edyears i.Q03pcm workst union logrgdpna top10[pweight=WEIGHT] || country2: homclass_res, family(ordinal)
estimates store model3

meglm redist c.homclass_res##c.middle50 i.class3res female agenum edyears i.Q03pcm workst union logrgdpna middle50[pweight=WEIGHT] || country2: homclass_res, family(ordinal)
estimates store model4

meglm redist c.homclass_res##c.d10d1 i.class3res female agenum edyears i.Q03pcm workst union logrgdpna d10d1[pweight=WEIGHT] || country2: homclass_res, family(ordinal)
estimates store model5

meglm redist c.homclass_res##c.giniindex i.class3res female agenum edyears i.Q03pcm workst union logrgdpna giniindex[pweight=WEIGHT] || country2: homclass_res, family(ordinal)
estimates store model6

meglm redist c.homclass_res##c.ttheilge1 i.class3res female agenum edyears i.Q03pcm workst union logrgdpna ttheilge1[pweight=WEIGHT] || country2: homclass_res, family(ordinal)
estimates store model7

estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\fullsample-ologit-homclass-respondent-macro.est", replace
esttab model1 model2 model3 model4 model5 model6 model7, drop(logrgdpna 1.class3res 2.class3res 3.class3res female agenum edyears workst union 1.Q03pcm 2.Q03pcm 3.Q03pcm 4.Q03pcm )
