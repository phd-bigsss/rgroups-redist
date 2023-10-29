/* Multilevel ordinal logistica regression cross level interactions*/
/* Non confusian sample*/

*# Ordinal logit full network ---------------------------------------------------
{
clear all
use "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\input\data\proc\study1_country.dta"
*For the regression tables:
*ssc install estout, replace
*https://repec.sowi.unibe.ch/stata/estout/esttab.html
* check how to save estimations: https://www.radyakin.org/statalist/2015/20150702_1300997.do

gen logrgdpna = log(rgdpna)
gen age2 = agenum*agenum
keep if country2 != "TWN" 
keep if country2 != "CHN"
keep if country2 != "JPN" 
tab country2
qui meglm redist c.homclass##c.gini_disp i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna gini_disp [pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model1

qui meglm redist c.homclass##c.gini_mkt i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna gini_mkt [pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model2

qui meglm redist c.homclass##c.palmaratio i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna palmaratio[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model3

qui meglm redist c.homclass##c.top10 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna top10[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model4

qui meglm redist c.homclass##c.middle50 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna middle50[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model5

qui meglm redist c.homclass##c.d10d1 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna d10d1[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model6

qui meglm redist c.homclass##c.giniindex i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna giniindex[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model7

qui meglm redist c.homclass##c.ttheilge1 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna ttheilge1[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model8

estimates restore model1
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est", replace

estimates restore model2
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est", append

estimates restore model3
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est", append

estimates restore model4
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est", append

estimates restore model5
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est", append

estimates restore model6
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est", append

estimates restore model7
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est", append

estimates restore model8
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est", append
}
*# Ordinal logit strong ties ---------------------------------------------------
{
clear all
use "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\input\data\proc\study1_country.dta"
*For the regression tables:
*ssc install estout, replace
*https://repec.sowi.unibe.ch/stata/estout/esttab.html
* check how to save estimations: https://www.radyakin.org/statalist/2015/20150702_1300997.do

gen logrgdpna = log(rgdpna)
gen age2 = agenum*agenum
replace homclass=homclass_s 
keep if country2 != "TWN" 
keep if country2 != "CHN"
keep if country2 != "JPN"

meglm redist c.homclass##c.gini_disp i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna gini_disp [pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model1

meglm redist c.homclass##c.gini_mkt i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna gini_mkt [pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model2

meglm redist c.homclass##c.palmaratio i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna palmaratio[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model3

meglm redist c.homclass##c.top10 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna top10[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model4

meglm redist c.homclass##c.middle50 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna middle50[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model5

meglm redist c.homclass##c.d10d1 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna d10d1[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model6

meglm redist c.homclass##c.giniindex i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna giniindex[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model7

meglm redist c.homclass##c.ttheilge1 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna ttheilge1[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model8

estimates restore model1
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est", replace

estimates restore model2
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est", append

estimates restore model3
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est", append

estimates restore model4
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est", append

estimates restore model5
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est", append

estimates restore model6
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est", append

estimates restore model7
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est", append

estimates restore model8
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est", append

}
*# Ordinal logit weak ties -----------------------------------------------------
 {
clear all
use "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\input\data\proc\study1_country.dta"
*For the regression tables:
*ssc install estout, replace
*https://repec.sowi.unibe.ch/stata/estout/esttab.html
* check how to save estimations: https://www.radyakin.org/statalist/2015/20150702_1300997.do

gen logrgdpna = log(rgdpna)
gen age2 = agenum*agenum
replace homclass=homclass_w
keep if country2 != "TWN" 
keep if country2 != "CHN"
keep if country2 != "JPN"

meglm redist c.homclass##c.gini_disp i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna gini_disp [pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model1

meglm redist c.homclass##c.gini_mkt i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna gini_mkt [pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model2

meglm redist c.homclass##c.palmaratio i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna palmaratio[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model3

meglm redist c.homclass##c.top10 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna top10[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model4

meglm redist c.homclass##c.middle50 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna middle50[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model5

meglm redist c.homclass##c.d10d1 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna d10d1[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model6

meglm redist c.homclass##c.giniindex i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna giniindex[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model7

meglm redist c.homclass##c.ttheilge1 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna ttheilge1[pweight=WEIGHT] || country2: homclass, family(ordinal)
estimates store model8

estimates restore model1
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est", replace

estimates restore model2
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est", append

estimates restore model3
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est", append

estimates restore model4
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est", append

estimates restore model5
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est", append

estimates restore model6
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est", append

estimates restore model7
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est", append

estimates restore model8
eret list
estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est", append
}
*# Ordinal logit R's class------------------------------------------------------
{
*clear all
*use "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\input\data\proc\study1_country.dta"
*For the regression tables:
*ssc install estout, replace
*https://repec.sowi.unibe.ch/stata/estout/esttab.html
* check how to save estimations: https://www.radyakin.org/statalist/2015/20150702_1300997.do

*gen logrgdpna = log(rgdpna)
*gen age2 = agenum*agenum
*replace homclass=homclass_res
*keep if country2 != "TWN" 
*keep if country2 != "CHN"
*keep if country2 != "JPN"
*meglm redist c.homclass##c.gini_disp i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna gini_disp [pweight=WEIGHT] || country2: homclass, family(ordinal)
*estimates store model1
*
*meglm redist c.homclass##c.gini_mkt i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna gini_mkt [pweight=WEIGHT] || country2: homclass, family(ordinal)
*estimates store model2
*
*meglm redist c.homclass##c.palmaratio i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna top10[pweight=WEIGHT] || country2: homclass, family(ordinal)
*estimates store model3
*
*meglm redist c.homclass##c.top10 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna top10[pweight=WEIGHT] || country2: homclass, family(ordinal)
*estimates store model4
*
*meglm redist c.homclass##c.middle50 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna middle50[pweight=WEIGHT] || country2: homclass, family(ordinal)
*estimates store model5
*
*meglm redist c.homclass##c.d10d1 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna d10d1[pweight=WEIGHT] || country2: homclass, family(ordinal)
*estimates store model6
*
*meglm redist c.homclass##c.giniindex i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna giniindex[pweight=WEIGHT] || country2: homclass, family(ordinal)
*estimates store model7
*
*meglm redist c.homclass##c.ttheilge1 i.class3 female agenum age2 edyears i.Q03pcm workst union logrgdpna ttheilge1[pweight=WEIGHT] || country2: homclass, family(ordinal)
*estimates store model8
*
*estimates restore model1
*eret list
*estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-respondent-macro.est", replace
*
*estimates restore model2
*eret list
*estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-respondent-macro.est", append
*
*estimates restore model3
*eret list
*estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-respondent-macro.est", append
*
*estimates restore model4
*eret list
*estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-respondent-macro.est", append
*
*estimates restore model5
*eret list
*estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-respondent-macro.est", append
*
*estimates restore model6
*eret list
*estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-respondent-macro.est", append
*
*estimates restore model7
*eret list
*estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-respondent-macro.est", append
*
*estimates restore model8
*eret list
*estimates save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-respondent-macro.est", append
}

*# Generate Tables--------------------------------------------------------------
clear all
eret list
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est,  number(1)
estimates store model1_f
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est,  number(2)
estimates store model2_f
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est,  number(3)
estimates store model3_f
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est,  number(4)
estimates store model4_f
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est,  number(5)
estimates store model5_f
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est,  number(6)
estimates store model6_f
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est,  number(7)
estimates store model7_f
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-macro.est,  number(8)
estimates store model8_f

estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est,  number(1)
estimates store model1_s
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est,  number(2)
estimates store model2_s
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est,  number(3)
estimates store model3_s
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est,  number(4)
estimates store model4_s
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est,  number(5)
estimates store model5_s
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est,  number(6)
estimates store model6_s
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est,  number(7)
estimates store model7_s
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-strong-macro.est,  number(8)
estimates store model8_s

estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est,  number(1)
estimates store model1_w
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est,  number(2)
estimates store model2_w
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est,  number(3)
estimates store model3_w
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est,  number(4)
estimates store model4_w
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est,  number(5)
estimates store model5_w
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est,  number(6)
estimates store model6_w
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est,  number(7)
estimates store model7_w
estimates use C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-weak-macro.est,  number(8)
estimates store model8_w


esttab /// 
 model1_f model2_f model3_f model4_f model5_f model6_f model7_f model8_f ///
 model1_s model2_s model3_s model4_s model5_s model6_s model7_s model8_s ///  
 model1_w model2_w model3_w model4_w model5_w model6_w model7_w model8_w /// 
 using C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\output\non-confusian-ologit-homclass-full-strong-weak-macro.html, replace ///
 title(Multilevel ordinal logit regression: full country sample cross-level interaction) ///
 legend label collabels(none) ///
 nodepvars ///
 cells(b(fmt(2)) t(fmt(2))) ///
 order(logrgdpna) ///
 drop(age2 1.class3 2.class3 3.class3 female agenum edyears workst union 1.Q03pcm 2.Q03pcm 3.Q03pcm 4.Q03pcm)
 
