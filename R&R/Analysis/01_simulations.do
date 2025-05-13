************************************************************
* SIMULATION LOOP
* This section repeats the simulation for varying numbers of countries
* It stores the estimated Gini effect and whether it was statistically significant
*************************************************************

*program drop mlsim3, clear
program define mlsim3, rclass
    version 11
    syntax [, obs(integer 20) ]
	
	drop _all
    set obs `obs'
    * Declare tempvars
    tempvar beta0 country2 egal gini_disp wstate2 loggdppercapita dclass3res_V_2 dclass3res_V_3 homclass3_V_res know_total agenum edyears female partner Q03pcm2 Q03pcm3 Q03pcm4 workst
    * Simulate level-2 (country-level) predictors
    gen gini_disp        = rnormal(-1.24e-15, 1)
    gen wstate2          = rnormal(-4.07e-17, 1)
    gen loggdppercapita  = rnormal(1.42e-16, 1)
    gen beta0 = 81.00665 + gini_disp * 0.6745096 + wstate2 * 2.98459 + loggdppercapita * (-5.010294) + rnormal(0, sqrt(59.72618))
    gen country2 = _n

    * Expand to individual-level (level-1)
    expand 1500

    * Simulate individual-level predictors
    gen homclass3_V_res = rnormal(0.3655077, 0.2325711)
    gen know_total      = rnormal(5.809112, 2.262096)
    gen agenum          = rnormal(48.57705, 10.36967)
    gen edyears         = rnormal(12.64277, 4.282257)
    gen dclass3res_V_2  = rbinomial(1, 0.2537073)
    gen dclass3res_V_3  = rbinomial(1, 0.324604)
    gen female          = rbinomial(1, 0.509781)
    gen partner         = rbinomial(1, 0.5745883)
    gen Q03pcm2         = rbinomial(1, 0.270619)
    gen Q03pcm3         = rbinomial(1, 0.2922635)
    gen Q03pcm4         = rbinomial(1, 0.2902194)
    gen workst          = rbinomial(1, 0.335868)

    * Generate dependent variable (egal_sim)
    gen egal = beta0 + ///
        dclass3res_V_2 * 1.535723 + ///
        dclass3res_V_3 * 6.171381 + ///
        homclass3_V_res * (-3.654196) + ///
        know_total * (-0.3017059) + ///
        agenum * (-0.7791011) + ///
        edyears * (-0.1190159) + ///
        female * 2.229523 + ///
        partner * (-1.058019) + ///
        Q03pcm2 * (-2.11517) + ///
        Q03pcm3 * (-4.518728) + ///
        Q03pcm4 * (-3.876539) + ///
        workst * (-2.169849) + ///
        rnormal(0, sqrt(487.071))

    * Run multilevel model
    mixed egal gini_disp wstate2 loggdppercapita dclass3res_V_2 dclass3res_V_3 ///
        homclass3_V_res know_total agenum edyears female partner ///
        Q03pcm2 Q03pcm3 Q03pcm4 workst || country2:, variance

    * Extract estimated coefficient and p-value for Gini
    matrix results = r(table)
    scalar pval = results[4, colnumb(results, "gini_disp")]
    return scalar pval = pval
    return scalar detected = (pval < 0.05)
    return scalar effect = _b[gini_disp]
end


clear
save power_sim_gini.dta, emptyok replace

set seed 7654321
foreach n in 90 {
    di "Running simulation for `n' countries..."

    simulate pval=r(pval) detected=r(detected) effect=r(effect), reps(1000): ///
        mlsim2, obs(`n')

    gen ncountries = `n'
    append using power_sim_gini.dta
    save power_sim_gini_90.dta, replace
}


*************************************************************
* PLOT: POWER AS A FUNCTION OF ESTIMATED EFFECT SIZE
* - Bins the estimated Gini effect sizes
* - Calculates proportion of significant results per bin
* - Plots power curves for each sample size
*************************************************************

use power_sim_gini.dta, clear
gen effect_bin = round(effect, 0.05)
collapse (mean) power=detected, by(effect_bin ncountries)

twoway ///
(line power effect_bin if ncountries==31, lcolor(gs6) lpattern(solid)) ///
(line power effect_bin if ncountries==31, lcolor(blue) lpattern(dash)), ///
legend(order(1 "31 countries (solid)" 2 "31 countries (dash)")) ///
title("Power to Detect Gini Effect (31 Countries)") ///
xtitle("Estimated Gini Coefficient") ///
ytitle("Power (Proportion Significant)")

