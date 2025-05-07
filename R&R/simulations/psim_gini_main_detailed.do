
*************************************************************
* Full Simulation-Based Power Analysis for Gini Effect
* Author: [Your Name]
* Language: English (US)
* Purpose:
*   - Simulate a dependent variable (egal_sim) with a known true effect of Gini
*   - Include macro-level controls: GDP and welfare state size
*   - Run mixed-effects models across multiple country sample sizes
*   - Visualize power as a function of the estimated effect size and sample size
*************************************************************


* Load the dataset with real structure
*use df1_forsim.dta, clear
*mixed egal gini_disp wstate2 gdp i.dclass3res_V homclass3_V_res  ///
 *       know_total c.agenum c.edyears ///
 *       i.female i.partner i.Q03pcm i.workst ///
 *      [pweight=WEIGHT] ///
 *      || country2:, variance
		
*---------------------------------------------------------------------------------------------- 
*                             |               Robust
*                        egal |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
*-----------------------------+----------------------------------------------------------------
*                   gini_disp |   .6745096   1.658845     0.41   0.684    -2.576766    3.925786
*                     wstate2 |    2.98459   1.677851     1.78   0.075    -.3039387    6.273118
*                gdppercapita |    -.00031   .0001081    -2.87   0.004    -.0005219   -.0000981
*                             |
*                dclass3res_V |
*Intermediate class (III+IV)  |   1.535723   .4454677     3.45   0.001     .6626219    2.408823
*   Working Class (V+VI+VII)  |   3.171381   .6079215     5.22   0.000     1.979876    4.362885
*                             |
*             homclass3_V_res |  -.6514996   .8472649    -0.77   0.442    -2.312108    1.009109
*                  know_total |  -.3017059   .0807686    -3.74   0.000    -.4600094   -.1434024
*                      agenum |   .0779101   .0230404     3.38   0.001     .0327518    .1230685
*                     edyears |  -.1190159   .0662896    -1.80   0.073    -.2489412    .0109094
*                             |
*                      female |
*                     Female  |   2.229523   .4572419     4.88   0.000     1.333345      3.1257
*                             |
*                     partner |
*              Has a partner  |  -1.058019   .4369476    -2.42   0.015     -1.91442   -.2016174
*                             |
*                      Q03pcm |
*                        T02  |   -2.11517   .4762885    -4.44   0.000    -3.048679   -1.181662
*                        T03  |  -4.518728   .7626824    -5.92   0.000    -6.013559   -3.023898
*                    Missing  |  -3.876539    .654268    -5.93   0.000    -5.158881   -2.594197
*                             |
*                      workst |
*           Not in paid work  |  -.2169849   .3881611    -0.56   0.576    -.9777666    .5437968
*                       _cons |   81.00665   4.688275    17.28   0.000      71.8178     90.1955
*----------------------------------------------------------------------------------------------

		
*--- INITIAL SETUP ---*
clear all
set more off
set seed 12345

*************************************************************
* DEFINE SIMULATION PROGRAM: psim_gini_main
* This program performs one replication of the following:
*   1. Draws a random sample of countries
*   2. Simulates a dependent variable with a true Gini effect of 0.67
*   3. Estimates a mixed-effects model including macro and micro controls
*   4. Returns the estimated effect size, p-value, and significance indicator
*************************************************************

program define mlsim2, rclass
    version 11
    syntax [, obs(integer 20) ]

    drop _all
    set obs `obs'

    * Declare tempvars solo para las que no usas en el modelo
    tempvar beta0 country2 egal

    * Simular predictores a nivel país (nivel 2) con nombres fijos
    gen gini_disp        = rnormal(-1.24e-15, 1)
    gen wstate2          = rnormal(-4.07e-17, 1)
    gen loggdppercapita  = rnormal(1.42e-16, 1)
    gen `beta0' = 81.00665 + gini_disp * 0.6745096 + wstate2 * 2.98459 + ///
                  loggdppercapita * (-5.010294) + rnormal(0, sqrt(59.72618))
    gen `country2' = _n

    * Expandir a nivel individual (nivel 1)
    expand 1500

    * Simular predictores individuales con nombres fijos
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

    * Generar variable dependiente
    gen egal = `beta0 + ///
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

    * Modelo multinivel
    mixed egal gini_disp wstate2 loggdppercapita dclass3res_V_2 dclass3res_V_3 ///
        homclass3_V_res know_total agenum edyears female partner ///
        Q03pcm2 Q03pcm3 Q03pcm4 workst || country2:

    * Extract estimated coefficient and p-value for Gini
    matrix results = r(table)
    scalar pval = results[4, colnumb(results, "gini_disp")]
    return scalar pval = pval
    return scalar detected = (pval < 0.05)
    return scalar effect = _b[gini_disp]
end

cap program drop psim_gini_main
program define psim_gini_main, rclass
    version 15.0

    syntax , NCOUNTRIES(integer)

    * Load the dataset with real structure
    use df1_forsim.dta, clear

    * Keep only variables needed for simulation and modeling
    keep homclass3_V_res dclass3res_V gini_disp gdp wstate2 country2 ///
         know_total female agenum partner edyears Q03pcm works WEIGHT

    * Randomly select N countries to simulate variation in macro sample size
    preserve
    duplicates drop country2, force
    gen r = runiform()
    sort r
    keep if _n <= `ncountries'
    tempfile selected
    save `selected'
    restore

    merge m:1 country2 using `selected', keep(match) nogenerate

    * Recode categorical variables for use in the model
    capture confirm string variable dclass3res_V
    if !_rc {
        encode dclass3res_V, gen(class)
    }
    else {
        gen class = dclass3res_V
    }

    capture confirm string variable Q03pcm
    if !_rc {
        encode Q03pcm, gen(income_tertiles)
    }
    else {
        gen income_tertiles = Q03pcm
    }

    capture confirm string variable female
    if !_rc {
        encode female, gen(female_recoded)
    }
    else {
        gen female_recoded = female
    }

    capture confirm string variable partner
    if !_rc {
        encode partner, gen(partner_recoded)
    }
    else {
        gen partner_recoded = partner
    }

    capture confirm string variable works
    if !_rc {
        encode works, gen(works_recoded)
    }
    else {
        gen works_recoded = works
    }

    * Simulate dependent variable (egal_sim) with known effect of Gini = 0.84
    * Includes macro controls and micro-level variation
    gen egal_sim = 2 + 0.64*gini_disp + rnormal(0, 1)

    * Estimate the mixed-effects model with random intercept for country
    mixed egal_sim gini_disp wstate2 gdp ///
        c.know_total c.agenum c.edyears i.class c.homclass3_V_res ///
        i.female_recoded i.partner_recoded i.income_tertiles i.works_recoded ///
        [pweight=WEIGHT] ///
        || country2:, variance

    * Extract estimated coefficient and p-value for Gini
    matrix results = r(table)
    scalar pval = results[4, colnumb(results, "gini_disp")]
    return scalar pval = pval
    return scalar detected = (pval < 0.05)
    return scalar effect = _b[gini_disp]
end

*************************************************************
* SIMULATION LOOP
* This section repeats the simulation for varying numbers of countries
* It stores the estimated Gini effect and whether it was statistically significant
*************************************************************

clear
save power_sim_gini.dta, emptyok replace

foreach n in 31 60 {
    di "Running simulation for `n' countries..."

    simulate pval=r(pval) detected=r(detected) effect=r(effect), reps(4): ///
        psim_gini_main, ncountries(`n')

    gen ncountries = `n'
    append using power_sim_gini.dta
    save power_sim_gini.dta, replace
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
*(line power effect_bin if ncountries==31, lcolor(gs6) lpattern(solid)) ///
(line power effect_bin if ncountries==60, lcolor(blue) lpattern(dash)) ///
*(line power effect_bin if ncountries==90, lcolor(red) lpattern(shortdash)) ///
(line power effect_bin if ncountries==120, lcolor(green) lpattern(dot)), ///
*legend(order(1 "31 countries" 2 "60 countries" 3 "90 countries" 4 "120 countries")) ///
title("Power to Detect Gini Effect by Estimated Effect Size and Sample Size") ///
xtitle("Estimated Gini Coefficient") ///
ytitle("Power (Proportion Significant)") ///
ylabel(0(.1)1)

* Export the graph in PDF format for compatibility
graph export "power_gini_effect_simulated.pdf", replace
