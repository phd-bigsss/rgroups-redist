
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

clear
* Load the dataset with real structure
use df1_forsim.dta, clear

tabulate dclass3res_V, generate(dclass3res_V_)
tabulate Q03pcm, generate(Q03pcm)
sum egal gini_disp wstate2 loggdppercapita  dclass3res_V_2 dclass3res_V_3 homclass3_V_res  ///
        know_total c.agenum c.edyears ///
        i.female i.partner Q03pcm2 Q03pcm3 Q03pcm4 i.workst

mixed egal gini_disp wstate2 loggdppercapita  dclass3res_V_2 dclass3res_V_3 homclass3_V_res  ///
        know_total c.agenum c.edyears ///
        i.female i.partner Q03pcm2 Q03pcm3 Q03pcm4 i.workst ///
       [pweight=WEIGHT] ///
       || country2: 
	   
	   
                                   (Std. Err. adjusted for 31 clusters in country2)
-----------------------------------------------------------------------------------
                  |               Robust
             egal |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
------------------+----------------------------------------------------------------
        gini_disp |   .6745096   1.658845     0.41   0.684    -2.576766    3.925786
          wstate2 |    2.98459   1.677851     1.78   0.075    -.3039387    6.273118
  loggdppercapita |  -5.010294   1.747275    -2.87   0.004     -8.43489   -1.585697
   dclass3res_V_2 |   1.535723   .4454677     3.45   0.001     .6626219    2.408823
   dclass3res_V_3 |   3.171381   .6079215     5.22   0.000     1.979876    4.362885
  homclass3_V_res |  -.6514996   .8472649    -0.77   0.442    -2.312108    1.009109
       know_total |  -.3017059   .0807686    -3.74   0.000    -.4600094   -.1434024
           agenum |   .0779101   .0230404     3.38   0.001     .0327518    .1230685
          edyears |  -.1190159   .0662896    -1.80   0.073    -.2489412    .0109094
                  |
           female |
          Female  |   2.229523   .4572419     4.88   0.000     1.333345      3.1257
                  |
          partner |
   Has a partner  |  -1.058019   .4369476    -2.42   0.015     -1.91442   -.2016174
          Q03pcm2 |   -2.11517   .4762885    -4.44   0.000    -3.048679   -1.181662
          Q03pcm3 |  -4.518728   .7626824    -5.92   0.000    -6.013559   -3.023898
          Q03pcm4 |  -3.876539    .654268    -5.93   0.000    -5.158881   -2.594197
                  |
           workst |
Not in paid work  |  -.2169849   .3881611    -0.56   0.576    -.9777666    .5437968
            _cons |   69.61476    2.06962    33.64   0.000     65.55838    73.67114
-----------------------------------------------------------------------------------

------------------------------------------------------------------------------
                             |               Robust           
  Random-effects Parameters  |   Estimate   Std. Err.     [95% Conf. Interval]
-----------------------------+------------------------------------------------
country2: Identity           |
                  var(_cons) |   59.72617   14.54408      37.05837    96.25938
-----------------------------+------------------------------------------------
               var(Residual) |    487.071   16.92333      455.0061    521.3956
------------------------------------------------------------------------------




clear
set seed 987654
drop _all
set obs 240
gen gini_disp    = rnormal(-1.24e-15, 1)
gen wstate2      = rnormal(-4.07e-17, 1)
gen loggdppercapita = rnormal(1.42e-16, 1)


/* level-2 model */
generate beta0=81.00665+gini_disp*.6745096+ wstate2*2.98459 +loggdppercap*-5.010294 + rnormal(0,sqrt(59.72618))
generate country2=_n  /* create an id variable */
sum beta0
 
reg beta0 gini_disp wstate2 loggdppercap


/* level-1 model */
sort country2
expand 1500


gen homclass3_V_res = rnormal(.3655077, .2325711)
gen know_total   = rnormal(5.809112, 2.262096)
gen agenum       = rnormal(48.57705, 10.36967)
gen edyears      = rnormal(12.64277, 4.282257)

gen dclass3res_V_2 = rbinomial(1,.2537073)
gen dclass3res_V_3= rbinomial(1,.324604)
gen female       = rbinomial(1,.509781)
gen partner      = rbinomial(1,.5745883)
gen Q03pcm2      = rbinomial(1,.270619)
gen Q03pcm3      = rbinomial(1,.2922635)
gen Q03pcm4      = rbinomial(1,.2902194)
gen workst       = rbinomial(1,.335868)


gen egal = beta0 + dclass3res_V_2  * 1.535723 ///
+ dclass3res_V_3  * 6.171381 /// 
+ homclass3_V_res * -3.654196 ///
+ know_total      * -0.3017059 ///
+ agenum          * -0.7791011 ///
+ edyears         * -0.1190159 ///
+ female          * 2.229523 ///
+ partner         * -1.058019 ///
+ Q03pcm2         * -2.11517 ///
+ Q03pcm3         * -4.518728 ///
+ Q03pcm4         * -3.876539 ///
+ workst          * -2.169849 ///
+ rnormal(0,sqrt(487.071))


mixed egal gini_disp wstate2 loggdppercapita  dclass3res_V_2 dclass3res_V_3 homclass3_V_res  ///
        know_total c.agenum c.edyears ///
        i.female i.partner Q03pcm2 Q03pcm3 Q03pcm4 i.workst ///
       || country2: 
	   

set seed 7654321
simulate pv=r(pv) tv=r(t), reps(1): mlsim, obs(20)

************************************************************
* SIMULATION LOOP
* This section repeats the simulation for varying numbers of countries
* It stores the estimated Gini effect and whether it was statistically significant
*************************************************************

program drop mlsim3
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
foreach n in 31 {
    di "Running simulation for `n' countries..."

    simulate pval=r(pval) detected=r(detected) effect=r(effect), reps(3): ///
        mlsim2, obs(`n')

    gen ncountries = `n'
    append using power_sim_gini.dta
    save power_sim_gini.dta, replace
}


