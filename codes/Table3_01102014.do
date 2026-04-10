***********************************************************************************

* File: Table3_01102014.do
* Author: Jon Petkun (jbpetkun@mit.edu)
* Date: January 10, 2014
* Paper: Greenstone & Hanna 2014, Environmental Regulations & Pollution in India
* Contact: Rema Hanna - rema_hanna@hks.harvard.edu or Michael Greenstone - mgreenst@mit.edu

/*
Objective: The objective of this program is to generate Table 3 of the paper. This program
	performs mean shift and trend break analysis of air pollution policies using both a 
	single-stage standard DiD approach as well as a two-stage event study and trend break
	approach. 


Structure:
0. Stata Setup
1. Data prep: Generate policy event-years on air dataset
2. Run single-stage and two-stage regressions 
	a. CAT policy
	b. SCAP policy
3. Clean-up

Specifications:
1. Single-stage approach (standard difference-in-differences regressions):
	a. Mean shift (includes post*treatment only)
	b. Mean shift (includes post*treatment and city-year time trends)
	c. Trendbreak (included post*treatment, city-year time trends, and post*treatment*time-trend)
2. Two-stage approach (event study followed by mean-shift or trend-break regressions)
	a. Mean shift: reg taub post*treatment [aw=1/tause]
	b. Mean shift w/ time trend: reg taub post*treatment timetrend [aw=1/tause]
	c. Trend break: reg taub post*treatment timetrend post*treatment*timetrend [1/tause]
*/

***********************************************************************************
***********************************************************************************
* 0. Setup
***********************************************************************************

* 0. Setup
clear all
set more off
* Les versions récentes de Stata gèrent la mémoire automatiquement, 
* mais on laisse ces lignes pour la compatibilité.
cap set mem 500m
cap set matsize 10000

cap log close _all

* ------------------------------------------------------------------------------
* GESTION DES CHEMINS AUTOMATIQUE
* ------------------------------------------------------------------------------
if "`c(username)'" == "gab" {
    global mypath "C:/Users/gab/Documents/GitHub/ Effect_of_environmental_regulation_on_Air_Pollution_and_Infant_Mortality/"
}
else {
    global mypath "C:/Users/NOM_DE_MATIS/Documents/GitHub/Effect_of_environmental_regulation_on_Air_Pollution_and_Infant_Mortality/"
}

cd "$mypath"

* Création des dossiers nécessaires (très important pour les exports outreg2)
cap mkdir "Log"
cap mkdir "Output"
cap mkdir "Output/T3"

log using "Log/Table3_01102014.txt", replace text name(logT3)
***********************************************************************************
* 1. Data prep: Generate policy event-years on air dataset
***********************************************************************************

* Supreme Court Action Plan and Catalytic Converter policies *
foreach pollutant in spm so2 no2 { 
	use "${mypath}data/combined.dta", clear
	egen city_id = group(city)
	bys state city: egen count = count(e_`pollutant'_mean)

	* Generate Supreme Court Action Plan taus
	cap drop temp*
	g temp = year if actionplan_sc == 1
	egen temp2 = min(temp), by(city_id) 
	egen temp3 = min(year), by(city_id) 
	g tauSC = year - temp2 if temp2 > temp3 & e_`pollutant'_mean<.
	g neveradoptSC = temp2 >= .
	replace tauSC = 0 if neveradoptSC == 1 & e_`pollutant'_mean < . // We code the non-adopting cities so that they are always in event year 0
	g tempx = 1 if actionplan_sc==1 & e_`pollutant'_mean<.		// the following 3 lines of code determine whether there are any cities which enacted the policy but are missing pollution data for ALL post-policy years
	egen tempy = min(tempx) if temp2<., by(city_id)
	ta tempy if temp2<., missing		// no cities fit this description, so no more changes need be made

	g temp4 = tauSC >= 3 & tauSC < .
	g temp5 = tauSC <= -3 & tauSC < .
	egen Mtemp4 = max(temp4), by(city_id) 
	egen Mtemp5 = max(temp5), by(city_id) 
	g useSC = (Mtemp4 == 1 & Mtemp5 == 1 & count > 1) | (neveradoptSC == 1 & count > 1)

	* Generate Catalytic Converter Policy taus
	cap drop Mtemp* temp*
	g temp = year if catconverter == 1
	egen temp2 = min(temp), by(city_id) 
	egen temp3 = min(year), by(city_id) 
	g tauCAT = year - temp2 if temp2 > temp3 & e_`pollutant'_mean<.
	g neveradoptCAT = temp2 >= .
	replace tauCAT = 0 if neveradoptCAT == 1 & e_`pollutant'_mean < . // We code the non-adopting cities so that they are always in event year 0
	g tempx = 1 if catconverter==1 & e_`pollutant'_mean<.
	egen tempy = min(tempx) if temp2<., by(city_id)
	ta tempy if temp2<., missing
	replace neveradoptCAT = 1 if tempy>=. & temp2<.
	replace tauCAT = . if tempy>=. & temp2<.

	* The following lines restrict the sample to only those cities with at an observation at least three years prior to and after
	*	policy adoption. We also want to estimate Eqns (1) and (2) with an unrestricted sample. For this reason, we'll save restricted
	*	and unrestricted versions of the dataset.
	g temp4 = tauCAT >= 3 & tauCAT < .
	g temp5 = tauCAT <= -3 & tauCAT < .
	egen Mtemp4 = max(temp4), by(city_id) 
	egen Mtemp5 = max(temp5), by(city_id)
	g useCAT = (Mtemp4 == 1 & Mtemp5 == 1 & count > 1) | (neveradoptCAT == 1 & count > 1)

	drop if useSC==0 | useCAT==0
	drop if e_`pollutant'_mean >= .
	ta tauSC
	ta tauCAT
	g one = 1

	* Generate individual event-year dummies for Supreme Court Action Plans
	forv tau = 7(-1)1 {
		g tauSCm`tau' = tauSC == -`tau'
		la var tauSCm`tau' "This obs is `tau' years before action plan began"
	}
	forv tau = 0/3 {
		g tauSC`tau' = tauSC == `tau'
		la var tauSC`tau' "This obs is `tau' years after action plan began"
	}
	g tauSCL = tauSC<-7
	g tauSCR = tauSC>3 & tauSC<.

	* Generate individual event-year dummies for Catalytic Converter Policy
	forv tau = 7(-1)1 {
		g tauCATm`tau' = tauCAT == -`tau'
		la var tauCATm`tau' "This obs is `tau' years before catalytic converters mandated"
	}
	forv tau = 0/9 {
		g tauCAT`tau' = tauCAT == `tau'
		la var tauCAT`tau' "This obs is `tau' years after catalytic converters mandated"
	}
	g tauCATL = tauCAT<-7
	g tauCATR = tauCAT>9 & tauCAT<.
	
	* Save pollutant specific dataset
	save "${mypath}data/combined_taus_`pollutant'_stddid.dta", replace	
}

***********************************************************************************
* 2. Run single-stage and two-stage regressions
***********************************************************************************

* Catalytic Converter policy *
local placeholder "replace" // we use this to control the outreg command
foreach pollutant in spm so2 no2 {
	use "${mypath}data/combined_taus_`pollutant'_stddid.dta", clear
* Single-Stage Regressions:
	gen scaprange = (tauSC >= -7 & tauSC <= 3) // the two-stage regressions only include SC event years between -7 & 3
	gen scap = (tauSC >= 0) & (neveradoptSC == 0)
	gen scappolicy = scap*scaprange
	gen scaptau = tauSC*scaprange
	gen scap_trend = scap*scaprange*tauSC
	replace scap_trend = scap_trend + 1 if (tauSC >= 0 & tauSC <= 3)
	gen catrange = (tauCAT >= -7 & tauCAT <= 9) // the two-stage regressions only include CAT event years between -7 & 9
	gen catconv = (tauCAT >= 0) & (neveradoptCAT == 0)
	gen catconvpolicy = catconv*catrange
	gen catconvtau = tauCAT*catrange
	gen catconv_trend = catconv*catrange*tauCAT
	replace catconv_trend = catconv_trend + 1 if (tauCAT >= 0 & tauCAT <= 9)

*	Mean shift (includes post*policy plus the usual controls)
	xi: reg e_`pollutant'_mean scaprange scappolicy tauSCL tauSCR catrange catconvpolicy tauCATL tauCATR lit_urban mean i.city i.year [aw = pop_urban], cluster(city)
		outreg2 catconvpolicy using "${mypath}Output/T3/Table3-CAT-`pollutant'.xls", dec(2) se par ctitle("Eqn 3a") aster(coef) replace
*	Mean shift (includes post*policy and city-year time trend, plus the usual controls)
	xi: reg e_`pollutant'_mean scaprange scappolicy tauSCL tauSCR scaptau catrange catconvpolicy tauCATL tauCATR catconvtau lit_urban mean i.city i.year [aw = pop_urban], cluster(city)
		outreg2 catconvpolicy catconvtau using "${mypath}Output/T3/Table3-CAT-`pollutant'.xls", dec(2) se par ctitle("Eqn 3b") aster(coef) append
*	Trendbreak (includes post*treated, city-year time trend, and post*treated*time-trend, plus the usual controls)
	xi: reg e_`pollutant'_mean scaprange scappolicy tauSCL tauSCR scaptau scap_trend catrange catconvpolicy tauCATL tauCATR catconvtau catconv_trend lit_urban mean i.city i.year [aw = pop_urban], cluster(city)
		lincom catconvpolicy + 5*catconv_trend
		local coef = r(estimate)
		local coef = round(`coef', .01)
		local coef = "`coef'"
		if length("`coef'") < 3 {
			local coef = "`coef'" + "0"
		}
		local coef = substr("`coef'", 1, 6)
		local se = r(se)
		local tstat = r(estimate)/r(se)
		local pval = tprob(r(df), abs(`tstat'))
		local pval = round(`pval', .01)
		if `pval' <= .01 {
			local coef = "`coef'" + "***"
		}
		if `pval' > .01 & `pval' <= .05 {
			local coef = "`coef'" + "**"
		}
		if `pval' > .05 & `pval' <= .1 {
			local coef = "`coef'" + "*"
		}
		if length("`pval'") < 3 {
			local pval = "`pval'" + "0"
		}
		outreg2 catconvpolicy catconvtau catconv_trend using "${mypath}Output/T3/Table3-CAT-`pollutant'.xls", dec(2) se par ctitle("Eqn 3c") aster(coef) addtext("5-Year Effect", "`coef'", "p-value", "[`pval']") append
		
* Two-Stage Regressions:
	* Recall that, for the purposes of the single-stage regressions, control cities were coded as event year 0. Revert them to 
	*	event year missing.
	replace tauCAT = . if neveradoptCAT == 1 & e_`pollutant'_mean < .
	replace tauSC = . if neveradoptSC == 1 & e_`pollutant'_mean < .
	replace tauCAT0 = 0 if neveradoptCAT == 1 & e_`pollutant'_mean < .
	replace tauSC0 = 0 if neveradoptSC == 1 & e_`pollutant'_mean < .	
*	First stage (Event study)
	qui xi: reg e_`pollutant'_mean tauSCm7-tauSCm1 tauSC0-tauSC3 tauSCL tauSCR tauCATm7-tauCATm1 tauCAT0-tauCAT9 tauCATL tauCATR lit_urban mean i.city i.year [aw = pop_urban]

	* Prepare sigmat-hats
	gen taub = .
	gen tause = .
	collapse (mean) taub tause tauCATm7-tauCAT9, by(tauCAT)
	drop if tauCAT > 9 | tauCAT < -7
	foreach k of varlist tauCATm7-tauCATm1 tauCAT0-tauCAT9 {
		replace taub = _b[`k'] if `k' == 1
		replace tause = _se[`k'] if `k' == 1
	}
	drop tauCATm7-tauCAT9
	gen catconv = (tauCAT >= 0)
	gen tau_trend = tauCAT + 8
	gen catconv_trend = catconv*tauCAT
	replace catconv_trend = catconv_trend + 1 if tauCAT >= 0
	
* Save the dataset of sigma-hats (for use with breakpoint tests)
	save "${mypath}data/sigmas_`pollutant'.dta", replace		
	
*	Second stage (mean shift and trendbreak regressions)	
	* Trendbreak regressions - Eqn (2a)
	display "Outcome: `pollutant'; Policy: Catalytic Converters; Sample: all city*years"
	regress taub catconv [aw = 1/tause]
		outreg2 catconv using "${mypath}Output/T3/Table3-CAT-`pollutant'.xls", dec(2) se par ctitle("Eqn 2a") aster(coef) append	

	* Trendbreak regressions - Eqn (2b)
	reg taub tauCAT catconv [aw=1/tause]
		outreg2 catconv tauCAT using "${mypath}Output/T3/Table3-CAT-`pollutant'.xls", dec(2) se par ctitle("Eqn 2b") aster(coef) append
	
	* Trendbreak regressions - Eqn (2c)
	reg taub tauCAT catconv catconv_trend [aw=1/tause]
		lincom catconv + 5*catconv_trend
			local coef = r(estimate)
			local coef = round(`coef', .01)
			local coef = "`coef'"
			if length("`coef'") < 3 {
				local coef = "`coef'" + "0"
			}
			local coef = substr("`coef'", 1, 6)
			local tstat = r(estimate)/r(se)
			local pval = tprob(r(df), abs(`tstat'))
			local pval = round(`pval', .01)
			if `pval' <= .01 {
				local coef = "`coef'" + "***"
			}
			if `pval' > .01 & `pval' <= .05 {
				local coef = "`coef'" + "**"
			}
			if `pval' > .05 & `pval' <= .1 {
				local coef = "`coef'" + "*"
			}
			if length("`pval'") < 3 {
				local pval = "`pval'" + "0"
			}
			outreg2 catconv tauCAT catconv_trend using "${mypath}Output/T3/Table3-CAT-`pollutant'.xls", dec(2) se par ctitle("Eqn 2c") aster(coef) addtext("5-Year Effect", "`coef'", "p-value", "[`pval']") append
}
*/
*SCAP policy *
local placeholder "replace" // we use this to control the outreg command
foreach pollutant in spm so2 no2 {
	use "${mypath}data/combined_taus_`pollutant'_stddid.dta", clear
* Single-Stage Regressions:
	gen scaprange = (tauSC >= -7 & tauSC <= 3) // the two-stage regressions only include SC event years between -7 & 3
	gen scap = (tauSC >= 0) & (neveradoptSC == 0)
	gen scappolicy = scap*scaprange
	gen scaptau = tauSC*scaprange
	gen scap_trend = scap*scaprange*tauSC
	replace scap_trend = scap_trend + 1 if (tauSC >= 0 & tauSC <= 3)
	gen catrange = (tauCAT >= -7 & tauCAT <= 9) // the two-stage regressions only include CAT event years between -7 & 9
	gen catconv = (tauCAT >= 0) & (neveradoptCAT == 0)
	gen catconvpolicy = catconv*catrange
	gen catconvtau = tauCAT*catrange
	gen catconv_trend = catconv*catrange*tauCAT
	replace catconv_trend = catconv_trend + 1 if (tauCAT >= 0 & tauCAT <= 9)

*	Mean shift (includes post*policy plus the usual controls)
	xi: reg e_`pollutant'_mean scaprange scappolicy tauSCL tauSCR catrange catconvpolicy tauCATL tauCATR lit_urban mean i.city i.year [aw = pop_urban], cluster(city)
		outreg2 scappolicy using "${mypath}Output/T3/Table3-SCAP-`pollutant'.xls", dec(2) se par ctitle("Eqn 3a") aster(coef) replace
*	Mean shift (includes post*policy and city-year time trend, plus the usual controls)
	xi: reg e_`pollutant'_mean scaprange scappolicy tauSCL tauSCR scaptau catrange catconvpolicy tauCATL tauCATR catconvtau lit_urban mean i.city i.year [aw = pop_urban], cluster(city)
		outreg2 scappolicy scaptau using "${mypath}Output/T3/Table3-SCAP-`pollutant'.xls", dec(2) se par ctitle("Eqn 3b") aster(coef) append
*	Trendbreak (includes post*treated, city-year time trend, and post*treated*time-trend, plus the usual controls)
	xi: reg e_`pollutant'_mean scaprange scappolicy tauSCL tauSCR scaptau scap_trend catrange catconvpolicy tauCATL tauCATR catconvtau catconv_trend lit_urban mean i.city i.year [aw = pop_urban], cluster(city)	
		lincom scappolicy + 5*scap_trend
		local coef = r(estimate)
		local coef = round(`coef', .01)
		local coef = "`coef'"
		if length("`coef'") < 3 {
			local coef = "`coef'" + "0"
		}
		local coef = substr("`coef'", 1, 6)
		local se = r(se)
		local tstat = r(estimate)/r(se)
		local pval = tprob(r(df), abs(`tstat'))
		local pval = round(`pval', .01)
		if `pval' <= .01 {
			local coef = "`coef'" + "***"
		}
		if `pval' > .01 & `pval' <= .05 {
			local coef = "`coef'" + "**"
		}
		if `pval' > .05 & `pval' <= .1 {
			local coef = "`coef'" + "*"
		}
		if length("`pval'") < 3 {
			local pval = "`pval'" + "0"
		}
		outreg2 scappolicy scaptau scap_trend using "${mypath}Output/T3/Table3-SCAP-`pollutant'.xls", dec(2) se par ctitle("Eqn 3c") aster(coef) addtext("5-Year Effect", "`coef'", "p-value", "[`pval']") append
		
* Two-Stage Regressions:
	* Recall that, for the purposes of the single-stage regressions, control cities were coded as event year 0. Revert them to 
	*	event year missing.
	replace tauCAT = . if neveradoptCAT == 1 & e_`pollutant'_mean < .
	replace tauSC = . if neveradoptSC == 1 & e_`pollutant'_mean < .
	replace tauCAT0 = 0 if neveradoptCAT == 1 & e_`pollutant'_mean < .
	replace tauSC0 = 0 if neveradoptSC == 1 & e_`pollutant'_mean < .	
*	First stage (Event study)
	qui xi: reg e_`pollutant'_mean tauSCm7-tauSCm1 tauSC0-tauSC3 tauSCL tauSCR tauCATm7-tauCATm1 tauCAT0-tauCAT9 tauCATL tauCATR lit_urban mean i.city i.year [aw = pop_urban]

	* Prepare sigmat-hats
	gen taub = .
	gen tause = .
	collapse (mean) taub tause tauSCm7-tauSC3, by(tauSC)
	drop if tauSC > 3 | tauSC < -7
	foreach k of varlist tauSCm7-tauSCm1 tauSC0-tauSC3 {
		replace taub = _b[`k'] if `k' == 1
		replace tause = _se[`k'] if `k' == 1
	}
	drop tauSCm7-tauSC3
	gen scap = (tauSC >= 0)
	gen tau_trend = tauSC + 8
	gen scap_trend = scap*tauSC
	replace scap_trend = scap_trend + 1 if tauSC >= 0
	
*	Second stage (mean shift and trendbreak regressions)	
	* Trendbreak regressions - Eqn (2a)
	display "Outcome: `pollutant'; Policy: SCAP; Sample: all city*years"
	regress taub scap [aw = 1/tause] 
		outreg2 scap using "${mypath}Output/T3/Table3-SCAP-`pollutant'.xls", dec(2) se par ctitle("Eqn 2a") aster(coef) append	

	* Trendbreak regressions - Eqn (2b)
	reg taub tauSC scap [aw=1/tause]
		outreg2 scap tauSC using "${mypath}Output/T3/Table3-SCAP-`pollutant'.xls", dec(2) se par ctitle("Eqn 2b") aster(coef) append
	
	* Trendbreak regressions - Eqn (2c)
	reg taub tauSC scap scap_trend [aw=1/tause]
		lincom scap + 5*scap_trend
			local coef = r(estimate)
			local coef = round(`coef', .01)
			local coef = "`coef'"
			if length("`coef'") < 3 {
				local coef = "`coef'" + "0"
			}
			local coef = substr("`coef'", 1, 6)
			local tstat = r(estimate)/r(se)
			local pval = tprob(r(df), abs(`tstat'))
			local pval = round(`pval', .01)
			if `pval' <= .01 {
				local coef = "`coef'" + "***"
			}
			if `pval' > .01 & `pval' <= .05 {
				local coef = "`coef'" + "**"
			}
			if `pval' > .05 & `pval' <= .1 {
				local coef = "`coef'" + "*"
			}
			if length("`pval'") < 3 {
				local pval = "`pval'" + "0"
			}
			outreg2 scap tauSC scap_trend using "${mypath}Output/T3/Table3-SCAP-`pollutant'.xls", dec(2) se par ctitle("Eqn 2c") aster(coef) addtext("5-Year Effect", "`coef'", "p-value", "[`pval']") append
}

***********************************************************************************
* 3. Clean-up
***********************************************************************************

* Effacer les fichiers temporaires pour ne pas encombrer le GitHub
foreach pollutant in spm so2 no2 {
	cap erase "Data/Air Data/Final Data/combined_taus_`pollutant'_stddid.dta"
}

cap log close logT3