/*----------------------------------------------------------------------------


Objective: Harmoniza the cadastre



--------------------------------------------------------------------------*/

* paths

global data "C:/Users/cdelo/Dropbox/Mining_HK_Chile/EDUCACION/Estimation/Data/Catastro/harm2"

/*------------------------------------------------------------------------------
			TEMPORAL VARS Expltación Constiuida...

Por el momento solo lo hacemos con explotación y constituidas...
------------------------------------------------------------------------------*/

* Rename vars to identify year....

foreach x in 32 83{
	foreach z in l_c c_c{
		use "${data}/explotacion19`x'`z'.dta", clear
		destring year, replace
		foreach y of varlist _all {
			rename `y' `y'_`x'
		} 
		rename year_`x' year 
		capture rename cod_comuna_`x' cod_comuna
		capture rename cod_funtionalarea_`x' cod_funtionalarea
		sa, replace
	}
}


* Merge 83 and 32 for L and C.
	
use "${data}/explotacion1983l_c.dta"
merge 1:1 year cod_funtionalarea using "${data}/explotacion1932l_c.dta" , gen(merc)
rename 	cod_funtionalarea id // it's going to make things easier for the stocks loop.
sa "${data}/l_c.dta", replace

use "${data}/explotacion1983c_c.dta"
merge 1:1 year cod_comuna using "${data}/explotacion1932c_c.dta", gen(merc)
rename 	cod_comuna id // it's going to make things easier for the stocks loop.
sa "${data}/c_c.dta", replace


* create an aggregated measure of 32 and 83....

foreach z in c_c l_c{
	use "${data}/`z'.dta"
	foreach y in per_cam per_ori num area_cam area_ori{
		recode `y'_`z'_32 (.=0)
		recode `y'_`z'_83 (.=0)
		drop if year==.
		gen `y'_`z'_all=`y'_`z'_32+`y'_`z'_83	
	}
	sa, replace
}

* Create "Stock" Variables


foreach z in c_c l_c{
	
	use "${data}/`z'.dta"

	fillin id year // to make a balanced panel

	unique year
	local tope=r(unique)

	foreach y in area_cam area_ori num per_cam per_ori {
		foreach x in all 32 83{
			gen `y'_`z'_`x'_accum=.

			forvalues n = 1(1)`tope' {
				sort id year
				cap drop i`n'
				bys id: gen i`n' = 1 if  year[`n']>=year
				bys id: egen var`n' = total(`y'_`z'_`x') if i`n'==1
				bys id: replace `y'_`z'_`x'_accum = var`n' if mi(`y'_`z'_`x'_accum)
				drop  i`n'
				drop var`n'
			}
		}
	}
	capture bys id:  egen cod_funtionalarea_new=max(cod_funtionalarea)
	sa, replace
}




* Merge comunas with labor markets



use "${data}/l_c.dta", clear
rename id cod_funtionalarea_new
merge 1:m year cod_funtionalarea_new using "${data}/c_c.dta", gen(mer_lc)
keep if mer_lc==3
drop cod_funtionalarea
rename id cod_comuna
rename cod_funtionalarea_new cod_funtionalarea
sa "${data}/all_explotacionconstituida.dta", replace



* Label Variables

foreach x in all 83 32{


	label var per_cam_l_c_`x' "% of area, calculated by CDR, of market with minas const in year" 
	label var per_cam_c_c_`x' "% of area, calculated by CDR, of comuna with minas const in year" 
	
	label var per_ori_l_c_`x' "% of area, original, of market with minas const in year" 
	label var per_ori_c_c_`x' "% of area, original, of comuna with minas const in year" 
	
	label var area_cam_l_c_`x' "area, calculated by CDR, in market of minas const in year" 
	label var area_cam_c_c_`x' "area, calculated by CDR, in comuna of minas const in year" 
	
	label var area_ori_l_c_`x' "area, original, in labor market of minas const in year" 
	label var area_ori_c_c_`x' "area, original, in comuna of minas const in year" 
	
	
	
	label var per_cam_l_c_`x'_accum "% of area, calculated by CDR, of market with minas const accum until year" 
	label var per_cam_c_c_`x'_accum "% of area, calculated by CDR, of comuna with minas const accum until year" 
	
	label var per_ori_l_c_`x'_accum "% of area, original, of market with minas const accum until year" 
	label var per_ori_c_c_`x'_accum "% of area, original, of comuna with minas const accum until year" 
	
	label var area_cam_l_c_`x'_accum "area, calculated by CDR, in market of minas const accum until year" 
	label var area_cam_c_c_`x'_accum "area, calculated by CDR, in comuna of minas const accum until year" 
	
	label var area_ori_l_c_`x'_accum "area, original, in market of minas const accum until year" 
	label var area_ori_c_c_`x'_accum "area, original, in comuna of minas const accum until year" 

}

sa, replace





/*------------------------------------------------------------------------------
						
										STATIC VARS

------------------------------------------------------------------------------*/


* Rename vars to identify year... I only need to do it for exploracion as the explotacion is ready 

foreach z in l_c c_c l_t c_t{
	use "${data}/exploracion`z'.dta", clear
	destring year, replace
	foreach y of varlist _all {
		rename `y' `y'_ex
	} 
	rename year_`x' year 
	capture rename cod_comuna_`x' cod_comuna
	capture rename cod_funtionalarea_`x' cod_funtionalarea
	sa, replace
}

* collapse.... 

foreach x in c_c c_t{
	use "${data}/exploracion`x'.dta", clear
	drop year
	capture drop cod_region*
	ds cod_comuna cod_funtionalarea, not
	collapse (sum) `r(varlist)', by(cod_comuna cod_funtionalarea)
	foreach j in `r(varlist)'{
		recode `j'(.=0)
	}
	sa "${data}/coll_exploracion`x'.dta", replace
}

foreach x in l_c l_t {
	use "${data}/exploracion`x'.dta", clear
	drop year
	ds cod_funtionalarea, not
	collapse (sum) `r(varlist)', by(cod_funtionalarea)
	foreach j in `r(varlist)'{
		recode `j'(.=0)
	}
	sa "${data}/coll_exploracion`x'.dta", replace
}



/*------------------------------------------------------------------------------

The same but for explotacion fot the ones that do not have it still... 

------------------------------------------------------------------------------*/

foreach x in 32 83{
	foreach z in l_t c_t{
		use "${data}/explotacion19`x'`z'.dta", clear
		destring year, replace
		foreach y of varlist _all {
			rename `y' `y'_`x'
		} 
		rename year_`x' year 
		capture rename cod_comuna_`x' cod_comuna
		capture rename cod_funtionalarea_`x' cod_funtionalarea
		sa, replace
	}
}

* collapse.... 
foreach x in 32 83{
	foreach z in l_c l_t {
		use "${data}/explotacion19`x'`z'.dta", clear
		drop year
		ds cod_funtionalarea, not
		collapse (sum) `r(varlist)', by(cod_funtionalarea)
		foreach j in `r(varlist)'{
			recode `j'(.=0)
		}
		sa "${data}/coll_expl19`x'`z'.dta", replace
	}
}


foreach x in 32 83{
	foreach z in c_c c_t{
		use "${data}/explotacion19`x'`z'.dta", clear
		drop year
		capture drop cod_region*
		ds cod_comuna cod_funtionalarea, not
		collapse (sum) `r(varlist)', by(cod_comuna cod_funtionalarea)
		foreach j in `r(varlist)'{
			recode `j'(.=0)
		}
		sa "${data}/coll_expl19`x'`z'.dta", replace
	}
}





/*------------------------------------------------------------------------------

Merge for labor markets and Comunas separetly

------------------------------------------------------------------------------*/



use "${data}/coll_expl1932l_c.dta", clear
merge 1:1 cod_funtionalarea using "${data}/coll_expl1983l_c.dta", gen(m_l1)
merge 1:1 cod_funtionalarea using "${data}/coll_exploracionl_c.dta", gen(m_l2)
merge 1:1 cod_funtionalarea using "${data}/coll_expl1983l_t.dta", gen(m_l3)
merge 1:1 cod_funtionalarea using "${data}/coll_exploracionl_t.dta", gen(m_l4)
merge 1:1 cod_funtionalarea using "${data}/coll_expl1932l_t.dta", gen(m_l5)
sa "${data}/coll_l.dta", replace

use "${data}/coll_expl1932c_c.dta", clear
merge 1:1 cod_comuna cod_funtionalarea using "${data}/coll_expl1983c_c.dta", gen(m_c1)
merge 1:1 cod_comuna cod_funtionalarea using "${data}/coll_exploracionc_c.dta", gen(m_c2)
merge 1:1 cod_comuna cod_funtionalarea using "${data}/coll_expl1983c_t.dta", gen(m_c3)
merge 1:1 cod_comuna cod_funtionalarea using "${data}/coll_exploracionc_t.dta", gen(m_c4)
merge 1:1 cod_comuna cod_funtionalarea using "${data}/coll_expl1932c_t.dta", gen(m_c5)
sa "${data}/coll_c.dta", replace



* Merge both data sets.... 
use "${data}/coll_c.dta", clear
merge m:1 cod_funtionalarea using "${data}/coll_l.dta", gen(m_c_l)
sa "${data}/collapsed_all.dta", replace


/*------------------------------------------------------------------------------

Create the aggregate measures that you want for labor markets and Comunas separetly

------------------------------------------------------------------------------*/
use "${data}/collapsed_all.dta", clear

keep per_cam* num* cod_funtionalarea cod_comuna


foreach z in c_c l_c l_t c_t {
	foreach y in per_cam num{
		gen `y'_`z'_noex=`y'_`z'_32+`y'_`z'_83
		gen `y'_`z'_all=`y'_`z'_32+`y'_`z'_83+`y'_`z'_ex
	}
}

foreach y in per_cam num{

 gen `y'_c_all_all=`y'_c_c_all+`y'_c_t_all
 gen `y'_l_all_all=`y'_l_c_all+`y'_l_t_all


}

sa "${data}/collapsed_all.dta", replace

* Label Variables

label var per_cam_l_c_all "% of area of market with minas const exploracion & explotacion" 
label var per_cam_c_c_all "% of area of comuna with minas const exploracion & explotacion" 
	
label var per_cam_l_c_ex "% of area of market with minas const exploracion" 
label var per_cam_c_c_ex "% of area of comuna with minas const exploracion" 
	
label var per_cam_l_c_noex "% of area of market with minas const explotacion" 
label var per_cam_c_c_noex "% of area of comuna with minas const explotacion" 
		
label var num_l_c_all "number of const titles in market exploracion & explotacion" 
label var num_c_c_all "number of const titles in comuna exploracion & explotacion" 
	
label var num_l_c_ex "number of const titles in market exploracion" 
label var num_c_c_ex "number of const titles in comuna exploracion" 
	
label var num_l_c_noex "number of const titles in market explotacion" 
label var num_c_c_noex "number of const titles in comuna explotacion" 
	
label var num_c_all_all "number of titles in comuna explotacion & explotacion" 
label var num_l_all_all "number of titles in market explotacion" 

label var per_cam_c_all_all "% of area of comuna with titles explotacion & explotacion" 
label var per_cam_l_all_all "% of area of market with titles explotacion & explotacion" 

sa, replace



