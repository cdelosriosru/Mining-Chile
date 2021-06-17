/*----------------------------------------------------------------------------


Objective: start checking data for expansiones

Notes: 
1. There are observations with the same RUT but with different names
2. There are also observations with the same name but with different RUT.
3. There are abservations with the same address but with different RUT and Name

So... 
3. it's obvious. Just think about buildings with several firms working in it. 
1. There are some that are just typos, others change more dramatically
2. For the RUTs of people, well it could actually be the case that there are two persons that work on mining. 
	When it is a firm, I think it is much less likely. 

I'd say that the best thing we can do is: 

	1. Filter out the observations with same name, diferent RUT, BUT RUT from a natural person. 
	2. Create new RUTS for those with the same name, different RUT, but RUT from a Firm
	3. We create our expansion measures using the RUT. It's the most reliable thing we could make. 
	4. At the end, we are going to make the merge with NAMES! yes, thats the only thing we have from the data
	5. Some mining companies produce more than one metal. We can try to filter that out. 

--------------------------------------------------------------------------*/

* paths

global data "C:/Users/cdelo/Dropbox/Mining_HK_Chile/EDUCACION/Estimation/Data/Catastro/"






use "${data}/Explotacion1983/explotacion83.dta", clear


gen minas=1
destring year, replace force
drop if year==.
gen year_max=year
gen year_min=year
collapse (sum) mina hectareas (max) year_max (min) year_min, by(rut_titular nom_titular dir_titular)

foreach x in rut_titular nom_titular dir_titular{

	unique `x'
}
duplicates tag rut_titular dir_titular, gen(dup)


sort nom_titular
bys nom_titular: egen first_rut=first(rut_titular) 
gen dif_ti=(rut_titular!=first_rut)
bys nom_titular: egen dif_ti2=max(dif_ti)

sort rut_titular
bys rut_titular: egen first_nom=first(nom_titular)
gen dif_nom=(nom_titular!=first_nom)
bys rut_titular: egen dif_nom2=max(dif_nom)

sort rut_titular
bys rut_titular: egen first_dir=first(dir_titular)
gen dif_dir=(dir_titular!=first_dir)
bys rut_titular: egen dif_dir2=max(dif_dir)


sort nom_titular
br if dif_ti2==1

sort rut_titular
br if dif_nom2==1 & dif_dir2==1







use "${data}/Explotacion1932/explotacion32.dta", clear


gen minas=1
destring year, replace force
drop if year==.
gen year_max=year
gen year_min=year
collapse (sum) mina hectareas (max) year_max (min) year_min, by(rut_titular nom_titular dir_titular)

foreach x in rut_titular nom_titular dir_titular{

	unique `x'
}
duplicates tag rut_titular dir_titular, gen(dup)


sort nom_titular
bys nom_titular: egen first_rut=first(rut_titular) 
gen dif_ti=(rut_titular!=first_rut)
bys nom_titular: egen dif_ti2=max(dif_ti)

sort rut_titular
bys rut_titular: egen first_nom=first(nom_titular)
gen dif_nom=(nom_titular!=first_nom)
bys rut_titular: egen dif_nom2=max(dif_nom)

sort rut_titular
bys rut_titular: egen first_dir=first(dir_titular)
gen dif_dir=(dir_titular!=first_dir)
bys rut_titular: egen dif_dir2=max(dif_dir)


sort nom_titular
br if dif_ti2==1

sort rut_titular
br if dif_nom2==1 & dif_dir2==1







