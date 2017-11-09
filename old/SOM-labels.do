***********************************************
* At first, get lables for food and non-food
***********************************************
/* Run only first time. 

*parameters
*Country
local co ="SOM"
*number of modules
local M = 4
*number of simulations
local N = 2
*number of different items per module (the lower the more equal shares per module): >=1 (std: 2)
local ndiff = 3

*local lc_sdTemp "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\`co'\d3m4\Temp"
local lc_sdBase = "${gsdOutput}/`co'/d`ndiff'm`M'"
local lc_sdTemp = "`lc_sdBase'/Temp"
cd "`lc_sdTemp'"

*data directory
local sData = "${gsdDataBox}/SOM-SLHS13"

use "`sData'/non_food_clean.dta", clear
des nonfoodid
label save nonfoodid using "`lc_sdTemp'/lnf.do"

use "`sData'/food_consumption_clean.dta", clear
des foodid 
label save B02 using "`lc_sdTemp'/lf.do"
*/


*program to combine food and non-food
capture: program drop labvalcombine
program labvalcombine
	version 8
	gettoken names 0 : 0 , parse(,)
	syntax [, lblname(str)]

	foreach name of local names {
		capture label list `name'
		if _rc {
			di as err "value label `name' not found"
			exit _rc
		}
	}	

	tempfile dofile1 dofile2
	tempname in out
	qui label save `names' using `"`dofile1'"'
	file open `in' using `"`dofile1'"', r
	file open `out' using `"`dofile2'"', w

	file read `in' line
	tokenize `"`line'"'
	if "`lblname'" == "" local lblname `3'

	local cmd "label define `lblname'"
	
	while r(eof) == 0 {
		local line : subinstr local line "`1' `2' `3'" "`cmd'"
		file write `out' `"`line'"' _n
		file read `in' line
		tokenize `"`line'"'
	}
	file close `out'
	
	qui do `"`dofile2'"'
	label list `lblname'
end


*Food IDs
label define B02 1101 `"Paddy"', modify
label define B02 1102 `"Rice, husked"', modify
label define B02 1103 `"Green maize cob"', modify
label define B02 1104 `"Maize, grain"', modify
label define B02 1105 `"Maize, flour"', modify
label define B02 1106 `"Millet, grain"', modify
label define B02 1107 `"Millet, flour"', modify
label define B02 1108 `"Sorghum, grain"', modify
label define B02 1109 `"Sorghum, flour"', modify
label define B02 1110 `"Wheat, grain"', modify
label define B02 1111 `"Wheat, flour"', modify
label define B02 1112 `"Barley and other cereals"', modify
label define B02 1113 `"Bread"', modify
label define B02 1114 `"Baby foods excluding milk"', modify
label define B02 1115 `"Biscuits"', modify
label define B02 1116 `"Buns, cakes, small bread etc."', modify
label define B02 1117 `"Cooking oats, corn flakes"', modify
label define B02 1118 `"Macaroni, spaghetti"', modify
label define B02 1119 `"Other Cereals, Grains and Cereal Products (specify)"', modify
label define B02 1201 `"Potatoes"', modify
label define B02 1202 `"Cooking bananas, plantains"', modify
label define B02 1203 `"Other 'Roots, Tubers, and Plantains (specify)"', modify
label define B02 1301 `"Groundnuts in shell"', modify
label define B02 1302 `"Groundnuts shelled"', modify
label define B02 1303 `"Coconuts"', modify
label define B02 1304 `"Cashewnuts"', modify
label define B02 1305 `"Almonds and other nuts"', modify
label define B02 1306 `"Peas, dry"', modify
label define B02 1307 `"Beans, dry"', modify
label define B02 1308 `"Lentils and other pulse products"', modify
label define B02 1309 `"Pulse products"', modify
label define B02 1310 `"White beans"', modify
label define B02 1401 `"Carrots"', modify
label define B02 1402 `"Radhishes, beets, turnips"', modify
label define B02 1403 `"Garlic"', modify
label define B02 1404 `"Onion"', modify
label define B02 1405 `"Leeks"', modify
label define B02 1406 `"Spinach"', modify
label define B02 1407 `"Lettuce"', modify
label define B02 1408 `"Cabbage"', modify
label define B02 1409 `"Other leafy vegetables"', modify
label define B02 1410 `"Tomatoes"', modify
label define B02 1411 `"Ladies finger/okra"', modify
label define B02 1412 `"Cucumber/pumpkins"', modify
label define B02 1413 `"Eggplant/Brinjal"', modify
label define B02 1414 `"Canned vegetables"', modify
label define B02 1415 `"Dried vegetables"', modify
label define B02 1416 `"Other vegetables (specify:"', modify
label define B02 1501 `"Goat/Sheep"', modify
label define B02 1502 `"Cattle meat, incl. Mince sausages"', modify
label define B02 1503 `"Other domesticated animals"', modify
label define B02 1504 `"Wild animals"', modify
label define B02 1505 `"Offal (liver, kidney)"', modify
label define B02 1506 `"Dried or salted meat"', modify
label define B02 1507 `"Canned meat"', modify
label define B02 1508 `"Chicken and other poultry"', modify
label define B02 1509 `"Wild birds and insects"', modify
label define B02 1510 `"Eggs"', modify
label define B02 1511 `"Fresh fish"', modify
label define B02 1512 `"Dried or salted fish/shellfish"', modify
label define B02 1513 `"Canned fish/shellfish"', modify
label define B02 1514 `"Bones souce"', modify
label define B02 1601 `"Sweet/ripe bananas"', modify
label define B02 1602 `"Oranges/tangerines"', modify
label define B02 1603 `"Grapefruits, lemons, guavas, limes"', modify
label define B02 1604 `"Mangoes, avocado pears"', modify
label define B02 1605 `"Pawpaw"', modify
label define B02 1606 `"Pineapples"', modify
label define B02 1607 `"Melons"', modify
label define B02 1608 `"Sugar canes"', modify
label define B02 1609 `"Jack fruit"', modify
label define B02 1610 `"Apples, pears"', modify
label define B02 1611 `"Dried fruits"', modify
label define B02 1612 `"Canned fruits"', modify
label define B02 1701 `"Yoghurt"', modify
label define B02 1702 `"Cream"', modify
label define B02 1703 `"Cheese"', modify
label define B02 1704 `"Milk"', modify
label define B02 1705 `"Canned milk"', modify
label define B02 1706 `"Milk Powder"', modify
label define B02 1801 `"Breakfast"', modify
label define B02 1802 `"Lunch"', modify
label define B02 1803 `"Dinner"', modify
label define B02 1804 `"School meals"', modify
label define B02 1805 `"Tea, coffee, soft drinks"', modify
label define B02 1806 `"Other 'Cooked Foods from Vendors (specify)"', modify
label define B02 1807 `"Sugar"', modify
label define B02 1808 `"Honey"', modify
label define B02 1809 `"Syrup, jams, marmalade, jellies, chocolate, sweets"', modify
label define B02 1810 `"Sesame/Sunflower oil"', modify
label define B02 1811 `"Coconut cooking oil"', modify
label define B02 1812 `"Butter, Margarine"', modify
label define B02 1813 `"Other cooking oil"', modify
label define B02 1901 `"Tea"', modify
label define B02 1902 `"Coffee (beans, ground, instant)"', modify
label define B02 1903 `"Bottled Soft Drinks"', modify
label define B02 1904 `"Water bottles/container"', modify
label define B02 1905 `"Canned and bottled juices and squashes"', modify
label define B02 1906 `"Fresh fruit juices, ice cream and other non-alcoholic drinks"', modify
label define B02 1907 `"Purchased/prepared tea/coffee consumed at home"', modify
label define B02 1908 `"Other Beverages(specify)"', modify
label define B02 1909 `"Salt"', modify
label define B02 1910 `"Red/Black Pepper"', modify
label define B02 1911 `"Other Spices"', modify
label define B02 1912 `"Curry Powder"', modify
label define B02 1913 `"Vinegar"', modify
label define B02 1914 `"Yeast, baking powder"', modify
label define B02 1915 `"Cocoa, cooking chocolate"', modify
label define B02 1916 `"Purchased/prepared meals consumed at home"', modify
label define B02 1917 `"Rock Salt"', modify

*Nonfood IDs
label define nonfoodid 2101 `"Charcoal"', modify
label define nonfoodid 2102 `"Paraffin or kerosene"', modify
label define nonfoodid 2103 `"Cigarettes or other tobacco"', modify
label define nonfoodid 2104 `"Candles"', modify
label define nonfoodid 2105 `"Matches"', modify
label define nonfoodid 2106 `"Newspapers or magazines"', modify
label define nonfoodid 2107 `"Public transport - Bicycle Taxi"', modify
label define nonfoodid 2108 `"Public transport - Bus/Minibus"', modify
label define nonfoodid 2109 `"Public transport - Other (Truck, Oxcart, etc.)"', modify
label define nonfoodid 2110 `"Kat"', modify
label define nonfoodid 2201 `"Milling fees, grain"', modify
label define nonfoodid 2202 `"Bar soap (body soap or clothes soap)"', modify
label define nonfoodid 2203 `"Clothes soap (powder, paste)"', modify
label define nonfoodid 2204 `"Toothpaste, toothbrush"', modify
label define nonfoodid 2205 `"Toilet paper"', modify
label define nonfoodid 2206 `"Glycerine, Vaseline, skin creams"', modify
label define nonfoodid 2207 `"Other personal products (shampoo, razor blades, cosmetics, h"', modify
label define nonfoodid 2209 `"Light bulbs"', modify
label define nonfoodid 2210 `"Postage stamps or other postal fees"', modify
label define nonfoodid 2211 `"Donation - to church, charity, beggar"', modify
label define nonfoodid 2212 `"Petrol or diesel"', modify
label define nonfoodid 2213 `"Motor vehicle service, repair, or parts"', modify
label define nonfoodid 2214 `"Bicycle service, repair, or parts"', modify
label define nonfoodid 2215 `"Wages paid to servants"', modify
label define nonfoodid 2216 `"Mortgage - regular payment to purchase house"', modify
label define nonfoodid 2217 `"Repairs & maintenance to dwelling"', modify
label define nonfoodid 2218 `"Repairs to household and personal items"', modify
label define nonfoodid 2219 `"Expenditures on pets"', modify
label define nonfoodid 2220 `"Batteries"', modify
label define nonfoodid 2221 `"Recharging batteries, cell phones"', modify
label define nonfoodid 2222 `"Internet/Cable TV and other communication"', modify
label define nonfoodid 2223 `"Medical care items"', modify
label define nonfoodid 2224 `"Gas (natural and liquified)"', modify
label define nonfoodid 2225 `"Taxes and social benefit contribution"', modify
label define nonfoodid 2226 `"Expenditures for Electricity"', modify
label define nonfoodid 2301 `"Infant clothing"', modify
label define nonfoodid 2302 `"Baby nappies/diapers"', modify
label define nonfoodid 2303 `"Boy's trousers"', modify
label define nonfoodid 2304 `"Boy's shirts"', modify
label define nonfoodid 2305 `"Boy's jackets"', modify
label define nonfoodid 2306 `"Boy's other clothing"', modify
label define nonfoodid 2307 `"Men's trousers"', modify
label define nonfoodid 2308 `"Men's shirts"', modify
label define nonfoodid 2309 `"Men's jackets"', modify
label define nonfoodid 2310 `"Men's other clothing"', modify
label define nonfoodid 2311 `"Girl's blouse/shirt"', modify
label define nonfoodid 2312 `"Girl's dress/skirt"', modify
label define nonfoodid 2313 `"Girl's other clothing"', modify
label define nonfoodid 2314 `"Lady's blouse/shirt"', modify
label define nonfoodid 2315 `"Chitenje cloth"', modify
label define nonfoodid 2316 `"Lady's dress/skirt"', modify
label define nonfoodid 2317 `"Lady's other clothing"', modify
label define nonfoodid 2318 `"Boy's shoes"', modify
label define nonfoodid 2319 `"Men's shoes"', modify
label define nonfoodid 2320 `"Girl's shoes"', modify
label define nonfoodid 2321 `"Lady's shoes"', modify
label define nonfoodid 2322 `"Cloth, thread, other sewing material"', modify
label define nonfoodid 2323 `"Laundry, dry cleaning, tailoring fees"', modify
label define nonfoodid 2324 `"Bowls, glassware, plates, silverware, etc."', modify
label define nonfoodid 2325 `"Cooking utensils (cookpots, stirring spoons and whisks, etc."', modify
label define nonfoodid 2326 `"Cleaning utensils (brooms, brushes, etc.)"', modify
label define nonfoodid 2327 `"Torch / flashlight"', modify
label define nonfoodid 2328 `"Umbrella"', modify
label define nonfoodid 2329 `"Paraffin lamp (hurricane or pressure)"', modify
label define nonfoodid 2330 `"Stationery items (not for school)"', modify
label define nonfoodid 2331 `"Books (not for school)"', modify
label define nonfoodid 2332 `"Music or video cassette or CD/DVD"', modify
label define nonfoodid 2333 `"Tickets for sports / entertainment events"', modify
label define nonfoodid 2334 `"House decorations"', modify
label define nonfoodid 2335 `"Night's lodging in rest house or hotel"', modify
label define nonfoodid 2336 `"Electricity"', modify
label define nonfoodid 2337 `"Sewage"', modify
label define nonfoodid 2401 `"Carpet, rugs, drapes, curtains"', modify
label define nonfoodid 2402 `"Linen - towels, sheets, blankets"', modify
label define nonfoodid 2403 `"Mat - sleeping or for drying maize flour"', modify
label define nonfoodid 2404 `"Moskito Net"', modify
label define nonfoodid 2405 `"Mattress"', modify
label define nonfoodid 2406 `"Sports & hobby equipment, musical instruments, toys"', modify
label define nonfoodid 2407 `"Film, film processing, camera"', modify
label define nonfoodid 2408 `"Cement"', modify
label define nonfoodid 2409 `"Bricks"', modify
label define nonfoodid 2410 `"Construction timber"', modify
label define nonfoodid 2411 `"Council rates"', modify
label define nonfoodid 2412 `"Insurance - health (MASM, etc.), auto,home, life"', modify
label define nonfoodid 2413 `"Losses to theft (value of items or cash lost)"', modify
label define nonfoodid 2414 `"Fines or legal fees"', modify
label define nonfoodid 2415 `"Bridewealth costs"', modify
label define nonfoodid 2416 `"Marriage ceremony costs"', modify
label define nonfoodid 2417 `"Funeral costs, household members"', modify
label define nonfoodid 2418 `"Giving zakat to the poor"', modify
label define nonfoodid 2419 `"Funeral costs, nonhousehold members (relatives, neighbors/fr"', modify
label define nonfoodid 2420 `"Other celebration/ritual costs"', modify
label define nonfoodid 2421 `"Educational expenses"', modify
label define nonfoodid 2422 `"Health care expenses"', modify
label define nonfoodid 2423 `"Woodpoles, bamboo"', modify
label define nonfoodid 2424 `"Grass for thatching roof or other use"', modify
label define nonfoodid 2425 `"Fire wood"', modify

quiet: labvalcombine B02 nonfoodid, lblname(litemid)
