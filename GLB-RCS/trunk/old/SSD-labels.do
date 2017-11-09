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
label define lfoodid 11101 "Dura", replace
label define lfoodid 11103 "Yellow maize (Dura Shami)", add
label define lfoodid 11104 "Millet (Dukhn)", add
label define lfoodid 11105 "Wheat", add
label define lfoodid 11106 "Maize (in the cob)", add
label define lfoodid 11109 "Rice (imported)", add
label define lfoodid 11111 "Wheat flour (Fino,local)", add
label define lfoodid 11114 "Dura flour", add
label define lfoodid 11116 "Maize flour", add
label define lfoodid 11117 "Millet flour", add
label define lfoodid 11118 "Other flour", add
label define lfoodid 11121 "Macaroni, spaghetti, noodles etc", add
label define lfoodid 11122 "Breakfast cereals", add
label define lfoodid 11127 "Reels of pasta", add
label define lfoodid 11128 "Bread", add
label define lfoodid 11130 "Kisra and asida", add
label define lfoodid 11132 "Local biscuit", add
label define lfoodid 11136 "Buns", add
label define lfoodid 11139 "Infant feeding", add
label define lfoodid 11140 "Other cereals and cereal products", add
label define lfoodid 11201 "Sheep meat (fresh, with bone, local)", add
label define lfoodid 11202 "Goat meat (with bones, fresh, local)", add
label define lfoodid 11203 "Liver (sheep/goat)", add
label define lfoodid 11204 "Meat accessories (sheep/goat)", add
label define lfoodid 11205 "Fresh beef", add
label define lfoodid 11206 "Pork meat", add
label define lfoodid 11210 "Liver (cattle/veal)", add
label define lfoodid 11211 "Accessories beef/veal", add
label define lfoodid 11212 "Camel meat (local fresh)", add
label define lfoodid 11213 "Camel liver", add
label define lfoodid 11214 "Chicken and poultry", add
label define lfoodid 11215 "Small animals (rabbits, mice,….)", add
label define lfoodid 11216 "Insects", add
label define lfoodid 11217 "Blood and blood products", add
label define lfoodid 11219 "Sausages (cattle/veal)", add
label define lfoodid 11220 "Other meat and animal products", add
label define lfoodid 11301 "Fresh fish, Bolati and others", add
label define lfoodid 11303 "Fissekh, salted fish (local)", add
label define lfoodid 11305 "Dried fish (local)", add
label define lfoodid 11306 "Tinned fish, sardine 125 grams, tuna, etc", add
label define lfoodid 11401 "Fresh milk", add
label define lfoodid 11404 "Milk powder", add
label define lfoodid 11406 "Milk products; cheese, yoghurt, etc", add
label define lfoodid 11411 "Eggs", add
label define lfoodid 11501 "Animal and vegetable butter", add
label define lfoodid 11503 "Ghee (samin)", add
label define lfoodid 11505 "Cooking oil", add
label define lfoodid 11602 "Apples", add
label define lfoodid 11603 "Local banana", add
label define lfoodid 11606 "Oranges", add
label define lfoodid 11614 "Mangoes", add
label define lfoodid 11616 "Pineapple", add
label define lfoodid 11619 "Dates", add
label define lfoodid 11626 "Papaya", add
label define lfoodid 11627 "Avocado", add
label define lfoodid 11628 "Other fruits", add
label define lfoodid 11701 "Dry Egyptian beans (local)", add
label define lfoodid 11702 "Dry chick peas", add
label define lfoodid 11703 "Green okra", add
label define lfoodid 11704 "Dry okra (dry Alweka)", add
label define lfoodid 11705 "Natural groundnut (Roasted)", add
label define lfoodid 11706 "Groundnut flour", add
label define lfoodid 11707 "Soya bean flour", add
label define lfoodid 11711 "Lentils", add
label define lfoodid 11713 "White beans", add
label define lfoodid 11715 "Lentils (Adasia)", add
label define lfoodid 11718 "Carrots", add
label define lfoodid 11719 "Cabbage", add
label define lfoodid 11720 "Cucumber", add
label define lfoodid 11725 "Onions", add
label define lfoodid 11728 "Fresh tomatoes", add
label define lfoodid 11734 "Potato (Irish)", add
label define lfoodid 11735 "Sweet potato", add
label define lfoodid 11737 "Milokhia", add
label define lfoodid 11741 "Pumpkin (Gara’a)", add
label define lfoodid 11744 "Tomato sauce (canned)", add
label define lfoodid 11745 "Tinned pulses", add
label define lfoodid 11749 "Cassava tubers", add
label define lfoodid 11750 "Yam", add
label define lfoodid 11751 "Cassava flour", add
label define lfoodid 11752 "Cooking banana", add
label define lfoodid 11753 "Other roots, tubers, vegetables", add
label define lfoodid 11801 "Sugar", add
label define lfoodid 11802 "Sugar cane", add
label define lfoodid 11803 "Natural honey", add
label define lfoodid 11805 "Tahnieh Halawa", add
label define lfoodid 11806 "Chocolate", add
label define lfoodid 11807 "Jam (the malty) & jelly", add
label define lfoodid 11809 "Candy", add
label define lfoodid 11810 "Other sugar", add
label define lfoodid 11901 "Green spicy (pungent)", add
label define lfoodid 11902 "Red chili (hot pepper)", add
label define lfoodid 11903 "Grain black pepper", add
label define lfoodid 11906 "Ginger powder", add
label define lfoodid 11912 "Yeast", add
label define lfoodid 11914 "Promises cinnamon", add
label define lfoodid 11915 "Cinnamon powder", add
label define lfoodid 11916 "Food salt", add
label define lfoodid 11919 "Baking powder", add
label define lfoodid 11923 "Coriander", add
label define lfoodid 11925 "Okra dry powder (waika)", add
label define lfoodid 11931 "Other spices", add
label define lfoodid 12101 "Coffee", add
label define lfoodid 12103 "Black tea imported", add
label define lfoodid 12105 "Tea bags", add
label define lfoodid 12106 "Nescafe (coffee instant)", add
label define lfoodid 12107 "Cocoa", add
label define lfoodid 12201 "Local mineral water", add
label define lfoodid 12202 "Orange juice (fruit juice)", add
label define lfoodid 12215 "Bottle of Fanta Sprite", add
label define lfoodid 12218 "Traditional beer", add
label define lfoodid 12219 "Canned/bottled beer", add
label define lfoodid 12220 "Liquor", add
label define lfoodid 12221 "Other beverage products", add
label define lfoodid 22001 "Cigarettes", add
label define lfoodid 22009 "Tombac, tobacco", add
label define lfoodid 22010 "Honeyed tobacco (Aoasl)", add
label define lfoodid 111101 "Lunch in a restaurant", add
label define lfoodid 111102 "Coffee or tea in the market", add
label define lfoodid 111103 "Fresh orange juice in a restaurant", add
label define lfoodid 111104 "Meals and breakfast for one person in a restaurant", add
label define lfoodid 111105 "Sandwich Tamiya / beans", add
label define lfoodid 111106 "Egyptian boiled beans", add
label define lfoodid 111201 "Maize boiled/roasted", add
label define lfoodid 111202 "Cassava boiled", add
label define lfoodid 111203 "Eggs boiled", add
label define lfoodid 111204 "Chicken", add
label define lfoodid 111205 "Meat", add
label define lfoodid 111206 "Fish", add
label define lfoodid 111207 "Meat dishes in a restaurant", add
label define lfoodid 111208 "Fish dishes in a restaurant", add
label define lfoodid 111209 "Other cooked food from venders", add
label define lfoodid 112041 "Sheep head fresh and cleaned (without skin) full head", add
label define lfoodid 112042 "Goat head cleaned and fresh (without skin) full head", add
label define lfoodid 112043 "Feet/foot of sheep/goat, fresh and cleaned (without skin)", add
label define lfoodid 112045 "Mutton tripes (intestines) sheep/goat, fresh and cleaned", add
label define lfoodid 112111 "Accessories meat (head of cattle/veal) fresh and clean without skin", add
label define lfoodid 112112 "Accessories meat (cow guilt/veal) fresh", add
label define lfoodid 112113 "Feet/foot of cow/veal, fresh and clean without skin", add
label define lfoodid 112114 "Mutton tripes (intestines) cow/veal fresh and clean", add
label define lfoodid 116141 "Indian mango (local)", add
label define lfoodid 116142 "Mango peal (municipal mango)", add
label define lfoodid 117441 "Tomato sauce (small pack of 70 grams)", add
label define lfoodid 117442 "Tomato sauce (large pack of local 500 grams)", add
label define lfoodid 118091 "Jelly", add
label define lfoodid 121031 "Khazalten tea or other", add
label define lfoodid 122011 "Local mineral water 1.5 liters", add
label define lfoodid 122012 "Local mineral water 0.5 liters", add
label define lfoodid 122151 "Bottle of Fanta or Sprite 300-350 milliliters", add
label define lfoodid 122152 "Aluminium box Fanta or Sprite 350 milliliters", add

*Nonfood IDs
label define lnonfoodid 101 "Pre primary and primary education", replace
label define lnonfoodid 102 "Secondary education", add
label define lnonfoodid 103 "Post secondary education", add
label define lnonfoodid 104 "Higher education", add
label define lnonfoodid 105 "Un-specified educational level", add
label define lnonfoodid 112 "Accommodation services, hotel rent etc. Not for the house", add
label define lnonfoodid 311 "Clothing materials, tissue etc", add
label define lnonfoodid 313 "Other type of clothing (hat, tie etc)", add
label define lnonfoodid 314 "Laundry, repair and rental", add
label define lnonfoodid 315 "Tailoring fees", add
label define lnonfoodid 721 "Spare parts and accessories for personal transport", add
label define lnonfoodid 722 "Fuel, oils and lubricants for personal transport", add
label define lnonfoodid 723 "Maintenance and repair of personal transport", add
label define lnonfoodid 724 "Other services related to personal transport", add
label define lnonfoodid 915 "Repair of equipment", add
label define lnonfoodid 952 "Newspapers and periodicals", add
label define lnonfoodid 954 "Stationary and painting", add
label define lnonfoodid 1254 "Relating insurance transport", add
label define lnonfoodid 31201 "Infant and boys clothing", add
label define lnonfoodid 31213 "Menís clothing", add
label define lnonfoodid 31214 "Girls clothing", add
label define lnonfoodid 31218 "Ladyís clothing", add
label define lnonfoodid 32101 "Menís shoes (normal skin)", add
label define lnonfoodid 32111 "Menís Slippers", add
label define lnonfoodid 32115 "Womenís shoes (normal skin)", add
label define lnonfoodid 32117 "Womenís leather slippers", add
label define lnonfoodid 32124 "Girlís shoes imitation leather", add
label define lnonfoodid 43101 "Occupied family housing maintenance cost", add
label define lnonfoodid 44101 "Monthly water fees", add
label define lnonfoodid 44201 "Waste fees", add
label define lnonfoodid 44403 "Other related fees and services", add
label define lnonfoodid 45101 "Electricity to homes from grid (kw)", add
label define lnonfoodid 45201 "Gas cylinder 12.5 kg", add
label define lnonfoodid 45301 "Kerosene", add
label define lnonfoodid 45302 "Generator fuel", add
label define lnonfoodid 45401 "Charcoal", add
label define lnonfoodid 45402 "Wood fuel", add
label define lnonfoodid 45403 "Other energy source", add
label define lnonfoodid 51101 "Furniture", add
label define lnonfoodid 51201 "Linoleum /plastic floring", add
label define lnonfoodid 51202 "Carpet imported", add
label define lnonfoodid 52001 "Bed sheets, mattress, pillows mosquito net etc", add
label define lnonfoodid 53101 "Refrigerators, washing machines, air coolers etc", add
label define lnonfoodid 53102 "Ceiling fan, electric iron, mixers etc", add
label define lnonfoodid 53103 "Other major household appliances", add
label define lnonfoodid 53301 "Filling of refrigerator gas", add
label define lnonfoodid 53303 "Mixer repair", add
label define lnonfoodid 53304 "Other electrical household appliances repair", add
label define lnonfoodid 54001 "Cooking suit (pots)", add
label define lnonfoodid 54002 "Glass plate", add
label define lnonfoodid 54003 "Glass bowl (imported)", add
label define lnonfoodid 54009 "Spoons, knives, forks", add
label define lnonfoodid 54012 "Tea cups, glasses etc", add
label define lnonfoodid 55201 "Tools and hand equipments", add
label define lnonfoodid 55202 "Dry-cell battery (Haggar battery ñlarge size)", add
label define lnonfoodid 55203 "Torch/Flash light", add
label define lnonfoodid 55204 "Paraffin lamp", add
label define lnonfoodid 56101 "Match boxes", add
label define lnonfoodid 56102 "Laundry soap (local)", add
label define lnonfoodid 56103 "Soap (powder)", add
label define lnonfoodid 56104 "Bathing soap", add
label define lnonfoodid 56117 "Other non-durable household goods", add
label define lnonfoodid 56201 "Service cost weekly salary at family house", add
label define lnonfoodid 56202 "Other kind of domestic services", add
label define lnonfoodid 61101 "Cough Syrup medicine (cold)", add
label define lnonfoodid 61102 "Drug tabs and roots for reducing fever and malaria", add
label define lnonfoodid 61103 "Antibiotics", add
label define lnonfoodid 61204 "Other pharmaceutical products", add
label define lnonfoodid 61301 "Medical eye glasses", add
label define lnonfoodid 61302 "Hearing aid", add
label define lnonfoodid 62101 "Specialist and general doctors", add
label define lnonfoodid 62103 "Medical consultation at hospital", add
label define lnonfoodid 62105 "Planning blood vessels", add
label define lnonfoodid 62201 "Filling and treatment of teeth", add
label define lnonfoodid 62301 "Malaria blood testing", add
label define lnonfoodid 62302 "Other tests (blood, urine, feces)", add
label define lnonfoodid 62303 "x-ray test", add
label define lnonfoodid 62304 "Physiotherapy", add
label define lnonfoodid 63001 "Birth in general hospital", add
label define lnonfoodid 63004 "Operations in hospital", add
label define lnonfoodid 63005 "Traditional healers fee/medicine", add
label define lnonfoodid 71101 "Private sedan cars for family use", add
label define lnonfoodid 71201 "Motorcycle for private use", add
label define lnonfoodid 71301 "Bicycle", add
label define lnonfoodid 73101 "Movement and freight using train or road transport", add
label define lnonfoodid 73103 "Boda-boda, taxi and bus fares", add
label define lnonfoodid 73301 "Tickets for air travel", add
label define lnonfoodid 73401 "Tickets for travel by sea or river", add
label define lnonfoodid 81001 "Cost of sending mail and parcel", add
label define lnonfoodid 82001 "Mobile and fix phone costs and their repair", add
label define lnonfoodid 83001 "Monthly telephone subscription fees for housing", add
label define lnonfoodid 83002 "Fixed telephone bills", add
label define lnonfoodid 83003 "Mobile airtime and internet and fax fees", add
label define lnonfoodid 91101 "Color TV, radio etc", add
label define lnonfoodid 91301 "Computers and calculators", add
label define lnonfoodid 91401 "Photographic and computers tapes/CD", add
label define lnonfoodid 93201 "Football and other sports equipment", add
label define lnonfoodid 93401 "Spending on pets and related products", add
label define lnonfoodid 94101 "Participate and fees in sports clubs and tickets", add
label define lnonfoodid 94102 "Boda boda fares", add
label define lnonfoodid 94103 "Marriage ceremony costs", add
label define lnonfoodid 95101 "Spending on books including textbooks", add
label define lnonfoodid 96001 "Organized travels incl Hajj and Umrah", add
label define lnonfoodid 121101 "Hair cut for men, hair dressing for women", add
label define lnonfoodid 121102 "Sauna bath", add
label define lnonfoodid 121105 "Decoration for women", add
label define lnonfoodid 121106 "Other personal care services", add
label define lnonfoodid 121201 "Small electric hairdryer etc", add
label define lnonfoodid 121303 "Shampoo, creams and perfumes", add
label define lnonfoodid 121313 "Tooth paste and teeth brush", add
label define lnonfoodid 121314 "Ordinary razor", add
label define lnonfoodid 121321 "Talh wood and shaf", add
label define lnonfoodid 121322 "Other personal care services and equipment", add
label define lnonfoodid 123101 "Gold and silver", add
label define lnonfoodid 123103 "Wristwatch and wall clock", add
label define lnonfoodid 123201 "Suitcase, schoolbags etc", add
label define lnonfoodid 125401 "Compulsory car insurance", add
label define lnonfoodid 127001 "Charges for official document inclusive ID card", add
label define lnonfoodid 127002 "Driving license fees", add
label define lnonfoodid 127003 "Birth certificate fees", add
label define lnonfoodid 127004 "Marriage document fees", add
label define lnonfoodid 127005 "Passport fees", add
label define lnonfoodid 127007 "Ownership document for real estate", add
label define lnonfoodid 127010 "Other services n.e.s. classified", add
label define lnonfoodid 431011 "Faucet (tap)", add
label define lnonfoodid 431012 "Neon bulb", add
label define lnonfoodid 431013 "Glass for building", add
label define lnonfoodid 431014 "Portland cement", add
label define lnonfoodid 431015 "Switch (electric)", add
label define lnonfoodid 431016 "Other materials for housing maintenance", add
label define lnonfoodid 552011 "Hand operated screwdriver", add
label define lnonfoodid 552012 "Hand saw", add
label define lnonfoodid 552013 "Tree branch scissor", add
label define lnonfoodid 552014 "Mobile ladder", add
label define lnonfoodid 552015 "Bulb charger (imported)", add
label define lnonfoodid 552016 "Electrical link", add
label define lnonfoodid 630041 "Government hospital", add
label define lnonfoodid 630042 "Private hospital", add

quiet: labvalcombine lfoodid lnonfoodid, lblname(litemid)
