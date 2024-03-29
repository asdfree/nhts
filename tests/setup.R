# commuter patterns,
# truckin'. what a long strange trip
# who went when where why
library(haven)

nhts_dl_uz <-
	function( this_url ){
	
		tf <- tempfile()

		download.file( this_url , tf , mode = 'wb' )

		unzip( tf , exdir = tempdir() )
	}
	
unzipped_survey_data <-
	nhts_dl_uz( "https://nhts.ornl.gov/assets/2016/download/sas.zip" )

unzipped_replicate_weights <-
	nhts_dl_uz( "https://nhts.ornl.gov/assets/2016/download/Replicates.zip" )

unzipped_trip_chains <-
	nhts_dl_uz( "https://nhts.ornl.gov/assets/2016/download/TripChain/TripChain17.zip" )
nhts_import <-
	function( this_prefix , this_unzip ){
		
		this_sas7bdat <-
			grep( 
				paste0( this_prefix , "\\.sas7bdat$" ) , 
				this_unzip , 
				value = TRUE 
			)
		
		this_tbl <- read_sas( this_sas7bdat )
		
		this_df <- data.frame( this_tbl )
		
		names( this_df ) <- tolower( names( this_df ) )
		
		this_df
	}
	
hhpub_df <- nhts_import( "hhpub" , unzipped_survey_data )
perpub_df <- nhts_import( "perpub" , unzipped_survey_data )
trippub_df <- nhts_import( "trippub" , unzipped_survey_data )
vehpub_df <- nhts_import( "vehpub" , unzipped_survey_data )

hhwgt_df <- nhts_import( "hhwgt" , unzipped_replicate_weights )
perwgt_df <- nhts_import( "perwgt" , unzipped_replicate_weights )
hhpub_df[ , 'one' ] <- 1

perpub_df[ , 'one' ] <- 1

trippub_df[ , 'one' ] <- 1

trippub_df[ !( trippub_df[ , 'trpmiles' ] %in% -9 ) , 'tripmiles_no_nines' ] <-
	trippub_df[ !( trippub_df[ , 'trpmiles' ] %in% -9 ) , 'trpmiles' ]
trips_per_person <- 
	with( 
		trippub_df , 
		aggregate( 
			cbind( one , tripmiles_no_nines ) , 
			list( houseid , personid ) , 
			sum , 
			na.rm = TRUE 
		) 
	)

names( trips_per_person ) <-
	c( 'houseid' , 'personid' , 'trips_per_person' , 'miles_per_person' )

walks_per_person <- 
	with( 
		subset( trippub_df , trptrans == '01' ) , 
		aggregate( 
			cbind( one , tripmiles_no_nines ) , 
			list( houseid , personid ) , 
			sum , 
			na.rm = TRUE 
		) 
	)

names( walks_per_person ) <-
	c( 'houseid' , 'personid' , 'walks_per_person' , 'walk_miles_per_person' )
nhts_df <- merge( perpub_df , trips_per_person , all.x = TRUE )
nhts_df[ is.na( nhts_df[ , 'trips_per_person' ] ) , 'trips_per_person' ] <- 0
nhts_df[ is.na( nhts_df[ , 'miles_per_person' ] ) , 'miles_per_person' ] <- 0

nhts_df <- merge( nhts_df , walks_per_person , all.x = TRUE )
nhts_df[ is.na( nhts_df[ , 'walks_per_person' ] ) , 'walks_per_person' ] <- 0
nhts_df[ is.na( nhts_df[ , 'walk_miles_per_person' ] ) , 'walk_miles_per_person' ] <- 0

stopifnot( nrow( nhts_df ) == nrow( perpub_df ) )
# nhts_fn <- file.path( path.expand( "~" ) , "NHTS" , "this_file.rds" )
# saveRDS( nhts_df , file = nhts_fn , compress = FALSE )
# nhts_df <- readRDS( nhts_fn )
library(survey)

hhpub_df <- hhpub_df[ order( hhpub_df[ , 'houseid' ] ) , ]
hhwgt_df <- hhwgt_df[ order( hhwgt_df[ , 'houseid' ] ) , ]

hh_design <-
	svrepdesign(
		weight = ~ wthhfin ,
		repweights =
			hhwgt_df[ grep( 'wthhfin[0-9]' , names( hhwgt_df ) , value = TRUE ) ] ,
		scale = 6 / 7 ,
		rscales = 1 ,
		type = 'JK1' ,
		mse = TRUE ,
		data = hhpub_df
	)
nhts_df <- nhts_df[ do.call( order , nhts_df[ , c( 'houseid' , 'personid' ) ] ) , ]
perwgt_df <- perwgt_df[ do.call( order , perwgt_df[ , c( 'houseid' , 'personid' ) ] ) , ]

nhts_design <-
	svrepdesign(
		weight = ~ wtperfin ,
		repweights =
			perwgt_df[ grep( 'wtperfin[0-9]' , names( perwgt_df ) , value = TRUE ) ] ,
		scale = 6 / 7 ,
		rscales = rep( 1 , 98 ) ,
		type = 'JK1' ,
		mse = TRUE ,
		data = nhts_df
	)
trippub_df <- trippub_df[ do.call( order , trippub_df[ , c( 'houseid' , 'personid' ) ] ) , ]
perwgt_df <- perwgt_df[ do.call( order , perwgt_df[ , c( 'houseid' , 'personid' ) ] ) , ]

trip_design <-
	svrepdesign(
		weight = ~ wttrdfin ,
		repweights =
			perwgt_df[ grep( 'wttrdfin[0-9]' , names( perwgt_df ) , value = TRUE ) ] ,
		scale = 6 / 7 ,
		rscales = 1 ,
		type = 'JK1' ,
		mse = TRUE ,
		data = trippub_df
	)
hh_design <-
	update(
		hh_design ,
		hhsize_categories =
			factor(
				findInterval( hhsize , 1:4 ) ,
				levels = 1:4 ,
				labels = c( 1:3 , '4 or more' )
			)
	)
	

nhts_design <- 
	update( 
		nhts_design , 
		
		urban_area = as.numeric( urbrur == '01' )
		
	)
sum( weights( nhts_design , "sampling" ) != 0 )

svyby( ~ one , ~ r_sex_imp , nhts_design , unwtd.count )
svytotal( ~ one , nhts_design )

svyby( ~ one , ~ r_sex_imp , nhts_design , svytotal )
svymean( ~ miles_per_person , nhts_design )

svyby( ~ miles_per_person , ~ r_sex_imp , nhts_design , svymean )
svymean( ~ hhstate , nhts_design )

svyby( ~ hhstate , ~ r_sex_imp , nhts_design , svymean )
svytotal( ~ miles_per_person , nhts_design )

svyby( ~ miles_per_person , ~ r_sex_imp , nhts_design , svytotal )
svytotal( ~ hhstate , nhts_design )

svyby( ~ hhstate , ~ r_sex_imp , nhts_design , svytotal )
svyquantile( ~ miles_per_person , nhts_design , 0.5 )

svyby( 
	~ miles_per_person , 
	~ r_sex_imp , 
	nhts_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE 
)
svyratio( 
	numerator = ~ walk_miles_per_person , 
	denominator = ~ miles_per_person , 
	nhts_design 
)
sub_nhts_design <- subset( nhts_design , nbiketrp > 0 )
svymean( ~ miles_per_person , sub_nhts_design )
this_result <- svymean( ~ miles_per_person , nhts_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ miles_per_person , 
		~ r_sex_imp , 
		nhts_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nhts_design )
svyvar( ~ miles_per_person , nhts_design )
# SRS without replacement
svymean( ~ miles_per_person , nhts_design , deff = TRUE )

# SRS with replacement
svymean( ~ miles_per_person , nhts_design , deff = "replace" )
svyciprop( ~ urban_area , nhts_design ,
	method = "likelihood" )
svyttest( miles_per_person ~ urban_area , nhts_design )
svychisq( 
	~ urban_area + hhstate , 
	nhts_design 
)
glm_result <- 
	svyglm( 
		miles_per_person ~ urban_area + hhstate , 
		nhts_design 
	)

summary( glm_result )
hhsize_counts <- svytotal( ~ hhsize_categories , hh_design )

stopifnot(
	all( round( coef( hhsize_counts ) / 1000 , 0 ) == c( 32952 , 40056 , 18521 , 26679 ) )
)

hhsize_ci <- confint( hhsize_counts , df = ncol( hh_design$repweights ) )

hhsize_moe <- hhsize_ci[ , 2 ] - coef( hhsize_counts )

stopifnot( all( round( hhsize_moe / 1000 , 0 ) == c( 0 , 0 , 97 , 97 ) ) )	
unwtd_n <- with( nhts_df , tapply( trips_per_person , worker , sum ) )
stopifnot( all( unwtd_n == c( 79295 , 28 , 497944 , 346305 ) ) )

surveyed_n <- with( nhts_df , tapply( trips_per_person , worker , mean ) )
stopifnot( all( round( surveyed_n , 2 ) == c( 2.84 , 1.65 , 3.88 , 3.21 ) ) )

this_mean <- svyby( ~ trips_per_person , ~ worker , nhts_design , svymean )
stopifnot( round( coef( this_mean ) , 2 ) == c( 2.78 , 1.28 , 3.77 , 3.01 ) )

this_ci <- confint( this_mean , df = ncol( nhts_design$repweights ) )

this_moe <- this_ci[ , 2 ] - coef( this_mean )

stopifnot( all( round( this_moe , 2 ) == c( 0.06 , 2.21 , 0.03 , 0.06 ) ) )

library(srvyr)
nhts_srvyr_design <- as_survey( nhts_design )
nhts_srvyr_design %>%
	summarize( mean = survey_mean( miles_per_person ) )

nhts_srvyr_design %>%
	group_by( r_sex_imp ) %>%
	summarize( mean = survey_mean( miles_per_person ) )
