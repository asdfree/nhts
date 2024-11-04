# commuter patterns,
# truckin'. what a long strange trip
# who went when where why
library(haven)

tf <- tempfile()

download.file( "https://nhts.ornl.gov/assets/2022/download/sas.zip" , tf , mode = 'wb' )

unzipped_files <- unzip( tf , exdir = tempdir() )
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
	
hhpub_df <- nhts_import( "hhv2pub" , unzipped_files )
perpub_df <- nhts_import( "perv2pub" , unzipped_files )
trippub_df <- nhts_import( "tripv2pub" , unzipped_files )
vehpub_df <- nhts_import( "vehv2pub" , unzipped_files )
hhpub_df[ , 'one' ] <- 1

perpub_df[ , 'one' ] <- 1

trippub_df[ , 'one' ] <- 1

trippub_df[ !( trippub_df[ , 'trpmiles' ] %in% -9 ) , 'wtd_tripmiles_no_nines' ] <-
	trippub_df[ !( trippub_df[ , 'trpmiles' ] %in% -9 ) , 'trpmiles' ] *
	trippub_df[ !( trippub_df[ , 'trpmiles' ] %in% -9 ) , 'wttrdfin' ]
trips_per_person <- 
	with( 
		trippub_df , 
		aggregate( 
			cbind( wttrdfin , wtd_tripmiles_no_nines ) , 
			list( houseid , personid ) , 
			sum , 
			na.rm = TRUE 
		) 
	)

names( trips_per_person ) <-
	c( 'houseid' , 'personid' , 'wtd_trips' , 'wtd_miles' )

walks_per_person <- 
	with( 
		subset( trippub_df , trptrans == '20' ) , 
		aggregate( 
			cbind( wttrdfin , wtd_tripmiles_no_nines ) , 
			list( houseid , personid ) , 
			sum , 
			na.rm = TRUE 
		) 
	)

names( walks_per_person ) <-
	c( 'houseid' , 'personid' , 'wtd_walks' , 'wtd_walk_miles' )
nhts_df <- merge( perpub_df , trips_per_person , all.x = TRUE )

nhts_df <- merge( nhts_df , walks_per_person , all.x = TRUE )

for( this_variable in c( 'wtd_trips' , 'wtd_miles' , 'wtd_walks' , 'wtd_walk_miles' ) ){
	nhts_df[ is.na( nhts_df[ , this_variable ] ) , this_variable ] <- 0
}

stopifnot( nrow( nhts_df ) == nrow( perpub_df ) )
# nhts_fn <- file.path( path.expand( "~" ) , "NHTS" , "this_file.rds" )
# saveRDS( nhts_df , file = nhts_fn , compress = FALSE )
# nhts_df <- readRDS( nhts_fn )
library(survey)

hh_design <-
	svydesign( 
		id = ~ houseid , 
		strata = ~ stratumid , 
		data = hhpub_df , 
		weights = ~ wthhfin
	)

nhts_design <-
	svydesign(
		id = ~ houseid ,
		strata = ~ stratumid ,
		data = nhts_df ,
		weights = ~ wtperfin
	)

trip_design <-
	svydesign(
		id = ~ houseid ,
		strata = ~ stratumid ,
		data = trippub_df ,
		weights = ~ wttrdfin
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
		
		urban_area = as.numeric( urbrur == '01' ) ,
		
		daily_person_trips = ( wtd_trips / ( 365 * wtperfin ) ) ,
		
		daily_person_miles_of_travel = ( wtd_miles / ( 365 * wtperfin ) ) ,
		
		daily_person_walks = ( wtd_walks / ( 365 * wtperfin ) ) ,
		
		daily_person_walk_miles_of_travel = ( wtd_walk_miles / ( 365 * wtperfin ) ) ,
		
		work_status = 
			factor( 
				as.numeric( worker ) , 
				levels = 2:1 , 
				labels = c( 'non-worker' , 'worker' ) 
			)

	)
sum( weights( nhts_design , "sampling" ) != 0 )

svyby( ~ one , ~ r_sex_imp , nhts_design , unwtd.count )
svytotal( ~ one , nhts_design )

svyby( ~ one , ~ r_sex_imp , nhts_design , svytotal )
svymean( ~ daily_person_walks , nhts_design )

svyby( ~ daily_person_walks , ~ r_sex_imp , nhts_design , svymean )
svymean( ~ work_status , nhts_design , na.rm = TRUE )

svyby( ~ work_status , ~ r_sex_imp , nhts_design , svymean , na.rm = TRUE )
svytotal( ~ daily_person_walks , nhts_design )

svyby( ~ daily_person_walks , ~ r_sex_imp , nhts_design , svytotal )
svytotal( ~ work_status , nhts_design , na.rm = TRUE )

svyby( ~ work_status , ~ r_sex_imp , nhts_design , svytotal , na.rm = TRUE )
svyquantile( ~ daily_person_walks , nhts_design , 0.5 )

svyby( 
	~ daily_person_walks , 
	~ r_sex_imp , 
	nhts_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE 
)
svyratio( 
	numerator = ~ daily_person_walk_miles_of_travel , 
	denominator = ~ daily_person_miles_of_travel , 
	nhts_design 
)
sub_nhts_design <- subset( nhts_design , last30_bike == '01' )
svymean( ~ daily_person_walks , sub_nhts_design )
this_result <- svymean( ~ daily_person_walks , nhts_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ daily_person_walks , 
		~ r_sex_imp , 
		nhts_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nhts_design )
svyvar( ~ daily_person_walks , nhts_design )
# SRS without replacement
svymean( ~ daily_person_walks , nhts_design , deff = TRUE )

# SRS with replacement
svymean( ~ daily_person_walks , nhts_design , deff = "replace" )
svyciprop( ~ urban_area , nhts_design ,
	method = "likelihood" )
svyttest( daily_person_walks ~ urban_area , nhts_design )
svychisq( 
	~ urban_area + work_status , 
	nhts_design 
)
glm_result <- 
	svyglm( 
		daily_person_walks ~ urban_area + work_status , 
		nhts_design 
	)

summary( glm_result )
hhsize_counts <- svytotal( ~ hhsize_categories , hh_design )

stopifnot(
	all( round( coef( hhsize_counts ) / 1000 , 0 ) == c( 36409 , 44751 , 19001 , 27384 ) )
)

hhsize_ci <- confint( hhsize_counts )

hhsize_moe <- hhsize_ci[ , 2 ] - coef( hhsize_counts )

stopifnot( all( round( hhsize_moe / 1000 , 0 ) == c( 1807 , 1760 , 1448 , 1742 ) ) )	
this_mean <- svymean( ~ daily_person_trips , nhts_design )

stopifnot( round( coef( this_mean ) , 2 ) == 2.28 )

this_ci <- confint( this_mean )

this_moe <- this_ci[ , 2 ] - coef( this_mean )

stopifnot( round( this_moe , 2 ) == 0.06 )

this_mean <- svymean( ~ daily_person_miles_of_travel , nhts_design )

stopifnot( round( coef( this_mean ) , 2 ) == 28.55 )

this_ci <- confint( this_mean )

this_moe <- this_ci[ , 2 ] - coef( this_mean )

stopifnot( round( this_moe , 2 ) == 2.39 )
this_mean <- svymean( ~ trpmiles , subset( trip_design , trpmiles >= 0 ) )

stopifnot( round( coef( this_mean ) , 2 ) == 12.56 )

this_ci <- confint( this_mean )

this_moe <- this_ci[ , 2 ] - coef( this_mean )

stopifnot( round( this_moe , 2 ) == 1.04 )

library(srvyr)
nhts_srvyr_design <- as_survey( nhts_design )
nhts_srvyr_design %>%
	summarize( mean = survey_mean( daily_person_walks ) )

nhts_srvyr_design %>%
	group_by( r_sex_imp ) %>%
	summarize( mean = survey_mean( daily_person_walks ) )
