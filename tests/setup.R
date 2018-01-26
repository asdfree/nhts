if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
lodown( "nhts" , output_dir = file.path( getwd() ) )
library(lodown)
# examine all available NHTS microdata files
nhts_cat <-
	get_catalog( "nhts" ,
		output_dir = file.path( getwd() ) )

# 2011 only
nhts_cat <- subset( nhts_cat , year == 2011 )
# download the microdata to your local computer


library(DBI)
library(SQLite)
library(survey)

options( survey.lonely.psu = "adjust" )

prestratified_design <-
	svydesign(
		id = ~v4618 ,
		strata = ~v4617 ,
		data = nhts_cat[ 1 , "db_tablename" ] ,
		weights = ~pre_wgt ,
		nest = TRUE ,
		dbtype = "SQLite" ,
		dbname = nhts_cat[ 1 , "dbfile" ]
	)
	
nhts_design <- 
	lodown:::pnad_postStratify( 
		design = prestratified_design ,
		strata.col = 'v4609' ,
		oldwgt = 'pre_wgt'
	)
nhts_design <- 
	update( 
		nhts_design , 
		age_categories = factor( 1 + findInterval( v8005 , seq( 5 , 60 , 5 ) ) ) ,
		male = as.numeric( v0302 == 2 ) ,
		teenagers = as.numeric( v8005 > 12 & v8005 < 20 ) ,
		started_working_before_thirteen = as.numeric( v9892 < 13 )
	)
sum( weights( nhts_design , "sampling" ) != 0 )

svyby( ~ one , ~ region , nhts_design , unwtd.count )
svytotal( ~ one , nhts_design )

svyby( ~ one , ~ region , nhts_design , svytotal )
svymean( ~ v4720 , nhts_design , na.rm = TRUE )

svyby( ~ v4720 , ~ region , nhts_design , svymean , na.rm = TRUE )
svymean( ~ age_categories , nhts_design )

svyby( ~ age_categories , ~ region , nhts_design , svymean )
svytotal( ~ v4720 , nhts_design , na.rm = TRUE )

svyby( ~ v4720 , ~ region , nhts_design , svytotal , na.rm = TRUE )
svytotal( ~ age_categories , nhts_design )

svyby( ~ age_categories , ~ region , nhts_design , svytotal )
svyquantile( ~ v4720 , nhts_design , 0.5 , na.rm = TRUE )

svyby( 
	~ v4720 , 
	~ region , 
	nhts_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ started_working_before_thirteen , 
	denominator = ~ teenagers , 
	nhts_design ,
	na.rm = TRUE
)
sub_nhts_design <- subset( nhts_design , v4011 == 1 )
svymean( ~ v4720 , sub_nhts_design , na.rm = TRUE )
this_result <- svymean( ~ v4720 , nhts_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ v4720 , 
		~ region , 
		nhts_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nhts_design )
svyvar( ~ v4720 , nhts_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ v4720 , nhts_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ v4720 , nhts_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ male , nhts_design ,
	method = "likelihood" )
svyttest( v4720 ~ male , nhts_design )
svychisq( 
	~ male + age_categories , 
	nhts_design 
)
glm_result <- 
	svyglm( 
		v4720 ~ male + age_categories , 
		nhts_design 
	)

summary( glm_result )
library(convey)
nhts_design <- convey_prep( nhts_design )

sub_nhts_design <- 
	subset( 
		nhts_design , 
		!is.na( v4720 ) & v4720 != 0 & v8005 >= 15
	)

svygini( ~ v4720 , sub_nhts_design , na.rm = TRUE )
library(dbplyr)
library(srvyr)
nhts_srvyr_design <- as_survey( nhts_design )
nhts_srvyr_design %>%
	summarize( mean = survey_mean( v4720 , na.rm = TRUE ) )

nhts_srvyr_design %>%
	group_by( region ) %>%
	summarize( mean = survey_mean( v4720 , na.rm = TRUE ) )
svytotal( ~one , nhts_design )
svytotal( ~factor( v0302 ) , nhts_design )
cv( svytotal( ~factor( v0302 ) , nhts_design ) )
