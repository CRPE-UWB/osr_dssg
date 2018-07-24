Set up the workspace.

    # Uncomment the line below if you're using RStudio to run the file
    # (don't use if you're running knitr!)
    # (makes sure data files are saved in same location as this file)
    # setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

    library(tidyverse)
    library(rgdal)  # for working with spatial data frames
    library(rgeos)  # for working with spatial data frames
    library(splitstackshape)  # for splitting strings and turning into binary columns
    library(gsheet)  # only needed for the afterschool programs, to merge with annotated data

    source('open_data_functions.R')  # our functions

### Afterschool Programs

First we look at afterschool programs.

    afterSchool <- GetOpenData("afterschool_programs")

Merge the Denver Open Data with our manual annotations of program type
(done in Google Sheets), which include the same categories as
Blueprint4Summer (academic, arts, cooking, dance, drama, music, nature,
sports, stem), as well as additional annotations (which we will ignore).

    # Get the data from the google sheet
    gurl <- construct_download_url('https://docs.google.com/spreadsheets/d/1nnz6fKMPNJSIjN8eSQ2axwORCt7JV-_w0tzbsg5NkJ0/edit?usp=sharing')
    gsheetData <- as.data.frame(gsheet2tbl(gurl))

    # Merge the annotations into the original data
    afterSchoolFull <- merge(x = afterSchool, y = gsheetData)

    # Replace NA's by 0's in annotated columns.
    for (colnum in 14:29) {
      afterSchoolFull@data[is.na(afterSchoolFull@data[,colnum]),colnum] <- 0
    }

    # Sanity checks about the results
    colSums(is.na(afterSchoolFull@data)) # check for any leftover NAs

    ##            POINT_X            POINT_Y         ADDRESS_ID 
    ##                  0                  0                  0 
    ##         NEIGHBORHO           LOC_NAME         ORGANIZATI 
    ##                  0                 97                  0 
    ##         FIRST_NAME          LAST_NAME         FULL_ADDRE 
    ##                  2                  2                  0 
    ##              EMAIL                URL              PHONE 
    ##                  1                  1                  1 
    ##         DESCRIPTIO          mAcademic              mArts 
    ##                  8                  0                  0 
    ##           mCooking             mDance             mDrama 
    ##                  0                  0                  0 
    ##             mMusic            mNature            mSports 
    ##                  0                  0                  0 
    ##              mStem              mGLBT     mGirls Program 
    ##                  0                  0                  0 
    ##     mMental Health mCommunity Service        mLeadership 
    ##                  0                  0                  0 
    ##     mNoDescription             mOther             xcoord 
    ##                  0                  0                 86 
    ##             ycoord 
    ##                 86

    head(afterSchoolFull)

    ##               coordinates POINT_X POINT_Y ADDRESS_ID         NEIGHBORHO
    ## 183 (-105.0162, 39.75421) 3136021 1699964      52170     Jefferson Park
    ## 306  (-104.9727, 39.7526) 3148272 1699446      56698           Whittier
    ## 307  (-104.9727, 39.7526) 3148272 1699446      56698           Whittier
    ## 262 (-104.9871, 39.74169) 3144243 1695447     220860 North Capitol Hill
    ## 352 (-104.9574, 39.72201) 3152634 1688329     182637       Cherry Creek
    ## 353 (-104.9574, 39.72201) 3152634 1688329     182637       Cherry Creek
    ##                   LOC_NAME                                     ORGANIZATI
    ## 183                   <NA>                     Colorado Childrens Chorale
    ## 306           Whittier K-8  Lights on After School (LOAS) at Whittier K-8
    ## 307           Whittier K-8                Summer Scholars at Whittier K-8
    ## 262 YMCA - Downtown Branch                         YMCA - Downtown Branch
    ## 352    Bromwell Elementary Girl Scouts of Colorado at Bromwell Elementary
    ## 353    Bromwell Elementary        Girls on the Run at Bromwell Elementary
    ##     FIRST_NAME LAST_NAME        FULL_ADDRE                        EMAIL
    ## 183      Emily     Crile   2420 W 26th Ave    mail@childrenschorale.org
    ## 306  Catherine      Wise 2480 N Downing St    catherine_wise@dpsk12.org
    ## 307    Erienne    Massie 2480 N Downing St   emassie@summerscholars.org
    ## 262        Kim    Schulz     25 E 16th Ave        kshulz@denverymca.org
    ## 352        Joy    Henika    2500 E 4th Ave    joy.henika@gscolorado.org
    ## 353       Lisa   Johnson    2500 E 4th Ave info@girlsontherundenver.org
    ##                              URL          PHONE
    ## 183     www.childrenschorale.org (303)-892-5600
    ## 306               www.dpsk12.org (720)-424-8256
    ## 307       www.summerscholars.org (720)-424-3075
    ## 262           www.denverymca.org (303)-861-8300
    ## 352 www.girlscoutsofcolorado.org (303)-778-8774
    ## 353  www.girlsontherundenver.org (720)-530-1064
    ##                                                                                                               DESCRIPTIO
    ## 183                                                   Participants receive instruction in choral singing and performing.
    ## 306                                                             Various activities supported through the LOAS Initiative
    ## 307                       Free afterschool tutoring| homework help| and enrichment activities| 5 days/wk for grades 1-5.
    ## 262                                                                                                 Summer Day Camp Only
    ## 352                                    Builds girls of courage| confidence and caring who make the world a better place.
    ## 353 Emotional| social| mental| spiritual| and physical development for girls ages 8-13 as they train for a 3.1 mile run.
    ##     mAcademic mArts mCooking mDance mDrama mMusic mNature mSports mStem
    ## 183         0     1        0      0      0      0       0       0     0
    ## 306         0     0        0      0      0      0       0       0     0
    ## 307         1     0        0      0      0      0       0       0     0
    ## 262         0     0        0      0      0      0       0       0     0
    ## 352         0     0        0      0      0      0       0       0     0
    ## 353         0     0        0      0      0      0       0       1     0
    ##     mGLBT mGirls.Program mMental.Health mCommunity.Service mLeadership
    ## 183     0              0              0                  0           0
    ## 306     0              0              0                  0           0
    ## 307     0              0              0                  0           0
    ## 262     0              0              0                  0           0
    ## 352     0              1              0                  0           1
    ## 353     0              1              1                  0           0
    ##     mNoDescription mOther    xcoord   ycoord
    ## 183              0      0 -105.0162 39.75421
    ## 306              0      1 -104.9727 39.75260
    ## 307              0      0 -104.9727 39.75260
    ## 262              0      0        NA       NA
    ## 352              0      0 -104.9574 39.72201
    ## 353              0      0 -104.9574 39.72201

Next, subset to only the variables existing in the Blueprint4Summer
data, and rewrite column names to be more understandable.

    afterSchoolFinal <- afterSchoolFull[, c('LOC_NAME', 'ORGANIZATI', 'mAcademic', 'mArts', 'mCooking', 'mDance', 'mDrama', 'mMusic', 'mNature', 'mSports', 'mStem', 'mGirls Program', 'DESCRIPTIO')]

    colnames(afterSchoolFinal@data) <- c('location', 'organization', 'has_academic', 'has_arts', 'has_cooking', 'has_dance', 'has_drama', 'has_music', 'has_nature', 'has_sports', 'has_stem', 'girls_only', 'description')

    head(afterSchoolFinal)

    ##               coordinates               location
    ## 183 (-105.0162, 39.75421)                   <NA>
    ## 306  (-104.9727, 39.7526)           Whittier K-8
    ## 307  (-104.9727, 39.7526)           Whittier K-8
    ## 262 (-104.9871, 39.74169) YMCA - Downtown Branch
    ## 352 (-104.9574, 39.72201)    Bromwell Elementary
    ## 353 (-104.9574, 39.72201)    Bromwell Elementary
    ##                                       organization has_academic has_arts
    ## 183                     Colorado Childrens Chorale            0        1
    ## 306  Lights on After School (LOAS) at Whittier K-8            0        0
    ## 307                Summer Scholars at Whittier K-8            1        0
    ## 262                         YMCA - Downtown Branch            0        0
    ## 352 Girl Scouts of Colorado at Bromwell Elementary            0        0
    ## 353        Girls on the Run at Bromwell Elementary            0        0
    ##     has_cooking has_dance has_drama has_music has_nature has_sports
    ## 183           0         0         0         0          0          0
    ## 306           0         0         0         0          0          0
    ## 307           0         0         0         0          0          0
    ## 262           0         0         0         0          0          0
    ## 352           0         0         0         0          0          0
    ## 353           0         0         0         0          0          1
    ##     has_stem girls_only
    ## 183        0          0
    ## 306        0          0
    ## 307        0          0
    ## 262        0          0
    ## 352        0          1
    ## 353        0          1
    ##                                                                                                              description
    ## 183                                                   Participants receive instruction in choral singing and performing.
    ## 306                                                             Various activities supported through the LOAS Initiative
    ## 307                       Free afterschool tutoring| homework help| and enrichment activities| 5 days/wk for grades 1-5.
    ## 262                                                                                                 Summer Day Camp Only
    ## 352                                    Builds girls of courage| confidence and caring who make the world a better place.
    ## 353 Emotional| social| mental| spiritual| and physical development for girls ages 8-13 as they train for a 3.1 mile run.

### Rec Centers

Next, we look at recreation centers.

    recCenters <- GetOpenData("recreation_centers")

    colnames(recCenters@data)

    ##  [1] "REC_NAME"   "LOC_CODE"   "ADDRESS_ID" "ADDRESS_LI" "ADDRESS__1"
    ##  [6] "CITY"       "STATE"      "ZIP"        "PHONE"      "REC_TYPE"  
    ## [11] "MARKETING_" "MARKETED_F" "MARKETED_P" "PROGRAMS_L" "POOL_HOURS"
    ## [16] "NEWS_LINK"  "HOURS"      "PHOTO"      "YEAR_BUILT" "YEAR_REMOD"
    ## [21] "BLDG_SQFT"  "LABEL"      "LATITUDE"   "LONGITUDE"  "FACILITIES"

    head(recCenters)

    ##             coordinates                       REC_NAME LOC_CODE ADDRESS_ID
    ## 1 (-105.0197, 39.68551)       Athmar Recreation Center      RA2          0
    ## 2 (-105.0045, 39.77751)       Aztlan Recreation Center      RA3          0
    ## 3  (-105.028, 39.72226)       Barnum Recreation Center      RB1          0
    ## 4 (-104.9944, 39.71571)   La Familia Recreation Center      RL2          0
    ## 5 (-105.0232, 39.67144) College View Recreation Center      RC2          0
    ## 6  (-104.9638, 39.7841)      Johnson Recreation Center      RJ1          0
    ##          ADDRESS_LI ADDRESS__1   CITY STATE  ZIP        PHONE REC_TYPE
    ## 1 2680 W Mexico Ave       <NA> Denver    CO <NA> 720-865-2180        R
    ## 2    4435 Navajo St       <NA> Denver    CO <NA> 720-865-4380        R
    ## 3     360 Hooker St       <NA> Denver    CO <NA> 720-865-0350        R
    ## 4     65 S Elati St       <NA> Denver    CO <NA> 720-865-2170        R
    ## 5 2525 S Decatur St       <NA> Denver    CO <NA> 303-937-4630        T
    ## 6      4809 Race St       <NA> Denver    CO <NA> 720-865-0600        T
    ##   MARKETING_
    ## 1         RF
    ## 2         RF
    ## 3         RF
    ## 4         RF
    ## 5          T
    ## 6          T
    ##                                                                              MARKETED_F
    ## 1  Dance Space, Gym (Small), Lockers, Meeting Room, Pool (Indoor), Showers, Weight Room
    ## 2              Gym (Small), Lockers, Pool (Outdoor), Showers, Weight Room, Meeting Room
    ## 3 Dance Space, Gym (Large), Lockers, Meeting Room, Pool (Outdoor), Showers, Weight Room
    ## 4               Gym (Large), Lockers, Meeting Room, Pool (Indoor), Showers, Weight Room
    ## 5                       This facility is owned by the City but operated by third party.
    ## 6                       This facility is owned by the City but operated by third party.
    ##                                                                                                                                                   MARKETED_P
    ## 1 Aquatics, Arts & Culture, Day Activity or Sports Camps, Education, Fitness & Health, Seniors, Social Enrichment Clubs & Activities, Special Events, Sports
    ## 2                               Aquatics, Arts & Culture, Education, Fitness & Health, Seniors, Social Enrichment Clubs & Activities, Special Events, Sports
    ## 3                               Aquatics, Arts & Culture, Education, Fitness & Health, Seniors, Social Enrichment Clubs & Activities, Special Events, Sports
    ## 4                                                                                Aquatics, Arts & Culture, Fitness & Health, Seniors, Special Events, Sports
    ## 5           Arts & Culture, Day Activity or Sports Camps, Education, Fitness & Health, Seniors, Social Enrichment Clubs & Activities, Special Events, Sports
    ## 6                                         Arts & Culture, Education, Fitness & Health, Seniors, Social Enrichment Clubs & Activities, Special Events, Sports
    ##                PROGRAMS_L POOL_HOURS
    ## 1 documents/southwest.pdf       <NA>
    ## 2 documents/northwest.pdf       <NA>
    ## 3 documents/southwest.pdf       <NA>
    ## 4 documents/southeast.pdf       <NA>
    ## 5   documents/partner.pdf       <NA>
    ## 6   documents/partner.pdf       <NA>
    ##                                                                                                                                                    NEWS_LINK
    ## 1     www.denvergov.org/content/denvergov/en/denver-parks-and-recreation/recreation-centers-pools/recreation-centers-schedules/athmar-recreation-center.html
    ## 2     www.denvergov.org/content/denvergov/en/denver-parks-and-recreation/recreation-centers-pools/recreation-centers-schedules/aztlan-recreation-center.html
    ## 3     www.denvergov.org/content/denvergov/en/denver-parks-and-recreation/recreation-centers-pools/recreation-centers-schedules/barnum-recreation-center.html
    ## 4 www.denvergov.org/content/denvergov/en/denver-parks-and-recreation/recreation-centers-pools/recreation-centers-schedules/la-familia-recreation-center.html
    ## 5                                                                                                                               http://dicp.org/college-view
    ## 6                                                                                                                                  http://coloradominers.org
    ##                                         HOURS
    ## 1     documents/Recreation/Regional_Hours.pdf
    ## 2 documents/Recreation/Neighborhood_Hours.pdf
    ## 3 documents/Recreation/Neighborhood_Hours.pdf
    ## 4        documents/Recreation/Local_Hours.pdf
    ## 5                                        <NA>
    ## 6                                        <NA>
    ##                              PHOTO YEAR_BUILT YEAR_REMOD BLDG_SQFT
    ## 1      images/recctrs/athmarrc.jpg    Unknown       1996     23391
    ## 2      images/recctrs/aztlanrc.jpg       1975    Unknown     10188
    ## 3      images/recctrs/barnumrc.jpg       1968 1976, 1978     19494
    ## 4   images/recctrs/lafamiliarc.jpg       1970       1997     21864
    ## 5 images/recctrs/collegeviewrc.jpg       1975 1981, 1987     15953
    ## 6     images/recctrs/johnsonrc.jpg     1900's 1977, 1982     11373
    ##          LABEL LATITUDE LONGITUDE
    ## 1       Athmar 39.68551 -105.0197
    ## 2       Aztlan 39.77751 -105.0045
    ## 3       Barnum 39.72226 -105.0280
    ## 4   La Familia 39.71571 -104.9944
    ## 5 College View 39.67144 -105.0232
    ## 6      Johnson 39.78410 -104.9638
    ##                                                                                                                                                                         FACILITIES
    ## 1                                                                                                                         Indoor Pool,Gym (Reg. Size),Weight Room,Aerobics,Kitchen
    ## 2                                                                         Outdoor Pool,Gym (Reg. Size),Weight Room,Aerobics,Arts & Crafts,Pottery/Kiln,Game Room,Billiards,Kitchen
    ## 3        Senior Center,,Outdoor Pool,Gym (Reg. Size),Weight Room,Cardio Eqpmt,Aerobics,Climbing Wall,Arts & Crafts,Pottery/Kiln,Computer Lab,Billiards,Dance,Meeting space,Kitchen
    ## 4 Senior Center,Indoor Pool,Indoor Kiddie Pool,Gym (Reg. Size),Weight Room,Cardio Eqpmt,Aerobics,Arts & Crafts,Pottery/Kiln,Game Room,Tennis Court,Billiards,Meeting space,Kitchen
    ## 5                         Senior Center,,Gym (Reg. Size),Weight Room,Cardio Eqpmt,Aerobics,Arts & Crafts,Pottery/Kiln,Game Room,Computer Lab,Billiards,Dance,Meeting space,Kitchen
    ## 6         Senior Center,,Gym (Non Reg. Size),Weight Room,Cardio Eqpmt,Aerobics,Arts & Crafts,Pottery/Kiln,Game Room,Racquetball,Computer Lab,Billiards,Dance,Meeting space,Kitchen

Delete unuseful columns (urls, links to pdfs and photos, address info,
contact info, hours) (Note: 'FACILITIES' is an old version of
'MARKETED\_F' which is marketed facilities.)

    recSmall <- recCenters[, c('REC_NAME', 'REC_TYPE', 'MARKETED_F', 'MARKETED_P', 'YEAR_BUILT', 'YEAR_REMOD', 'BLDG_SQFT', 'LABEL')]
    colnames(recSmall@data) <- c('name', 'type', 'FACILITIES', 'PROGRAMS', 'year_built', 'year_last_remodeled', 'bldg_sqft', 'short_name')
    head(recSmall)

    ##             coordinates                           name type
    ## 1 (-105.0197, 39.68551)       Athmar Recreation Center    R
    ## 2 (-105.0045, 39.77751)       Aztlan Recreation Center    R
    ## 3  (-105.028, 39.72226)       Barnum Recreation Center    R
    ## 4 (-104.9944, 39.71571)   La Familia Recreation Center    R
    ## 5 (-105.0232, 39.67144) College View Recreation Center    T
    ## 6  (-104.9638, 39.7841)      Johnson Recreation Center    T
    ##                                                                              FACILITIES
    ## 1  Dance Space, Gym (Small), Lockers, Meeting Room, Pool (Indoor), Showers, Weight Room
    ## 2              Gym (Small), Lockers, Pool (Outdoor), Showers, Weight Room, Meeting Room
    ## 3 Dance Space, Gym (Large), Lockers, Meeting Room, Pool (Outdoor), Showers, Weight Room
    ## 4               Gym (Large), Lockers, Meeting Room, Pool (Indoor), Showers, Weight Room
    ## 5                       This facility is owned by the City but operated by third party.
    ## 6                       This facility is owned by the City but operated by third party.
    ##                                                                                                                                                     PROGRAMS
    ## 1 Aquatics, Arts & Culture, Day Activity or Sports Camps, Education, Fitness & Health, Seniors, Social Enrichment Clubs & Activities, Special Events, Sports
    ## 2                               Aquatics, Arts & Culture, Education, Fitness & Health, Seniors, Social Enrichment Clubs & Activities, Special Events, Sports
    ## 3                               Aquatics, Arts & Culture, Education, Fitness & Health, Seniors, Social Enrichment Clubs & Activities, Special Events, Sports
    ## 4                                                                                Aquatics, Arts & Culture, Fitness & Health, Seniors, Special Events, Sports
    ## 5           Arts & Culture, Day Activity or Sports Camps, Education, Fitness & Health, Seniors, Social Enrichment Clubs & Activities, Special Events, Sports
    ## 6                                         Arts & Culture, Education, Fitness & Health, Seniors, Social Enrichment Clubs & Activities, Special Events, Sports
    ##   year_built year_last_remodeled bldg_sqft   short_name
    ## 1    Unknown                1996     23391       Athmar
    ## 2       1975             Unknown     10188       Aztlan
    ## 3       1968          1976, 1978     19494       Barnum
    ## 4       1970                1997     21864   La Familia
    ## 5       1975          1981, 1987     15953 College View
    ## 6     1900's          1977, 1982     11373      Johnson

    # there are some weird entries in year_built and year_remodeled - fix these
    levels(recSmall@data$year_built) <- gsub("Unknown", NA, levels(recSmall@data$year_built))
    levels(recSmall@data$year_built) <- gsub("1900's", "1900", levels(recSmall@data$year_built))
    levels(recSmall@data$year_built) <- gsub("1960's", "1960", levels(recSmall@data$year_built))

    levels(recSmall@data$year_last_remodeled) <- gsub("Unknown", NA, levels(recSmall@data$year_last_remodeled))
    levels(recSmall@data$year_last_remodeled) <- gsub("None", NA, levels(recSmall@data$year_last_remodeled))
    levels(recSmall@data$year_last_remodeled) <- gsub("\\d{4}, ", "", levels(recSmall@data$year_last_remodeled))

Split up the facility categories (currently contains lists of facilities
in a single column) into separate, binary columns. Only keep the
meaningful columns.

    # Turn each facility type into a column
    recFinal <- SplitCommas(recSmall, 'FACILITIES')
    colnames(recFinal@data)

    ##  [1] "name"                                                                                           
    ##  [2] "type"                                                                                           
    ##  [3] "PROGRAMS"                                                                                       
    ##  [4] "year_built"                                                                                     
    ##  [5] "year_last_remodeled"                                                                            
    ##  [6] "bldg_sqft"                                                                                      
    ##  [7] "short_name"                                                                                     
    ##  [8] "FACILITIES_Aerobics"                                                                            
    ##  [9] "FACILITIES_Cardio.Eqpmnt"                                                                       
    ## [10] "FACILITIES_Cardio.Eqpmt"                                                                        
    ## [11] "FACILITIES_Climbing.Wall"                                                                       
    ## [12] "FACILITIES_Dance.Space"                                                                         
    ## [13] "FACILITIES_Event.Space"                                                                         
    ## [14] "FACILITIES_Gym..Large."                                                                         
    ## [15] "FACILITIES_Gym..Reg..Size."                                                                     
    ## [16] "FACILITIES_Gym..Small."                                                                         
    ## [17] "FACILITIES_Indoor.Kiddie.Pool"                                                                  
    ## [18] "FACILITIES_Indoor.Pool"                                                                         
    ## [19] "FACILITIES_Indoor.Track"                                                                        
    ## [20] "FACILITIES_Kid.Watch"                                                                           
    ## [21] "FACILITIES_Kid.Watch.Area"                                                                      
    ## [22] "FACILITIES_Kitchen"                                                                             
    ## [23] "FACILITIES_Lockers"                                                                             
    ## [24] "FACILITIES_Meeting.Room"                                                                        
    ## [25] "FACILITIES_Meeting.Space"                                                                       
    ## [26] "FACILITIES_Pool..Indoor."                                                                       
    ## [27] "FACILITIES_Pool..Outdoor."                                                                      
    ## [28] "FACILITIES_Racquetball"                                                                         
    ## [29] "FACILITIES_Showers"                                                                             
    ## [30] "FACILITIES_Spin.Room"                                                                           
    ## [31] "FACILITIES_Suana"                                                                               
    ## [32] "FACILITIES_This.facility.is.in.transition.and.currently.serves.as.Citywide.Sports.headquarters."
    ## [33] "FACILITIES_This.facility.is.owned.by.the.City.but.operated.by.third.party."                     
    ## [34] "FACILITIES_Water.Feature"                                                                       
    ## [35] "FACILITIES_Weight.Room"

    # Compute interesting facility distinctions: HAS_CARDIO, HAS_WEIGHTS, HAS_POOL
    recFinal@data$has_cardio <- pmax(recFinal@data$FACILITIES_Aerobics, recFinal@data$FACILITIES_Cardio.Eqpmnt, recFinal@data$FACILITIES_Cardio.Eqpmt)

    recFinal@data$has_weights <- recFinal@data$FACILITIES_Weight.Room

    # we won't use pools anymore, since there is a separate dataset about pools!
    # recFinal@data$has_pool <- pmax(recFinal@data$FACILITIES_Pool..Indoor., recFinal@data$FACILITIES_Pool..Outdoor.,recFinal@data$FACILITIES_Indoor.Kiddie.Pool)

    recFinal@data$has_gym <- pmax(recFinal@data$FACILITIES_Gym..Large., recFinal@data$FACILITIES_Gym..Reg..Size.,recFinal@data$FACILITIES_Gym..Small.)

    # Delete unnecessary columns
    recFinal@data[,8:35] <- NULL

Similarly, split up the program categories into separate, binary
columns.

    # Turn each facility type into a column
    recFinal <- SplitCommas(recFinal, 'PROGRAMS')
    colnames(recFinal@data)

    ##  [1] "name"                                         
    ##  [2] "type"                                         
    ##  [3] "year_built"                                   
    ##  [4] "year_last_remodeled"                          
    ##  [5] "bldg_sqft"                                    
    ##  [6] "short_name"                                   
    ##  [7] "has_cardio"                                   
    ##  [8] "has_weights"                                  
    ##  [9] "has_gym"                                      
    ## [10] "PROGRAMS_Aquatics"                            
    ## [11] "PROGRAMS_Aquatics..using.Wash.Park.Pool."     
    ## [12] "PROGRAMS_Arts...Culture"                      
    ## [13] "PROGRAMS_Day.Activity.or.Sports.Camps"        
    ## [14] "PROGRAMS_Education"                           
    ## [15] "PROGRAMS_Fitness...Health"                    
    ## [16] "PROGRAMS_Seniors"                             
    ## [17] "PROGRAMS_Social.Enrichment.Clubs...Activities"
    ## [18] "PROGRAMS_Special.Events"                      
    ## [19] "PROGRAMS_Sports"

    # Combine aquatics programs into one column
    recFinal@data$has_aquatics <- pmax(recFinal@data$PROGRAMS_Aquatics, recFinal@data$PROGRAMS_Aquatics..using.Wash.Park.Pool.)
    recFinal@data[,10:11] <- NULL

    # Rename the rest of the binary columns
    colnames(recFinal@data)[10:17] <- c("has_arts_culture", "has_day_camps", "has_educ_programs", "has_fitness_health_programs", "has_senior_programs", "has_social_enrich_clubs", "has_special_events", "has_sports")

    head(recFinal)

    ##             coordinates                           name type year_built
    ## 1 (-105.0197, 39.68551)       Athmar Recreation Center    R       <NA>
    ## 2 (-105.0045, 39.77751)       Aztlan Recreation Center    R       1975
    ## 3  (-105.028, 39.72226)       Barnum Recreation Center    R       1968
    ## 4 (-104.9944, 39.71571)   La Familia Recreation Center    R       1970
    ## 5 (-105.0232, 39.67144) College View Recreation Center    T       1975
    ## 6  (-104.9638, 39.7841)      Johnson Recreation Center    T       1900
    ##   year_last_remodeled bldg_sqft   short_name has_cardio has_weights
    ## 1                1996     23391       Athmar          0           1
    ## 2                <NA>     10188       Aztlan          0           1
    ## 3                1978     19494       Barnum          0           1
    ## 4                1997     21864   La Familia          0           1
    ## 5                1987     15953 College View          0           0
    ## 6                1982     11373      Johnson          0           0
    ##   has_gym has_arts_culture has_day_camps has_educ_programs
    ## 1       1                1             1                 1
    ## 2       1                1             0                 1
    ## 3       1                1             0                 1
    ## 4       1                1             0                 0
    ## 5       0                1             1                 1
    ## 6       0                1             0                 1
    ##   has_fitness_health_programs has_senior_programs has_social_enrich_clubs
    ## 1                           1                   1                       1
    ## 2                           1                   1                       1
    ## 3                           1                   1                       1
    ## 4                           1                   1                       0
    ## 5                           1                   1                       1
    ## 6                           1                   1                       1
    ##   has_special_events has_sports has_aquatics
    ## 1                  1          1            1
    ## 2                  1          1            1
    ## 3                  1          1            1
    ## 4                  1          1            1
    ## 5                  1          1            0
    ## 6                  1          1            0

### Athletic fields

Next, look at athletic fields.

    fields <- GetOpenData("athletic_fields")

    colnames(fields@data)

    ##  [1] "FEATURE"    "LOCATION"   "LOC_CODE"   "FIELD_NUMB" "OVERLAP"   
    ##  [6] "SURFACE_TY" "LIGHTS"     "FENCED"     "FIELD_TIER" "FIELD_SIZE"
    ## [11] "NOTES"      "ASSET_CLAS" "CLASS_CATE" "GIS_ASSET_" "INFOR_ASSE"

    head(fields@data)

    ##      FEATURE                LOCATION LOC_CODE FIELD_NUMB OVERLAP
    ## 0   Softball      Village Place Park      540        SB1     SC1
    ## 1   Softball    City of Nairobi Park      217        SB1     N/A
    ## 2     Soccer  Capitol Hill Reservoir      303        SC5     N/A
    ## 3 Volleyball            Garland Park      728        VB2     N/A
    ## 4 Volleyball        Observatory Park      818        VB1     N/A
    ## 5   Softball Lindsley (Henry S) Park      401        SB1     N/A
    ##        SURFACE_TY LIGHTS FENCED FIELD_TIER FIELD_SIZE      NOTES
    ## 0 Skinned Infield     No     No          C       <NA>       <NA>
    ## 1            Turf     No     No          C       <NA>       <NA>
    ## 2            Turf     No   <NA>          C      60x90 Youth Only
    ## 3            Turf     No   <NA>          C    20 nets       <NA>
    ## 4            Turf     No   <NA>          C    20 nets       <NA>
    ## 5 Skinned Infield     No     No          C       <NA> Youth Only
    ##   ASSET_CLAS CLASS_CATE GIS_ASSET_ INFOR_ASSE
    ## 0   ATHFIELD       BF-C      15045     111348
    ## 1   ATHFIELD       BF-C      15158     111513
    ## 2   ATHFIELD    MU-TURF      15103     111534
    ## 3   ATHFIELD    MU-TURF      15162     111442
    ## 4   ATHFIELD    MU-TURF      15201     111469
    ## 5   ATHFIELD       BF-C      15070     111367

Subset to useful variables and rename columns for simplicity.

    fieldsFinal <- fields[, c('FEATURE', 'LOCATION', 'FIELD_TIER', 'CLASS_CATE')]
    colnames(fieldsFinal@data) <- c('sport', 'location', 'tier', 'class')
    head(fieldsFinal@data)

    ##        sport                location tier   class
    ## 0   Softball      Village Place Park    C    BF-C
    ## 1   Softball    City of Nairobi Park    C    BF-C
    ## 2     Soccer  Capitol Hill Reservoir    C MU-TURF
    ## 3 Volleyball            Garland Park    C MU-TURF
    ## 4 Volleyball        Observatory Park    C MU-TURF
    ## 5   Softball Lindsley (Henry S) Park    C    BF-C

### Playgrounds

Next, look at playgrounds.

    playgrounds <- GetOpenData("playgrounds")

    colnames(playgrounds@data)

    ##  [1] "LOCATION"   "LOC_CODE"   "PG_CODE"    "SURFACE_TY" "YEAR_REHAB"
    ##  [6] "NOTES"      "ASSET_CLAS" "CLASS_CATE" "GIS_ASSET_" "INFOR_ASSE"

    head(playgrounds@data)

    ##             LOCATION LOC_CODE PG_CODE SURFACE_TY YEAR_REHAB NOTES
    ## 0             Walker      227 227 - 1 Wood Fiber       2010  <NA>
    ## 1  City of Axum Park      417 417 - 1 Wood Fiber       2010  <NA>
    ## 2    41st & Ensenada      524 524 - 1 Wood Fiber       2004  <NA>
    ## 3 Pferdesteller Park      017 017 - 1 Wood Fiber       2010  <NA>
    ## 4  Lowry Sports Park      L10 L10 - 1 Wood Fiber       2007  <NA>
    ## 5    Elemendorf Park      536 536 - 1 Wood Fiber       2010  <NA>
    ##   ASSET_CLAS      CLASS_CATE GIS_ASSET_ INFOR_ASSE
    ## 0         PG PG-NEIGHBORHOOD      40137     111304
    ## 1         PG PG-NEIGHBORHOOD      40138     111305
    ## 2         PG    PG-COMMUNITY      40139     111181
    ## 3         PG PG-NEIGHBORHOOD      40140     111306
    ## 4         PG    PG-COMMUNITY      40141     111182
    ## 5         PG PG-NEIGHBORHOOD      40143     111307

Subset to useful variables and rename columns for simplicity.

    playgroundsSmall <- playgrounds[, c('LOCATION', 'YEAR_REHAB', 'CLASS_CATE')]
    colnames(playgroundsSmall@data) <- c('location', 'year_rehab', 'class')
    head(playgroundsSmall@data)

    ##             location year_rehab           class
    ## 0             Walker       2010 PG-NEIGHBORHOOD
    ## 1  City of Axum Park       2010 PG-NEIGHBORHOOD
    ## 2    41st & Ensenada       2004    PG-COMMUNITY
    ## 3 Pferdesteller Park       2010 PG-NEIGHBORHOOD
    ## 4  Lowry Sports Park       2007    PG-COMMUNITY
    ## 5    Elemendorf Park       2010 PG-NEIGHBORHOOD

    # There are weird values in year_rehab - fix them
    levels(playgroundsSmall@data$year_rehab)

    ##  [1] "1988"                   "1989"                  
    ##  [3] "1990"                   "1991"                  
    ##  [5] "1992"                   "1993"                  
    ##  [7] "1994"                   "1995"                  
    ##  [9] "1996"                   "1997"                  
    ## [11] "1998"                   "1999"                  
    ## [13] "2000"                   "2001"                  
    ## [15] "2002"                   "2003"                  
    ## [17] "2004"                   "2005"                  
    ## [19] "2006"                   "2007"                  
    ## [21] "2008"                   "2009"                  
    ## [23] "2010"                   "2011"                  
    ## [25] "2012"                   "2013"                  
    ## [27] "2014"                   "2015"                  
    ## [29] "2015 - partial upgrade" "2016"                  
    ## [31] "2017"                   "unknown"

    levels(playgroundsSmall@data$year_rehab) <- gsub("unknown", NA, levels(playgroundsSmall@data$year_rehab))
    levels(playgroundsSmall@data$year_rehab) <- gsub("2015 - partial upgrade", "2015", levels(playgroundsSmall@data$year_rehab))

    playgroundsFinal <- playgroundsSmall

### Skate Parks

Next, look at skate parks. There are only 5 of them.

    skateParks <- GetOpenData("skate_parks")

Look at data and subset to useful values, then save.

    head(skateParks@data)

    ##            LOCATION LOC_CODE ASSET_CLAS CLASS_CATE GIS_ASSET_ INFOR_ASSE
    ## 0 La Alma / Lincoln      315     SKATEP     SK8-SM          0       <NA>
    ## 1       Town Center      558     SKATEP     SK8-SM      50001     109864
    ## 2         Parkfield      516     SKATEP     SK8-SM      50002     109862
    ## 3     Greenway Park      556     SKATEP     SK8-SM      50003     109861
    ## 4 Denver Skate Park      924     SKATEP     SK8-LG      50000     109865
    ## 5            Elyria      204     SKATEP     SK8-SM      50004     109863

    skateParksFinal <- skateParks[,c(1,4)]
    colnames(skateParksFinal@data) <- c('location', 'size')
    levels(skateParksFinal@data$size) <- c("large", "small")
    head(skateParksFinal@data)

    ##            location  size
    ## 0 La Alma / Lincoln small
    ## 1       Town Center small
    ## 2         Parkfield small
    ## 3     Greenway Park small
    ## 4 Denver Skate Park large
    ## 5            Elyria small

### Rec Court Surfaces

Next look at recreational court surfaces. From the description on Denver
Open Data, these are: "Polygon representation of recreational courts or
other playing surfaces such as basketball, tennis, handball, bocce, sand
volleyball, horseshoepits, and lawn bowling in parks, golf courses, and
other areas maintained by the Department of Parks and Recreation in the
City and County of Denver."

    courts <- GetOpenData("recreational_court_surfaces")

Look at data and subset to useful values, then save the result.

    head(courts@data)

    ##         FEATURE                 LOCATION LOC_CODE COURT_NUM LIGHTS
    ## 0 Horseshoe Pit Rocky Mountain Lake Park      007      <NA>   <NA>
    ## 1 Horseshoe Pit      Russell Square Park      206      <NA>   <NA>
    ## 2 Horseshoe Pit      Russell Square Park      206      <NA>   <NA>
    ## 3 Horseshoe Pit              Athmar Park      802      <NA>   <NA>
    ## 4 Horseshoe Pit              Athmar Park      802      <NA>   <NA>
    ## 5 Horseshoe Pit          Washington Park      711      <NA>   <NA>
    ##        SURFACE_TY YEAR_BUILT YEAR_RESUR ASSET_CLAS CLASS_CATE GIS_ASSET_
    ## 0 Natural Surface       <NA>       <NA>      COURT   CT-HORSE      20313
    ## 1 Natural Surface       <NA>       <NA>      COURT   CT-HORSE      20252
    ## 2 Natural Surface       <NA>       <NA>      COURT   CT-HORSE      20292
    ## 3 Natural Surface       <NA>       <NA>      COURT   CT-HORSE      20253
    ## 4 Natural Surface       <NA>       <NA>      COURT   CT-HORSE      20316
    ## 5 Natural Surface       <NA>       <NA>      COURT   CT-HORSE      20293
    ##   INFOR_ASSE
    ## 0     111722
    ## 1     111575
    ## 2     111670
    ## 3     111576
    ## 4     111748
    ## 5     111671

    courtsSmall <- courts[,c(1,2,7,8)]
    colnames(courtsSmall@data) <- c('sport', 'location', 'year_built', 'year_resurfaced')

    # Some null entries are specified as <Null> for some reason - replace these with NAs
    levels(courtsSmall@data$year_resurfaced) <- gsub("<Null>", NA, levels(courtsSmall@data$year_resurfaced))

    # Also weirdly one year is labeled as 2014 - Patch... replace with 2014
    levels(courtsSmall@data$year_resurfaced) <- gsub("2014 - Patch", "2014", levels(courtsSmall@data$year_resurfaced))

    courtsFinal <- courtsSmall
    head(courtsFinal@data)

    ##           sport                 location year_built year_resurfaced
    ## 0 Horseshoe Pit Rocky Mountain Lake Park       <NA>            <NA>
    ## 1 Horseshoe Pit      Russell Square Park       <NA>            <NA>
    ## 2 Horseshoe Pit      Russell Square Park       <NA>            <NA>
    ## 3 Horseshoe Pit              Athmar Park       <NA>            <NA>
    ## 4 Horseshoe Pit              Athmar Park       <NA>            <NA>
    ## 5 Horseshoe Pit          Washington Park       <NA>            <NA>

### Libraries

Next we look at libraries.

    libraries <- GetOpenData("libraries")

A lot of this info is redundant, such as abbreviations,
address/state/zip (which is contained in the shapefile metadata). Other
info is unnecessary, such as the status (whether under construction,
temporary construction, etc). We also will rename the columns to
understandable names.

Look at the data, subset, retype columns, and then save result.

    head(libraries)

    ##             coordinates            LIBRARY_NA BRANCH_COD ADDRESS_ID
    ## 1 (-104.9871, 39.71477)         Ross-Broadway        BDY      80946
    ## 2 (-104.9378, 39.66542) Ross-University Hills        UNH     143542
    ## 3 (-104.9522, 39.72106)     Ross-Cherry Creek        CRK      84797
    ## 4 (-104.9593, 39.70183)          Eugene Field        FIE      85982
    ## 5 (-105.0252, 39.76279)              Woodbury        WDB      46116
    ## 6  (-104.9624, 39.7818)          Valdez-Perry        VAL     179029
    ##              ADDRESS_LI ADDRESS__1   CITY STATE   ZIP DOOR_COUNT
    ## 1       33 E Bayaud Ave       <NA> Denver    CO 80209      78909
    ## 2    4310 E Amherst Ave       <NA> Denver    CO 80222     191607
    ## 3    305 N Milwaukee St       <NA> Denver    CO 80206     195128
    ## 4 810 S University Blvd       <NA> Denver    CO 80209     112537
    ## 5   3265 N Federal Blvd       <NA> Denver    CO 80211     117615
    ## 6        4690 N Vine St       <NA> Denver    CO 80216      50075
    ##   CIRCULATIO TOTAL_BLDG STATUS
    ## 1     183930       4500   <NA>
    ## 2     641750      21143   <NA>
    ## 3     333433      17808   <NA>
    ## 4     303551      10500   <NA>
    ## 5     288001      10023   <NA>
    ## 6      46111       4883   <NA>

    librariesFinal <- libraries[,c(1,9,10,11)]
    names(librariesFinal) <- c("name","patron_count","circulation_volume","sqft")

    # make sure the types are correct
    librariesFinal[["name"]] <- as.character(librariesFinal[["name"]])
    librariesFinal[["patron_count"]] <- as.numeric(as.character(librariesFinal[["patron_count"]]))
    librariesFinal[["circulation_volume"]] <- as.numeric(as.character(librariesFinal[["circulation_volume"]]))
    librariesFinal[["sqft"]] <- as.numeric(as.character(librariesFinal[["sqft"]]))

    head(librariesFinal)

    ##             coordinates                  name patron_count
    ## 1 (-104.9871, 39.71477)         Ross-Broadway        78909
    ## 2 (-104.9378, 39.66542) Ross-University Hills       191607
    ## 3 (-104.9522, 39.72106)     Ross-Cherry Creek       195128
    ## 4 (-104.9593, 39.70183)          Eugene Field       112537
    ## 5 (-105.0252, 39.76279)              Woodbury       117615
    ## 6  (-104.9624, 39.7818)          Valdez-Perry        50075
    ##   circulation_volume  sqft
    ## 1             183930  4500
    ## 2             641750 21143
    ## 3             333433 17808
    ## 4             303551 10500
    ## 5             288001 10023
    ## 6              46111  4883

### Swimming Pools

Next we look at swimming pools.

    pools <- GetOpenData("swimming_pools")

Look at and subset the data.

    poolsFinal <- pools[,c(1,3,11)]
    names(poolsFinal) <- c("name","type","location")
    head(poolsFinal)

    ##             coordinates                                    name   type
    ## 1 (-104.9705, 39.70263)       Washington Park Recreation Center Indoor
    ## 2 (-104.9093, 39.77072) Martin Luther King Jr Recreation Center Indoor
    ## 3 (-104.8947, 39.72738)             Montclair Recreation Center Indoor
    ## 4 (-104.8056, 39.79274)             Montbello Recreation Center Indoor
    ## 5   (-104.99, 39.75137)      Twentieth Street Recreation Center Indoor
    ## 6 (-104.9218, 39.76429)     Hiawatha Davis Jr Recreation Center Indoor
    ##                             location
    ## 1                    Washington Park
    ## 2         Martin Luther King Jr Park
    ## 3         Monclair Recreation Center
    ## 4                          Parkfield
    ## 5 Twentieth Street Recreation Center
    ## 6                       Skyland Park

### Licensed Childcare Facilities

Next we look at childcare facilities.

    care <- GetOpenData("licensed_child_care_facilities")

Look at and subset data, then save.

    summary(care)

    ## Object of class SpatialPointsDataFrame
    ## Coordinates:
    ##                  min        max
    ## coords.x1 -105.10630 -104.73584
    ## coords.x2   39.61694   39.79788
    ## Is projected: FALSE 
    ## proj4string :
    ## [+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0]
    ## Number of points: 501
    ## Data attributes:
    ##              BFN                                           BUS_PROF_N 
    ##  2000-BFN-30448:  1   DENVER PUBLIC SCHOOLS                     :119  
    ##  2001-BFN-17918:  1   DPR-COMMUNITY RECREATION SOAR AFTER SCHOOL:  4  
    ##  2001-BFN-32197:  1   HEART & HAND CENTER                       :  4  
    ##  2001-BFN-4085 :  1   KIDS ADVENTURES, INC                      :  4  
    ##  2001-BFN-44826:  1   OPENWORLD LEARNING                        :  4  
    ##  2001-BFN-46078:  1   SEWALL CHILD DEVELOPMENT CENTER           :  4  
    ##  (Other)       :495   (Other)                                   :362  
    ##                             FULL_ADDRE                    LIC_STATUS 
    ##  2701 N LIMA ST                  :  6   ABOUT TO EXPIRE        :  1  
    ##  19451 E MAXWELL PL              :  3   LICENSE ISSUED - ACTIVE:500  
    ##  3590 N JASMINE ST               :  3                                
    ##  8499 E STOLL PL                 :  3                                
    ##  1000 S HOLLY ST  DENVER CO 80246:  2                                
    ##  1005 N YOSEMITE ST              :  2                                
    ##  (Other)                         :482                                
    ##               LIC_TYPE        ISSUE_DATE        END_DATE  
    ##  CHILD CARE CENTER:393   2018/04/26: 76   2019/06/30: 74  
    ##  CHILD CARE HOME  :108   2018/03/29: 20   2019/04/15: 40  
    ##                          2018/03/28: 19   2019/06/08:  7  
    ##                          2018/07/10: 11   2019/08/21:  7  
    ##                          2018/05/31: 10   2018/09/26:  6  
    ##                          2017/09/20:  9   2018/08/25:  4  
    ##                          (Other)   :356   (Other)   :363  
    ##     X_COORD           Y_COORD          ADDRESS_ID    
    ##  Min.   :3110892   Min.   :1649893   Min.   :    -1  
    ##  1st Qu.:3137174   1st Qu.:1676679   1st Qu.:  1982  
    ##  Median :3153571   Median :1692403   Median : 99546  
    ##  Mean   :3155797   Mean   :1690213   Mean   :124105  
    ##  3rd Qu.:3168406   3rd Qu.:1702836   3rd Qu.:175749  
    ##  Max.   :3214742   Max.   :1716271   Max.   :526481  
    ##                                                      
    ##                  ADDRESS_LI    ADDRESS__1      CITY     STATE   
    ##  2701 N LIMA ST       :  6   # 10   :  1   DENVER:501   CO:501  
    ##  3590 N JASMINE ST    :  4   # 201B :  1                        
    ##  19451 E MAXWELL PL   :  3   #100D  :  1                        
    ##  2540 E 29TH AVE      :  3   1      :  1                        
    ##  3245 E EXPOSITION AVE:  3   2      :  1                        
    ##  3440 W VIRGINIA AVE  :  3   (Other): 10                        
    ##  (Other)              :479   NA's   :486                        
    ##       ZIP     
    ##  80219  : 47  
    ##  80205  : 44  
    ##  80220  : 35  
    ##  80239  : 30  
    ##  80210  : 29  
    ##  (Other):305  
    ##  NA's   : 11

    careFinal <- care[c(2,5)]
    names(careFinal) <- c("name", "license_type")
    levels(careFinal@data$license_type) <- c("center", "home")

    head(careFinal)

    ##             coordinates                                  name license_type
    ## 1 (-104.9327, 39.68661)                    OPENWORLD LEARNING       center
    ## 2 (-104.9262, 39.68988)                 DALTON GANG PRESCHOOL         home
    ## 3 (-105.0459, 39.68244)                      RAQUEL'S RASCALS         home
    ## 4 (-104.9555, 39.78556)              DPR COMMUNITY RECREATION       center
    ## 5 (-104.9784, 39.75952)         FAMILY STAR MONTESSORI SCHOOL       center
    ## 6  (-104.929, 39.73023) CHRIST THE KING ROMAN CATHOLIC SCHOOL       center

### Parks

Finally, look at parks.

    parks <- GetOpenData("parks")

Look at and subset parks data.

    head(parks@data)

    ##                 LOCATION                          FORMAL_NAM LOC_CODE
    ## 0       Weir Gulch South             Weir Gulch Park - South      837
    ## 1                 Aztlan                         Aztlan Park      032
    ## 2 Denver Botanic Gardens              Denver Botanic Gardens      356
    ## 3      Florida & Raritan Sanderson Gulch - Florida & Raritan      851
    ## 4       Lowry Open Space                    Lowry Open Space      L28
    ## 5   Montclair Rec Center         Montclair Recreation Center      L12
    ##    PARK_TYPE        PARK_CLASS GIS_ACRES          DESIGNATED FIRST_AQ_D
    ## 0       Park            Linear 10.509715             Partial       1960
    ## 1       Park      Neighborhood  3.688487                 Yes       1974
    ## 2       Park       Special Use 22.751852      Partial - 1956       1890
    ## 3       Park            Linear 15.612426                 Yes       1970
    ## 4 Open Space        Open Space 56.553171  Partial - 2014 May       2013
    ## 5    Grounds Recreation Center  5.613818 Yes - 2014 November       2002
    ##   MASTER_PLA MAINT_DIST COUNCIL_DI POLICE_DIS
    ## 0       2010         SW          3          4
    ## 1       None         NW          1          1
    ## 2       2007          E         10          3
    ## 3       2010         SW          7          4
    ## 4       2000         NE          5          3
    ## 5       None         NE          5          3
    ##                              CROSS_STRE ADDRESS_ID         ADDRESS_LI
    ## 0 Sheridan & Center to Alameda & Xavier          0 4900 W Alameda Ave
    ## 1                   44th Av & Navajo St      32090   4435 N Navajo St
    ## 2                     10th Av & York St      66355     1005 N York St
    ## 3               Florida Av & Raritan St          0 1800 W Florida Ave
    ## 4             Yosemite St & Sports Blvd     378758 8592 E Sports Blvd
    ## 5                   8th Ave & Ulster Wy     174032   729 N Ulster Way
    ##   ADDRESS__1   CITY STATE  ZIP LATITUDE LONGITUDE MARKETING_
    ## 0       <NA> Denver    CO <NA> 39.70776 -105.0498         PK
    ## 1       <NA> Denver    CO <NA> 39.77756 -105.0047         PK
    ## 2       <NA> Denver    CO <NA> 39.73203 -104.9615       <NA>
    ## 3       <NA> Denver    CO <NA> 39.68890 -105.0121       <NA>
    ## 4       <NA> Denver    CO <NA> 39.71928 -104.8808         PK
    ## 5       <NA> Denver    CO <NA> 39.72743 -104.8951       <NA>
    ##                                                                                                                       FACILITIES
    ## 0                                                              Bike/Pedestrian Path, Weir Gulch Trail, Open Space, Natural Areas
    ## 1 Recreation Center, Benches, Picnic Tables, Playground, Outdoor Pool, Basketball Court, Baseball / Softball Field, Soccer Field
    ## 2                                                                                    Gardens, Fountains, Paid Admission Required
    ## 3                            Bike/Pedestrian Path, Sanderson Gulch Trail, Basball / Softball Field, Picnic Tables, Natural Areas
    ## 4                                                                                  Dog Park, Natural Areas, Bike/Pedestrian Path
    ## 5                                                                                                              Recreation Center
    ##                                  DIAGRAM
    ## 0      ParkArt_Weir Gulch Park South.pdf
    ## 1                ParkArt_Aztlan Park.pdf
    ## 2                                   <NA>
    ## 3 ParkArt_Sanderson Gulch Open Space.pdf
    ## 4           ParkArt_Lowry Open Space.pdf
    ## 5                                   <NA>
    ##                                 PHOTO PARCEL_MAT     BND_QC
    ## 0        images/parks/no_imagerec.gif         No 2009/02/18
    ## 1         images/parks/atzlanpark.jpg        Yes 2008/11/20
    ## 2        images/parks/no_imagerec.gif        Yes 2009/09/16
    ## 3 images/parks/sandersongulchpark.jpg        Yes 2009/08/11
    ## 4                                <NA>         No 2014/03/06
    ## 5                                <NA>        Yes 2014/01/01

    parksFinal <- parks[c(2,5,23)]
    names(parksFinal) <- c("name","class","facilities")
    parksFinal[["name"]] <- as.character(parksFinal[["name"]])

Split facilities lists into separate columns. Only keep

    parksFinal <- SplitCommas(parksFinal, "facilities")
    colSums(parksFinal@data[,3:194])

    ##                      facilities_Adjacent.to.the.Highline.Canal.Trail 
    ##                                                                    1 
    ##                                              facilities_Amphitheater 
    ##                                                                    1 
    ##                                  facilities_Basball...Softball.Field 
    ##                                                                    1 
    ##                                 facilities_Baseball...Softball.Field 
    ##                                                                   69 
    ##                                 facilities_Baseball...Softball.FIeld 
    ##                                                                    1 
    ##                         facilities_Baseball...Softball.Field.Complex 
    ##                                                                    6 
    ##                                          facilities_Basketball.Court 
    ##                                                                   85 
    ##                                          facilities_Bear.Creek.Trail 
    ##                                                                    6 
    ##                                    facilities_Beaver.Brook.Trailhead 
    ##                                                                    1 
    ##                                                   facilities_Bemches 
    ##                                                                    1 
    ##                                                   facilities_Benches 
    ##                                                                  159 
    ##                                        facilities_Bike.Skills.Course 
    ##                                                                    2 
    ##                                      facilities_Bike.pedestrian.Path 
    ##                                                                    5 
    ##                                      facilities_Bike.Pedestrian.Path 
    ##                                                                  148 
    ##                   facilities_Bike.Pedestrian.Path..First.Creek.Trail 
    ##                                                                    1 
    ##               facilities_Bike.Pedestrian.Path..High.Line.Canal.Trail 
    ##                                                                    1 
    ##                facilities_Bike.Pedestrian.Path..Lakewood.Gulch.Trail 
    ##                                                                    1 
    ##            facilities_Bike.Pedestrian.Path..South.Platte.River.Trail 
    ##                                                                    1 
    ##                                                facilities_Bison.Herd 
    ##                                                                    1 
    ##                                            facilities_Bison.Preserve 
    ##                                                                    1 
    ##                                                 facilities_Bleachers 
    ##                                                                    8 
    ##                                              facilities_Boat.Rentals 
    ##                                                                    1 
    ##                                                   facilities_Boating 
    ##                                                                    1 
    ##                                          facilities_Bocce.Ball.Court 
    ##                                                                    3 
    ##                                       facilities_Buchanan.Park.Trail 
    ##                                                                    1 
    ##                               facilities_Buffalo.Bill.Grave...Museum 
    ##                                                                    1 
    ##                                               facilities_Campgrounds 
    ##                                                                    1 
    ##                                        facilities_Cherry.Creek.Trail 
    ##                                                                    7 
    ##                                          facilities_Chief.Hosa.Lodge 
    ##                                                                    1 
    ##                                                 facilities_Clubhouse 
    ##                                                                    1 
    ##                                          facilities_Community.Garden 
    ##                                                                    6 
    ##                                        facilities_Dedisse.Park.Trail 
    ##                                                                    1 
    ##                                          facilities_Disc.Golf.Course 
    ##                                                                    3 
    ##                                                  facilities_Dog.Park 
    ##                                                                   11 
    ##                                         facilities_Drinking.Fountain 
    ##                                                                   83 
    ##                                             facilities_Driving.Range 
    ##                                                                    1 
    ##                                           facilities_Echo.Lake.Lodge 
    ##                                                                    1 
    ##                                    facilities_Educational.Activities 
    ##                                                                    1 
    ##                                            facilities_Event.Facility 
    ##                                                                    1 
    ##                                                    facilities_Events 
    ##                                                                    1 
    ##                                                  facilities_Exhibits 
    ##                                                                    1 
    ##                                                  facilities_Firepits 
    ##                                                                    1 
    ##                                                   facilities_Fishing 
    ##                                                                    3 
    ##                                            facilities_Fishing.Access 
    ##                                                                    1 
    ##                                            facilities_Fitness.Course 
    ##                                                                    1 
    ##                                              facilities_Fitness.Zone 
    ##                                                                   10 
    ##                             facilities_Five.Mile.Scenic.Lariat.Trail 
    ##                                                                    1 
    ##                            facilities_Flemming.Mansion..Special.Use. 
    ##                                                                    1 
    ##                                                facilities_Flower.Bed 
    ##                                                                    1 
    ##                                               facilities_Flower.Beds 
    ##                                                                   28 
    ##                                            facilities_Football.Field 
    ##                                                                   32 
    ##                                                  facilities_Fountain 
    ##                                                                   11 
    ##                                                 facilities_Fountains 
    ##                                                                    1 
    ##                                              facilities_Futsol.Court 
    ##                                                                    2 
    ##            facilities_Future.Bike.Pedestrian.Path..First.Creek.Trail 
    ##                                                                    1 
    ##        facilities_Future.Bike.Pedestrian.Path..High.Line.Canal.Trail 
    ##                                                                    1 
    ##                            facilities_Future.neighborhood.park.site. 
    ##                                                                    1 
    ##                                   facilities_Future.Park.Development 
    ##                                                                    4 
    ##                                  facilities_Future.Trail.Development 
    ##                                                                    1 
    ##                                   facilities_Future.Trunk.Open.Space 
    ##                                                                    2 
    ##                                                   facilities_Gardens 
    ##                                                                    1 
    ##                                       facilities_Gates.Tennis.Center 
    ##                                                                    2 
    ##                                         facilities_Geologic.Overlook 
    ##                                                                    1 
    ##                                     facilities_Goldsmith.Gulch.Trail 
    ##                                                                    2 
    ##                                               facilities_Golf.Course 
    ##                                                                    8 
    ##                                  facilities_Grant.Humphrey.s.Mansion 
    ##                                                                    1 
    ##                                        facilities_Greek.Amphitheater 
    ##                                                                    1 
    ##                                                     facilities_Grill 
    ##                                                                    1 
    ##                                                    facilities_Grills 
    ##                                                                   15 
    ##                                            facilities_Handball.Court 
    ##                                                                    1 
    ##                                       facilities_Harvard.Gulch.Trail 
    ##                                                                    4 
    ##                                     facilities_High.Line.Canal.Trail 
    ##                                                                    7 
    ##                                             facilities_Hiking.Trails 
    ##                                                                    2 
    ##                                     facilities_Historic.Amphitheatre 
    ##                                                                    1 
    ##                               facilities_Historic.Fireplace.Monument 
    ##                                                                    1 
    ##                                   facilities_Historic.Picnic.Shelter 
    ##                                                                    1 
    ##                                            facilities_Historic.Ranch 
    ##                                                                    1 
    ##                                          facilities_Historic.Shelter 
    ##                                                                    4 
    ##                                             facilities_Historic.Site 
    ##                                                                    2 
    ##                                     facilities_Historic.Spring.House 
    ##                                                                    1 
    ##                                  facilities_Historic.Stone.Wellhouse 
    ##                                                                    3 
    ##                                           facilities_Historical.Site 
    ##                                                                    2 
    ##                                            facilities_Horseshoe.Pits 
    ##                                                                   23 
    ##                                               facilities_Ice.Skating 
    ##                                                                    1 
    ##                                      facilities_Interactive.Fountain 
    ##                                                                    4 
    ##                                     facilities_Interpretive.Overlook 
    ##                                                                    1 
    ##                                              facilities_Jogging.Path 
    ##                                                                    2 
    ##                                              facilities_Kayak.Course 
    ##                                                                    1 
    ##                                                      facilities_Lake 
    ##                                                                   19 
    ##                                                 facilities_Lakehouse 
    ##                                                                    1 
    ##                                      facilities_Lakewood.Gulch.Trail 
    ##                                                                    4 
    ##                                              facilities_Lawn.Bowling 
    ##                                                                    1 
    ##                                             facilities_Leased.to.ELK 
    ##                                                                    1 
    ##                                                   facilities_Library 
    ##                                                                    3 
    ##                facilities_Located.just.off.of.the.Cherry.Creek.Trail 
    ##                                                                    1 
    ##                                         facilities_Meadow.View.Trail 
    ##                                                                    1 
    ##                                       facilities_Memorial...Monument 
    ##                                                                    5 
    ##                                     facilities_Miniature.Golf.Course 
    ##                                                                    1 
    ##                    facilities_Montclair.Civic.Building..The.Mulkery. 
    ##                                                                    1 
    ##                                       facilities_Multi.purpose.Field 
    ##                                                                    3 
    ##                                                    facilities_Museum 
    ##                                                                    2 
    ##                                             facilities_Natural.Areas 
    ##                                                                   72 
    ##                                        facilities_Natural.Open.Space 
    ##                                                                   22 
    ##                                             facilities_Nature.Center 
    ##                                                                    1 
    ##                                  facilities_Nature.Center...Preserve 
    ##                                                                    1 
    ##                                               facilities_Observatory 
    ##                                                                    1 
    ##                               facilities_Off.street.Trail.Connection 
    ##                                                                    1 
    ##                                                facilities_Open.Space 
    ##                                                                   29 
    ##                                         facilities_Outdoor.Classroom 
    ##                                                                    1 
    ##                                              facilities_Outdoor.Pool 
    ##                                                                   16 
    ##                                                  facilities_Overlook 
    ##                                                                    1 
    ##                                            facilities_Overlook.Plaza 
    ##                                                                    1 
    ##                                             facilities_Pahaska.Tepee 
    ##                                                                    1 
    ##                                   facilities_Paid.Admission.Required 
    ##                                                                    2 
    ##                                      facilities_Panorama.Point.Trail 
    ##                                                                    1 
    ##                                                   facilities_Parking 
    ##                                                                    1 
    ##                                               facilities_Parking.Lot 
    ##                                                                   12 
    ##                                        facilities_Passive.Open.Space 
    ##                                                                   10 
    ##                                                  facilities_Pavilion 
    ##                                                                    4 
    ##                                           facilities_Pedestrian.Path 
    ##                                                                    1 
    ##                                          facilities_Pickleball.Court 
    ##                                                                    2 
    ##                                               facilities_Picnic.Area 
    ##                                                                   67 
    ##                                         facilities_Picnic.Loop.Trail 
    ##                                                                    1 
    ##                                            facilities_Picnic.Shelter 
    ##                                                                   55 
    ##                                             facilities_Picnic.Tables 
    ##                                                                  170 
    ##                                           facilities_Ping.Pong.Table 
    ##                                                                    3 
    ##                                             facilities_Pioneer.Trail 
    ##                                                                    1 
    ##                                                facilities_Playground 
    ##                                                                  136 
    ##                                  facilities_Playground..Nature.Play. 
    ##                                                                    3 
    ##                                                     facilities_Plaza 
    ##                                                                    9 
    ##                                       facilities_Prairie.Dog.Habitat 
    ##                                                                    1 
    ##                                                 facilities_Press.Box 
    ##                                                                    3 
    ##                                                facilities_Public.Art 
    ##                                                                    8 
    ##                                facilities_Recreation...Senior.Center 
    ##                                                                    1 
    ##                                        facilities_Recreation.Ceneter 
    ##                                                                    1 
    ##                                         facilities_Recreation.Center 
    ##                                                                   29 
    ##                                                facilities_Restaurant 
    ##                                                                    3 
    ##                                                  facilities_Restroom 
    ##                                                                   57 
    ##                                    facilities_Riparian.Natural.Areas 
    ##                                                                    3 
    ##                                              facilities_River.Access 
    ##                                                                    2 
    ##                                              facilities_Ropes.Course 
    ##                                                                    1 
    ##                                             facilities_Running.Track 
    ##                                                                    2 
    ##                                          facilities_Sand.Creek.Trail 
    ##                                                                    1 
    ##                                     facilities_Sand.Volleyball.Court 
    ##                                                                    5 
    ##                                     facilities_Sanderson.Gulch.Trail 
    ##                                                                    3 
    ##                                           facilities_Scenic.Overlook 
    ##                                                                    2 
    ##                                          facilities_Scenic.View.Shed 
    ##                                                                   20 
    ##                                              facilities_Scenic.Views 
    ##                                                                    2 
    ##                                           facilities_Scenic.viewshed 
    ##                                                                    1 
    ##                                           facilities_Shade.Structure 
    ##                                                                    3 
    ##                                                   facilities_Shelter 
    ##                                                                    1 
    ##                                           facilities_Shoemaker.Plaza 
    ##                                                                    1 
    ##                                                facilities_Skate.Park 
    ##                                                                    6 
    ##                                              facilities_Soccer.Field 
    ##                                                                   42 
    ##                                      facilities_Soccer.Field.Complex 
    ##                                                                    3 
    ##                                            facilities_Softball.Field 
    ##                                                                    3 
    ##                                  facilities_South.Platte.River.Trail 
    ##                                                                   18 
    ##                                            facilities_Special.Events 
    ##                                                                    1 
    ##                                facilities_Special.Permit.Picnic.Area 
    ##                                                                    1 
    ##                                    facilities_Special.Use.Activities 
    ##                                                                    2 
    ##                                  facilities_Stapleton.Greenway.Trail 
    ##                                                                    1 
    ##                                               facilities_Stone.Bench 
    ##                                                                    1 
    ##                                           facilities_Synthetic.Field 
    ##                                                                    2 
    ##                                                    facilities_Tables 
    ##                                                                    7 
    ##                                              facilities_Tennis.Court 
    ##                                                                   32 
    ##                             facilities_Trading.Post..Welcome.Center. 
    ##                                                                    1 
    ##                                        facilities_Trading.Post.Trail 
    ##                                                                    1 
    ##                                                     facilities_Trail 
    ##                                                                    1 
    ##             facilities_Trail.Access..Beaver.Brook.via.Colorow.Trail. 
    ##                                                                    1 
    ##                                          facilities_Trail.Connection 
    ##                                                                    1 
    ##      facilities_Trail.connection.through.Jefferson.County.Open.Space 
    ##                                                                    1 
    ##            facilities_Trail.connections.to.adjacent.Open.Space.Parks 
    ##                                                                    1 
    ##                          facilities_Trail.Connections..Pioneer.Trail 
    ##                                                                    1 
    ##                                                    facilities_Trails 
    ##                                                                    1 
    ##                                facilities_Trails...Trail.Connections 
    ##                                                                    2 
    ##                                               facilities_Undeveloped 
    ##                                                                    2 
    ## facilities_Undeveloped.parcel.adjacent.to.the.Cherry.Creek.Greenway. 
    ##                                                                    1 
    ##                          facilities_Undeveloped.Trail.and.Open.Space 
    ##                                                                    1 
    ##                                                     facilities_Views 
    ##                                                                    4 
    ##                                            facilities_Visitor.Center 
    ##                                                                    1 
    ##                                          facilities_Volleyball.Court 
    ##                                                                    6 
    ##                                         facilities_Voorhies.Memorial 
    ##                                                                    1 
    ##                                      facilities_Walking.Jogging.Path 
    ##                                                                    3 
    ##                                     facilities_Walking.Jogging.Paths 
    ##                                                                    1 
    ##                                          facilities_Weir.Gulch.Trail 
    ##                                                                    4 
    ##                                  facilities_West.Harvard.Gulch.Trail 
    ##                                                                    1 
    ##                                          facilities_West.Ridge.Trail 
    ##                                                                    1 
    ##                                      facilities_Westerly.Creek.Trail 
    ##                                                                    4 
    ##                                                  facilities_Wetlands 
    ##                                                                    1 
    ##                                           facilities_Wilderness.Area 
    ##                                                                    1 
    ##                                          facilities_Wildlife.Habitat 
    ##                                                                    7 
    ##                                                       facilities_Zoo 
    ##                                                                    1

    # What columns do we have now?
    colnames(parksFinal@data)

    ##   [1] "name"                                                                
    ##   [2] "class"                                                               
    ##   [3] "facilities_Adjacent.to.the.Highline.Canal.Trail"                     
    ##   [4] "facilities_Amphitheater"                                             
    ##   [5] "facilities_Basball...Softball.Field"                                 
    ##   [6] "facilities_Baseball...Softball.Field"                                
    ##   [7] "facilities_Baseball...Softball.FIeld"                                
    ##   [8] "facilities_Baseball...Softball.Field.Complex"                        
    ##   [9] "facilities_Basketball.Court"                                         
    ##  [10] "facilities_Bear.Creek.Trail"                                         
    ##  [11] "facilities_Beaver.Brook.Trailhead"                                   
    ##  [12] "facilities_Bemches"                                                  
    ##  [13] "facilities_Benches"                                                  
    ##  [14] "facilities_Bike.Skills.Course"                                       
    ##  [15] "facilities_Bike.pedestrian.Path"                                     
    ##  [16] "facilities_Bike.Pedestrian.Path"                                     
    ##  [17] "facilities_Bike.Pedestrian.Path..First.Creek.Trail"                  
    ##  [18] "facilities_Bike.Pedestrian.Path..High.Line.Canal.Trail"              
    ##  [19] "facilities_Bike.Pedestrian.Path..Lakewood.Gulch.Trail"               
    ##  [20] "facilities_Bike.Pedestrian.Path..South.Platte.River.Trail"           
    ##  [21] "facilities_Bison.Herd"                                               
    ##  [22] "facilities_Bison.Preserve"                                           
    ##  [23] "facilities_Bleachers"                                                
    ##  [24] "facilities_Boat.Rentals"                                             
    ##  [25] "facilities_Boating"                                                  
    ##  [26] "facilities_Bocce.Ball.Court"                                         
    ##  [27] "facilities_Buchanan.Park.Trail"                                      
    ##  [28] "facilities_Buffalo.Bill.Grave...Museum"                              
    ##  [29] "facilities_Campgrounds"                                              
    ##  [30] "facilities_Cherry.Creek.Trail"                                       
    ##  [31] "facilities_Chief.Hosa.Lodge"                                         
    ##  [32] "facilities_Clubhouse"                                                
    ##  [33] "facilities_Community.Garden"                                         
    ##  [34] "facilities_Dedisse.Park.Trail"                                       
    ##  [35] "facilities_Disc.Golf.Course"                                         
    ##  [36] "facilities_Dog.Park"                                                 
    ##  [37] "facilities_Drinking.Fountain"                                        
    ##  [38] "facilities_Driving.Range"                                            
    ##  [39] "facilities_Echo.Lake.Lodge"                                          
    ##  [40] "facilities_Educational.Activities"                                   
    ##  [41] "facilities_Event.Facility"                                           
    ##  [42] "facilities_Events"                                                   
    ##  [43] "facilities_Exhibits"                                                 
    ##  [44] "facilities_Firepits"                                                 
    ##  [45] "facilities_Fishing"                                                  
    ##  [46] "facilities_Fishing.Access"                                           
    ##  [47] "facilities_Fitness.Course"                                           
    ##  [48] "facilities_Fitness.Zone"                                             
    ##  [49] "facilities_Five.Mile.Scenic.Lariat.Trail"                            
    ##  [50] "facilities_Flemming.Mansion..Special.Use."                           
    ##  [51] "facilities_Flower.Bed"                                               
    ##  [52] "facilities_Flower.Beds"                                              
    ##  [53] "facilities_Football.Field"                                           
    ##  [54] "facilities_Fountain"                                                 
    ##  [55] "facilities_Fountains"                                                
    ##  [56] "facilities_Futsol.Court"                                             
    ##  [57] "facilities_Future.Bike.Pedestrian.Path..First.Creek.Trail"           
    ##  [58] "facilities_Future.Bike.Pedestrian.Path..High.Line.Canal.Trail"       
    ##  [59] "facilities_Future.neighborhood.park.site."                           
    ##  [60] "facilities_Future.Park.Development"                                  
    ##  [61] "facilities_Future.Trail.Development"                                 
    ##  [62] "facilities_Future.Trunk.Open.Space"                                  
    ##  [63] "facilities_Gardens"                                                  
    ##  [64] "facilities_Gates.Tennis.Center"                                      
    ##  [65] "facilities_Geologic.Overlook"                                        
    ##  [66] "facilities_Goldsmith.Gulch.Trail"                                    
    ##  [67] "facilities_Golf.Course"                                              
    ##  [68] "facilities_Grant.Humphrey.s.Mansion"                                 
    ##  [69] "facilities_Greek.Amphitheater"                                       
    ##  [70] "facilities_Grill"                                                    
    ##  [71] "facilities_Grills"                                                   
    ##  [72] "facilities_Handball.Court"                                           
    ##  [73] "facilities_Harvard.Gulch.Trail"                                      
    ##  [74] "facilities_High.Line.Canal.Trail"                                    
    ##  [75] "facilities_Hiking.Trails"                                            
    ##  [76] "facilities_Historic.Amphitheatre"                                    
    ##  [77] "facilities_Historic.Fireplace.Monument"                              
    ##  [78] "facilities_Historic.Picnic.Shelter"                                  
    ##  [79] "facilities_Historic.Ranch"                                           
    ##  [80] "facilities_Historic.Shelter"                                         
    ##  [81] "facilities_Historic.Site"                                            
    ##  [82] "facilities_Historic.Spring.House"                                    
    ##  [83] "facilities_Historic.Stone.Wellhouse"                                 
    ##  [84] "facilities_Historical.Site"                                          
    ##  [85] "facilities_Horseshoe.Pits"                                           
    ##  [86] "facilities_Ice.Skating"                                              
    ##  [87] "facilities_Interactive.Fountain"                                     
    ##  [88] "facilities_Interpretive.Overlook"                                    
    ##  [89] "facilities_Jogging.Path"                                             
    ##  [90] "facilities_Kayak.Course"                                             
    ##  [91] "facilities_Lake"                                                     
    ##  [92] "facilities_Lakehouse"                                                
    ##  [93] "facilities_Lakewood.Gulch.Trail"                                     
    ##  [94] "facilities_Lawn.Bowling"                                             
    ##  [95] "facilities_Leased.to.ELK"                                            
    ##  [96] "facilities_Library"                                                  
    ##  [97] "facilities_Located.just.off.of.the.Cherry.Creek.Trail"               
    ##  [98] "facilities_Meadow.View.Trail"                                        
    ##  [99] "facilities_Memorial...Monument"                                      
    ## [100] "facilities_Miniature.Golf.Course"                                    
    ## [101] "facilities_Montclair.Civic.Building..The.Mulkery."                   
    ## [102] "facilities_Multi.purpose.Field"                                      
    ## [103] "facilities_Museum"                                                   
    ## [104] "facilities_Natural.Areas"                                            
    ## [105] "facilities_Natural.Open.Space"                                       
    ## [106] "facilities_Nature.Center"                                            
    ## [107] "facilities_Nature.Center...Preserve"                                 
    ## [108] "facilities_Observatory"                                              
    ## [109] "facilities_Off.street.Trail.Connection"                              
    ## [110] "facilities_Open.Space"                                               
    ## [111] "facilities_Outdoor.Classroom"                                        
    ## [112] "facilities_Outdoor.Pool"                                             
    ## [113] "facilities_Overlook"                                                 
    ## [114] "facilities_Overlook.Plaza"                                           
    ## [115] "facilities_Pahaska.Tepee"                                            
    ## [116] "facilities_Paid.Admission.Required"                                  
    ## [117] "facilities_Panorama.Point.Trail"                                     
    ## [118] "facilities_Parking"                                                  
    ## [119] "facilities_Parking.Lot"                                              
    ## [120] "facilities_Passive.Open.Space"                                       
    ## [121] "facilities_Pavilion"                                                 
    ## [122] "facilities_Pedestrian.Path"                                          
    ## [123] "facilities_Pickleball.Court"                                         
    ## [124] "facilities_Picnic.Area"                                              
    ## [125] "facilities_Picnic.Loop.Trail"                                        
    ## [126] "facilities_Picnic.Shelter"                                           
    ## [127] "facilities_Picnic.Tables"                                            
    ## [128] "facilities_Ping.Pong.Table"                                          
    ## [129] "facilities_Pioneer.Trail"                                            
    ## [130] "facilities_Playground"                                               
    ## [131] "facilities_Playground..Nature.Play."                                 
    ## [132] "facilities_Plaza"                                                    
    ## [133] "facilities_Prairie.Dog.Habitat"                                      
    ## [134] "facilities_Press.Box"                                                
    ## [135] "facilities_Public.Art"                                               
    ## [136] "facilities_Recreation...Senior.Center"                               
    ## [137] "facilities_Recreation.Ceneter"                                       
    ## [138] "facilities_Recreation.Center"                                        
    ## [139] "facilities_Restaurant"                                               
    ## [140] "facilities_Restroom"                                                 
    ## [141] "facilities_Riparian.Natural.Areas"                                   
    ## [142] "facilities_River.Access"                                             
    ## [143] "facilities_Ropes.Course"                                             
    ## [144] "facilities_Running.Track"                                            
    ## [145] "facilities_Sand.Creek.Trail"                                         
    ## [146] "facilities_Sand.Volleyball.Court"                                    
    ## [147] "facilities_Sanderson.Gulch.Trail"                                    
    ## [148] "facilities_Scenic.Overlook"                                          
    ## [149] "facilities_Scenic.View.Shed"                                         
    ## [150] "facilities_Scenic.Views"                                             
    ## [151] "facilities_Scenic.viewshed"                                          
    ## [152] "facilities_Shade.Structure"                                          
    ## [153] "facilities_Shelter"                                                  
    ## [154] "facilities_Shoemaker.Plaza"                                          
    ## [155] "facilities_Skate.Park"                                               
    ## [156] "facilities_Soccer.Field"                                             
    ## [157] "facilities_Soccer.Field.Complex"                                     
    ## [158] "facilities_Softball.Field"                                           
    ## [159] "facilities_South.Platte.River.Trail"                                 
    ## [160] "facilities_Special.Events"                                           
    ## [161] "facilities_Special.Permit.Picnic.Area"                               
    ## [162] "facilities_Special.Use.Activities"                                   
    ## [163] "facilities_Stapleton.Greenway.Trail"                                 
    ## [164] "facilities_Stone.Bench"                                              
    ## [165] "facilities_Synthetic.Field"                                          
    ## [166] "facilities_Tables"                                                   
    ## [167] "facilities_Tennis.Court"                                             
    ## [168] "facilities_Trading.Post..Welcome.Center."                            
    ## [169] "facilities_Trading.Post.Trail"                                       
    ## [170] "facilities_Trail"                                                    
    ## [171] "facilities_Trail.Access..Beaver.Brook.via.Colorow.Trail."            
    ## [172] "facilities_Trail.Connection"                                         
    ## [173] "facilities_Trail.connection.through.Jefferson.County.Open.Space"     
    ## [174] "facilities_Trail.connections.to.adjacent.Open.Space.Parks"           
    ## [175] "facilities_Trail.Connections..Pioneer.Trail"                         
    ## [176] "facilities_Trails"                                                   
    ## [177] "facilities_Trails...Trail.Connections"                               
    ## [178] "facilities_Undeveloped"                                              
    ## [179] "facilities_Undeveloped.parcel.adjacent.to.the.Cherry.Creek.Greenway."
    ## [180] "facilities_Undeveloped.Trail.and.Open.Space"                         
    ## [181] "facilities_Views"                                                    
    ## [182] "facilities_Visitor.Center"                                           
    ## [183] "facilities_Volleyball.Court"                                         
    ## [184] "facilities_Voorhies.Memorial"                                        
    ## [185] "facilities_Walking.Jogging.Path"                                     
    ## [186] "facilities_Walking.Jogging.Paths"                                    
    ## [187] "facilities_Weir.Gulch.Trail"                                         
    ## [188] "facilities_West.Harvard.Gulch.Trail"                                 
    ## [189] "facilities_West.Ridge.Trail"                                         
    ## [190] "facilities_Westerly.Creek.Trail"                                     
    ## [191] "facilities_Wetlands"                                                 
    ## [192] "facilities_Wilderness.Area"                                          
    ## [193] "facilities_Wildlife.Habitat"                                         
    ## [194] "facilities_Zoo"

    # Add variable about whether the park has some kind of natural area
    natureBool <- grepl("Nature|nature|NATURE|Natural|natural|NATURAL", colnames(parksFinal@data))
    colnames(parksFinal@data)[natureBool]  # look at what columns you're getting

    ## [1] "facilities_Natural.Areas"           
    ## [2] "facilities_Natural.Open.Space"      
    ## [3] "facilities_Nature.Center"           
    ## [4] "facilities_Nature.Center...Preserve"
    ## [5] "facilities_Playground..Nature.Play."
    ## [6] "facilities_Riparian.Natural.Areas"

    parksFinal@data$has_nature <- apply(parksFinal@data[,natureBool], 1, max)

    # Add variable about whether the park has a garden
    gardenBool <- grepl("Garden|garden|GARDEN", colnames(parksFinal@data))
    colnames(parksFinal@data)[gardenBool]  # look at what columns you're getting

    ## [1] "facilities_Community.Garden" "facilities_Gardens"

    parksFinal@data$has_garden <- apply(parksFinal@data[,gardenBool], 1, max)

    # Add variable about whether the park has a trail.
    trailBool <- grepl("Trail|trail|TRAIL", colnames(parksFinal@data))
    colnames(parksFinal@data)[trailBool]  # look at what columns you're getting

    ##  [1] "facilities_Adjacent.to.the.Highline.Canal.Trail"                
    ##  [2] "facilities_Bear.Creek.Trail"                                    
    ##  [3] "facilities_Beaver.Brook.Trailhead"                              
    ##  [4] "facilities_Bike.Pedestrian.Path..First.Creek.Trail"             
    ##  [5] "facilities_Bike.Pedestrian.Path..High.Line.Canal.Trail"         
    ##  [6] "facilities_Bike.Pedestrian.Path..Lakewood.Gulch.Trail"          
    ##  [7] "facilities_Bike.Pedestrian.Path..South.Platte.River.Trail"      
    ##  [8] "facilities_Buchanan.Park.Trail"                                 
    ##  [9] "facilities_Cherry.Creek.Trail"                                  
    ## [10] "facilities_Dedisse.Park.Trail"                                  
    ## [11] "facilities_Five.Mile.Scenic.Lariat.Trail"                       
    ## [12] "facilities_Future.Bike.Pedestrian.Path..First.Creek.Trail"      
    ## [13] "facilities_Future.Bike.Pedestrian.Path..High.Line.Canal.Trail"  
    ## [14] "facilities_Future.Trail.Development"                            
    ## [15] "facilities_Goldsmith.Gulch.Trail"                               
    ## [16] "facilities_Harvard.Gulch.Trail"                                 
    ## [17] "facilities_High.Line.Canal.Trail"                               
    ## [18] "facilities_Hiking.Trails"                                       
    ## [19] "facilities_Lakewood.Gulch.Trail"                                
    ## [20] "facilities_Located.just.off.of.the.Cherry.Creek.Trail"          
    ## [21] "facilities_Meadow.View.Trail"                                   
    ## [22] "facilities_Off.street.Trail.Connection"                         
    ## [23] "facilities_Panorama.Point.Trail"                                
    ## [24] "facilities_Picnic.Loop.Trail"                                   
    ## [25] "facilities_Pioneer.Trail"                                       
    ## [26] "facilities_Sand.Creek.Trail"                                    
    ## [27] "facilities_Sanderson.Gulch.Trail"                               
    ## [28] "facilities_South.Platte.River.Trail"                            
    ## [29] "facilities_Stapleton.Greenway.Trail"                            
    ## [30] "facilities_Trading.Post.Trail"                                  
    ## [31] "facilities_Trail"                                               
    ## [32] "facilities_Trail.Access..Beaver.Brook.via.Colorow.Trail."       
    ## [33] "facilities_Trail.Connection"                                    
    ## [34] "facilities_Trail.connection.through.Jefferson.County.Open.Space"
    ## [35] "facilities_Trail.connections.to.adjacent.Open.Space.Parks"      
    ## [36] "facilities_Trail.Connections..Pioneer.Trail"                    
    ## [37] "facilities_Trails"                                              
    ## [38] "facilities_Trails...Trail.Connections"                          
    ## [39] "facilities_Undeveloped.Trail.and.Open.Space"                    
    ## [40] "facilities_Weir.Gulch.Trail"                                    
    ## [41] "facilities_West.Harvard.Gulch.Trail"                            
    ## [42] "facilities_West.Ridge.Trail"                                    
    ## [43] "facilities_Westerly.Creek.Trail"

    parksFinal@data$has_trail <- apply(parksFinal@data[,trailBool], 1, max)
    parksFinal@data$has_trail <- NULL  # actually ignore this variable because they all have trails (lol)

    # bike paths
    bikeBool <- grepl("Bike|bike|BIKE|cycle|Cycle|CYCLE", colnames(parksFinal@data))
    colnames(parksFinal@data)[bikeBool]  # look at what columns you're getting

    ## [1] "facilities_Bike.Skills.Course"                                
    ## [2] "facilities_Bike.pedestrian.Path"                              
    ## [3] "facilities_Bike.Pedestrian.Path"                              
    ## [4] "facilities_Bike.Pedestrian.Path..First.Creek.Trail"           
    ## [5] "facilities_Bike.Pedestrian.Path..High.Line.Canal.Trail"       
    ## [6] "facilities_Bike.Pedestrian.Path..Lakewood.Gulch.Trail"        
    ## [7] "facilities_Bike.Pedestrian.Path..South.Platte.River.Trail"    
    ## [8] "facilities_Future.Bike.Pedestrian.Path..First.Creek.Trail"    
    ## [9] "facilities_Future.Bike.Pedestrian.Path..High.Line.Canal.Trail"

    parksFinal@data$has_biking <- apply(parksFinal@data[,bikeBool], 1, max)

    # delete the extra variables
    parksFinal@data[,3:194] <- NULL

    head(parksFinal@data)

    ##                                  name             class has_nature
    ## 0             Weir Gulch Park - South            Linear          1
    ## 1                         Aztlan Park      Neighborhood          0
    ## 2              Denver Botanic Gardens       Special Use          0
    ## 3 Sanderson Gulch - Florida & Raritan            Linear          1
    ## 4                    Lowry Open Space        Open Space          1
    ## 5         Montclair Recreation Center Recreation Center          0
    ##   has_garden has_biking
    ## 0          0          1
    ## 1          0          0
    ## 2          1          0
    ## 3          0          1
    ## 4          0          1
    ## 5          0          0

Adding neighborhoods to each dataset, from lat/longs
----------------------------------------------------

Uploading to RDS
----------------

Savings to csvs:

    SavePointsAsCSV(afterSchoolFinal, "afterschool.csv")
    SavePointsAsCSV(recFinal, "rec_centers.csv")
    SavePolygonsAsCSV(fieldsFinal, "fields.csv")
    SavePolygonsAsCSV(playgroundsFinal, "playgrounds.csv")
    SavePolygonsAsCSV(skateParksFinal, "skate_parks.csv")
    SavePolygonsAsCSV(courtsFinal, "rec_courts.csv")
    SavePointsAsCSV(librariesFinal, "libraries.csv")
    SavePointsAsCSV(poolsFinal, "pools.csv")
    SavePointsAsCSV(careFinal, "licensed_child_care.csv")
    SavePolygonsAsCSV(parksFinal, "parks.csv")

Uploading directly to RDS:

    # load the function
    mypath <- getwd()
    rds_file <- file.path(dirname(mypath), "update_rds.R")
    source(rds_file)

    ## Loading required package: RPostgreSQL

    ## Loading required package: DBI

    # # upload everything
    # update_rds(afterSchoolFinal, "clean", "afterschool")
    # update_rds(recFinal, "clean", "rec_centers")
    # update_rds(fieldsFinal, "clean", "fields")
    # update_rds(playgroundsFinal, "clean", "playgrounds")
    # update_rds(skateParksFinal, "clean", "skate_parks")
    # update_rds(courtsFinal, "clean", "rec_courts")
    # update_rds(librariesFinal, "clean", "libraries")
    # update_rds(poolsFinal, "clean", "pools")
    # update_rds(careFinal, "clean", "child_care")
    # update_rds(parksFinal, "clean", "parks")

Make the codebook
-----------------

Build the codebook, i.e. get variable names for each csv saved above in
clean\_data. Save the results as a csv for easy referencing later.

    filenameList <- c("afterschool.csv", "rec_centers.csv", "fields.csv", "playgrounds.csv", 
                      "skate_parks.csv", "rec_courts.csv", "libraries.csv", "pools.csv", 
                      "licensed_child_care.csv", "parks.csv")

    maxVars <- 50
    codebook <- data.frame(matrix(nrow=maxVars, ncol=0))

    for (filename in filenameList) {
      # load csv into workspace
      file <- read.csv(file.path("clean_data",filename) )
      
      vars <- rep(NA, maxVars)
      vars[1:length(names(file))] <- names(file)
      
      # save column names to dataframe
      codebook[[filename]] <- vars
    }

    write.csv(codebook, file=file.path("clean_data","codebook_resources.csv"), row.names=FALSE)

Here is a function for getting the unique values in a column, too, which
will be useful for the "values" section in the codebook (but only for
the variables for which this is relevant: i.e. the factors). We'll use
this on specified columns later, but I won't put it in the markdown
because that seems excessive.

    # Function to get codes from a particular column in a data frame
    #    input:   data frame, column name (string)
    #    output:  list of codes used in that column
    GetCodes <- function(df, colName) {
      vals <- sort(unique(df[[colName]]))
      print(vals)
    }
