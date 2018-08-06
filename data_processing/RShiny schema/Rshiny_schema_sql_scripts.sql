/*Create an intermediate table joining the reschool data with the neighborhoods*/
create table reschool_nbhds
as (select A.*, B.nbhd_id, B.nbhd_name 
    from clean.reschool_summer_programs as A, clean.blockgroup_nbhds as B
    where A.bgroup_id2 = B.bgroup_id2
    );
   
/*Subset only the relavent columns to be used in RShiny*/
create table shiny.summer_programs
as (select 
session_name,
session_date_start,
session_date_end,
session_cost,
lat,
long,
nbhd_id,
nbhd_name,
camp_name,
session_short_description,
has_special_needs_offerings,
has_scholarships,
has_academic,
has_arts,
has_cooking,
has_dance,
has_drama,
has_music,
has_nature,
has_sports,
has_stem

from reschool_nbhds);

/*aggregate the total number of sessions that took place in each of the neighborhood*/
/*This will be later joined with the shape file dataset to be used in the RShiny dashboard*/
create table shiny.aggregate_programs_nbhd as
(select count(session_id), nbhd_name
from reschool_nbhds
group by nbhd_name); 

/*aggregate student demographics from the dps public school data to be used in rshiny app*/
create table dps_student_aggregate_nbhd as
(select
    nbhd_name,
    count(case
            when primary_disability != 'No Disability' then primary_disability
            else null
            end) as students_disability,
    count (student_number) as unique_students,
    count(case
            when race = 'Hispanic' then race
            else null
            end) as hispanic_students,
    count(case
            when has_transportation = 'Yes' then has_transportation
            else null
            end) as students_with_transportation,
    count(case
            when primary_home_language != 'English' then primary_home_language
            else null
            end) as non_english_speakers
from shiny.dps_students
group by nbhd_name);

/*Calculating the percentages for the dps aggregate dataset*/
create table shiny.dps_student_aggregate_nbhd as
(select nbhd_name, 
round(students_disability*100.0/unique_students, 1) as perc_disable_students, 
round(hispanic_students*100.0/unique_students, 1) as perc_hispanic_students,
round(students_with_transportation*100.0/unique_students, 1) as perc_with_transport_students,
round(non_english_speakers*100.0/unique_students, 1) as perc_nonenglish_students
from dps_student_aggregate_nbhd);

/*Joining libraries, playgrounds, museums, rec_centers and parks */
/*Libraries*/
create table shiny.libraries
as (select A.*, B.nbhd_id, B.nbhd_name 
    from clean.libraries as A, clean.blockgroup_nbhds as B
    where A.bgroup_id2 = B.bgroup_id2
    );
 
/*Playgrounds*/
create table shiny.playgrounds
as (select A.*, B.nbhd_id, B.nbhd_name 
    from clean.playgrounds as A, clean.blockgroup_nbhds as B
    where A.bgroup_id2 = B.bgroup_id2
    );  
   
 /*Parks*/
create table shiny.parks
as (select A.*, B.nbhd_id, B.nbhd_name 
    from clean.parks as A, clean.blockgroup_nbhds as B
    where A.bgroup_id2 = B.bgroup_id2
    );  
   
/*Rec_centers*/
create table shiny.rec_centers
as (select A.*, B.nbhd_id, B.nbhd_name 
    from clean.rec_centers as A, clean.blockgroup_nbhds as B
    where A.bgroup_id2 = B.bgroup_id2
    );  
   
/*Rec_centers*/
create table shiny.museums
as (select A.*, B.nbhd_id, B.nbhd_name 
    from clean.museums as A, clean.blockgroup_nbhds as B
    where A.bgroup_id2 = B.bgroup_id2
    );  
