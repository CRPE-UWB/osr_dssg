/* Kellie helping Sree with the Shiny schema */
create table shiny.demographics
as (select dem.*, nbhd_id, nbhd_name
	from clean.acs_demographics as dem, clean.blockgroup_nbhds as bg_nbhd
	where dem.id2=bg_nbhd.bgroup_id2
	);

/* Kellie helping Sree with the Shiny schema */
create table shiny.dps_students
as (select students.*, block_group, nbhd_id, nbhd_name
	from clean.dps_students as students, clean.dps_block_locations as blocks, clean.blockgroup_nbhds as bg_nbhd
	where students.block=blocks.block and blocks.block_group=bg_nbhd.bgroup_id2
	);

/* Kellie helping Sree with the Shiny schema */
create table shiny.nbhd_program_summary as (
select nbhd_id, nbhd_name,
sum(has_academic::int) as total_academic,
sum(has_arts::int) as total_arts,
sum(has_cooking::int) as total_cooking,
sum(has_dance::int) as total_dance,
sum(has_drama::int) as total_drama,
sum(has_music::int) as total_music,
sum(has_nature::int) as total_nature,
sum(has_scholarships::int) as total_scholarships,
sum(has_special_needs_offerings::int) as total_special_needs,
sum(has_sports::int) as total_sports,
sum(has_stem::int) as total_stem
from shiny.summer_programs
group by nbhd_id, nbhd_name
)

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
            when race = 'Black' then race
            else null
            end) as black_students,
    count(case
            when race = 'White' then race
            else null
            end) as white_students,
    count(case
            when has_transportation = 'Yes' then has_transportation
            else null
            end) as students_with_transportation,
    count(case
            when primary_home_language != 'English' then primary_home_language
            else null
            end) as non_english_speakers,
    count(case
            when primary_disability = 'Autism' then primary_disability
            else null
            end) as students_with_autism,
    count(case
            when primary_disability = 'Emotional Disability' then primary_disability
            else null
            end) as students_with_emotional_disability,
    count(case
            when primary_disability = 'Intellectual Disability' then primary_disability
            else null
            end) as students_with_intel_disability,
    count(case
            when primary_disability = 'Other Health Impairment' then primary_disability
            else null
            end) as students_with_other_health_impairment,
    count(case
            when primary_disability = 'SLD' then primary_disability
            else null
            end) as students_with_sld,
    count(case
            when primary_disability = 'SLI' then primary_disability
            else null
            end) as students_with_sli,
    count(case
            when primary_disability != 'No Disability' then primary_disability
            else null
            end) as students_with_any_disability
from shiny.dps_students
group by nbhd_name);

create table dps_student_aggregate_bgs as
(select
    block_group,
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
            when race = 'Black' then race
            else null
            end) as black_students,
    count(case
            when race = 'White' then race
            else null
            end) as white_students,
    count(case
            when has_transportation = 'Yes' then has_transportation
            else null
            end) as students_with_transportation,
    count(case
            when primary_home_language != 'English' then primary_home_language
            else null
            end) as non_english_speakers
from shiny.dps_students
group by block_group);

/*Calculating the percentages for the dps aggregate dataset*/
create table shiny.dps_student_aggregate_nbhd as
(select nbhd_name,
round(students_disability*100.0/unique_students, 1) as perc_disable_students,
students_with_any_disability,
students_with_autism,
students_with_emotional_disability,
students_with_other_health_impairment,
students_with_intel_disability,
students_with_sli,
students_with_sld,
round(hispanic_students*100.0/unique_students, 1) as perc_hispanic_students,
round(black_students*100.0/unique_students, 1) as perc_black_students,
round(white_students*100.0/unique_students, 1) as perc_white_students,
round(students_with_transportation*100.0/unique_students, 1) as perc_with_transport_students,
round(non_english_speakers*100.0/unique_students, 1) as perc_nonenglish_students
from dps_student_aggregate_nbhd);

create table shiny.dps_student_aggregate_bgs as
(select block_group,
round(students_disability*100.0/unique_students, 1) as perc_disable_students,
round(hispanic_students*100.0/unique_students, 1) as perc_hispanic_students,
round(black_students*100.0/unique_students, 1) as perc_black_students,
round(white_students*100.0/unique_students, 1) as perc_white_students,
round(students_with_transportation*100.0/unique_students, 1) as perc_with_transport_students,
round(non_english_speakers*100.0/unique_students, 1) as perc_nonenglish_students
from dps_student_aggregate_bgs);

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

/* Kellie making neighborhood level program type summary table */
create table shiny.nbhd_program_summary as (
select nbhd_id, nbhd_name,
sum(has_academic::int) as total_academic,
sum(has_arts::int) as total_arts,
sum(has_cooking::int) as total_cooking,
sum(has_dance::int) as total_dance,
sum(has_drama::int) as total_drama,
sum(has_music::int) as total_music,
sum(has_nature::int) as total_nature,
sum(has_scholarships::int) as total_scholarships,
sum(has_special_needs_offerings::int) as total_special_needs,
sum(has_sports::int) as total_sports,
sum(has_stem::int) as total_stem
from shiny.summer_programs
group by nbhd_id, nbhd_name
)

insert into shiny.nbhd_program_summary
values (
null,
'No neighborhood selected',
(select sum(total_academic) from shiny.nbhd_program_summary),
(select sum(total_arts) from shiny.nbhd_program_summary),
(select sum(total_cooking) from shiny.nbhd_program_summary),
(select sum(total_dance) from shiny.nbhd_program_summary),
(select sum(total_drama) from shiny.nbhd_program_summary),
(select sum(total_music) from shiny.nbhd_program_summary),
(select sum(total_nature) from shiny.nbhd_program_summary),
(select sum(total_scholarships) from shiny.nbhd_program_summary),
(select sum(total_special_needs) from shiny.nbhd_program_summary),
(select sum(total_sports) from shiny.nbhd_program_summary),
(select sum(total_stem) from shiny.nbhd_program_summary)
)
