# A string which will be converted into html for the manual page in the Shiny app

manual <- '

<h3>About This Tool</h3>

<p>
Here we collect some basic information about the functionalities of this interactive web tool.
</p>

<h4>Overlaying Demographics Information</h4>

<p>
In the "B4S Programs" and "Other Resources" tabs, you can select a demographics variable
and the concentration of that demographic group across the city will be visualized on the map 
(along with the resources themselves). 
Demographic data comes from two sources, the census  (general population data) or Denver 
Public Schools (student data). Toggle between the two data sources in the left sidebar, 
and then select a demographics variable to visualize.
</p>
<p>
You can also obtain demographic information about a neighborhood by hovering your mouse
over that neighborhood in the map. Whether census (general population) or student demographics 
is displayed when hovering is dependent on the selection in the left sidebar.
Note that the student demographic information comes from a <i>subset</i> of the students in 
Denver Public Schools (for whom we have addresses, who represent the majority of students who 
participated in school choice); the number of students in the sample for a given neighborhood 
is reported as the "sample size" in the hovering text.
</p>
<p>
Demographic information for the census block groups in the "Access Index" tab can also be 
seen by hovering over the block groups (similar to the neighborhoods in the first two tabs). 
This demographic information is pulled from the census (general population) data. Student demographics 
are not reported, since there are block groups with very small student samples.
</p>

<h4>B4S Programs Tab</h4>

<p>
The "B4S Programs" tab shows all summer programs listed on the 
<a href="https://blueprint4summer.com/co/">Blueprint4Summer website</a>, 
which catalogs summer programs in Denver.
Programs appear as yellow dots in the "Map" sub-tab, and the left sidebar allows you to filter
programs by category, cost, and whether the programs have stated that they can accommodate 
special needs students. You can also filter by the neighborhood in which the program occurs, 
either by typing in a neighborhood name in the left sidebar, or by clicking on the neighborhood on the map.
Selection of multiple neighborhoods is allowed, and a thick gray outline will appear around the selected 
neighborhoods on the map.
</p>
<p>
Hovering over a yellow program dot will display information about that program. Since some programs occur 
at the same location, all program locations have been slightly offset from their original, exact location 
in order to prevent program dots from landing directly on top of one another. You may have to zoom in to see 
the program dots not overlap, however.
</p>

<h4>Other Resources Tab</h4>

<p>
The "Other Resources" tab shows public resources in the city of Denver that students might 
access during out-of-school time: parks, playgrounds, rec centers, libraries, museums, and athletic fields. 
Museum locations were collected by hand using Google searches, all other data is from the 
<a href="https://denvergov.org/opendata">Denver Open Data Catalog</a>. 
Resource names and (limited) characteristics appear if you hover over the dots in the map. 
You can filter the resources to those located in a specific set of neighborhoods, precisely as in the 
"B4S Programs" tab.
</p>

<h4>B4S Searches Tab</h4>

<p>

Sree maybe write this?

</p>

<h4>Access Index Tab</h4>

<p>
The <strong>access index</strong> returns a score from 0-100 for every block group, and describes the 
access from the centerpoint of that block group to summer program sessions in Denver. 
Here 0 refers to the lowest access in Denver and 100 refers to the highest. Access 
is based on the sum of travel times to all program locations, adjusted by the decay function, multiplied 
by the number of sessions at each location. Separate indices have been calculated for each program type, 
cost threshold, driving and transit times. You can specify in the left sidebar which types and 
costs of programs to include for the visualized index. Here are the equations:
</p>

<p>
Decay function:    <img src="decay_function.png" alt="Decay Function", height="50">
</p>
<p>
Access Index:    <img src="ai_equation.png" alt="Access Index Equation" height="50">
</p>


<p>
The <i>overall</i> driving and transit access index refers 
to the average score for each type of program, at any cost. Driving and transit access results 
are shown on the same 0-100 scale; given that transit tiems are always longer than driving, transit access 
is categorically lower than driving access.
</p>

<p>
As expected, access index scores reflect primarily the concentration of sessions in Denver, as well 
the accessibility of highways and relatively quick driving travel to these sessions. Access index 
scores are highest in the Southeast Central Washington Park area and in the central business district 
area of downtown Denver. In contrast, access is lowest for the outer edges of Denver, specifically block 
groups in the Northeast and Southwest. In addition to the transit index returning lower 
scores overall than the driving index, transit access is also considerably more concentrated, 
with only a handful of block groups being in close proximity to a large number of sessions and 
having an access index score greater than 10.
</p>

<p>
For more details about the calculation of the access index, please see the report that accompanies this dashboard. 
</p>

<h4>Credits and Contact Info</h4>

<p> 
This web-based interactive tool was created during the Summer 2018 
<a href = "http://escience.washington.edu/dssg/">Data Science 
for Social Good</a> program at the eScience Institute at the University of Washington. 
The team members involved in creating it were:
<ul>
<li>Sivan Tuchman, Project Lead (Center on Reinventing Public Education)</li>
<li>Jose Hernandez, Data Scientist (eScience Institute)</li>
<li>Karen Lavi, Data Scientist (Volunteer)</li>
<li>Joe Abbate, Student Fellow</li>
<li>Sreekanth Krishnaiah, Student Fellow</li>
<li>Kellie MacPhee, Student Fellow</li>
<li>Andrew Taylor, Student Fellow</li>
<li>Haowen Zheng, Student Fellow</li>
</ul>
This tool is built using R Shiny, and the code is available on 
<a href="https://github.com/CRPE-UWB/osr_dssg2018">GitHub</a>. 
After August 17th, 2018, Sivan Tuchman (stuchman@uw.edu) is the 
primary contact for any questions related to the tool.
</p>


'
