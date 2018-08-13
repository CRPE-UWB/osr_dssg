# A string which will be converted into html for the manual page in the Shiny app

manual <- '

<h3>About the Project</h3>

<p> 
This interactive tool, built using RShiny, was created during the Summer 2018 
<a href = "http://escience.washington.edu/dssg/">Data Science 
for Social Good</a> program at the eScience Institute at the University of Washington. 
The team members who were involved in creating this tool are:

<ul>
<li>Sivan Tuchman, Project Lead</li>
<li>Jose Hernandez, Data Scientist</li>
<li>Karen Lavi, Data Scientist</li>
<li>Joe Abbate, Student Fellow</li>
<li>Andrew Taylor, Student Fellow</li>
<li>Kellie MacPhee, Student Fellow</li>
<li>Sreekanth Krishnaiah, Student Fellow</li>
<li>Haowen Zheng, Student Fellow.</li>
</ul>

Contact Sivan Tuchman (stuchman@uw.edu) for information about this tool if questions 
arise after August 17th, 2018.
</p>

<h3>General Use of This Tool</h3>

<h4>B4S Programs Tab</h4>

<p>fill this in later</p>




<h4>Other Resources Tab</h4>

<p>fill this in later</p>




<h4>B4S Searches Tab</h4>

<p>fill this in later</p>




<h4>Access Index Tab</h4>

<p>fill this in later</p>





<h3>Data Sources</h3>

<p>
Demographics data comes from 3 primary sources. <strong>Student-level data</strong> 
is from Denver Public Schools, and represents <i>only students who participated in school choice</i> 
(as these are the only students for whom we have location data). <strong>Census data</strong> comes 
from American Fact Finder at the block group level, and the American Community Survey 
(via Denver Open Data) at the neighborhood level. Neighborhoods are defined locally for the Denver 
area, and their boundaries and names were also obtained from Denver Open Data.
</p>


<p>maybe a glossary of census terms?</p>



<h3>About the Access Index</h3>

<p>
The access index returns a score from 0-100 for every block group that describes the <i>access</i> 
from the centerpoint of that block group to all available summer program sessions in Denver, 
where 0 refers to the lowest access in Denver and 100 refers to the highest. <i>Access</i> here 
refers to the sum of travel times to all locations, adjusted by the decay function, multiplied 
by the number of sessions at each location. Separate indices were calculated for each program type, 
cost threshold, driving and transit times. The <i>overall</i> driving and transit access index refers 
to the average score for each category of program, at any cost. Driving and transit access results 
are shown on the same 0-100 scale, given that transit is always longer than driving, <i>access</i> 
over transit is categorically lower than driving (see the bottom for the details of the calculation).
</p>

<p>
As expected, access index scores reflect primarily the concentration of sessions in Denver, as well 
the accessibility of highways and relatively quick driving travel to these sessions. Access index 
scores are highest in the Southeast Central Washington Park area and in the central business district 
area of downtown Denver. In contrast, access is lowest is in the outer edges of Denver on the 
Northeast and Southeast most block groups. In addition to the transit index returning overall lower 
scores, Access is also considerably more concentrated, with only a handful of block groups with almost 
relatively near proximity to a large number of sessions having an Access Index score greater than 10.
</p>

<p>
For more details about the calculation of the access index, check the report that accompanies this dashboard. 
</p>


<p>fill this in later</p>



'
