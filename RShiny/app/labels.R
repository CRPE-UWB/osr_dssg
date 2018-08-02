# Make labels for tooltips / popups for RShiny app

# Adding a custom html label for the racial distributions
shape_census@data$racial_dist_html <- mapply(
  
  # Inputs: 
  # neighborhood name (string),
  # percents of different races (numerics),
  # color palette generated from brewer.pal()
  function(nbhd, pct_hisp, pct_white, pct_black, pct_native, pct_asian){
    
    black_color <- colors_all_races[1]
    hispanic_color <- colors_all_races[2]
    white_color <- colors_all_races[3]
    other_color <- "gray"
    
    sprintf(
      "<div style='font-size:12px;width:180px;float:left'>
      <span style='font-size:16 px;font-weight:bold'>%s</span><br/>
      <div style='width:100%%'>
      <span style='background:%s;width:%s%%;position:absolute;left:0'>&nbsp;</span>
      <span style='background:%s;width:%s%%;position:absolute;left:%s%%'>&nbsp;</span>
      <span style='background:%s;width:%s%%;position:absolute;left:%s%%'>&nbsp;</span>
      <span style='background:%s;width:%s%%;position:absolute;left:%s%%'>&nbsp;</span>
      <br/>
      <span style='color:%s;float:left'>%.2f%% Black</span><br/>
      <span style='color:%s;float:left'>%.2f%% Hispanic</span><br/>
      <span style='color:%s;float:left'>%.2f%% White</span><br/>
      <span style='color:%s;float:left'>%.2f%% Other</span><br clear='all'/>
      </div>
      </div>",
      nbhd,
      black_color, pct_black, 
      hispanic_color, pct_hisp, pct_black,
      white_color, pct_white, pct_hisp + pct_black,
      other_color, 100 - (pct_white + pct_hisp + pct_black), pct_white + pct_hisp + pct_black,
      black_color, pct_black, 
      hispanic_color, pct_hisp, 
      white_color, pct_white, 
      other_color, 100 - (pct_white + pct_hisp + pct_black)
    ) %>% lapply(htmltools::HTML)
  },
  
  shape_census@data$NBHD_NA,
  shape_census@data$PCT_HIS,
  shape_census@data$PCT_WHI,
  shape_census@data$PCT_BLA,
  shape_census@data$PCT_NAT,
  shape_census@data$PCT_ASI
  
)

