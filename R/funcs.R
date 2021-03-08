# get legend from an existing ggplot object
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# reasonable assurance table function 
ratab <- function(seg, yrsel, epcdata, outtxt1 = 'All years below threshold so far, not necessary for NMC Actions 2-5', outtxt2 = "All years met threshold, not necessary for NMC Actions 3-5", outtxt3 = "Not necessary due to observed water quality and seagrass conditions in the bay segment", outtxt45 = "Not necessary when chlorophyll-<i>a</i> threshold met"){
  
  trgs <- targets %>% 
    filter(bay_segment %in% !!seg) %>% 
    select(bay_segment, chla_thresh)
  
  segname <- targets %>% 
    filter(bay_segment %in% !!seg) %>% 
    pull(name)
  
  hydroload <- tibble(
    bay_segment = c('OTB', 'HB', 'MTB', 'LTB'), 
    loadest = c(486, 1451, 799, 349)
  ) %>% 
    filter(bay_segment %in% !!seg) %>% 
    pull(loadest)
  
  avedat <- anlz_avedat(epcdata) %>%
    .$ann %>% 
    filter(yr > 2014) %>% 
    filter(var %in% 'mean_chla') %>% 
    filter(bay_segment %in% !!seg) %>% 
    select(-var) %>% 
    mutate(yr = factor(yr, levels = seq(2015, 2021))) %>% 
    complete(bay_segment, yr) %>% 
    left_join(trgs, by = 'bay_segment') %>% 
    mutate(
      yr = as.numeric(as.character(yr)),
      val = case_when(
        yr <= yrsel ~ val, 
        T ~ NaN
      ),
      met = val >= chla_thresh, 
      out1 = ifelse(met, 'Yes', 'No'), 
      out1 = ifelse(is.na(met), '', paste0(round(val, 1), ' (', out1, ')')), 
      out1col = case_when(
        grepl('No', out1) ~ 'lightgreen', 
        grepl('Yes', out1) ~ '#FF6347', 
        out1 == '' ~ 'white'
      ),
      sums = stats::filter(met, filter= rep(1, 2), sides = 1), 
      sums = case_when(
        sums >= 2 ~ 'Yes', 
        sums < 2 ~ 'No', 
        is.na(sums) ~ ''
      ),
      sumscol = case_when(
        grepl('No', sums) ~ 'lightgreen', 
        grepl('Yes', sums) ~ '#FF6347', 
        sums == '' ~ 'white'
      ),
      act3 = case_when(
        sums == 'No' ~ 'N/A',
        sums == 'Yes' ~ 'Check data',
        T ~ sums
      ),
      act3col = case_when(
        act3 == 'N/A' ~ 'lightgreen', 
        act3 == 'Check data' ~ '#FF6347', 
        T ~ 'white'
      )
    ) %>% 
    filter(yr > 2016)
  
  out <- paste0('
      <table border="1" style="background-color:lightblue;text-align:center;color:black">
        <col width = "500">
        <col width = "100">
        <col width = "100">
        <col width = "100">
        <col width = "100">
        <col width = "100">
        <col width = "400">
        <tr>
          <td rowspan="2">Bay Segment Reasonable Assurance Assessment Steps</td>
          <td colspan="5">DATA USED TO ASSESS ANNUAL REASONABLE ASSURANCE</td>
          <td rowspan="2">OUTCOME</td>
        </tr>
        <tr>
          <td>Year 1 (2017)</td>
          <td>Year 2 (2018)</td>
          <td>Year 3 (2019)</td>
          <td>Year 4 (2020)</td>
          <td>Year 5 (2021)</td>
        </tr>
        <tr>
          <td style="text-align:left"><b>NMC Action 1:</b> Determine if observed chlorophyll-a exceeds FDEP threshold of ', trgs$chla_thresh, ' ug/L</td> 
          <td style="background-color:', avedat[avedat$yr == 2017, 'out1col'], '">', avedat[avedat$yr == 2017, "out1"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2018, 'out1col'], '">', avedat[avedat$yr == 2018, "out1"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2019, 'out1col'], '">', avedat[avedat$yr == 2019, "out1"], '</td>  
          <td style="background-color:', avedat[avedat$yr == 2020, 'out1col'], '">', avedat[avedat$yr == 2020, "out1"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2021, 'out1col'], '">', avedat[avedat$yr == 2021, "out1"], '</td>
          <td style="text-align:left">', outtxt1, '</td>
        </tr>
        <tr>
          <td style="text-align:left"><b>NMC Action 2:</b> Determine if any observed chlorophyll-<i>a</i> exceedences occurred for 2 consecutive years</td> 
          <td style="background-color:', avedat[avedat$yr == 2017, 'sumscol'], '">', avedat[avedat$yr == 2017, "sums"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2018, 'sumscol'], '">', avedat[avedat$yr == 2018, "sums"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2019, 'sumscol'], '">', avedat[avedat$yr == 2019, "sums"], '</td>  
          <td style="background-color:', avedat[avedat$yr == 2020, 'sumscol'], '">', avedat[avedat$yr == 2020, "sums"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2021, 'sumscol'], '">', avedat[avedat$yr == 2021, "sums"], '</td>
          <td style="text-align:left">', outtxt2, '</td>
        </tr>
        <tr>
          <td style="text-align:left"><b>NMC Action 3:</b> Determine if observed hydrologically-normalized total load exceeds federally-recognized TMDL of ', hydroload, ' tons/year </td> 
          <td style="background-color:', avedat[avedat$yr == 2017, 'act3col'], '">', avedat[avedat$yr == 2017, "act3"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2018, 'act3col'], '">', avedat[avedat$yr == 2018, "act3"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2019, 'act3col'], '">', avedat[avedat$yr == 2019, "act3"], '</td>  
          <td style="background-color:', avedat[avedat$yr == 2020, 'act3col'], '">', avedat[avedat$yr == 2020, "act3"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2021, 'act3col'], '">', avedat[avedat$yr == 2021, "act3"], '</td>
          <td style="text-align:left">', outtxt3, '</td>
        </tr>
        <tr>
          <td style="text-align:left" colspan="6"><b>NMC Actions 4-5</b>: Determine if any entity/source/facility specific exceedences of 5-yr average allocation occurred during implementation period</td>
          <td style="text-align:left">', outtxt45, '</td>
        </tr>
      </table>
      ')
  
  out <- htmltools::HTML(out)
  
  return(out)
  
}

# caption for ra table
ratabcap <- function(segin){
  
  namein <- targets %>% 
    filter(bay_segment %in% !!segin) %>% 
    pull(name)
  
  out <- paste0("<table><col width = '1200'><caption>(\\#tab:", segin, "outcome) Demonstration of reasonable assurance assessment steps for ", namein, ". Green and red squares indicate outcomes of decision points outlined in the Consortium's reasonable assurance assessment framework (Figure \\@ref(fig:decision)).</caption><tr></tr></table>")
  
  cat(out)
  
}