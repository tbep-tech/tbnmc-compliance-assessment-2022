# get legend from an existing ggplot object
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# customized version of show_thrpolot
show_rathrplot <- function(epcdata, bay_segment = c('OTB', 'HB', 'MTB', 'LTB'), thr = c('chla', 'la'), trgs = NULL, yrrng = c(1975, 2019),
                           family = NA, labelexp = TRUE, txtlab = TRUE, thrs = FALSE, partialyr = FALSE){


  maxyr <- yrrng[2]

  # default targets from data file
  if(is.null(trgs))
    trgs <- targets

  # yrrng must be in ascending order
  if(yrrng[1] >= yrrng[2])
    stop('yrrng argument must be in ascending order, e.g., c(1975, 2019)')

  # segment
  bay_segment <- match.arg(bay_segment)

  # wq to plot
  thr <- match.arg(thr)

  # colors
  cols <- c("Annual Mean"="red", "Management Target"="blue", "+1 se (small exceedance)"="blue", "+2 se (large exceedance)"="blue")

  # averages
  aves <- anlz_avedat(epcdata, partialyr = partialyr)

  # axis label
  if(labelexp)
    axlab <- dplyr::case_when(
      thr == 'chla' ~ expression("Mean Ann. Chl-a ("~ mu * "g\u00B7L"^-1 *")"),
      thr == 'la' ~ expression("Mean Ann. Light Att. (m  " ^-1 *")")
    )
  if(!labelexp)
    axlab <- dplyr::case_when(
      thr == 'chla' ~ "Mean Ann. Chl-a (ug/L)",
      thr == 'la' ~ "Mean Ann. Light Atten. (m-1)"
    )

  # get lines to plot
  toln <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment)
  trgnum <- toln %>% dplyr::pull(!!paste0(thr, '_target'))
  smlnum <- toln %>% dplyr::pull(!!paste0(thr, '_smallex'))
  thrnum <- toln %>% dplyr::pull(!!paste0(thr, '_thresh'))


  # change label location if thrs is true
  if(!thrs)
    num <- trgnum
  if(thrs)
    num <- thrnum

  # threshold label
  if(labelexp)
    trglab <- dplyr::case_when(
      thr == 'chla' ~ paste(num, "~ mu * g%.%L^{-1}"),
      thr == 'la' ~ paste(num, "~m","^{-1}")
    )
  if(!labelexp)
    trglab <- dplyr::case_when(
      thr == 'chla' ~ paste(num, "ug/L"),
      thr == 'la' ~ paste(num, "m-1")
    )

  # bay segment plot title
  ttl <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::pull(name)

  if(partialyr)
    ttl <- paste0(ttl, '*')

  # get data to plo
  toplo <- aves$ann %>%
    dplyr::filter(grepl(paste0('_', thr, '$'), var)) %>%
    mutate(var = 'yval') %>%
    dplyr::filter(bay_segment == !!bay_segment) %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    tidyr::spread(var, val)

  p <- ggplot(toplo) +
    geom_rect(xmin = 2022, xmax = 2026, ymin = -Inf, ymax = Inf, fill = 'grey', alpha = 0.6) +
    geom_point(data = toplo, aes(x = yr, y = yval, colour = "Annual Mean"), size = 3) +
    geom_line(data = toplo, aes(x = yr, y = yval, colour = "Annual Mean"), linetype = 'solid', linewidth = 0.75) +
    labs(y = axlab, title = ttl) +
    scale_x_continuous(breaks = seq(1975, maxyr, by = 5)) +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          plot.background = element_rect(fill = NA, color = NA),
          legend.position = 'top',#c(0.85, 0.95),
          legend.background = element_rect(fill=NA),
          legend.key = element_rect(fill = '#ECECEC'),
          legend.title = element_blank(),
          axis.text.y = element_text(colour = 'black', size = 14),
          axis.title = element_blank(),
          plot.title = element_text(size = 22, colour = 'black'),
          legend.text = element_text(size = 16, colour = 'black'),
          axis.text.x = element_text(colour = 'black', angle = 0, size = 14, hjust = 0.5),
          text = element_text(family)
    )

  # all targets/thresholds
  if(!thrs)
    p <- p +
    geom_hline(aes(yintercept = trgnum, colour = 'Management Target')) +
    geom_hline(aes(yintercept = smlnum, colour = '+1 se (small exceedance)'), linetype = 'dashed') +
    geom_hline(aes(yintercept = thrnum, colour = '+2 se (large exceedance)'), linetype = 'dotted') +
    scale_colour_manual(values = cols, labels = factor(names(cols), levels = names(cols))) +
    guides(colour = guide_legend(
      override.aes = list(
        shape = c(19, NA, NA, NA),
        colour = cols,
        linetype = c('solid', 'solid', 'dashed', 'dotted'),
        size = c(0.75, 0.5, 0.5, 0.5)
      )
    ))

  # thresholds only
  if(thrs)
    p <- p +
    geom_hline(aes(yintercept = thrnum, colour = '+2 se (large exceedance)'), linetype = 'dotted') +
    scale_colour_manual(values = cols[c(1, 4)], labels = factor(names(cols[c(1, 4)]), levels = names(cols[c(1, 4)]))) +
    guides(colour = guide_legend(
      override.aes = list(
        shape = c(19, NA),
        colour = cols[c(1, 4)],
        linetype = c('solid', 'dotted'),
        size = c(0.75, 0.5)
      )
    ))

  if(txtlab & !thrs)
    p <- p +
    geom_text(aes(yrrng[1], num, label = trglab), parse = labelexp, hjust = 0.2, vjust = 1, family = family, colour = 'blue')

  if(txtlab & thrs)
    p <- p +
    geom_text(aes(yrrng[1], label = trglab), y = max(toplo$yval), parse = labelexp, hjust = 0.2, vjust = 1, family = family, colour = 'blue')


  if(partialyr)
    p <- p +
    labs(caption = paste0('*Incomplete data for ', max(yrrng), ' estimated by five year average'))

  return(p)

}

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
    mutate(yr = factor(yr, levels = seq(2015, 2026))) %>%
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
          <td>Year 1 (2022)</td>
          <td>Year 2 (2023)</td>
          <td>Year 3 (2024)</td>
          <td>Year 4 (2025)</td>
          <td>Year 5 (2026)</td>
        </tr>
        <tr>
          <td style="text-align:left"><b>NMC Action 1:</b> Determine if observed chlorophyll-a exceeds FDEP threshold of ', trgs$chla_thresh, ' ug/L</td>
          <td style="background-color:', avedat[avedat$yr == 2022, 'out1col'], '">', avedat[avedat$yr == 2022, "out1"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2023, 'out1col'], '">', avedat[avedat$yr == 2023, "out1"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2024, 'out1col'], '">', avedat[avedat$yr == 2024, "out1"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2025, 'out1col'], '">', avedat[avedat$yr == 2025, "out1"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2026, 'out1col'], '">', avedat[avedat$yr == 2026, "out1"], '</td>
          <td style="text-align:left">', outtxt1, '</td>
        </tr>
        <tr>
          <td style="text-align:left"><b>NMC Action 2:</b> Determine if any observed chlorophyll-<i>a</i> exceedences occurred for 2 consecutive years</td>
          <td style="background-color:', avedat[avedat$yr == 2022, 'sumscol'], '">', avedat[avedat$yr == 2022, "sums"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2023, 'sumscol'], '">', avedat[avedat$yr == 2023, "sums"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2024, 'sumscol'], '">', avedat[avedat$yr == 2024, "sums"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2025, 'sumscol'], '">', avedat[avedat$yr == 2025, "sums"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2026, 'sumscol'], '">', avedat[avedat$yr == 2026, "sums"], '</td>
          <td style="text-align:left">', outtxt2, '</td>
        </tr>
        <tr>
          <td style="text-align:left"><b>NMC Action 3:</b> Determine if observed hydrologically-normalized total load exceeds federally-recognized TMDL of ', hydroload, ' tons/year </td>
          <td style="background-color:', avedat[avedat$yr == 2022, 'act3col'], '">', avedat[avedat$yr == 2022, "act3"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2023, 'act3col'], '">', avedat[avedat$yr == 2023, "act3"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2024, 'act3col'], '">', avedat[avedat$yr == 2024, "act3"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2025, 'act3col'], '">', avedat[avedat$yr == 2025, "act3"], '</td>
          <td style="background-color:', avedat[avedat$yr == 2026, 'act3col'], '">', avedat[avedat$yr == 2026, "act3"], '</td>
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
