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
    axlab <- ifelse(thr == 'chla', expression("Mean Ann. Chl-a ("~ mu * "g\u00B7L"^-1 *")"),
                    ifelse(thr == 'la', expression("Mean Ann. Light Att. (m  " ^-1 *")"), NA))
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
ratab <- function(seg, yrsel, epcdata, outtxt1 = 'All years below threshold so far, not necessary for NMC Actions 2-5', outtxt2 = "All years met threshold, not necessary for NMC Actions 3-5", outtxt3 = "Not necessary due to observed water quality and seagrass conditions in the bay segment", outtxt45 = "Not necessary when chlorophyll-*a* threshold met", fsz = 13){

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
      out1 = ifelse(is.na(met), '', paste0(out1, ' (', round(val, 1), ')')),
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

  totab <- tibble(
    col1 = c(
      'Bay Segment Reasonable Assurance Assessment Steps',
      NA,
      paste('**NMC Action 1:** Determine if observed chlorophyll-a exceeds FDEP threshold of', trgs$chla_thresh, 'ug/L'),
      '**NMC Action 2:** Determine if any observed chlorophyll-*a* exceedences occurred for 2 consecutive years',
      paste('**NMC Action 3:** Determine if observed hydrologically-normalized total load exceeds federally-recognized TMDL of ', hydroload, 'tons/year '),
      '**NMC Actions 4-5:** Determine if any entity/source/facility specific exceedences of 5-yr average allocation occurred during implementation period'
    ),
    col2 = c(
      'DATA USED TO ASSESS ANNUAL REASONABLE ASSURANCE',
      'Year 1 (2022)',
      avedat[avedat$yr == 2022, "out1", drop = T],
      avedat[avedat$yr == 2022, "sums", drop = T],
      avedat[avedat$yr == 2022, "act3", drop = T],
      NA
    ),
    col3 = c(
      NA,
      'Year 2 (2023)',
      avedat[avedat$yr == 2023, "out1", drop = T],
      avedat[avedat$yr == 2023, "sums", drop = T],
      avedat[avedat$yr == 2023, "act3", drop = T],
      NA
    ),
    col4 = c(
      NA,
      'Year 3 (2024)',
      avedat[avedat$yr == 2024, "out1", drop = T],
      avedat[avedat$yr == 2024, "sums", drop = T],
      avedat[avedat$yr == 2024, "act3", drop = T],
      NA
    ),
    col5 = c(
      NA,
      'Year 4 (2025)',
      avedat[avedat$yr == 2025, "out1", drop = T],
      avedat[avedat$yr == 2025, "sums", drop = T],
      avedat[avedat$yr == 2025, "act3", drop = T],
      NA
    ),
    col6 = c(
      NA,
      'Year 5 (2026)',
      avedat[avedat$yr == 2026, "out1", drop = T],
      avedat[avedat$yr == 2026, "sums", drop = T],
      avedat[avedat$yr == 2026, "act3", drop = T],
      NA
    ),
    col7 = c(
      'OUTCOME', NA, outtxt1, outtxt2, outtxt3, outtxt45
    )
  )

 out <- flextable(totab) %>%
    font(fontname = 'Lato light', part = 'all') %>%
    fontsize(size = fsz) %>%
    delete_part('header') %>%
    border_inner() %>%
    border_outer() %>%
    width(j = 1, width = 2, unit = 'in') %>%
    width(j = 2:6, width = 3 / 5, unit = 'in') %>%
    width(j = 7, width = 1.5, unit = 'in')%>%
    align(i = 1:2, align = 'center') %>%
    align(i = 3:5, j = 2:6, align = 'center') %>%
    colformat_md() %>%
    bg(bg = 'lightblue') %>%
    bg(i = 3, j = 2, bg = avedat[avedat$yr == 2022, 'out1col', drop = T]) %>%
    bg(i = 4, j = 2, bg = avedat[avedat$yr == 2022, 'sumscol', drop = T]) %>%
    bg(i = 5, j = 2, bg = avedat[avedat$yr == 2022, 'act3col', drop = T]) %>%
    bg(i = 3, j = 3, bg = avedat[avedat$yr == 2023, 'out1col', drop = T]) %>%
    bg(i = 4, j = 3, bg = avedat[avedat$yr == 2023, 'sumscol', drop = T]) %>%
    bg(i = 5, j = 3, bg = avedat[avedat$yr == 2023, 'act3col', drop = T]) %>%
    bg(i = 3, j = 4, bg = avedat[avedat$yr == 2024, 'out1col', drop = T]) %>%
    bg(i = 4, j = 4, bg = avedat[avedat$yr == 2024, 'sumscol', drop = T]) %>%
    bg(i = 5, j = 4, bg = avedat[avedat$yr == 2024, 'act3col', drop = T]) %>%
    bg(i = 3, j = 5, bg = avedat[avedat$yr == 2025, 'out1col', drop = T]) %>%
    bg(i = 4, j = 5, bg = avedat[avedat$yr == 2025, 'sumscol', drop = T]) %>%
    bg(i = 5, j = 5, bg = avedat[avedat$yr == 2025, 'act3col', drop = T]) %>%
    bg(i = 3, j = 6, bg = avedat[avedat$yr == 2026, 'out1col', drop = T]) %>%
    bg(i = 4, j = 6, bg = avedat[avedat$yr == 2026, 'sumscol', drop = T]) %>%
    bg(i = 5, j = 6, bg = avedat[avedat$yr == 2026, 'act3col', drop = T]) %>%
    merge_at(i = 1:2, j = 1) %>%
    merge_at(i = 1, j = 2:6) %>%
    merge_at(i = 1:2, j = 7) %>%
    merge_at(i = 6, j = 1:6)

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

# header table with to, from, etc.
headertab <- function(fsz = 13){

  totab <- tibble(
    first = c('TO:', '', 'FROM:', 'DATE:', 'SUBJECT:', 'cc', '', '', '', ''),
    second = c(
      'Adam Blalock, FDEP', 'Daniel Blackman, US EPA Region 4', 'Ed Sherwood, TBEP Executive Director (NMC Facilitator)',
      as.character(Sys.Date()), paste(params$maxyr, 'Tampa Bay Nutrient Management Compliance Assessment Results'),
      'Ken Weaver, Jessica Mostyn, Ben Ralys, Kevin O’Donnell, Kimberly Shugar (FDEP Tallahssee)',
      'Ramandeep Kaur, Vishwas Sathe, Jessica Pein, Astrid Flores Thiebaud (FDEP Tampa)',
      'Jeaneanne M. Gettle, Wade Lehmann, Jeffrey Lerner, Nancy Laurson, Felicia Burks, Tom McGill (EPA Region 4/HQ)',
      'Jeff Greenwell, Santino Provenzano, Tony Janicki, Ray Pribble (TBNMC)', 'Ed Sherwood, Maya Burke, Marcus Beck (TBEP)'
    )
  )

  out <- flextable(totab) %>%
    width(j = 1, 1) %>%
    width(j = 2, 5.5) %>%
    fontsize(i = 1:4, size = fsz) %>%
    fontsize(i = 6:10, size = fsz * 0.8461538) %>%
    delete_part('header') %>%
    border_remove() %>%
    font(fontname = 'Lato light', part = 'all') %>%
    valign(valign = 'top')

  return(out)

}

nmcstepstab <- function(fsz = 13){

  totab <- tibble(
    col1 = c(
      '**Assessment Step**',
      '**I.** Determine annual bay segment specific chlorophyll-a FDEP threshold attainment as traditionally assessed using the Decision Matrix management strategy developed by the TBEP [@tbep0400].',
      NA,
      '**II.** Review data and determine if an anomalous event(s) influenced non-attainment of the bay segment specific chlorophyll-a threshold.',
      NA,
      '**III.** Determine if the chlorophyll-a thresholds have been exceeded for <2 consecutive years.',
      NA,
      '**IV.** Determine if the bay segment specific federally-recognized TMDL has been achieved using the hydrologically-adjusted compliance assessment outlined in NMC Decision Memo #11 (Appendix 2-11).',
      NA,
      '**V.** For a given year or for multiple years, compile and report entity-specific combined source loads in comparison to 5-yr annual average reasonable assurance allocation.'
    ),
    col2 = c(
      '**Result**', '**Yes**', '**No**', '**Yes**', '**No**', '**Yes**', '**No**', '**Yes**', '**No**', '**Compile & Report**'
    ),
    col3 = c(
      '**Action**', '**NMC Action 1**', '**NMC Action 1**', '**NMC Action 2**', '**Go to III**', '**NMC Action 2**', '**Go to IV**', '**NMC Action 3**', '**Go to V**', '**NMC Action 4**'
    )
  )

  out <- flextable(totab) %>%
    font(fontname = 'Lato light', part = 'all') %>%
    fontsize(size = fsz) %>%
    delete_part('header') %>%
    border_inner() %>%
    border_outer() %>%
    width(j = 1, 3.5) %>%
    align(j = 2:3, align = 'center') %>%
    autofit() %>%
    merge_at(i = 2:3, j = 1) %>%
    merge_at(i = 4:5, j = 1) %>%
    merge_at(i = 6:7, j = 1) %>%
    merge_at(i = 8:9, j = 1) %>%
    colformat_md()

  return(out)

}

nmcactionstab <- function(fsz = 13){

  totab <- tibble(
    col1 = c("NMC Action 1 -", "NMC Action 2 -", "NMC Action 3 -", "NMC Action 4 -"),
    col2 = c(
      "A report assessing attainment of bay segment specific chlorophyll-a thresholds using the EPCHC dataset, as traditionally assessed using the Decision Matrix management strategy developed by the TBEP [@tbep0400] will be delivered to FDEP and EPA (this report).",
      "A report of the anomalous event(s) or data which influenced the bay segment chlorophyll-a exceedence will be delivered to FDEP and EPA, upon review by NMC participants (this report).",
      "Consider re-evaluation of the bay segment assimilative capacity based on nonattainment of bay segment chlorophyll-a threshold while meeting federally-recognized TMDL.",
      "If federally-recognized TMDL not achieved, compile results of hydrologic evaluation for FDEP's review and identify potential further actions needed to achieve reasonable assurance for bay segment allocations."
    )
  )

  out <- flextable(totab) %>%
    width(j = 1, 1.5) %>%
    width(j = 2, 6) %>%
    colformat_md() %>%
    font(fontname = 'Lato light', part = 'all') %>%
    delete_part('header') %>%
    border_remove() %>%
    valign(valign = 'top') %>%
    fontsize(size = fsz)

  return(out)

}
