# caption for ra table
ratabcap <- function(segin){

  namein <- targets %>%
    filter(bay_segment %in% !!segin) %>%
    pull(name)

  out <- paste0("Demonstration of reasonable assurance assessment steps for ", namein, ". Green and red squares indicate outcomes of decision points outlined in the Consortium's reasonable assurance assessment framework.")

  return(out)

}

# header table with to, from, etc.
headertab <- function(dt = NULL, maxyr ,fsz = 13){

  if(is.null(dt))
    dt <- as.character(Sys.Date())

  totab <- tibble(
    first = c('TO:', '', 'FROM:', 'DATE:', 'SUBJECT:', 'cc', '', '', '', ''),
    second = c(
      'Adam Blalock, FDEP', 'Daniel Blackman, US EPA Region 4', 'Ed Sherwood, TBEP Executive Director (NMC Facilitator)',
      dt,
      paste(maxyr, 'Tampa Bay Nutrient Management Compliance Assessment Results'),
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
    align(j = 2:3, align = 'center') %>%
    merge_at(i = 2:3, j = 1) %>%
    merge_at(i = 4:5, j = 1) %>%
    merge_at(i = 6:7, j = 1) %>%
    merge_at(i = 8:9, j = 1) %>%
    width(j = 1, 3.5) %>%
    width(j = 2:3, 1.5) %>%
    set_table_properties(
      opts_pdf = list(arraystretch = 3)
    ) %>%
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
    width(j = 2, 5) %>%
    colformat_md() %>%
    font(fontname = 'Lato light', part = 'all') %>%
    delete_part('header') %>%
    border_remove() %>%
    valign(valign = 'top') %>%
    fontsize(size = fsz)

  return(out)

}

# hydro load table
hydrotab <- function(maxyr, noaa_key, fsz = 13){

  levs <- c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay')

  # get adjustment estimates
  hydroload <- try(anlz_hydroload(maxyr, noaa_key), silent = T)
  while(inherits(hydroload, 'try-error'))
    hydroload <- try(anlz_hydroload(maxyr, noaa_key), silent = T)

  # extra static content
  histest <- tibble::tibble(
    `Bay Segment` = levs,
    `1992 - 1994 Hydrology (95% Prediction Interval, million m3)` = c('383 - 548', '753-1110', '524-756', '312-402')
  )

  # format
  totab <- hydroload %>%
    left_join(histest, ., by = 'Bay Segment') %>%
    select(-Year, -`Adjusted?`, -`Compliance Load`) %>%
    mutate(`Bay Segment` = factor(`Bay Segment`, levels = levs)) %>%
    arrange(`Bay Segment`)

  out <- flextable(totab) %>%
    width(j = 1:4, width = 6.5 / 4) %>%
    bold(part = 'header') %>%
    colformat_double(digits = 2) %>%
    font(fontname = 'Lato light', part = 'all') %>%
    valign(valign = 'top', part = 'header') %>%
    fontsize(size = fsz, part = 'all')

  return(out)

}

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

# annual chlorophyll figure
show_rachlplot <- function(wqdat, maxyr, fml){

  yrrng <- c(1975, maxyr)
  p1 <- show_rathrplot(wqdat, bay_segment = "OTB", thr = "chla", yrrng = yrrng, family = fml, thrs = T)
  p1leg <- g_legend(p1)
  p1 <- p1 + theme(legend.position = 'none')
  p2 <- show_rathrplot(wqdat, bay_segment = "HB", thr = "chla", yrrng = yrrng, family = fml, thrs = T) + theme(legend.position = 'none')
  p3 <- show_rathrplot(wqdat, bay_segment = "MTB", thr = "chla", yrrng = yrrng, family = fml, thrs = T) + theme(legend.position = 'none')
  p4 <- show_rathrplot(wqdat, bay_segment = "LTB", thr = "chla", yrrng = yrrng, family = fml, thrs = T) + theme(legend.position = 'none')

  # align
  # Get the widths
  pA <- ggplot_gtable(ggplot_build(p1))
  pB <- ggplot_gtable(ggplot_build(p2))
  pC <- ggplot_gtable(ggplot_build(p3))
  pD <- ggplot_gtable(ggplot_build(p4))
  maxWidth = grid::unit.pmax(pA$widths[2:3], pB$widths[2:3], pD$widths[2:3], pD$widths[2:3])

  # Set the widths
  pA$widths[2:3] <- maxWidth
  pB$widths[2:3] <- maxWidth
  pC$widths[2:3] <- maxWidth
  pD$widths[2:3] <- maxWidth

  grid.arrange(
    p1leg,
    arrangeGrob(pA, pB, ncol = 2),
    arrangeGrob(pC, pD, ncol = 2),
    ncol = 1, heights = c(0.1, 1, 1)
  )

}

# chlorophyll boxplots all segments
show_chlboxplot <- function(wqdat, maxyr, fml){

  yrrng <- c(1975, maxyr)
  txtcol <- 'black'
  thrthm <- theme(
    plot.background = element_rect(fill = NA, color = NA),
    axis.text.y = element_text(colour = txtcol, size = 12),
    axis.title = element_blank(),
    plot.title = element_text(size = 22, colour = txtcol),
    legend.text = element_text(size = 16, colour = txtcol),
    axis.text.x = element_text(size = 14, colour = txtcol, angle = 0, hjust = 0.5),
    text = element_text(family = fml)
  )

  p1 <- show_boxplot(wqdat, bay_segment = "OTB", yrrng = yrrng, yrsel = maxyr, family = fml) + thrthm
  p1leg <- g_legend(p1)
  p1 <- p1 + theme(legend.position = 'none')
  p2 <- show_boxplot(wqdat, bay_segment = "HB", yrrng = yrrng, yrsel = maxyr, family = fml) + thrthm + theme(legend.position = 'none')
  p3 <- show_boxplot(wqdat, bay_segment = "MTB", yrrng = yrrng, yrsel = maxyr, family = fml) + thrthm + theme(legend.position = 'none')
  p4 <- show_boxplot(wqdat, bay_segment = "LTB",  yrrng = yrrng, yrsel = maxyr, family = fml) + thrthm + theme(legend.position = 'none')

  # align
  # Get the widths
  pA <- ggplot_gtable(ggplot_build(p1))
  pB <- ggplot_gtable(ggplot_build(p2))
  pC <- ggplot_gtable(ggplot_build(p3))
  pD <- ggplot_gtable(ggplot_build(p4))
  maxWidth = grid::unit.pmax(pA$widths[2:3], pB$widths[2:3], pD$widths[2:3], pD$widths[2:3])

  # Set the widths
  pA$widths[2:3] <- maxWidth
  pB$widths[2:3] <- maxWidth
  pC$widths[2:3] <- maxWidth
  pD$widths[2:3] <- maxWidth

  grid.arrange(
    p1leg,
    arrangeGrob(pA, pB, ncol = 2),
    arrangeGrob(pC, pD, ncol = 2),
    ncol = 1, heights = c(0.1, 1, 1)
  )

}

# chloropyll matrix
show_chlmatrix <- function(wqdat, maxyr, fml){

  out <- show_wqmatrix(wqdat, param = 'chla', yrrng = c(1975, maxyr), txtsz = 5, abbrev = T, family = fml) +
    geom_hline(yintercept = 2021.5, size = 2, color = 'grey') +
    theme(
      plot.background = element_rect(fill = NA, color = NA),
      axis.text.y = element_text(size = 14, colour = 'black'),
      axis.text.x = element_text(size = 14, colour = 'black'),
      plot.title = element_text(size = 22, colour = 'black'),
      text = element_text(family = fml)
    )

  return(out)

}
