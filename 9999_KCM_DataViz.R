

kcm_fonts <- function(){
  sysfonts::font_add_google("lato", "lato")
  sysfonts::font_add_google("inter", "inter")
  sysfonts::font_add_google("inter", "inter")
  sysfonts::font_add_google("inter", family = "inter-light", regular.wt = 300)
  sysfonts::font_add_google("inter", family = "inter-light", regular.wt = 300)
  showtext::showtext_auto()
}

kcm_fonts()


kcm_hist <- function(data, outcome_var = data$prop, label_var = data$proplabel,
                     cat_var = data$cat,
                     ymin = 0,
                     ymax = 100,
                     lang = "en",
                     main_title = "",
                     subtitle = "",
                     source_info = "",
                     order = FALSE,
                     color_scheme = "#FDB71A"){
  if(order == TRUE){
    data = data[order(-data$prop), ]
    cat_var = cat_var[order(-outcome_var)]
    label_var = label_var[order(-outcome_var)]
    outcome_var = outcome_var[order(-outcome_var)]
  }
  update_geom_defaults("text", list(family = "inter"))
  ggplot(data, aes(x=factor(cat_var, levels = cat_var), y = outcome_var)) +
    geom_bar(stat = "identity", color = color_scheme, fill = paste0(color_scheme, "90"), width = 0.75) +
    geom_text(aes(label=label_var), vjust=-0.5, size = 5.5, fontface = "bold", color = color_scheme) +
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0.3), labels = function(x) paste0(x, "%")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
    labs(title=main_title,
         y = "",
         x = "",
         caption = source_info,
         subtitle = subtitle) +
    theme(text = element_text(size = 16, family = "inter"),
          plot.title = element_text(size = 20, family = "inter", face = "bold"),
          plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "inter", color="#585860"),
          plot.subtitle = element_text(size = 15, family = "inter-light", color="#242424"),
          axis.title.y = element_blank(),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          axis.ticks = element_blank(),
          axis.text = element_text(size = 16, family = "inter-light", color = "black"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_blank())
}

kcm_hist_flip <- function(data, outcome_var = data$prop, label_var = data$proplabel,
                     cat_var = data$cat,
                     ymin = 0,
                     ymax = 100,
                     lang = "en",
                     main_title = "",
                     subtitle = "",
                     source_info = "",
                     order = FALSE,
                     color_scheme = "#FDB71A"){
  if(order == TRUE){
    data = data[order(+data$prop), ]
    cat_var = cat_var[order(+outcome_var)]
    label_var = label_var[order(+outcome_var)]
    outcome_var = outcome_var[order(+outcome_var)]
  }
  update_geom_defaults("text", list(family = "inter"))
  ggplot(data, aes(x=factor(cat_var, levels = cat_var), y = outcome_var)) +
    geom_bar(stat = "identity", color = color_scheme, fill = color_scheme, width = 0.75) +
    geom_text(aes(label=label_var), vjust=0.5, hjust=-0.1, size = 5.5, fontface = "bold", color = color_scheme) +
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0.3), labels = function(x) paste0(x, "%")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
    labs(title=main_title,
         y = "",
         x = "",
         caption = source_info,
         subtitle = subtitle) +
    theme(text = element_text(size = 16, family = "inter"),
          plot.title = element_text(size = 20, family = "inter", face = "bold"),
          plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "inter", color="#585860"),
          plot.subtitle = element_text(size = 15, family = "inter-light", color="#242424"),
          axis.title.y = element_blank(),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          axis.ticks = element_blank(),
          axis.text = element_text(size = 16, family = "inter-light", color = "black"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_blank()) + coord_flip()
}


kcm_dv <- function(data, outcome_var = data$prop, lower_bound = data$prop_low, vallabel = data$cat,
                     upper_bound = data$prop_upp, label_var = data$proplabel,
                     ymin = 0,
                     ymax = 100,
                     lang = "en",
                     highlight = "",
                     main_title = "",
                     source_info = "",
                     subtitle = "",
                     sort = "",
                     color_scheme = "#784885",
                     label_size = 5){
  if(highlight != ""){
    data$hl_var = factor(ifelse(vallabel == highlight, 0, 1), labels = c("hl", "other"))
    fill_values = c(paste0(color_scheme, "90"), paste0(color_scheme, "90"))
  }
  else{
    data$hl_var = factor("other")
    fill_values = paste0(color_scheme, "90")
  }
  if(sort == "hi-lo"){
    data = data[order(-data$prop),]
  } else if(sort == "lo-hi"){
    data = data[order(data$prop),]
  } else if(sort == "alpha"){
    data = data[order(data$vallabel),]
  }
  if(sort == "hi-lo"){
    data = data[order(-data$prop),]
  } else if(sort == "lo-hi"){
    data = data[order(data$prop),]
  } else if(sort == "alpha"){
    data = data[order(data$vallabel),]
  }
  ci_text = paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#585860; font-size:13pt'>95% confidence </span>",
                          "<span style='color:#585860'>interval</span>")
  update_geom_defaults("text", list(family = "inter"))
  ggplot(data=data, aes(x=factor(vallabel, levels = vallabel), y=prop, fill = hl_var)) +
    geom_bar(stat="identity", color = color_scheme, width = 0.6) +
    geom_text(aes(label=label_var, y = upper_bound), vjust= -0.5,
              size=label_size, fontface = "bold", color = color_scheme) +
    geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.15, color = color_scheme, linetype = "solid") +
    scale_fill_manual(breaks = "other",
                      values = fill_values,
                      labels = paste0(" <span style='color:#585860; font-size:13pt'> ",
                                      subtitle,
                                      "<span style='color:#FFFFFF00'>-----------</span>",
                                      ci_text),
                      na.value = paste0(color_scheme, "90")) +
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0.3), labels = function(x) paste0(x, "%")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
        labs(title=main_title,
         y = "",
         x = "",
         caption = source_info) +
    theme(text = element_text(size = 16, family = "inter"),
          plot.title = element_text(size = 20, family = "inter", face = "bold"),
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0, color="#585860"),
          panel.background = element_blank(),
          #panel.border = element_rect(linetype = "solid", color = "#dddddf", fill = NA),
          axis.text = element_text(size = 16, family = "inter-light", color = "black"),
          #axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.title = element_blank(),
          legend.justification='left',
          legend.margin = margin(t=0, b=0),
          legend.text = element_markdown(family = "inter-light"))
}


kcm_dv_flip <- function(data, outcome_var = data$prop, lower_bound = data$prop_low, vallabel = data$cat,
                        upper_bound = data$prop_upp, label_var = data$proplabel,
                   ymin = 0,
                   ymax = 100,
                   lang = "en",
                   highlight = "",
                   main_title = "",
                   source_info = "",
                   subtitle = "",
                   sort = "",
                   color_scheme = "#784885",
                   label_size = 5){
  if(highlight != ""){
    data$hl_var = factor(ifelse(vallabel == highlight, 0, 1), labels = c("hl", "other"))
    fill_values = c(paste0(color_scheme, "90"), paste0(color_scheme, "90"))
  }
  else{
    data$hl_var = factor("other")
    fill_values = paste0(color_scheme, "90")
  }
  if(sort == "hi-lo"){
    data = data[order(-data$prop),]
  } else if(sort == "lo-hi"){
    data = data[order(data$prop),]
  } else if(sort == "alpha"){
    data = data[order(data$vallabel),]
  }
  if(sort == "hi-lo"){
    data = data[order(-data$prop),]
  } else if(sort == "lo-hi"){
    data = data[order(data$prop),]
  } else if(sort == "alpha"){
    data = data[order(data$vallabel),]
  }
  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#585860; font-size:13pt'>95% intervalo de confianza </span>"),
                   paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#585860; font-size:13pt'>95% confidence </span>",
                          "<span style='color:#585860'>interval</span>"))
  update_geom_defaults("text", list(family = "inter"))
  ggplot(data=data, aes(x=factor(vallabel, levels = vallabel), y=prop, fill = hl_var)) +
    geom_bar(stat="identity", color = color_scheme, width = 0.6) +
    geom_text(aes(label=label_var, y = upper_bound), vjust= 0.5, hjust=-0.15,
              size=label_size, fontface = "bold", color = color_scheme) +
    geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.15, color = color_scheme, linetype = "solid") +
    scale_fill_manual(breaks = "other",
                      values = fill_values,
                      labels = paste0(" <span style='color:#585860; font-size:13pt'> ",
                                      subtitle,
                                      "<span style='color:#FFFFFF00'>-----------</span>",
                                      ci_text),
                      na.value = paste0(color_scheme, "90")) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    #scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
    labs(title=main_title,
         y = "",
         x = "",
         caption = paste0(source_info)) +
    theme(text = element_text(size = 16, family = "inter"),
          plot.title = element_text(size = 20, family = "inter", face = "bold"),
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0, color="#585860"),
          panel.background = element_blank(),
          #panel.border = element_rect(linetype = "solid", color = "#dddddf", fill = NA),
          axis.text = element_text(size = 16, family = "inter-light", color = "black"),
          #axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.title = element_blank(),
          legend.justification='left',
          legend.margin = margin(t=0, b=0),
          legend.text = element_markdown(family = "inter-light", size=15)) + coord_flip()
}





kcm_dvgroup<- function(data,
                       depvar = data$depvar, outcome_var = data$prop,
                       lower_bound = data$prop_low, upper_bound = data$prop_upp,
                       label_var = data$proplabel, groupingvar = data$groupingvar,
                       ymin = 0,
                       ymax = 100,
                       lang = "en",
                       main_title = "",
                       source_info = "",
                       subtitle = "",
                       sort = "",
                       y_label = "",
                       x_label = "",
                       color_scheme = c("#D67619", "#264d5e", "#006848", "#4B2884"),
                       label_size = 4.25,
                       text_position = 0.75){
  fill_colors = paste0(color_scheme, "")
  if(sort == "var1"){
    data = data %>%
      group_by(groupingvar) %>%
      mutate(rank = rank(-prop)) %>%
      arrange(groupingvar, rank)
  } else if(sort == "var2"){
    data = data %>%
      group_by(groupingvar) %>%
      mutate(rank = rank(-prop)) %>%
      arrange(match(groupingvar, unique(groupingvar)[2]), rank)
  } else if(sort == "var3"){
    data = data %>%
      group_by(groupingvar) %>%
      mutate(rank = rank(-prop)) %>%
      arrange(match(groupingvar, unique(groupingvar)[3]), rank)
  } else if(sort == "alpha"){
    data = data[order(data$depvar),]
  }
  update_geom_defaults("text", list(family = "inter"))
  ggplot(data=data, aes(x=factor(depvar, levels = unique(depvar)), y=prop, fill = groupingvar, color = groupingvar)) +
    geom_bar(position = "dodge", stat="identity", width = 0.75) +
    geom_text(aes(label=label_var, y = prop, group = groupingvar),
              position = position_dodge(width = text_position),
              vjust=-.5, size = label_size, fontface = "bold",
              show.legend = FALSE) +
    #geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound),
    #              width = 0.15,
    #              position = position_dodge(width = 0.7), linetype = "solid",
    #              show.legend = FALSE) +
    scale_fill_manual(values = fill_colors) +
    scale_color_manual(values = color_scheme) +
    scale_y_continuous(limits = c(ymin, ymax), expand = expansion(mult = 0.002)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
    labs(title=main_title,
         y = y_label,
         x = x_label,
         caption = paste0(ifelse(lang == "es", "Fuente: ", "Source: "),
                          source_info)) +
    {if(subtitle != "")labs(subtitle = subtitle)}+
    theme(text = element_text(size = 16, family = "inter"),
          plot.title = element_text(size = 20, family = "inter", face = "bold"),
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0.02, family = "inter", color="#585860"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_line(linewidth = 0.6, linetype = "solid", colour = "#dddddf"),
          axis.text = element_text(size = 16, family = "inter-light", color = "black"),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.title = element_blank(),
          legend.justification='left',
          legend.margin = margin(t=0, b=0),
          legend.text = element_markdown(family = "inter-light", size=15))
}




kcm_agree <- function(data, outcome_var = data$prop, prop_labels = data$proplabel,
                      var_labels = data$varlabel, value_labels = data$vallabel,
                        lang = "en",
                        main_title = "",
                        subtitle = "",
                        source_info = "",
                        rev_values = FALSE,
                        rev_variables = FALSE,
                        hide_small_values = TRUE,
                        order_bars = FALSE,
                        subtitle_h_just = 0,
                        fixed_aspect_ratio = TRUE,
                        color_scheme = c("#FF0000", "#FF6666",  "#FFCC33", "#338585", "#006666")){
  if(!inherits(var_labels, "character") & !inherits(var_labels, "factor")){
    var_labels = as.character(var_labels)
    data$varlabels = as.character(data$varlabel)
  }
  if(!inherits(value_labels, "character") & !inherits(value_labels, "factor")){
    value_labels = as.character(value_labels)
    data$vallabel = as.character(data$vallabel)
  }
  mycolors = rev(color_scheme[seq_along(unique(value_labels))])
  if(rev_values == TRUE){
    value_labels = factor(value_labels, levels = unique(value_labels))
  } else{
    value_labels = factor(value_labels, levels = rev(unique(value_labels)))
  }
  positions = rev(unique(var_labels))
  update_geom_defaults("text", list(family = "inter"))
  if(order_bars == TRUE){
    var_labels = factor(var_labels, levels = unique(var_labels))
    data = data.frame(var_labels, value_labels, outcome_var, prop_labels)
    ggplot(data, aes(y = outcome_var, x = var_labels,
                     fill = reorder(value_labels, outcome_var), label = prop_labels)) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, data = data[data$var_labels == levels(data$var_labels)[1], ]) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, data = data[data$var_labels == levels(data$var_labels)[2], ]) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, data = data[data$var_labels == levels(data$var_labels)[3], ]) +
      geom_text(data = data[data$var_labels == levels(data$var_labels)[1], ],
                aes(label = prop_labels),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      geom_text(data = data[data$var_labels == levels(data$var_labels)[2], ],
                aes(label = ifelse(outcome_var >= 5, prop_labels, NA)),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      geom_text(data = data[data$var_labels == levels(data$var_labels)[3], ],
                aes(label = ifelse(outcome_var >= 5, prop_labels, NA)),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      geom_text(data = data[data$var_labels == levels(data$var_labels)[4], ],
                aes(label = ifelse(outcome_var >= 5, prop_labels, NA)),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      ggrepel::geom_text_repel(data = data[data$var_labels == levels(data$var_labels)[1], ],
                               aes(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA)),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4, family = "inter",
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      ggrepel::geom_text_repel(data = data[data$var_labels == levels(data$var_labels)[2], ],
                               aes(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA)),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4, family = "inter",
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      ggrepel::geom_text_repel(data = data[data$var_labels == levels(data$var_labels)[3], ],
                               aes(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA)),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4, family = "inter",
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      ggrepel::geom_text_repel(data = data[data$var_labels == levels(data$var_labels)[4], ],
                               aes(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA)),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4, family = "inter",
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      coord_flip() +
      scale_fill_manual(values = mycolors, guide=guide_legend(reverse = TRUE, nrow=1)) +
      scale_x_discrete(limits = positions, expand = c(0, 0)) +
      scale_y_continuous(expand = c(0.02, 0)) +
      labs(title = main_title,
           y = "",
           x = " ",
           caption = source_info,
           subtitle = subtitle) +
      theme(text = element_text(size = 14, family = "inter"),
            plot.title = element_text(size = 20, family = "inter", face = "bold"),
            plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "inter", color="#585860"),
            plot.subtitle = element_text(size = 20, family = "inter", face="bold"),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(margin=margin(r=0)),
            axis.ticks = element_blank(),
            # aspect.ratio = aspect_ratio,
            axis.text = element_text(size = 14, family = "inter", color = "#585860", margin=margin(r=5)),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            legend.position = "top",
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.text = element_text(family = "inter", color = "#585860"),
            legend.title = element_blank(),
            legend.justification='left',
            legend.key.size = unit(1, "line"),
            legend.margin = margin(t=5,b=5, 0, subtitle_h_just)) +
      {if(fixed_aspect_ratio)theme(aspect.ratio = 0.35)}
  } else{
    ggplot(data, aes(fill = value_labels, y = outcome_var, x = var_labels, label = prop_labels)) +
      geom_bar(position = "stack", stat = "identity", width = 0.6) +
      geom_text(label = ifelse(outcome_var >= 5, prop_labels, NA),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      ggrepel::geom_text_repel(label = ifelse(outcome_var < 5 & hide_small_values == FALSE, prop_labels, NA),
                               position = position_stack(vjust = 0.5),
                               color = "#FFFFFF", segment.color = 'transparent',
                               fontface = "bold", size = 4, family = "inter",
                               direction = "y",
                               force_pull = 0.2, force = 5) +
      coord_flip() +
      scale_fill_manual(values = mycolors, guide=guide_legend(reverse = TRUE, nrow=1)) +
      scale_x_discrete(limits = positions, expand = c(0, 0)) +
      scale_y_continuous(expand = c(0.02, 0)) +
      labs(title = main_title,
           y = "",
           x = " ",
           caption =  source_info,
           subtitle = subtitle) +
      theme(text = element_text(size = 14, family = "inter"),
            plot.title = element_text(size = 20, family = "inter", face = "bold"),
            plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "inter-light", color="#585860"),
            plot.subtitle = element_text(size = 20, family = "inter", color="black", face="bold"),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(margin=margin(r=0)),
            axis.ticks = element_blank(),
            # aspect.ratio = aspect_ratio,
            axis.text = element_text(size = 14, family = "inter", color = "#585860", margin=margin(r=5)),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            legend.position = "top",
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.text = element_text(family = "inter", color = "#585860"),
            legend.title = element_blank(),
            legend.justification='left',
            legend.key.size = unit(1, "line"),
            legend.margin = margin(t=5,b=5, 0, subtitle_h_just)) +
      {if(fixed_aspect_ratio)theme(aspect.ratio = 0.35)}
  }
}

