plot.simmr_input.new <- function (x, tracers = c(1, 2), title = "Tracers plot", xlab = colnames(x$mixtures)[tracers[1]], 
                              ylab = colnames(x$mixtures)[tracers[2]], sigmas = 1, group = 1, 
                              mix_name = "Mixtures", colour = TRUE, ...) 
{
  curr_rows = which(x$group %in% group)
  curr_mix = x$mixtures[curr_rows, , drop = FALSE]
  curr_n_groups = length(group)
  if (ncol(curr_mix) == 1) 
    stop("This function only works for two or more tracers")
  source_means_c = x$source_means + x$correction_means
  source_sds_c = sqrt(x$source_sds^2 + x$correction_sds^2)
  x2 = c(source_means_c[, tracers[1]], curr_mix[, tracers[1]])
  y = c(source_means_c[, tracers[2]], curr_mix[, tracers[2]])
  x_lower = c(source_means_c[, tracers[1]] - sigmas * source_sds_c[, 
                                                                   tracers[1]], curr_mix[, tracers[1]])
  x_upper = c(source_means_c[, tracers[1]] + sigmas * source_sds_c[, 
                                                                   tracers[1]], curr_mix[, tracers[1]])
  y_lower = c(source_means_c[, tracers[2]] - sigmas * source_sds_c[, 
                                                                   tracers[2]], curr_mix[, tracers[2]])
  y_upper = c(source_means_c[, tracers[2]] + sigmas * source_sds_c[, 
                                                                   tracers[2]], curr_mix[, tracers[2]])
  if (x$n_groups == 1) {
    Source = factor(c(x$source_names, rep(mix_name, nrow(curr_mix))), 
                    levels = c(mix_name, x$source_names))
  }
  else {
    Source = factor(c(x$source_names, paste( "Herbivore of Site", 
                                            x$group[curr_rows])), levels = c(paste( "Herbivore of Site", unique(x$group[curr_rows])), x$source_names))
  }
  size = c(rep(0.5, x$n_sources), rep(0.5, nrow(curr_mix)))
  df = data.frame(x = x2, y = y, x_lower, y_lower, x_upper, 
                  y_upper, Source, size)
  if (colour) {
    g = ggplot(data = df, aes(x = x, y = y, colour = Source) ) +
      scale_color_viridis(discrete = TRUE, option = "D") + 
      theme_bw() + 
      theme(plot.margin = unit(c(1,1,1,1), "cm"))+
      theme(plot.title = element_text(hjust = 0.5, size = 28), axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 16, vjust = -5), axis.title.y = element_text(size = 16))+
      labs(x = xlab, y = ylab, title = title) + 
      geom_errorbarh(aes(xmax = x_upper, xmin = x_lower, height = 0.3), size = 1) +
      geom_errorbar(aes(ymax = y_upper, ymin = y_lower, width = 0.3), size = 1)+
      geom_point(aes(x = x, y = y, shape = Source), size = 4) + 
      scale_shape_manual(values = 1:nlevels(df$Source)) + 
      theme(legend.title = element_blank(), legend.key = element_blank()) + 
      guides(color = guide_legend(override.aes = list(linetype = c(rep(0, curr_n_groups), rep(1, x$n_sources)))))+
      theme(legend.text= element_text(size=12))
     
      
  }
  else {
    g = ggplot(data = df, aes(x = x, y = y, colour = Source)) + 
      theme_bw() + labs(x = xlab, y = ylab, title = title) + 
      geom_errorbarh(aes(xmax = x_upper, xmin = x_lower, 
                         height = 0)) + geom_pointrange(aes(x = x, y = y, 
                                                            ymax = y_upper, ymin = y_lower, height = 0.2, shape = Source)) + 
      scale_shape_manual(values = 1:nlevels(df$Source)) + 
      theme(legend.title = element_blank(), legend.key = element_blank()) + 
      guides(color = guide_legend(override.aes = list(linetype = c(rep(0, 
                                                                       curr_n_groups), rep(1, x$n_sources))))) + scale_colour_grey()
  }
  print(g)
 
}
