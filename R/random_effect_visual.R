#' Function for visualizing fixed and random effects
#' Allows users to plot (in ggplot2) outcome variables vs time faceted by subject (e.g. random effect)
#' This is a simple visual used to look at longitudinal data by subject. Note that this function just takes the independent variable, the continuous (longitudinal variable), and an identifier (e.g. subject id).
#'
#' @return Faceted ggplot2 object.
#'

random_effect_visual <- function(data,
                                 independent_var,
                                 continous_var,
                                 identifier){
        p <- ggplot2::ggplot(data)+
                ggplot2::geom_point(
                        ggplot2::aes_string(x = paste(continous_var),
                                      y = paste(independent_var)))+
                ggplot2::facet_wrap(paste("~", identifier))
        p
}

