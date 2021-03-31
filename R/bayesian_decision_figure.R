#' Make a figure describing the Bayesian decision-making based on a range of lower and upper reference values
#' 
#' @param n The number of observations (per group)
#' @param lrv,urv The lower and upper reference values (target for a stop or go decision)
#' @param one_sided_prob The integrated probability outside of the reference value
#' @param sd_single_measure The standard deviation of a single measurement in the assessment
#' @param sd_scale Is the standard deviation on the "log" or "linear" scale
#' @param figure_edge What integrated probability should be outside of the figure edges?
#' @param figure_steps How many points should be shown in the figure?
#' @param add_points Should any specific points be added to the figure?
#' 
#' @return A list with "data" used to make the figures, "plot_prob", and "plot_lines"
#' 
#' @importFrom cowplot theme_cowplot
#' @importFrom dplyr mutate
#' @importFrom forcats fct_inorder
#' @importFrom ggplot2 aes ggplot geom_hline geom_line geom_pointrange geom_vline geom_text guides guide_legend labs position_nudge scale_colour_manual scale_x_log10
#' @importFrom PKNCA signifString
#' @importFrom tibble tibble
#' @importFrom stats pt
#' @export
bayesian_decision_figure <- function(n,
                                     lrv, urv,
                                     one_sided_prob, sd_single_measure,
                                     sd_scale=c("log", "linear"),
                                     figure_edge=0.05, figure_steps=100, add_points=NULL) {
  # Have devtools::check() and R CMD CHECK succeed without issues for "no
  # visible binding for global variable"
  x <- y <-
    q_stop_upper <- q_stop_lower <- q_go_lower <- q_go_upper <-
    p_stop <- p_between <- p_pause <- p_go <-
    hjust <- vjust <-
    x_mean <- decision <-
    NULL
  sd_scale <- match.arg(sd_scale)
  se_difference <- sqrt(2*sd_single_measure^2)/sqrt(n)
  ref_val_to_mean <- -qt(p=one_sided_prob, df=n)*se_difference
  mean_to_edge <- qt(p=figure_edge, df=n)*se_difference
  if (sd_scale == "log") {
    lrv_scaled <- log(lrv)
    urv_scaled <- log(urv)
    add_points_scaled <-
      if (!is.null(add_points)) {
        log(add_points)
      } else {
        add_points
      }
  } else {
    lrv_scaled <- lrv
    urv_scaled <- urv
    add_points_scaled <- add_points
  }
  mean_go <- lrv_scaled + ref_val_to_mean
  mean_stop <- urv_scaled - ref_val_to_mean
  # Find the maximum range of points
  x_max_prep <- max(c(add_points_scaled, mean_go - mean_to_edge))
  x_min_prep <- min(c(add_points_scaled, mean_stop + mean_to_edge))
  # Make the range symmetric around the reference values
  diff_max <- max(c(x_max_prep - urv_scaled, lrv_scaled - x_min_prep))
  x_max <- urv_scaled + diff_max
  x_min <- lrv_scaled - diff_max
  d_plot_p <-
    tibble::tibble(
      x=
        unique(sort(c(
          add_points_scaled, lrv_scaled, urv_scaled,
          seq(x_min, x_max, length.out=100)
        ))),
      q_stop_upper=(mean_stop - x)/se_difference,
      q_stop_lower=(x - mean_stop)/se_difference,
      q_go_upper=(mean_go - x)/se_difference,
      q_go_lower=(x - mean_go)/se_difference,
      p_stop=stats::pt(q_stop_upper, df=n)*(1-stats::pt(q_stop_lower, df=n))*(1-stats::pt(q_go_lower, df=n)),
      p_go=(1-stats::pt(q_go_upper, df=n))*stats::pt(q_go_lower, df=n)*(1-stats::pt(q_stop_upper, df=n)),
      p_between=stats::pt(q_stop_upper, df=n)*stats::pt(q_go_lower, df=n),
      p_pause=1 - p_stop - p_go - p_between
    )
  if (sd_scale == "log") {
    d_plot_p$x <- exp(d_plot_p$x)
  }
  p_probability <-
    ggplot2::ggplot(d_plot_p, ggplot2::aes(x=x)) +
    ggplot2::geom_line(aes(y=p_go, colour="Go"), colour="darkgreen") +
    ggplot2::geom_line(aes(y=p_pause, colour="Pause"), colour="orange") +
    ggplot2::geom_line(aes(y=p_between, colour="Between"), colour="blue") +
    ggplot2::geom_line(aes(y=p_stop, colour="Stop"), colour="darkred") +
    ggplot2::geom_vline(xintercept=c(lrv, urv), colour="gray", linetype="63") +
    ggplot2::geom_text(
      data=tibble::tibble(
        x=c(lrv, urv),
        hjust=c(1.1, -0.1),
        vjust=0.5
      ),
      aes(x=x, label=x, y=0, hjust=hjust, vjust=vjust),
      inherit.aes=FALSE
    ) +
    ggplot2::geom_hline(yintercept=one_sided_prob, colour="gray", linetype="63") +
    ggplot2::geom_text(
      data=tibble::tibble(
        x=min(d_plot_p$x),
        hjust=0,
        vjust=-0.3,
        y=one_sided_prob
      ),
      ggplot2::aes(x=x, label=y, y=y, hjust=hjust, vjust=vjust),
      inherit.aes=FALSE
    ) +
    ggplot2::guides(colour=ggplot2::guide_legend()) +
    cowplot::theme_cowplot() +
    ggplot2::labs(
      x="True Value",
      y="Probability of Decision",
      colour="Decision"
    )
  d_plot_lines <-
    tibble::tibble(
      decision=forcats::fct_inorder(c("Stop", "Pause", "Go"), ordered=TRUE),
      x_mean=c(mean_stop, mean(c(mean_stop, mean_go)), mean_go),
      x_min=x_mean - ref_val_to_mean,
      x_max=x_mean + ref_val_to_mean
    )
  if (sd_scale == "log") {
    d_plot_lines$x_mean <- exp(d_plot_lines$x_mean)
    d_plot_lines$x_min <- exp(d_plot_lines$x_min)
    d_plot_lines$x_max <- exp(d_plot_lines$x_max)
  }
  p_lines <-
    ggplot2::ggplot(d_plot_lines, ggplot2::aes(y=decision, colour=decision, x=x_mean, xmin=x_min, xmax=x_max)) +
    ggplot2::geom_pointrange() +
    ggplot2::geom_text(
      ggplot2::aes(label=PKNCA::signifString(x_mean, 3)),
      position=ggplot2::position_nudge(y=0.1),
      show.legend=FALSE
    ) +
    ggplot2::geom_text(
      aes(x=x_min, label=PKNCA::signifString(x_min, 3)),
      position=ggplot2::position_nudge(y=0.1),
      show.legend=FALSE
    ) +
    ggplot2::geom_text(
      aes(x=x_max, label=PKNCA::signifString(x_max, 3)),
      position=ggplot2::position_nudge(y=0.1),
      show.legend=FALSE
    ) +
    ggplot2::geom_vline(xintercept=c(lrv, urv), colour="gray", linetype="63") +
    ggplot2::scale_colour_manual(values=c("darkred", "orange", "darkgreen")) +
    cowplot::theme_cowplot() +
    labs(
      x="Value",
      y=NULL,
      colour="Decision"
    )
  if (sd_scale == "log") {
    p_probability <- p_probability + ggplot2::scale_x_log10()
    p_lines <- p_lines + ggplot2::scale_x_log10()
  }
  list(
    data=d_plot_p,
    plot_prob=p_probability,
    plot_lines=p_lines
  )
}
