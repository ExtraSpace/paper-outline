## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(comment = NA, echo = FALSE)
source("Extra/Notes/outline/script/00-source-functions.r")

## ---- message=FALSE, warning=FALSE, echo=FALSE, out.width="100%", fig.asp = 0.7, fig.width=7----
# p1 <- getCoefPlot(1, 1, "PCR", 1:3)
# p2 <- getErrorPlot(1, 1, "PCR")
# gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(2/3, 1/3))

## ---- echo = FALSE, include = FALSE--------------------------------------
# coef_df <- function(dgn, rep, method) {
#   chr2num <- function(x) as.numeric(gsub("[[:alpha:]]", "", x))
#   design <- design %>% slice(dgn) %>% 
#     mutate_if(is_list, map_chr, list2chr) %>% 
#     mutate(Design = as.integer(dgn))
#   coef <- coef %>% 
#     filter(Design %in% dgn, 
#            Replicate %in% rep) %>% 
#     select(Design, Replicate, estBeta, trueBeta)
#   true_coef <- coef %>% 
#     mutate(trueBeta = map(trueBeta, reshape2::melt, 
#                           varnames = c("Predictors", "Response"),
#                           value.name = "True")) %>% 
#     unnest(trueBeta)
#   est_coef <- coef %>% unnest(estBeta) %>% 
#     filter(Method %in% method) %>% 
#     mutate(coef = map(coef, function(cf){
#       reshape2::melt(
#         cf,
#         varnames = c("Predictors", "Response", "TuningParam"),
#         value.name = "Estimated"
#       ) %>% mutate_at(vars(Predictors, Response, TuningParam), chr2num)
#     })) %>% 
#     unnest(coef)
#   plot_df <- design %>% right_join(true_coef) %>% right_join(est_coef)
#   return(plot_df)
# }
# plot_coef <- function(coef_df, ncomp = 5) {
#   udf <- coef_df %>% select(n:q) %>% unique()
#   coef_df <- coef_df %>% 
#     gather(coef_type, coef, c(True, Estimated))
#   if (length(ncomp) == 1) {
#     coef_df <- coef_df %>% filter(TuningParam <= ncomp)
#   } else {
#     coef_df <- coef_df %>% filter(TuningParam %in% ncomp)
#   }
#   plt <- ggplot(coef_df, aes(Predictors, coef, 
#                       color = coef_type, 
#                       group = coef_type,
#                       linetype = coef_type)) +
#     stat_summary(fun.y = mean, geom = "point", size = 0.4) +
#     stat_summary(fun.y = mean, geom = "line") +
#     facet_grid(Response ~ TuningParam, 
#                labeller = label_both) +
#     theme_light() +
#     theme(legend.position = "bottom",
#           plot.subtitle = element_text(
#             family = "Arial Narrow"),
#           panel.grid = element_line(color = "#dfdfdf"),
#           strip.background = element_rect(fill = "#dedede"),
#           strip.text = element_text(color = "#232323")) +
#     labs(x = "Predictors", 
#          y = "Regression Coefficients",
#          color = unique(coef_df$Method), 
#          linetype = unique(coef_df$Method)) +
#     ggtitle(paste0("True and Estimated Regression Coefficients",
#                   " (Design: ", unique(coef_df$Design), ")"),
#             paste(paste(names(udf), udf, sep = ": "), 
#                   collapse = ", "))
#   return(plt)
# }


## ---- echo = TRUE, include = FALSE --------------------
# plot_coef(coef_df(16, 1:10, "PLS1"), 1:2)
# plot_coef(coef_df(16, 1:10, "PLS2"), 1:2)
# plot_coef(coef_df(16, 1:10, "PLS1"), 3:4)
# plot_coef(coef_df(16, 1:10, "PLS2"), 3:4)

## ---- CoefPlot, eval = FALSE-------------------------------------------------------
## Saving coefficient plots for each design for each model only using 1 to 7 components separately
for (mthd in coef$estBeta[[1]]$Method) {
  # pdf(file = paste0("Extra/Notes/outline/plots/indiv-coef/", mthd, ".pdf"),
  #     width = 12, height = 5, onefile = TRUE)
  svg(file = paste0("Extra/Notes/outline/plots/indiv-coef/", mthd, ".svg"),
      width = 12, height = 5, onefile = TRUE)
  for (dgn in unique(coef$Design)) {
    lbl <- design %>% mutate_if(is.list, map_chr, list2chr) %>% slice(dgn)
    plt <- getCoefPlot(dgn, 1:10, mthd, 1:7, lbl)
    plot(plt)
  }
  dev.off()
}

## ---- ErrorPlot, eval = FALSE-------------------------------------------------------
## Saving coefficient plots for each design for each model only using 1 to 7 components separately
for (mthd in coef$estBeta[[1]]$Method) {
  # pdf(file = paste0("Extra/Notes/outline/plots/error/", mthd, ".pdf"),
  #     width = 6, height = 7, onefile = TRUE)
  svg(file = paste0("Extra/Notes/outline/plots/error/", mthd, ".svg"),
      width = 6, height = 7, onefile = TRUE)
  for (dgn in unique(coef$Design)) {
    lbl <- design %>% select(-q, -type) %>% mutate_if(is.list, map_chr, list2chr) %>% slice(dgn)
    plt <- getErrorPlot(dgn, 1:10, mthd, label = lbl)
    plot(plt)
  }
  dev.off()
}

## ----pred_err, cache=TRUE------------------------------------------------
mthds <- unique(coef$estBeta[[1]]$Method)
dgns <- unique(coef$Design)
reps <- unique(coef$Replicate)
pred_err <- get_error(dgns, reps, mthds)

## ----manova--------------------------------------------------------------
dta_stack <- pred_err %>% 
  mutate_at("Response", function(x) paste0("Y", x)) %>% 
  left_join(design %>% mutate_if(is.list, map_chr, list2chr) %>% 
              mutate(Design = 1:n())) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_at(vars(p, R2, gamma), as.factor)
dta <- dta_stack %>% 
  spread(Response, Pred_Error) 
min_pred_err <- dta %>% 
  select(Design, Replicate, Method, Tuning_Param, 
         Y1, Y2, Y3, gamma, R2, p) %>% 
  group_by(Design, Replicate, Method, gamma, R2, p) %>% 
  summarize(Y1 = min(Y1), Y2 = min(Y2), Y3 = min(Y3))
mdl <- lm(cbind(Y1, Y2, Y3) ~ p * gamma * R2 * Method, 
          data = min_pred_err)
summary(manova(mdl))

## ----effect_plot, echo = FALSE, out.width = "100%", fig.width = 8, fig.asp=1----
eff <- allEffects(mdl)
eff_df <- map_df(eff[[1]], as.data.frame, .id = "Response") %>% as_tibble()
ggplot(eff_df, aes(Method, fit, ymin = lower, ymax = upper, color = Response)) +
  geom_line(aes(group = Response)) + geom_point() + geom_errorbar(width = 0.2) +
  facet_grid(p + gamma ~ R2, labeller = label_both) +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = NULL, y = "Average Minimum Prediction Error") +
  ggtitle("Effect plot from MANOVA model",
          paste("Averaged over replicated and taken only minimum",
                "error over differet tuning parameters"))

stat_mean <- function(plt, ...){
  plt + geom_ribbon(alpha = 0.4, size = 0.1) +
    stat_summary(fun.y = mean, geom = "line", ...) +
    stat_summary(fun.y = mean, geom = "point", shape = 21, 
                 color = "black", stroke = 0.8, ..., size = 0.5)
}
my_theme <- function(plt, ...){
  plt + theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.subtitle = element_text(family = "mono")
  )
}

plts <- lapply(1:3, function(x){
  plt <- ggplot(eff_df, aes(Method, fit, ymin = lower, ymax = upper, 
                          fill = gamma, color = gamma, group = gamma)) +
    ggforce::facet_grid_paginate(
      Response + R2 ~ p, labeller = label_both, 
      nrow = 2, ncol = 2, page = x) +
    labs(x = NULL, y = "Average prediction error",
         col = "Decay of predictor variables (gamma)",
         fill = "Decay of predictor variables (gamma)") +
    ggtitle("Effect plot of model:", 
            subtitle = capture.output(mdl$call$formula))
  plt <- plt %>% 
    stat_mean() %>% 
    my_theme()
})

# pdf(file = paste0("Extra/Notes/outline/plots/effect-plot", ".pdf"),
#     width = 6, height = 7, onefile = TRUE)
svg(file = paste0("Extra/Notes/outline/plots/effect-plot", ".svg"),
    width = 6, height = 7, onefile = TRUE)
plts
dev.off()

