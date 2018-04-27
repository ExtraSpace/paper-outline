## ---- echo = FALSE, warning=FALSE, message=FALSE, cache=FALSE------------
source("scripts/00-function.r")
source("scripts/01-setup.r")
knitr::opts_chunk$set(comment = NULL, out.width = "100%", echo = FALSE)

## ---- echo = FALSE, warning=FALSE, message=FALSE, cache=TRUE-------------
source("scripts/03-collection.r")

## ---- out.width="100%", fig.width = 8------------------------------------
coef_plot(1, "PCR", ncomp = 5)

## ---- out.width="100%", fig.width = 8------------------------------------
coef_plot(1, "PLS1", ncomp = 5)

## ---- out.width="100%", fig.width = 8------------------------------------
coef_plot(1, "PLS2", ncomp = 5)

## ---- out.width="100%", fig.width = 8------------------------------------
coef_plot(1, "Xenv", ncomp = 5)

## ---- out.width="100%", fig.width = 8------------------------------------
coef_plot(1, "Yenv", ncomp = 5)

## ---- out.width="100%", fig.width = 8------------------------------------
coef_plot(1, "Senv", ncomp = 5)

## ---- out.width="100%", fig.width = 8------------------------------------
coef_plot(1, "Ridge")

## ---- out.width="100%", fig.width = 8------------------------------------
coef_plot(1, "Lasso")

## ---- eval = TRUE, fig.asp = 1-------------------------------------------
p1 <- get_err_plot(14, "PCR", flip_facet = TRUE)
p2 <- get_err_plot(4, "PCR", flip_facet = TRUE)
gridExtra::grid.arrange(p1, p2, nrow = 2)

## ---- eval = TRUE, fig.asp = 1-------------------------------------------
p1 <- get_err_plot(14, "PCR", flip_facet = TRUE)
p2 <- get_err_plot(9, "PCR", flip_facet = TRUE)
gridExtra::grid.arrange(p1, p2, nrow = 2)

## ---- eval = TRUE, fig.asp = 1-------------------------------------------
p1 <- get_err_plot(14, "PCR", flip_facet = TRUE)
p2 <- get_err_plot(5, "PCR", flip_facet = TRUE)
gridExtra::grid.arrange(p1, p2, nrow = 2)

## ---- eval = TRUE, fig.asp = 1-------------------------------------------
p1 <- get_err_plot(14, "PCR", flip_facet = TRUE)
p2 <- get_err_plot(1, "PCR", flip_facet = TRUE)
gridExtra::grid.arrange(p1, p2, nrow = 2)

## ------------------------------------------------------------------------
est_dta <- design_chr %>% mutate(Design = as.character(1:n())) %>%
  mutate_at(vars(p, gamma, eta, R2), as.factor) %>%
  right_join(est_error, by = "Design") %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("p", as.factor) %>%
  mutate(Response = paste0("Y", Response))

est_dta_min <- est_dta %>%
  group_by(p, gamma, eta, R2, Replication, Method, Response) %>%
  summarize(Est_Error = min(Est_Error)) %>%
  spread(Response, Est_Error)

est_mdl <- lm(cbind(Y1, Y2, Y3) ~ p * gamma * eta * R2 * Method, data = est_dta_min %>% filter(Method != "Yenv"))
anova(est_mdl)

## ------------------------------------------------------------------------
pred_dta <- design_chr %>% mutate(Design = as.character(1:n())) %>%
  mutate_at(vars(p, gamma, eta, R2), as.factor) %>%
  right_join(pred_error, by = "Design") %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("p", as.factor) %>%
  mutate(Response = paste0("Y", Response))

pred_dta_min <- pred_dta %>%
  group_by(p, gamma, eta, R2, Replication, Method, Response) %>%
  summarize(Pred_Error = min(Pred_Error)) %>%
  spread(Response, Pred_Error)

pred_mdl <- lm(cbind(Y1, Y2, Y3) ~ p * gamma * eta * R2 * Method, data = pred_dta_min %>% filter(Method != "Yenv"))
anova(pred_mdl)

## ---- fig.asp=0.7, warnings = FALSE, message = FALSE---------------------
p1 <- eff_plot("p:gamma:eta", est_mdl, title = "Estimation Error", show_errorbar = TRUE)
p2 <- eff_plot("p:gamma:eta", pred_mdl, title = "Prediction Error", show_errorbar = TRUE)
gridExtra::grid.arrange(p1, p2, nrow = 1)

## ---- fig.asp=0.7, fig.width = 8, warnings = FALSE, message = FALSE------
p1 <- eff_plot("Method:gamma:eta", est_mdl, title = "Estimation Error")
p2 <- eff_plot("Method:gamma:eta", pred_mdl, title = "Prediction Error")
gridExtra::grid.arrange(p1, p2, nrow = 1)

## ---- Extra-Plot ----
p1 <- eff_plot("gamma:eta:R2:Method", est_mdl, title = "Estimation Error")
p2 <- eff_plot("gamma:eta:R2:Method", pred_mdl, title = "Prediction Error")
gridExtra::grid.arrange(p1, p2, nrow = 1)


eff_plot("gamma:eta:R2:Method", pred_mdl)
