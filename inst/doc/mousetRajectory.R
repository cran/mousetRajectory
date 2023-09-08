## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(mousetRajectory)
library(ggplot2)
library(dplyr)

## ----draw_exp_layout, echo=FALSE----------------------------------------------
draw_background <- function() {
  pi_seq <- seq(0, 2 * pi, length.out = 360)
  circle <- data.frame(x = sin(pi_seq), y = cos(pi_seq))
  rectangle <- data.frame(
    x <- c(-1, 1, 1, -1, -1),
    y <- c(-1, -1, 1, 1, -1)
  )

  p1 <- ggplot() +
    theme_void() +
    coord_equal()

  p1 <- p1 + geom_path(aes(2 * x, 0.5 + y), rectangle)
  p1 <- p1 + geom_path(aes(0 + 0.2 * x, 0 + 0.2 * y), circle) # home
  p1 <- p1 + geom_path(aes(1 + 0.2 * x, 1 + 0.2 * y), circle) # right target
  p1 <- p1 + geom_path(aes(-1 + 0.2 * x, 1 + 0.2 * y), circle) # left target

  return(p1)
}

draw_background() + 
  geom_path(aes(x = c(0, 1, 1, 1, 0.9), y = c(0, 1, 0.9, 1, 1)))

## ----define_trajectories, echo=FALSE------------------------------------------
dat <- data.frame(
  Trial = rep(1:4, each = 3),
  Time2 = rep(c(0, 50, 100), 4),
  Target = c(rep("left", 6), rep("right", 6)),
  x_coord = c(
    0, -0.5, -1, # 1
    0, -0.7, -1, # 2
    0, 0.55, 1, # 3
    0, 0.45, 1 # 4
  ),
  y_coord = c(
    0, 0.6, 1, # 1
    0, 0.6, 1, # 2
    0, 0.7, 1, # 3
    0, 0.8, 1 # 4
  )
) %>%
  group_by(Trial, Target) %>%
  reframe(
    Time = c(0:100),
    x_coord = interp1(Time2, x_coord, Time, method = "spline"),
    y_coord = interp1(Time2, y_coord, Time, method = "spline")
  ) %>%
  group_by(Trial, Target)
dat <- dat %>% as_tibble()

gg_background <- draw_background()

## ----show data----------------------------------------------------------------
head(dat)

# gg_background has been created previously and is a ggplot object
gg_background + geom_path(aes(x_coord, y_coord, group = Trial), dat)

## ----recode_x_coords----------------------------------------------------------
dat <- dat %>%
  group_by(Trial) %>%
  mutate(
    # will throw a warning if times are not monotonically increasing
    is_ok = is_monotonic(Time)
  )

## ----filter_data--------------------------------------------------------------
dat <- dat %>%
  group_by(Trial) %>%
  mutate(
    x_coord = ifelse(Target == "left", -x_coord, x_coord),
    InitiationTime = time_circle_left(
      x_coord,
      y_coord,
      Time,
      x_mid = 0,
      y_mid = 0,
      radius = 0.2
    ),
    CompletionTime = time_circle_entered(
      x_coord,
      y_coord,
      Time,
      x_mid = 1,
      y_mid = 1,
      radius = 0.2
    ), 
    MovementTime = CompletionTime - InitiationTime
  ) %>%
  filter(Time >= InitiationTime & Time < CompletionTime)

dat %>%
  group_by(Trial, InitiationTime, CompletionTime, MovementTime) %>%
  count()

gg_background + geom_path(aes(x_coord, y_coord, group = Trial), dat)

## ----interpolate, message=FALSE-----------------------------------------------
dat_int <- dat %>%
  group_by(Trial) %>%
  reframe(
    Time_new = 0:100,
    x_new = interp2(Time, x_coord, 101),
    y_new = interp2(Time, y_coord, 101),
  )

## ----compute_DVs--------------------------------------------------------------
dat_int %>%
  group_by(Trial) %>%
  summarise(
    MAD = max_ad(x_new, y_new),
    AUC = auc(x_new, y_new),
    CUR = curvature(x_new, y_new)
  )

## ----plot_avg-----------------------------------------------------------------
dat_avg <- dat_int %>%
  group_by(Time_new) %>%
  summarise(
    x_avg = mean(x_new),
    y_avg = mean(y_new)
  )

gg_background +
  geom_path(aes(x_avg, y_avg), dat_avg) +
  geom_path(aes(c(0, 1), c(0, 1)), linetype = "dashed") # ideal trajectory

