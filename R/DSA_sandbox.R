data_final <- data|> group_by(depthwholem)|>arrange(id)|>
  mutate( roll_sd = rollapply(DO_mgL, 5, sd, fill = NA),
          roll_mean = rollapply(DO_mgL, 5, mean, fill = NA),
          slope = abs(c(NA, diff(roll_mean))) )|>
  # Require stability AND low slope
  mutate(stable = roll_sd < 0.20 & slope < 0.10) |>
  # Keep only stable points AFTER first unstable change
  filter(row_number() > max(which(slope > 0.20), na.rm = TRUE)) |>
  filter(stable)|> summarise(across( -c(DateTime, depth_halfm, id),
                                      ~ mean(.x, na.rm = TRUE) ), .groups = "drop")|>
  select(any_of(c("depthwholem", "depth_m", "temperature_C", "sp_conductivity_uScm",
                  "pH_units", "pH_mV", "DO_mgL", "DO_per", "turbidity_NTU", "ORP_mV",
                  "chlorophyll_RFU", "bga_fluorescence_RFU", "pressure_psi",
                  "latitude_deg", "longitude_deg")))



dat <- read_datafile('inst/extdata/2025-05-27_LT1.csv')
dat <- read_datafile('inst/extdata/2025-09-16_LT1.csv')
# dat <- read_datafile('inst/extdata/2025-05-13_LW1.csv')
# dat <- read_datafile('inst/extdata/2025-10-01_LW1.csv')


dat <- rename_cols(dat) |>
  dplyr::mutate(depthwholem = (janitor::round_half_up(depth_m)))

try <- is_stationary(dat,drop_cols = T,
                     plot = F)


xx <- try |>
  dplyr::select(DateTime,depth_m,temperature_C,DO_mgL,pH_units)
# xx

# rolling slope function
roll_slope <- function(x, t) {
  if (any(is.na(x)) || any(is.na(t))) return(NA_real_) # returns NA if NA exists in data (sensor or time)
  coef(stats::lm(x ~ t))[2] # Extract slope of the linear regression
}

junk <-
xx |>
  dplyr::mutate(
    numdate = as.numeric(DateTime),
    param_sd = zoo::rollapplyr(
      pH_units,
      width = 15,
      FUN = stats::sd,
      na.rm = TRUE,
      fill = NA_real_
    ),
    param_slope = zoo::rollapplyr(
      align = 'right',
      data = cbind(numdate, xx$pH_units),
      width = 15,
      by.column = FALSE,
      FUN = function(mat) {
        # print(mat)
        # t <- mat[,1]
        t <- (mat[,1] - mat[1,1]) / 60  # center data to stabilize numerically in minutes for ease of interpretation
        x <- mat[,2]

        if (any(is.na(x)) || any(is.na(t))) return(NA_real_)

        coef(stats::lm(x ~ t))[2]
      },
      fill = NA_real_
    )
  )

#** Go to AI_broken for a skeleton of the implementation
#* 1) Identify stationary blocks
#* 2) Toss 15s of data (may increase, or make adjustable), the "jiggle zone"
#* 3) calculate slope across "stationary" observations
#*    a) Could check the overall (for drift)
#*    b) Use small window slopes (8-10s) for determining "stable"
#* 4) Take median of the stable datapoints

# library(ggplot2)
ggplot(junk,
       aes(x=DateTime)) +
  geom_point(aes(y=param_slope,color = 'slope')) +
  geom_point(aes(y=scale(pH_units),color = 'temp')) +
  coord_cartesian(ylim = c(-1.5,1.5)) +
  geom_hline(yintercept = 0, lty=2) +
  geom_hline(yintercept = c(-0.02,0.02),
             col = 'firebrick',lty = 2)

test <- cbind(as.numeric(xx$DateTime), xx$temperature_C)
test

# coef(stats::lm(test[1:5,1]~test[1:5,2]))[2]

zoo::rollapplyr(
  data = test,
  width = 5,
  by.column = FALSE,
  FUN = function(mat) {
     # print(mat)
    # t <- mat[,1]
    t <- (mat[,1] - mat[1,1]) / 60  # center data to stabilize numerically in minutes for ease of interpretation
    x <- mat[,2]

    if (any(is.na(x)) || any(is.na(t))) return(NA_real_)

    coef(stats::lm(x ~ t))[2]

  }
)

plotstuff <- function(df,param){
  trash <- df |> dplyr::ungroup() |>
    dplyr::select(DateTime,depth_m,depthwholem,!!sym(param),
                  is_stationary_status) |>
    dplyr::mutate(x = c(depthwholem - lag(depthwholem,default = 0)),
           label = ifelse(x==1,depthwholem,NA))

  ggplot2::ggplot(trash) +
    ggplot2::geom_point(ggplot2::aes(x = DateTime, y = !!sym(param),
                                     color = as.factor(is_stationary_status))) +
    ggplot2::geom_label(ggplot2::aes(x = DateTime, y = !!sym(param),label = label),
               nudge_y = 0.25)
}

plotstuff(df = try,'temperature_C')
plotstuff(df = dat,'DO_mgL')
plotstuff(df = dat, 'depth_m')


