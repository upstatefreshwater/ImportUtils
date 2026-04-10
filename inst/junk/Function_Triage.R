library(ImportUtils)

datafiles <- list.files('//aquadog/analysis/R_Scripts_Functions/UFI_Packages/Package_Build_Helpers/ImportUtils_testing',
                        full.names = TRUE)
stn_seconds <- 30
trim_starts <- 5
stn_depth_range_thresh <- 0.1

# Create a list to store results or error messages
process_log <- list()

for (i in seq_along(datafiles)) {
  cat(sprintf("Processing file %d of %d: %s/n", i, length(datafiles), basename(datafiles[i])))

  # Wrapping the core logic in tryCatch
  result <- tryCatch({

    # 1. Processing steps
    data_read       <- TROLL_read_data(datafiles[i])
    data_rename     <- TROLL_rename_cols(data_read)
    data_stationary <- is_stationary(data_rename,
                                     stationary_secs = stn_seconds,
                                     start_trim_secs = trim_starts,
                                     depth_range_threshold = stn_depth_range_thresh,
                                     plot = TRUE)

    data_stable     <- TROLL_sensor_stable(df = data_stationary)
    data_summary    <- TROLL_stable_summary(data_stable)

    TROLL_profile_compiler(path = datafiles[i])

    # Return a success indicator
    "Success"

  }, error = function(e) {
    # This block runs if an error occurs
    message(paste("Error in file:", datafiles[i]))
    message(paste("Reason:", e$message))

    # Return the error message so you can review it later
    return(paste("FAILED:", e$message))
  })

  # Store the outcome in our log
  process_log[[datafiles[i]]] <- result
}

# Quick summary of the run
table(unlist(process_log))


# Individual use ----
data_read       <- TROLL_read_data(datafiles[5])
data_rename     <- TROLL_rename_cols(data_read)
data_stationary <- is_stationary(data_rename,
                                 stationary_secs = 28,
                                 start_trim_secs = 10,
                                 depth_range_threshold = 0.1,
                                 plot = TRUE)

data_stable    <- TROLL_sensor_stable(df = data_stationary,
                                       value_col = turbidity_NTU,
                                       range_thresh = 8,
                                       slope_thresh = 1,
                                       plot = TRUE)

rangeandslope   <- TROLL_stable_summary(data_stable)
data_summary

xx <- cbind(default, rangeonly$turbidity_NTU_stable, rangeandslope$turbidity_NTU_stable)

# 1. Create the initial plot area.
# We use the first dataset to set the axes limits.
plot(y = xx$stationary_depth * -1,
     x = xx$turbidity_NTU_stable,
     pch = 19,
     type = 'l',
     xlab = "Turbidity (NTU)",
     ylab = "Depth (Negative)",
     main = "Depth vs Turbidity")

# 2. Add the second line.
# Make sure the column names match exactly what is in 'xx'
points(y = xx$stationary_depth * -1,
      x = xx$`rangeonly$turbidity_NTU_stable`,
      col = 'red',
      lwd = 2)
lines(y = xx$stationary_depth * -1,
       x = xx$`rangeandslope$turbidity_NTU_stable`,
       col = 'blue',
       lwd = 2)

# Mess with stationary blocks and various noise calculations ----
library(tidyverse)


# one <- TROLL_read_data('inst/extdata/2025-10-07_QL1.csv')
# two <- TROLL_rename_cols(one)

# data_stationary <- is_stationary(two,
#                                  stationary_secs = 38,
#                                  start_trim_secs = 6,
#                                  depth_range_threshold = 0.1,
#                                  plot = T) %>%
#   mutate(stationary_depth = round(stationary_depth,3))

min_n <- 5

# 1. Prep the data
junk <- data_stationary %>%
  filter(stationary_depth == 1.993) %>% #  0.993) %>%
  mutate(dttm_norm = as.numeric(DateTime - min(DateTime)) / 60)

# 2. Window Size
n_total <- nrow(junk)
n_windows <- n_total - min_n + 1

# Create a container to store results
results <- data.frame(window_start = 1:n_windows, slope = NA)

showplot <- F
# 3. The Loop
for (i in 1:n_windows) {
  # Subset data: from row 'i' to the end
  loop_dat <- junk[i:n_total, ]

  # Run model
  mod <- lm(turbidity_NTU ~ dttm_norm, data = loop_dat)
  # Store the absolute slope (b1)
  results$slope[i] <- coef(mod)[2]

  # Calculate variability stats
  mad <- stats::mad(loop_dat$turbidity_NTU)
  SE <- stats::sd(loop_dat$turbidity_NTU)/sqrt(nrow(loop_dat))

  SD <- sd(loop_dat$turbidity_NTU)

  results$SD[i] <- SD
  results$mad[i] <- mad
  results$SE[i] <- SE

  # print plots
  if(showplot){
  print(ggplot(loop_dat, aes(x = dttm_norm, y = turbidity_NTU)) +
          geom_point() + geom_smooth(method = 'lm', se = FALSE))
  }
}

# 4. Visualize the "Slope Development"
ggplot(results, aes(x = window_start, y = slope)) +
  geom_line(color = "steelblue") +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red', lty = 2) +
  labs(title = "Evolution of Slope (b1) as Start Points are Removed",
       x = "Starting Row Index",
       y = "Absolute Slope")

ggplot(results, aes(x = window_start, y = mad)) +
  geom_line(color = "steelblue") +
  geom_point() +
  labs(title = "Evolution of MAD as Start Points are Removed",
       x = "Starting Row Index",
       y = "MAD")

ggplot(results, aes(x = window_start, y = SE)) +
  geom_line(color = "steelblue") +
  geom_point() +
  labs(title = "Evolution of SE as Start Points are Removed",
       x = "Starting Row Index",
       y = "StdErr")

ggplot(results, aes(x = window_start, y = SD)) +
  geom_line(color = "steelblue") +
  geom_point() +
  labs(title = "Evolution of StdDev as Start Points are Removed",
       x = "Starting Row Index",
       y = "Std Dev")

junk %>% select(turbidity_NTU,dttm_norm)
new_rows <- matrix(NA, nrow = min_n, ncol = ncol(results)) %>%
  as_tibble(.name_repair = "minimal") %>%
  set_names(names(results))

results <- results %>%
  bind_rows(new_rows)

results$turbidity_NTU <- junk$turbidity_NTU

ggplot(junk, aes(x = row_number(dttm_norm), y = turbidity_NTU)) +
  geom_point() + geom_smooth(method = 'lm', se = FALSE, col = 'green') + geom_line() +
  geom_hline(yintercept = median(junk$turbidity_NTU), col = 'blue', lty = 2) +
  coord_cartesian(xlim = c(0,nrow(results)))


# openxlsx::write.xlsx(data_stationary,file = '//aquadog/analysis/R_Scripts_Functions/UFI_Packages/Package_Build_Helpers/ImportUtils_testing/stationary_processed_example.xlsx')
# openxlsx::write.xlsx(data_stationary,file = '//aquadog/analysis/R_Scripts_Functions/UFI_Packages/Package_Build_Helpers/ImportUtils_testing/QL_example_stationary.xlsx')

# Testing ----
mad_thresh <- 0.15
mad_diff_thresh <- 0.15
mad_diff <- abs(diff(results$mad))

stable_zone <- which(
  results$mad < mad_thresh &
    c(NA, mad_diff) < mad_diff_thresh
)
stable_zone

# Just use the tail
# Trim drops the jiggle, then drop 25% more and trust it
n_tail <- ceiling(nrow(junk)*0.75)-min_n

eh <- median(tail(junk$turbidity_NTU,n_tail))

se_eh <- sd(tail(junk$turbidity_NTU,n_tail))/sqrt(n_tail)
sd_eh <- sd(tail(junk$turbidity_NTU,n_tail))
mad_eh <- mad(tail(junk$turbidity_NTU,n_tail))

# Create a helper data frame for the SD lines
sd_lines <- data.frame(
  intercept = c(eh + sd_eh, eh - sd_eh),
  type = "SD"
)

ggplot(junk %>%
         mutate(color = ifelse(row_number() > nrow(.) - n_tail,
                               'Used',
                               'Not Used')),
                aes(x = row_number(dttm_norm), y = turbidity_NTU)) +
  geom_point(aes(color = color), cex =  5) +
  geom_smooth(method = 'lm', se = FALSE) + geom_line() +
  geom_hline(aes(yintercept = eh, color = 'Median')) +
  geom_hline(data = sd_lines, aes(yintercept = intercept, color = type)) +
  # geom_hline(yintercept = c(eh + mad_eh, eh - mad_eh), col = 'green') +
  scale_color_manual(name = '',
                     values = c("Used" = "blue", "Not Used" = "grey",
                                "Median" = "black", "SD" = "red"))
