x <- seq(1,10,1)
y <- round(rnorm(10,10,4),2)

x;y

xr <- rev(x)
yr <- rev(y)

# cumulative counts
n <- seq_along(xr)

# cumulative sums
sx  <- cumsum(xr)
sy  <- cumsum(yr)
sxx <- cumsum(xr * xr)
sxy <- cumsum(xr * yr)

# slope using regression identity
slope <- (n*sxy - sx*sy) / (n*sxx - sx^2)

# cumulative range
cum_min <- cummin(yr)
cum_max <- cummax(yr)
range <- cum_max - cum_min

# reverse back
slope <- rev(slope)
range <- rev(range)
