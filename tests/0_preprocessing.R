## Description: Capture plausible short stopping observations
rm(list = ls())
pkg <- c("data.table", "ggplot2", "Rose", "png")
sapply(pkg, require, character.only = TRUE)

core_path <- "[01]Projects/viscando/"
windows_path <- paste0("D:/", core_path)
unix_path <- paste0("/media/yslin/Avocet/", core_path)
wk <- ifelse(.Platform$OS.type == "windows", shortPathName(windows_path), 
             unix_path)
setwd(wk)


run_a_day <- function(i, x0) {
  require(png)
  require(dplyr)
  require(data.table)

  per <- records()
  msg0 <- ifelse(i <= 7, "Belle Road", "Queensway")
  pngfile <- ifelse(i <= 7, "tests/figs/streets/Belle_Isle_Road.png",
    "tests/figs/street/Queensway.png"
  )
  img <- png::readPNG(pngfile)

  h <- dim(img)[1] # image height
  w <- dim(img)[2] # image width
  metre_p_pixel_x <- w / (2 * ifelse(i <= 7, 24.77, 47.65))
  metre_p_pixel_y <- h / (2 * ifelse(i <= 7, 38.02, 42.27))

  ddayi <- x0 %>% dplyr::filter(
    Time > ymd_hm(per[i, 1]) &
      Time < ymd_hm(per[i, 2])
  )

  ddayi$Xmpp <- metre_p_pixel_x * (ddayi$X + metre_p_pixel_x)
  ddayi$Ympp <- metre_p_pixel_y * (ddayi$Y + metre_p_pixel_y)
  ddayi$Speed_ms <- ddayi$Speed * (1000 / 3600)
  ddayi$time_change <- c(NA, diff(ddayi$Time))
  ddayi$speed_change <- c(NA, diff(ddayi$Speed_ms))

  cat(
    msg0, "from", as.character(ymd_hm(per[i, 1])), "to",
    as.character(ymd_hm(per[i, 2])), "\n"
  )

  selected_columns <- c(
    "ID", "Time", "X", "Y", "Xmpp", "Ympp", "Speed_ms", "A", "time_change",
    "speed_change"
  )

  daday <- ddayi[, ..selected_columns]

  names(daday) <- c(
    "ID", "Time", "X", "Y", "Xmpp", "Ympp", "Speed", "A", "time_change",
    "speed_change"
  )

  fn <- paste0("tests/extdata/day", i, "/daday.rda")
  save(daday, file = fn)
  tools::resaveRdaFiles(fn, compress = "xz", compression_level = 9)
  return(NULL)
}

is_parallel <- TRUE

# Section 1 -----
# Accumulate individual csv's in data frame called x0
rawpath <- "tests/extdata/raw/"

# extract_data is a function in the package R folder
x0 <- extract_data(rawpath)

# Show the size of the big data frame in MB,
# Its size should be about 617MB
object.size(x0) / (1024^2)

# Check
# 1. the number of the four types of road user,
# 2. whether a sample is estimated or empirical, and
# 3. whether file IDs are correct.
cols <- c("Type", "Estimated", "fid")
sapply(x0[, ..cols], table)

# Examine how many road users are in each file. Each file stores
# one-day worth of data.
#
# Note that sometimes the camera and the CNN algorithm intepreted two
# road users as one, when they walked in pair, as observed in
# video files.
#
# Although there are 15 csv files, we use the recorded time reported
# by Viscando, which only have 14-day trajectory data.
# Use the function, record(), to see the recorded time.
table(x0$Type, x0$fid)
#         1       2       3       4       5       6       7       8
# 0    4435  142556  140787  132728  133399  120089   75899    1592
# 1    2436   35867   34182   34338   33091   26350   16865     222
# 2   44863 1044432 1128341 1063156 1123429  919983  613123    3987
# 3    2532   74205   83698   75611   83675   41728   19903     813
#
#         9      10      11      12      13      14      15
# 0  164643  179332  186407  174090  199000  119537   96366
# 1   38032   55318   44250   49197   46704   29123   25183
# 2  310260  371776  382412  442850  387210  262787  214529
# 3   33919   35672   38361   40660   31398   16381   25789


# Section 2 -----
# Separate all trajectory data, stored in x0, into individual one-day data
# and save to a file.
nday <- nrow(records())
idx <- vector("list", length = nday)
for (j in 1:nday) {
  idx[[j]] <- j
}

if (is_parallel) {
  ncore <- nday
  res <- parallel::mclapply(idx, run_a_day, x0,
    mc.cores = getOption("mc.cores", ncore)
  )
} else {
  res <- lapply(idx, run_a_day, x0)
}

# Section 3---------
# Check Viscando's time jump speeds
## Show that the manual calculated Speed has a positive correlation with 
# i <- 5; j <- 1131; k <- 5
# i <- 2; j <- 589; k <- 6
# i <- 2; j <- 59; k <- 6
i <- 11; j <- 147; k <- 4

fndat <- paste0("tests/tmp_data/day", i, "/p", j, "-v", k, ".rda")
load(fndat) 
dveh$Speed_ms <- dveh$Speed * (1000 / 3600)


tmp1 <- tim <- numeric(nrow(dveh)-1)
for(m in 2:nrow(dveh)) {
  dis <- sqrt( (dveh$X[m] - dveh$X[m-1])^2 + (dveh$Y[m] - dveh$Y[m-1])^2 )
  tim[m-1] <- (as.numeric(dveh$Time[m]) - as.numeric(dveh$Time[m-1]))      
  tmp1[m-1] <- dis/ (as.numeric(dveh$Time[m]) - as.numeric(dveh$Time[m-1]))      
}
plot(tim)
which(tim> 0.162)
dtmp[26,]

tmp2 <- dveh$Speed_ms[2:nrow(dveh)]
dtmp <- data.table(y = c(tmp1, tmp2), x = c(1:length(tmp1), 1:length(tmp2)),
                   z = c(rep("Calculated", length(tmp1)), rep("Viscando", length(tmp2))))
dtmp$z <- factor(dtmp$z)
p0 <- ggplot(data = dtmp) +
  geom_point(aes(x = x, y = y, colour = z)) +
  geom_line(aes(x = x, y = y, group = z))
p0


