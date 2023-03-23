## Description: Convert csv raw data to 14 data frames
isServer <- FALSE
pkg <- c("Rose", "data.table", "dplyr", "lubridate", "png")
sapply(pkg, require, character.only = TRUE)
wpath <- ifelse(isServer, "~/Documents/viscando/", 
                "/media/yslin/Avocet/Projects/viscando/"); wpath
wk <- ifelse(.Platform$OS.type == "windows", shortPathName("C:/"), wpath)
setwd(wk)
rm(list = ls())

run_a_day <- function(i, x0) {
  require(png)
  require(dplyr)
  require(data.table)

  per <- records()

  msg0 <- ifelse(i <= 7, "Belle Road", "Queensway")
  pngfile <- ifelse(i <= 7, "tests/figs/streets/ground - Belle_Isle.png", "tests/figs/queensway.png")
  img <- png::readPNG(pngfile) 

  h <- dim(img)[1] # image height
  w <- dim(img)[2] # image width
  metre_p_pixel_x <- w / (2*ifelse(i <= 7, 24.77, 47.65))
  metre_p_pixel_y <- h / (2*ifelse(i <= 7, 38.02, 42.27))

  ddayi <- x0 %>% dplyr::filter(Time > ymd_hm(per[i, 1]) & Time < ymd_hm(per[i, 2])) 
  Distance <- sqrt(ddayi$X^2 + ddayi$Y^2)

  ddayi$Xmpp <- metre_p_pixel_x*(ddayi$X + metre_p_pixel_x)
  ddayi$Ympp <- metre_p_pixel_y*(ddayi$Y + metre_p_pixel_y)

  ddayi$Speed_ms <- ddayi$Speed * (1000 / 3600)
  ddayi$time_change <- c(NA, diff(ddayi$Time))
  ddayi$speed_change <- c(NA, diff(ddayi$Speed_ms))
  ddayi$distance_change <- c(NA, diff(ddayi$Distance))
  ddayi$acceleration <- ddayi$speed_change / ddayi$time_change

  cat(msg0, "from", as.character(ymd_hm(per[i, 1])), "to", as.character(ymd_hm(per[i, 2])), "\n")  
  selected_columns <- c("ID", "Time", "X", "Y", "Xmpp", "Ympp", "Speed_ms", "A", "time_change", "speed_change", 
  "distance_change", "acceleration")
  daday <- ddayi[, ..selected_columns]

  names(daday) <- c("ID", "Time", "X", "Y", "Xmpp", "Ympp", "Speed", "Agent", "time_change", "speed_change",
  "distance_change", "acceleration")

  fn <- paste0("tests/extdata/day", i, "/daday.rda")
  save(daday, file = fn)
  tools::resaveRdaFiles(fn, compress = "xz", compression_level = 9)
  return(NULL)
}

is_parallel <- TRUE

# Extract and accumulate  all trajectory data in one object called x0 -----
rawpath <- "tests/extdata/raw/"
x0 <- extract_data(rawpath) 

# Divide the object size by the square of 1024 to get the size in MB, which is about 617MB
# object.size(x0) / (1024^2)

# Check the four road user types, whether a sample is estimated or empirical, and whether 
# fild IDs match up. 
cols <- c("Type", "Estimated", "fid") 
sapply(x0[, ..cols], table)

# Examine how many road users are in each file. Each file stores one-day worth of data.
# Note that sometimes the camera and CNN algorithm intepreted two road users as one, 
# when they walked in pair. We can see this by sampling a number of vide
# files and counting the number of road users.
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


# Extract all trajectory data in a day ----- 
# I store a function, named, per, in the package R folder, which has the dates and times 
# that we recorded the videos.
nday <- nrow(records())
idx <- vector("list", length = nday)
for(j in 1:nday) { idx[[j]] <- j }

if (is_parallel) {
  ncore <- nday
  res <- parallel::mclapply(idx, run_a_day, x0, 
                            mc.cores = getOption("mc.cores", ncore))

} else {
  res <- lapply(idx, run_a_day, x0)
}
