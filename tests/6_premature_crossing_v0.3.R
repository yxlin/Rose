rm(list = ls())
pkg <- c("data.table", "ggplot2", "Rose", "png")
sapply(pkg, require, character.only = TRUE)

core_path <- "[01]Projects/viscando/"
windows_path <- paste0("D:/", core_path)
unix_path <- paste0("/media/yslin/Avocet/", core_path)
wk <- ifelse(.Platform$OS.type == "windows", shortPathName(windows_path),
  unix_path
)
setwd(wk)

pal <- load_basic_colour_palette()

# Section 1 ------------------
# Retrieve data-time records
per <- records()
nday <- nrow(per)

# Define the period where I am going to examine the
# vehicle's speed dynamics
d2e_threshold <- 4

# Above this speed, I consider it a high speed.
# At the instance of d4-p140-v1, v1 had a minimal speed
# of 0.17 m/s.
speed_threshold <- 0.1

# Find the cases that pedestrians crossed earlier 
# when the car had a non-zero, positively increasing trend at its
# speed
message("Premature crossing: ")
d2e_threshold_veh <- 10

count_list <- list()
for (l in seq_len(nday)) {
  # Look up the interactive figure folder to check if there is any interactions
  folder_name <- paste0("tests/interact_figs/day", l)
  ninteraction <- list.files(folder_name)
  # If no interaction is found, skip this day -------
  if (length(ninteraction) == 0) next

  load(paste0("tests/extdata/day", l, "/pd.rda"))
  nxi <- nrow(x1)

  # Load individual interaction observation -------
  path2data <- "tests/interact_data"
  # Check the total count is consistent.
  count <- 0

  for (m in seq_len(nxi)) {
    i <- x1[m, 1]
    j <- x1[m, 2]
    k <- x1[m, 3]
    load(paste0(path2data, "/day", i, "/p", j, "-v", k, ".rda"))

    encounter_point <- Rose::find_encounter(dped, dveh)
    xy <- encounter_point[[1]]
    site <- ifelse(i <= 7, "belle", "qw")
    hotzone <- check_hotzone(dveh, xy, site = site)

    if (nrow(hotzone) == 0) {
      cat("\nUndefined zone in interation: ", c(i, j, k), "\n")
      next
    }

    # 1. Select the data in the period of 20 to 4 metres before the encounter.
    # Calculate the proportion of deceleration
    idx_vyield <- dveh$d2e <= d2e_threshold_veh & dveh$d2e > d2e_threshold
    nyield <- length(idx_vyield)
    early_yield_percentage <- sum(dveh[idx_vyield, ]$speed_change < 0,
      na.rm = TRUE
    ) / nyield

    # 2. 20 metres before the encounter, the vehicle must maintain its speed or
    # higher. Use a linear regression to check the speed dynamics, and see
    # its slope is positive.
    idx_vmin <- xy[1]
    idx_vthre <- which.max(dveh$d2e < d2e_threshold)
    crit_pts <- idx_vthre:idx_vmin
    ncrit <- length(crit_pts)

    speeds <- dveh[crit_pts, ]$Speed_ms
    times <- dveh[crit_pts, ]$abstime
    lm0 <- lm(speeds ~ times)
    beta <- lm0$coefficients[2]
    tta <- Rose::who_cross_first(dped, dveh)

    cat_near_stop <- any(dveh[1:idx_vmin, ]$Speed_ms < 0.1)


    if (early_yield_percentage > 0.35 && beta > 0 &&
      tta[[1]] == "Ped" && !cat_near_stop && unique(hotzone$id) != 2) {
      cat(c(i, j, k), "\n")
      count <- count + 1
    }
  }

  count_list[[l]] <- count
}

nevent <- unlist(count_list)
names(nevent) <- paste0("day", seq_along(nevent))
#  day1  day2  day3  day4  day5  day6  day7  day8  day9 day10 day11
#     4     3    11     7     6     0     2     2     0     0     0

# Section 2 -------------
# Selected the cases 
selected_behaviors <- matrix(c(
  2, 127, 6,
  2, 133, 4,
  2, 603, 5,
  2, 701, 5,
  3, 433, 5,
  3, 489, 6,
  3, 797, 4,
  4, 140, 1,
  4, 154, 6,
  4, 155, 3,
  4, 315, 4,
  4, 391, 5,
  4, 546, 4,
  4, 590, 3,
  4, 850, 4,
  4, 1001, 3,
  4, 1160, 5,
  5, 41, 5,
  5, 108, 4,
  5, 152, 3,
  5, 160, 4,
  5, 208, 5,
  5, 454, 3,
  5, 913, 6,
  6, 81, 5,
  6, 216, 4,
  6, 249, 4,
  6, 378, 5,
  6, 593, 4,
  6, 791, 5,
  9, 235, 3,
  9, 759, 5,
  10, 1016, 4,
  10, 1236, 5
), ncol = 3, byrow = TRUE)


# d2e_threshold <- 5
n <- nrow(selected_behaviors)
nxlabel <- 6
useId <- TRUE
x0 <- NULL # ped
x1 <- NULL # veh
msgv <- list()
zones <- list()
idxes <- list()
d2e_threshold_veh <- 10
PET <- NULL

# l <- 1
for (l in 1:n) {
  i <- selected_behaviors[l, 1]
  j <- selected_behaviors[l, 2]
  k <- selected_behaviors[l, 3]
  fndat <- paste0("tests/interact_data/day", i, "/p", j, "-v", k, ".rda")
  load(fndat)

  encounter_point <- Rose::find_encounter(dped, dveh)
  xy <- encounter_point[[1]]
  site <- ifelse(i <= 7, "belle", "qw")
  hotzone <- check_hotzone(dveh, xy, site = site)

  # Examine the pedestrian's behavior 5 metres before the encounter.
  idx_min <- xy[2]
  idx_thre <- which.max(dped$d2e < d2e_threshold)

  # The indexs of the sample points leading to the encounter point
  crit_pts <- idx_thre:idx_min
  ncrit <- length(crit_pts)

  dped$xx <- 1:nrow(dped)
  dped$acc <- dped$speed_change / dped$time_change
  dped$d2e2 <- round(dped$d2e, 1)
  dped1 <- dped[1:idx_min, ]

  xint <- dped1[d2e < d2e_threshold, ]$xx[1]
  xend <- dped1$xx[nrow(dped1)]
  if (nrow(dped1) >= nxlabel) {
    idx0 <- seq(1, nrow(dped1), length.out = nxlabel)
  } else {
    idx0 <- seq(1, nrow(dped1), length.out = nrow(dped1) / 2)
  }

  idx <- sort(c(floor(idx0), xint, xend))

  tta <- Rose::who_cross_first(dped, dveh)
  msg <- tta[[2]]

  # who arrived the encounter earlier?
  msglab <- paste0(tta[[1]], ", ", round(abs(tta[[3]]), 2), " s")

  if (useId) {
    msgv[[l]] <- paste0("d", i, "-p", j, "-v", k, ", ", msglab)
  } else {
    msgv[[l]] <- msglab
  }

  dped1$id <- paste0(i, "-", j, "-", k)
  x0 <- rbind(x0, dped1)
  
  # Car: Remove the first sample , bcuz sometimes first sample often
  # shows abnormalities.
  idx_min_veh <- which.min(dveh$d2e)
  dveh$acc <- dveh$speed_change / dveh$time_change
  dveh1 <- dveh[2:idx_min_veh, ]
  dveh1$xx <- 1:nrow(dveh1)
  
  dveh1$id <- paste0(i, "-", j, "-", k)
  x1 <- rbind(x1, dveh1)
  

  yup <- range(dped1$Speed_ms, dped1$acc)[1]
  ydown <- range(dped1$Speed_ms, dped1$acc)[2]
  dzone <- data.frame(x = c(xint, xint, xend, xend), y = c(yup, ydown, ydown, yup))


  zones[[l]] <- dzone
  idxes[[l]] <- idx
  
  # PET
  cols <- c("speed_change", "tta", "d2e", "Time") 
  zerotime <- dped[xy[2], ..cols]$Time
  endtime <- dveh[xy[1], ..cols]$Time
  PET_tmp <- as.numeric(endtime - zerotime)
  PET <- c(PET, PET_tmp)
}

PET
sort(PET)
# 2.00 2.72 2.80 2.88 2.88 2.96 2.96 3.04 3.04 3.12 3.12 3.36 3.44 3.52 3.68 3.68 3.68
# [18] 3.76 3.84 3.84 3.84 3.92 4.00 4.08 4.16 4.24 4.32 4.40 4.48 4.48 4.56 4.80 4.80 4.96
psych::describe(PET)
#   n mean   sd median trimmed  mad min  max range  skew kurtosis   se
#  34 3.69 0.71   3.72    3.69 0.89   2 4.96  2.96 -0.14    -0.75 0.12

# Plot them---------
lab_size <- 16
nxlabel <- 6

ids <- unique(x0$id)
p <- vector("list", length = n)
# i <- 1

for (i in 1:n) {
  dtmp <- x0[id %in% ids[i]]
  xint <- dtmp[d2e < d2e_threshold, ]$xx[1]
  xend <- dtmp$xx[nrow(dtmp)]

  if (nrow(dtmp) >= nxlabel) {
    idx0 <- seq(1, nrow(dtmp), length.out = nxlabel)
  } else {
    idx0 <- seq(1, nrow(dtmp), length.out = nrow(dtmp) / 2)
  }

  if (xend %in% idx0) {
    idx <- sort(c(floor(idx0), xint))
  } else {
    idx <- sort(c(floor(idx0), xint, xend))
  }

  if (i == 1) {
    ptmp <- ggplot(data = dtmp) +
      scale_x_continuous(
        labels = dtmp$d2e2[idx], breaks = dtmp$xx[idx],
        name = ""
      ) +
      scale_y_continuous(
        name = bquote(Acceleration(m / s^2)),
        sec.axis = sec_axis(trans = ~., name = "")
      )
  } else {
    ptmp <- ggplot(data = dtmp) +
      scale_x_continuous(
        labels = dtmp$d2e2[idx], breaks = dtmp$xx[idx],
        name = ""
      ) +
      scale_y_continuous(
        name = "",
        sec.axis = sec_axis(trans = ~., name = "")
      )
  }

  p[[i]] <- ptmp +
    geom_point(aes(x = xx, y = acc), colour = pal[3]) +
    geom_line(aes(x = xx, y = acc, group = ID)) +
    geom_point(aes(x = xx, y = Speed_ms), colour = pal[2]) +
    geom_line(aes(x = xx, y = Speed_ms, group = ID)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = xint, linetype = "dotted") +
    # ggtitle(msgv[[i]]) +
    geom_polygon(
      data = zones[[i]], aes(x = x, y = y), alpha = .2,
      colour = "lightblue"
    ) +
    theme(
      axis.text.x = element_text(size = lab_size, angle = 30),
      axis.text.y = element_text(size = lab_size),
      axis.title.y.right = element_text(colour = pal[2]),
      axis.title.y.left = element_text(colour = pal[3])
    )
}


# png("tests/docs/first_submit/figs/early_tmp2.png")
gridExtra::grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]],
  p[[5]], p[[6]], ncol = 2
)
# dev.off()

# Summary visualisation --------
dspeed_ped <- NULL
dacc_ped <- NULL

dspeed_veh <- NULL
dacc_veh <- NULL

for (i in 1:n) {
  dtmp <- x0[id %in% ids[i]]
  # Distance to encounter is less than 5 m
  # To reverse the label on x axis, showing a pedestrian was
  # approaching the encounter.
  
  dveh_tmp <- x1[id %in% ids[i]]
  xint_veh <- dveh_tmp[d2e < d2e_threshold_veh, ]$xx[1]
  xend_veh <- dveh_tmp$xx[nrow(dveh_tmp)]
  tmp_veh <- data.table(
    x = dveh_tmp[xint_veh:xend_veh, ]$xx,
    y = dveh_tmp[xint_veh:xend_veh, ]$Speed_ms,
    z = dveh_tmp[xint_veh:xend_veh, ]$d2e,
    ID = ids[i]
  )
  tmp2_veh <- data.table(
    x = dveh_tmp[xint_veh:xend_veh, ]$xx,
    y = dveh_tmp[xint_veh:xend_veh, ]$acc,
    z = dveh_tmp[xint_veh:xend_veh, ]$d2e,
    ID = ids[i]
  )
  dspeed_veh <- rbind(dspeed_veh, tmp_veh)
  dacc_veh <- rbind(dacc_veh, tmp2_veh)
  
  xint <- dtmp[d2e < d2e_threshold, ]$xx[1]
  xend <- dtmp$xx[nrow(dtmp)]

  tmp <- data.table(
    x = dtmp[xint:xend, ]$xx,
    y = dtmp[xint:xend, ]$Speed_ms,
    z = dtmp[xint:xend, ]$d2e,
    ID = i
  )
  tmp2 <- data.table(
    x = dtmp[xint:xend, ]$xx,
    y = dtmp[xint:xend, ]$acc,
    z = dtmp[xint:xend, ]$d2e,
    ID = i
  )

  dspeed_ped <- rbind(dspeed_ped, tmp)
  dacc_ped <- rbind(dacc_ped, tmp2)
}

save(dspeed_ped, dacc_ped, dspeed_veh, dacc_veh, 
     file = "tests/pydata/old/7_empirical_early_yield_tmp.rda")

# save(dspeed, dacc, file = "tests/pydata/old/7_empirical_early_yield.rda")



# Section 4 ---------
# Extract initial speeds
dout <- NULL
for (l in 1:n) {
  tmp <- selected_behaviors[l, ]
  i <- tmp[1]
  j <- tmp[2]
  k <- tmp[3]
  fndat <- paste0("tests/interact_data/day", i, "/p", j, "-v", k, ".rda")
  Id <- paste0("d", l, "-p", j, "-v", k)
  load(fndat)

  idx_ped <- min(which(dped$Speed_ms != 0))
  idx_veh <- min(which(dveh$Speed_ms != 0))

  speed_veh <- dveh[idx_veh, "Speed_ms"]$Speed_ms
  distance_veh <- dveh[idx_veh, "d2e"]$d2e

  speed_ped <- dped[idx_ped, "Speed_ms"]$Speed_ms
  distance_ped <- dped[idx_ped, "d2e"]$d2e

  dtmp <- data.frame(distance_ped, distance_veh, speed_ped, speed_veh, id = Id)
  dout <- rbind(dout, dtmp)
}


write.csv(dout,
  file = paste0("tests/extdata/7_early_yield.csv"),
  row.names = FALSE
)
