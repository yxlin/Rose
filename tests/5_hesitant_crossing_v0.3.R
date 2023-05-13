rm(list = ls())
pkg <- c("data.table", "ggplot2", "Rose", "png")
sapply(pkg, require, character.only = TRUE)

core_path <- "[01]Projects/viscando/"
windows_path <- paste0("F:/", core_path)
unix_path <- paste0("/media/yslin/Avocet/", core_path)
wk <- ifelse(.Platform$OS.type == "windows", shortPathName(windows_path),
  unix_path
)
setwd(wk)

pal <- load_basic_colour_palette()

# Section 1 ------------------
per <- records()
nday <- nrow(per)

# Define the threshold of the distance to the encounter point in metres.
d2e_threshold <- 4

# Find those pedestrians who slowed down when the car was also slow down------
# before stepping into the crosstalk
message("Hesitant crossing: ")

for (l in seq_len(nday)) {
  # Look up the interact figure folder to check if there is any interactions
  folder_name <- paste0("tests/interact_figs/day", l)
  ninteraction <- list.files(folder_name)
  # If no interaction is found, skip to a next day
  if (length(ninteraction) == 0) next

  # Look up indexing data in pd.rda
  file_name <- paste0("tests/extdata/day", l, "/pd.rda")
  load(file_name)
  nxi <- nrow(x1)
  # Load individual interaction pair
  path2data <- "tests/interact_data"
  for (m in seq_len(nxi)) {
    i <- x1[m, 1]
    j <- x1[m, 2]
    k <- x1[m, 3]
    fn <- paste0(path2data, "/day", i, "/p", j, "-v", k, ".rda")
    load(fn)

    encounter_point <- Rose::find_encounter(dped, dveh)
    xy <- encounter_point[[1]]
    site <- ifelse(i <= 7, "belle", "qw")
    hotzone <- check_hotzone(dveh, xy, site = site)

    # If the encounter point is not in the hot zone,
    # skip to a next pair
    if (nrow(hotzone) == 0) next

    # Vehicle
    idx_vthre <- which.max(dveh$d2e < d2e_threshold)

    # Pedestrian
    idx_pthre <- which.max(dped$d2e < d2e_threshold)

    # The indexes of the sample points leading to the encounter point
    crit_pts <- idx_pthre:xy[2]
    ncrit <- length(crit_pts)
    # the vehicle's encounter point
    crit_pts2 <- idx_vthre:xy[1]
    ncrit2 <- length(crit_pts2)

    ndeceleration <- sum(dveh$speed_change[crit_pts2] <= 0, na.rm = TRUE)
    deceleration_percentage <- ndeceleration / ncrit2
    # Find out who pass first
    tta <- Rose::who_cross_first(dped, dveh)
    msg <- tta[[2]]

    # 1. Has the pedestrian slowed down?
    speeds <- dped[crit_pts, ]$Speed_ms
    times <- dped[crit_pts, ]$abstime
    lm0 <- lm(speeds ~ times)
    beta <- lm0$coefficients[2]

    # Finally, look for the interaction where the car does show yielding sign
    # The deceleration percentage is greater than 50% and the pedestrian passed
    # at least 3.8 s earlier than the car.
    if (deceleration_percentage >= 0.5) {
      if (beta < 0) {
        if (tta[[1]] == "Ped") {
          seconds <- abs(as.numeric(tta[[3]]))
          if ((nrow(hotzone) != 0) && (seconds >= 2)) {
            # message("Negative slope")
            cat(c(i, j, k), " ", seconds, " \n")
          }
        }
      }
    }
  }
}

# 2 156 6
# 2 983 4
# 3 447 5
# 3 719 6
# 4 130 3
# 5 807 3
# 5 1008 4
# 9 1441 6
# 10 379 1
# 11 1218 5

# Section 2 ------------------
# Collect data for pedestrian speed profiles
# 4-520-5 is a camera error.
d2e_threshold <- 4
d2e_threshold_veh <- 10

selected_behaviors <- matrix(c(
  2, 156, 6,
  2, 983, 4,
  9, 1441, 6,
  # 10, 379, 1,
  10, 1292, 4
  # 11, 1218, 5
), ncol = 3, byrow = TRUE)

n <- nrow(selected_behaviors)
nxlabel <- 6
useId <- TRUE
# Ped
x0 <- NULL 
# Veh
x1 <- NULL

PET <- NULL
msgv <- list()
zones <- list()
idxes <- list()
# l <- 2
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

  # The Pedestrian's profile 5 metres before the encounter.
  idx_min <- xy[2]
  idx_thre <- which.max(dped$d2e < d2e_threshold)

  # The samples leading to the encounter point
  crit_pts <- idx_thre:idx_min
  ncrit <- length(crit_pts)

  # PET
  cols <- c("speed_change", "tta", "d2e", "Time") 
  zerotime <- dped[xy[2], ..cols]$Time
  endtime <- dveh[xy[1], ..cols]$Time
  PET_tmp <- as.numeric(endtime - zerotime)
  PET <- c(PET, PET_tmp)
  
  # 1. Has the pedestrian slowed down?
  # The percentage of deceleration in the critical points.
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

  # Car: Remove the first sample , bcuz sometimes first sample often
  # shows abnormalities.
  idx_min_veh <- which.min(dveh$d2e)
  dveh1 <- dveh[2:idx_min_veh, ]
  dveh1$xx <- 1:nrow(dveh1)
  dveh1$acc <- dveh1$speed_change / dveh1$time_change

  dped1$id <- paste0(i, "-", j, "-", k)
  x0 <- rbind(x0, dped1)

  dveh1$id <- paste0(i, "-", j, "-", k)
  x1 <- rbind(x1, dveh1)
  
  yup <- range(dped1$Speed_ms, dped1$acc)[1]
  ydown <- range(dped1$Speed_ms, dped1$acc)[2]
  dzone <- data.frame(x = c(xint, xint, xend, xend), y = c(yup, ydown, ydown, yup))


  zones[[l]] <- dzone
  idxes[[l]] <- idx
}


psych::describe(PET)
# [1] 2.64 2.96 4.56 2.48
#  n mean   sd median trimmed  mad  min  max range skew kurtosis   se
#  4 3.16 0.95    2.8    3.16 0.36 2.48 4.56  2.08 0.66    -1.76 0.48

# Plot them
lab_size <- 12
nxlabel <- 6

ids <- unique(x0$id)
p <- vector("list", length = n)
for (i in 1:n) {
  dtmp <- x0[id %in% ids[i]]
  # idx <- idxes[[i]]

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

  ptmp <- ggplot(data = dtmp) +
    scale_x_continuous(
      labels = dtmp$d2e2[idx], breaks = dtmp$xx[idx],
      name = ""
    ) +
    scale_y_continuous(
      name = bquote(Acceleration(m / s^2)),
      sec.axis = sec_axis(trans = ~., name = "Speed (m/s)")
    )

  p[[i]] <- ptmp +
    geom_point(aes(x = xx, y = acc), colour = pal[3]) +
    geom_line(aes(x = xx, y = acc, group = ID)) +
    geom_point(aes(x = xx, y = Speed_ms), colour = pal[2]) +
    geom_line(aes(x = xx, y = Speed_ms, group = ID)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = xint, linetype = "dotted") +
    ggtitle(msgv[[i]]) +
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

# cairo_pdf("tests/docs/first_submit/figs/hesitation_tmp2.pdf")
# png("tests/docs/first_submit/figs/hesitation.png")
# gridExtra::grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], ncol = 2)
# dev.off()


# Section 3 ---------
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
  distance_ped <- dped[1, "d2e"]$d2e
  speed_veh <- dveh[1, "Speed_ms"]$Speed_ms
  distance_veh <- dveh[1, "d2e"]$d2e

  speed_ped <- dped[1, "Speed_ms"]$Speed_ms
  dtmp <- data.frame(distance_ped, distance_veh, speed_ped, speed_veh, id = Id)
  dout <- rbind(dout, dtmp)
}


write.csv(dout,
  file = paste0("tests/extdata/6_hesitation.csv"),
  row.names = FALSE
)



# Section 4 ------------------
# Empirical data to compare with the simulation data.
d2e_threshold_veh <- 10
dspeed_ped <- NULL
dacc_ped <- NULL

dspeed_veh <- NULL
dacc_veh <- NULL
# i <- 2
for (i in 1:n) {
  # unique(x0$id)
  # unique(x1$id)
  dtmp <- x0[id %in% ids[i]]
  dveh_tmp <- x1[id %in% ids[i]]
  # Distance to encounter is less than 5 m
  # To reverse the label on x axis, showing a pedestrian was
  # approaching the encounter.

  xint <- dtmp[d2e < d2e_threshold, ]$xx[1]
  xend <- dtmp$xx[nrow(dtmp)]

  xint_veh <- dveh_tmp[d2e < d2e_threshold_veh, ]$xx[1]
  xend_veh <- dveh_tmp$xx[nrow(dveh_tmp)]
  

  tmp <- data.table(
    x = dtmp[xint:xend, ]$xx,
    y = dtmp[xint:xend, ]$Speed_ms,
    z = dtmp[xint:xend, ]$d2e,
    ID = ids[i]
  )
  tmp2 <- data.table(
    x = dtmp[xint:xend, ]$xx,
    y = dtmp[xint:xend, ]$acc,
    z = dtmp[xint:xend, ]$d2e,
    ID = ids[i]
  )

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
  
  
  dspeed_ped <- rbind(dspeed_ped, tmp)
  dacc_ped <- rbind(dacc_ped, tmp2)
  dspeed_veh <- rbind(dspeed_veh, tmp_veh)
  dacc_veh <- rbind(dacc_veh, tmp2_veh)
}
# i

save(dspeed_ped, dacc_ped, dspeed_veh, dacc_veh, 
     file = "tests/pydata/old/6_empirical_hesitation_tmp.rda")
