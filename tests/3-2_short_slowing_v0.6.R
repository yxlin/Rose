## Description: Capture plausible short stopping observations
rm(list = ls())
pkg <- c("data.table", "ggplot2", "Rose", "png", "lubridate")
sapply(pkg, require, character.only = TRUE)

core_path <- "[01]Projects/viscando/"
windows_path <- paste0("F:/", core_path)
unix_path <- paste0("/media/yslin/Avocet/", core_path)
wk <- ifelse(.Platform$OS.type == "windows", shortPathName(windows_path),
  unix_path
)
setwd(wk)

# Calculate total hours of the data
load(paste0("tests/extdata/day", 1, "/daday_.rda"))
start <- daday$Time[1]
load(paste0("tests/extdata/day", 14, "/daday_.rda"))
end <- daday$Time[nrow(daday)]
diff.time <- difftime(end, start, units = "hours")
diff.time


# Section 1 ----------
nday <- nrow(records())
# Section 2 ----------
# 1. Print out how many interactions were captured in each day.
day_with_interaction <- NULL
for (i in seq_len(nday)) {
  site <- ifelse(i <= 7, "belle", "qw")
  load(paste0("tests/extdata/day", i, "/pd.rda"))
  d <- data.table(cbind(x1, pet, zoneid))
  if (any(d$zoneid == 99)) {
    idx <- which(d$zoneid == 99)
    cat("Day", i, ": \n")
    cat(
      "pedestrian ", d[idx, ]$ped, "- vehicle ",
      d[idx, ]$veh, "has no zone id. \n"
    )

    # Fix the zone id that the interaction happened slightly outside the
    # drawing area.
    if (i == 2) {
      d[idx, "zoneid"] <- 2
    }
    if (i == 3) {
      d[idx, "zoneid"] <- 2
    }
    if (i == 5) {
      d[idx[1], "zoneid"] <- 2
    }
    if (i == 9) {
      d[idx[2], "zoneid"] <- 5
    }
  }

  cat("Day", i, "has ", nrow(d), "interaction(s): \n")

  if (nrow(d) > 0) {
    day_with_interaction <- c(day_with_interaction, i)
  }
}

day_with_interaction
# Day 1 has  0 interaction(s):
# Day 2 :
# pedestrian  288 - vehicle  4 has no zone id.
# Day 2 has  66 interaction(s):
#
# Day 3 :
# pedestrian  91 419 - vehicle  3 6 has no zone id.
# Day 3 has  60 interaction(s):
#
# Day 4 has  68 interaction(s):
#
# Day 5 :
# pedestrian  817 1131 - vehicle  2 5 has no zone id.
# Day 5 has  60 interaction(s):
#
# Day 6 has  39 interaction(s):
# Day 7 has  2 interaction(s):
# Day 8 has  0 interaction(s):
# Day 9 :
# pedestrian  213 913 - vehicle  4 3 has no zone id.
# Day 9 has  45 interaction(s):
#
# Day 10 has  63 interaction(s):
# Day 11 has  50 interaction(s):
#
# Day 12 :
# pedestrian  714 - vehicle  5 has no zone id.
# Day 12 has  54 interaction(s):
#
# Day 13 :
# pedestrian  126 - vehicle  3 has no zone id.
# Day 13 has  28 interaction(s):
# Day 14 has  0 interaction(s):

# Section 3-----------------------------------------------------------
# i <- 2
day_with_interaction <- c(2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13)
threshold <- 1
# Define the threshold of speed change. It has to be less than -3.5 m/s^2
# Test a slight different value to see if it makes a difference.
one_car_len <- 4
# Define the distance threshold to the stop line. The assumption is that
# within 10 m, a driver would start to consider whether he / she should
# yield, if any pedestrian is spotted.
distance_to_stop_line <- 10

for (i in day_with_interaction) {
  if (i <= 7) {
    # Skip Zone 1 and 2 in stopping short, because they are unmarked crossings.
    zones <- 3:4
    site <- "belle"
  } else {
    zones <- 5
    site <- "qw"
  }
  load(paste0("tests/extdata/day", i, "/pd.rda"))
  d <- data.table(cbind(x1, pet, zoneid))

  if (any(d$zoneid == 99)) {
    idx <- which(d$zoneid == 99)
    cat("Day", i, ": \n")
    cat(
      "pedestrian ", d[idx, ]$ped, "- vehicle ",
      d[idx, ]$veh, "has no zone id. \n"
    )
    if (i == 2) {
      d[idx, "zoneid"] <- 2
    }
    if (i == 3) {
      d[idx, "zoneid"] <- 2
    }
    if (i == 5) {
      d[idx[1], "zoneid"] <- 2
    }
    if (i == 9) {
      d[idx[2], "zoneid"] <- 5
    }
  }

  izone <- 3
  l <- 1


  for (izone in zones) {
    dzonei <- d[zoneid == izone]
    nint <- nrow(dzonei)
    if (nint == 0) next

    for (l in 1:nint) {
      j <- dzonei[l, ]$ped
      k <- dzonei[l, ]$veh
      fndat <- paste0("tests/interact_data/day", i, "/p", j, "-v", k, ".rda")
      load(fndat)
      # Calculate the acceleration. Note that when a time sample jumps it will
      # be incorrect
      dveh$acc <- dveh$speed_change / dveh$time_change

      # To show an approaching x axis on the chart, I created a
      # second x axis called xx. It is just a sequence of numbers.
      dveh$xx <- seq_len(nrow(dveh))
      nped <- nrow(dped)
      nveh <- nrow(dveh)

      tmp_xy <- find_encounter(dped, dveh) # Find the recorded encounter
      xy <- tmp_xy[[1]]
      encounter_distance <- tmp_xy[[2]]
      hotzone <- check_hotzone(dveh, xy, site = site)

      if (nrow(hotzone) == 0) next
      zoneline <- load_stopping_lines(hotzone)

      xdist2 <- (p2metre(dveh$Xmpp) - p2metre(zoneline[3, ]$x))^2
      ydist2 <- (p2metre(dveh$Ympp, FALSE, site = site) -
        p2metre(zoneline[3, ]$y, FALSE, site = site))^2
      dveh$d2marking <- sqrt(xdist2 + ydist2)

      # Stopping short was defined as the car stopping at a distance longer
      # than one-car length (4 m) before the stopping line
      # cols <- c("Speed_ms", "time_change", "speed_change", "otta", "d2e", "d2marking")
      # dveh[, ..cols]
      test_set <- dveh[d2marking < d2m_thres & d2marking > one_car_len]$Speed_ms

      # Examine if a car had a speed change smaller than -3.5 m/s
      if (any(test_set < threshold, na.rm = TRUE)) {
        message("A short stop: ", appendLF = F)
        cat(c(i, j, k), "\n")
      }
    }
  }
}


# Section 4 ---------
# d6-p27-v4, Veh, 4.88 s -> samples abnormal
short_stop <- matrix(c(
  2, 163, 3,
  3, 984, 3,
  5, 415, 4,
  5, 649, 5,
  5, 680, 5,
  # 6, 27, 4,
  6, 668, 4,
  6, 81, 5,
  6, 839, 4,
  9, 759, 5,
  9, 913, 3,
  9, 982, 5,
  10, 809, 2,
  10, 974, 5,
  10, 1032, 3,
  10, 1064, 2,
  10, 1099, 3,
  10, 1206, 4,
  10, 1236, 5,
  10, 1603, 3,
  11, 347, 2,
  11, 554, 5,
  11, 899, 2,
  11, 905, 5,
  11, 1088, 3,
  12, 205, 3,
  12, 1375, 4,
  13, 102, 3,
  13, 169, 1,
  13, 678, 6,
  13, 740, 4
), ncol = 3, byrow = TRUE)

n <- nrow(short_stop)
useId <- TRUE
x0 <- NULL # veh
x1 <- NULL # ped
# SST <- NULL
msgv <- list()
for (l in 1:n) {
  tmp <- short_stop[l, ]
  i <- tmp[1]
  j <- tmp[2]
  k <- tmp[3]
  fndat <- paste0("tests/interact_data/day", i, "/p", j, "-v", k, ".rda")
  load(fndat)
  site <- ifelse(i <= 7, "belle", "qw")

  tmp_xy <- find_encounter(dped, dveh) # Find the recorded encounter
  xy <- tmp_xy[[1]]
  hotzone <- check_hotzone(dveh, xy, site = site)
  zone_line <- load_stopping_lines(hotzone)

  xdist2 <- (p2metre(dveh$Xmpp) - p2metre(zone_line[3, ]$x))^2
  ydist2 <- (p2metre(dveh$Ympp, FALSE) - p2metre(zone_line[3, ]$y, FALSE))^2
  dveh$d2marking <- sqrt(xdist2 + ydist2)

  dveh$acc <- dveh$speed_change / dveh$time_change

  idx_min <- which.min(dveh$d2marking)
  dveh$d2m <- round(dveh$d2marking, 2)

  idx_min2e <- which.min(dveh$d2e)
  
  # Short slowing time
  cols <- c("Speed_ms", "tta", "d2e", "d2marking", "Time", "speed_change")
  dveh1 <- data.table::data.table(dveh[1:idx_min2e, ])
  test_set <- dveh1[d2marking < distance_to_stop_line & d2marking > one_car_len]


  vtta <- dveh$otta
  ptta <- dped$otta

  if (any(is.infinite(vtta))) {
    test4 <- dped[which(dped$otta == 0), ]$Time - dveh[which.min(otta), ]$Time
  } else if (any(is.infinite(ptta))) {
    test4 <- dped[which.min(ptta), ]$Time - dveh[which(dveh$otta == 0), ]$Time
  } else {
    test4 <- dped[which(dped$otta == 0), ]$Time - dveh[which(dveh$otta == 0), ]$Time
  }

  # who arrived the encounter earlier?
  msg <- ifelse(test4 <= 0, "Ped", "Veh")
  msglab <- paste0(msg, ", ", round(abs(test4), 2), " s")

  if (useId) {
    msgv[[l]] <- paste0("d", tmp[1], "-p", tmp[2], "-v", tmp[3], ", ", msglab)
  } else {
    msgv[[l]] <- msglab
  }

  dveh1$xx <- 1:nrow(dveh1)
  dveh1$day <- tmp[1]
  dveh1$ped <- tmp[2]
  dveh1$veh <- tmp[3]
  dveh1$id <- paste0(dveh1$day, "-", dveh1$ped, "-", dveh1$veh)
  x0 <- rbind(x0, dveh1)

  dped$xx <- 1:nrow(dped)
  dped$acc <- dped$speed_change / dped$time_change
  dped$id <- paste0(tmp[1], "-", tmp[2], "-", tmp[3])
  idx_min_ped <- which.min(dped$d2e)
  dped1 <- dped[2:idx_min_ped, ]

  x1 <- rbind(x1, dped1)
}


# Save up the data for visualisation --------
dspeed_veh <- NULL
dacc_veh <- NULL
dspeed_ped <- NULL
dacc_ped <- NULL
d2e_threshold <- 10
ids <- unique(x0$id)
d2e_threshold_ped <- 4
# Distance to give-way is less than 5 m
# To reverse the label on x axis, showing a car was
# approaching the encounter.
for (i in 1:n) {
  dtmp <- x0[id %in% ids[i]]

  xint <- dtmp[d2m < d2e_threshold, ]$xx[1]
  xend <- dtmp$xx[nrow(dtmp)]

  tmp <- data.table(
    x = dtmp[xint:xend, ]$xx,
    y = dtmp[xint:xend, ]$Speed_ms,
    z = dtmp[xint:xend, ]$d2m,
    ID = i
  )
  tmp2 <- data.table(
    x = dtmp[xint:xend, ]$xx,
    y = dtmp[xint:xend, ]$acc,
    z = dtmp[xint:xend, ]$d2m,
    ID = i
  )

  dspeed_veh <- rbind(dspeed_veh, tmp)
  dacc_veh <- rbind(dacc_veh, tmp2)

  dped_tmp <- x1[id %in% ids[i]]
  xint_ped <- dped_tmp[d2e < d2e_threshold_ped, ]$xx[1]
  xend_ped <- dped_tmp$xx[nrow(dped_tmp)]
  tmp_ped <- data.table(
    x = dped_tmp[xint_ped:xend_ped, ]$xx,
    y = dped_tmp[xint_ped:xend_ped, ]$Speed_ms,
    z = dped_tmp[xint_ped:xend_ped, ]$d2e,
    ID = ids[i]
  )
  tmp2_ped <- data.table(
    x = dped_tmp[xint_ped:xend_ped, ]$xx,
    y = dped_tmp[xint_ped:xend_ped, ]$acc,
    z = dped_tmp[xint_ped:xend_ped, ]$d2e,
    ID = ids[i]
  )
  dspeed_ped <- rbind(dspeed_ped, tmp_ped)
  dacc_ped <- rbind(dacc_ped, tmp2_ped)
}

save(dspeed_veh, dacc_veh, dspeed_ped, dacc_ped,
  file = "tests/pydata/old/4_empirical_short_stop_correct_tmp.rda"
)

# Plot individual speed profiles. -------------
# unit(c(t, r, b, l), "cm")
margin1 <- c(3.5, 3.5, 3.5, 3.5)
margin2 <- c(3.5, 1, 3.5, 3.5)
margin3 <- c(3.5, .25, 3.5, .25)
d2m_thres <- 10

ids <- unique(x0$id)
adj_axis <- FALSE
lab_size <- 6
show_msg <- TRUE
msgsize <- 2

nxlabel <- 4
dtmp2 <- NULL
pal <- Manu::get_pal("Tui")
p <- vector("list", length = n)

for (i in 1:n) {
  dtmp <- x0[id %in% ids[i]]
  dtmp2 <- rbind(dtmp2, dtmp)

  if (nrow(dtmp) >= nxlabel) {
    idx0 <- seq(1, nrow(dtmp), length.out = nxlabel)
  } else {
    idx0 <- seq(1, nrow(dtmp), length.out = nrow(dtmp) / 2)
  }

  xint <- dtmp[d2marking < d2m_thres, ]$xx[1]
  x4 <- which.max(dtmp$d2marking < one_car_len)
  xend <- dtmp$xx[nrow(dtmp)]

  yup <- range(dtmp$Speed_ms, dtmp$acc)[1]
  ydown <- range(dtmp$Speed_ms, dtmp$acc)[2]
  idx <- sort(c(floor(idx0)))

  dzone <- data.frame(x = c(xint, xint, x4, x4), y = c(yup, ydown, ydown, yup))
  
  cols <- c("xx", "Speed_ms", "tta", "d2e", "d2m", "Time", "speed_change")
  idx_min2e <- which.min(dtmp$d2e)
  dtmp[1:idx_min2e, ..cols]
  
  dtmp$lab <- paste0(dtmp$d2m, " - ", round(dtmp$d2e, 1))
  

  
  
  p[[i]] <- ggplot(data = dtmp) + 
    geom_point(aes(x = xx, y = acc), colour = pal[3]) +
    geom_line(aes(x = xx, y = acc, group = id)) +
    geom_point(aes(x = xx, y = Speed_ms), colour = pal[2]) +
    geom_line(aes(x = xx, y = Speed_ms, group = id)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 1, linetype = "dotted", col = pal[4], size = 1) +
    geom_polygon(
      data = dzone, aes(x = x, y = y), alpha = .1,
      colour = "lightblue"
    ) +
    scale_x_continuous(
      labels = dtmp$lab[idx], breaks = dtmp$xx[idx],
      name = "Dist. to Stopping line - Encounter (m)"
    ) +
    scale_y_continuous(
      name = bquote(Acceleration(m / s^2)),
      sec.axis = sec_axis(trans = ~., name = "Speed (m/s)")
    ) +
    theme(
      plot.margin = unit(margin1, "pt"),
      axis.text.x = element_text(size = lab_size, angle = 30),
      axis.text.y = element_text(size = lab_size),
      axis.title.y.right = element_text(colour = pal[2]),
      axis.title.y.left = element_text(colour = pal[3]))

  if (show_msg) {
    w <- diff(range(dtmp$xx))
    h <- diff(range(dtmp$acc, dtmp$Speed_ms))
    dmsg <- data.table(x = w / 2, y = 0, lab = msgv[[i]])
    p[[i]] <- p[[i]] +
      ggtitle(msgv[[i]]) +
      theme(plot.title = element_text(size = 8))
  }
  
}

length(p)
cairo_pdf("tests/docs/first_submit/figs/shortstop_correct_tmp1.pdf")
gridExtra::grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]],
  p[[6]], p[[7]], p[[8]], p[[9]], p[[10]],
  p[[11]], p[[12]],
  ncol = 3
)
dev.off()

cairo_pdf("tests/docs/first_submit/figs/shortstop_correct_tmp2.pdf")
gridExtra::grid.arrange(p[[13]], p[[14]], p[[15]], p[[16]], p[[17]],
                        p[[18]], p[[19]], p[[20]], p[[21]], p[[22]],
                        p[[23]], p[[24]],
                        ncol = 3
)
dev.off()

cairo_pdf("tests/docs/first_submit/figs/shortstop_correct_tmp3.pdf")
gridExtra::grid.arrange(p[[25]], p[[26]], p[[27]], p[[28]], p[[29]],
                        p[[30]], p[[31]],
                        ncol = 3
)
dev.off()




# Section 5 ---------
# Extract initial speeds
dout <- NULL
for (l in 1:n) {
  tmp <- short_stop[l, ]
  i <- tmp[1]
  j <- tmp[2]
  k <- tmp[3]
  fndat <- paste0("tests/interact_data/day", i, "/p", j, "-v", k, ".rda")
  Id <- paste0("d", i, "-p", j, "-v", k)
  load(fndat)
  distance_ped <- dped[1, "d2e"]$d2e
  cols <- c("Speed_ms", "d2e")
  dveh[, ..cols]
  dped[, ..cols]
  
  first_nonzero_v <- which.max(dveh$Speed_ms != 0)
  first_nonzero_p <- which.max(dped$Speed_ms != 0)
  
  speed_veh <- dveh[first_nonzero_v, "Speed_ms"]$Speed_ms
  distance_veh <- dveh[first_nonzero_v, "d2e"]$d2e

  speed_ped <- dped[first_nonzero_p, "Speed_ms"]$Speed_ms
  distance_ped <- dveh[first_nonzero_p, "d2e"]$d2e
  
  dtmp <- data.frame(distance_ped, distance_veh, speed_ped, speed_veh, id = Id)
  dout <- rbind(dout, dtmp)
}


write.csv(dout,
  file = paste0("tests/extdata/4_short_stop_correct.csv"),
  row.names = FALSE
)
