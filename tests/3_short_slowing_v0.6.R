## Description: Capture plausible short stopping observations
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

pal <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
  "#D55E00", "#CC79A7"
)

# Section 1 ----------
# Define the threshold of speed change. It has to be less than -3.5 m/s^2
# threshold <- -3.5
threshold <- -4.2
d2m_thres <- 10

# Define the distance threshold to the stop line. The assumption is that
# within 10 m, a driver would start to consider whether he / she should
# yield, if any pedestrian is spotted.
distance_to_stop_line <- 10

# Define the jump for the x axis label to avoid cluttering
jump <- 5
nday <- nrow(records())
# Section 2 ----------
# 1. Print out how many interactions were captured in each day.
# 2. Fixed day 3, which has one interaction was captured in the middle of the
# intersection.
# This interaction has no zone id. I will skip this interaction.
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
  }

  cat("Day", i, "has ", nrow(d), "interaction(s): \n")

  if (nrow(d) > 0) {
    day_with_interaction <- c(day_with_interaction, i)
  }
}

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
day_with_interaction <- c(2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13)
for (i in day_with_interaction) {
  if (i <= 7) {
    zones <- 3:4
    site <- "belle"
  } else {
    zones <- 5
    site <- "qw"
  }
  load(paste0("tests/extdata/day", i, "/pd.rda"))

  # Fix the zone id for day 2, 3, 5, cuz I draw a narrow zone for them
  if (i == 2 || i == 3 || i == 5) {
    idx <- which(zoneid == 99)
    zoneid[idx] <- 2
  }

  d <- data.table(cbind(x1, pet, zoneid))
  # I will skip Zone 1 and 2, because they are unmarked crossings.
  # Zone 3 and 4 have zebra and stop lines

  for (izone in zones) {
    dzonei <- d[zoneid == izone]
    nint <- nrow(dzonei)
    if (nint == 0) next

    for (l in 1:nint) {
      j <- dzonei[l, ]$ped
      k <- dzonei[l, ]$veh
      fndat <- paste0("tests/interact_data/day", i, "/p", j, "-v", k, ".rda")
      load(fndat)

      dveh$time_change <- c(NA, diff(dveh$Time))
      dveh$speed_change <- c(NA, diff(dveh$Speed_ms))

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
      hotzone

      if (nrow(hotzone) == 0) {
        next
      }

      zoneline <- load_stopping_lines(hotzone)

      xdist2 <- (p2metre(dveh$Xmpp) - p2metre(zoneline[3, ]$x))^2
      ydist2 <- (p2metre(dveh$Ympp, FALSE, site = site) -
        p2metre(zoneline[3, ]$y, FALSE, site = site))^2
      dveh$d2marking <- sqrt(xdist2 + ydist2)

      idx_min <- which.min(dveh$d2marking)
      dveh$d2m <- round(dveh$d2marking, 2)

      # Extract the data less than 10 metre away from the stop line
      dveh_before <- dveh[1:idx_min, ]

      # Find a speed change that can halt the car (an average
      # length of car 4.2 m)
      test_set <- dveh_before[d2m < d2m_thres]$speed_change

      dveh1 <- dveh_before[2:nrow(dveh_before)]

      ## Set up many plotting controls to make an insightful plot
      idx_ss <- which.min(dveh1$acc)
      idx <- sort(c(seq(1, nrow(dveh1), jump), idx_ss))
      using_idx <- !(c(diff(idx), NA) %in% c(1))
      idx <- idx[using_idx]

      zonename <- paste0("Zone ", unique(hotzone$id))

      # Examine if a car had a speed change smaller than -3.5 m/s
      if (any(test_set < threshold, na.rm = TRUE)) {
        message("A short stop: ", appendLF = F)
        cat(c(i, j, k), "\n")

        xint <- dveh1[d2m < d2m_thres, ]$xx[1]

        ptmp <- ggplot(data = dveh1) +
          scale_x_continuous(
            labels = dveh1$d2m[idx], breaks = dveh1$xx[idx],
            name = "Distance to Give-way Line (m)"
          ) +
          scale_y_continuous(
            name = bquote(Acceleration(m / s^2)),
            sec.axis = sec_axis(trans = ~., name = "Speed (m/s)")
          )

        p2 <- ptmp + geom_point(aes(x = xx, y = acc),
          colour = pal[3],
          size = 5
        ) +
          geom_line(aes(x = xx, y = acc, group = ID)) +
          geom_point(aes(x = xx, y = Speed_ms), colour = pal[2], size = 5) +
          geom_line(aes(x = xx, y = Speed_ms, group = ID)) +
          geom_hline(yintercept = 0, linetype = "dotted") +
          geom_vline(xintercept = xint, linetype = "dotted") +
          ggtitle(zonename) +
          theme_classic(base_size = 24) +
          theme(
            axis.text.x = element_text(angle = 30),
            axis.text.y = element_text(),
            axis.title.y.right = element_text(colour = pal[2]),
            axis.title.y.left = element_text(colour = pal[3])
          )

        # fn <- paste0("tests/tmp/d", i, "-p", j, "-v", k, "tmp.png")
        # png(filename = fn, 800, 800)
        # print(p2)
        # dev.off()
      }
    }
  }
}


# Section 4 ---------
# 2, 59, 6, has a time jump
# 3, 45, 6
# 3, 387, 4, has a time jump
# 4, 119, 4, has a time jump
# 5, 208, 4, has a time jump
# 6, 420, 6
# 12 cases show speed-acceleration profiles fit to the definition.
short_stop <- matrix(
  c(
    2, 550, 3, # zone 4
    2, 1022, 3, # zone 4
    3, 157, 3, # zone 4
    3, 984, 3, # zone 4
    5, 1055, 3, # zone 4
    9, 1083, 1, # zone 5
    10, 1023, 1, # zone 5
    10, 1206, 3, # zone 5
    10, 1099, 3, # zone 5
    11, 182, 3, # zone 5
    11, 1218, 5, # zone 5
    12, 150, 6 # zone 5
  ),
  ncol = 3, byrow = TRUE
)


n <- nrow(short_stop)
useId <- TRUE
x0 <- NULL # veh
x1 <- NULL # ped
SST <- NULL
msgv <- list()
# Test 2-550-3
l <- 3

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

  dveh$Speed_ms <- dveh$Speed * (1000 / 3600)
  dveh$time_change <- c(NA, diff(dveh$Time))
  dveh$speed_change <- c(NA, diff(dveh$Speed_ms))
  dveh$acc <- dveh$speed_change / dveh$time_change

  idx_min <- which.min(dveh$d2marking)
  dveh$d2m <- round(dveh$d2marking, 2)

  # Short slowing time
  cols <- c("speed_change", "tta", "d2e", "d2m", "Time") 
  # dveh[1:idx_min, ..cols]
  first_negative <- which.max(dveh$speed_change < 0)
  
  SST_tmp <- dveh[idx_min, ..cols]$Time - dveh[first_negative, ..cols]$Time
  SST <- c(SST, as.numeric(SST_tmp))
  
  # Remove the first sample on the ground that across all Viscando's data, its
  # first sample often shows abnormalities.
  dtmp_v <- dveh[2:idx_min, ]

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

  dtmp_v$xx <- 1:nrow(dtmp_v)
  dtmp_v$day <- tmp[1]
  dtmp_v$ped <- tmp[2]
  dtmp_v$veh <- tmp[3]
  dtmp_v$id <- paste0(dtmp_v$day, "-", dtmp_v$ped, "-", dtmp_v$veh)
  x0 <- rbind(x0, dtmp_v)
  
  dped$xx <- 1:nrow(dped)
  dped$acc <- dped$speed_change / dped$time_change
  dped$id <- paste0(tmp[1], "-", tmp[2], "-", tmp[3])
  idx_min_ped <- which.min(dped$d2e)
  dped1 <- dped[2:idx_min_ped, ]
  
  x1 <- rbind(x1, dped1)
}

SST
# [1]  5.76  5.44  9.12  6.24  5.28 11.68  8.16  7.36  9.60 10.56  2.24  8.96  9.12
psych::describe(SST)
# vars  n mean   sd median trimmed  mad  min   max range  skew kurtosis   se
# X1    1 12 7.53 2.66   7.76    7.65 2.85 2.24 11.68  9.44 -0.27    -0.92 0.77
hist(SST)

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
  dtmp0 <- x0[id %in% ids[i]]
  dtmp <- dtmp0[2:nrow(dtmp0)]
  dtmp$d2m <- round(dtmp$d2m, 1)
  

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
     file = "tests/pydata/old/4_empirical_short_stop_tmp.rda")

# Plot individual speed profiles. -------------
# unit(c(t, r, b, l), "cm")
margin1 <- c(3.5, 3.5, 3.5, 3.5)
margin2 <- c(3.5, 1, 3.5, 3.5)
margin3 <- c(3.5, .25, 3.5, .25)
d2m_thres <- 10

ids <- unique(x0$id)
jump <- 5
adj_axis <- FALSE
lab_size <- 6
show_msg <- TRUE
msgsize <- 2

nxlabel <- 4
dtmp2 <- NULL

p <- vector("list", length = n)

for (i in 1:n) {
  dtmp0 <- x0[id %in% ids[i]]
  dtmp <- dtmp0[2:nrow(dtmp0)]
  dtmp$d2m <- round(dtmp$d2m, 1)
  dtmp2 <- rbind(dtmp2, dtmp)

  if (nrow(dtmp) >= nxlabel) {
    idx0 <- seq(1, nrow(dtmp), length.out = nxlabel)
  } else {
    idx0 <- seq(1, nrow(dtmp), length.out = nrow(dtmp) / 2)
  }

  xint <- dtmp[d2m < d2m_thres, ]$xx[1]
  xend <- dtmp$xx[nrow(dtmp)]

  yup <- range(dtmp$Speed_ms, dtmp$acc)[1]
  ydown <- range(dtmp$Speed_ms, dtmp$acc)[2]
  idx_ss <- which.min(dtmp$acc)

  if (i %in% c(1, 4, 5)) {
    idx <- sort(c(floor(idx0), xend))
  } else {
    idx <- sort(c(floor(idx0), idx_ss, xend))
  }

  dzone <- data.frame(x = c(xint, xint, xend, xend), y = c(yup, ydown, ydown, yup))


  if (adj_axis) {
    if (i %in% c(1, 4, 7)) {
      ptmp <- ggplot(data = dtmp) +
        scale_x_continuous(
          labels = dtmp$d2m[idx], breaks = dtmp$xx[idx],
          name = ""
        ) +
        scale_y_continuous(
          name = bquote(Acceleration(m / s^2)),
          sec.axis = sec_axis(trans = ~., name = "")
        )
    } else if (i %in% c(9)) {
      ptmp <- ggplot(data = dtmp) +
        scale_x_continuous(
          labels = dtmp$d2m[idx], breaks = dtmp$xx[idx],
          name = "Dist. to Give-way (m)"
        ) +
        scale_y_continuous(
          name = "",
          sec.axis = sec_axis(trans = ~., name = "Speed (m/s)")
        )
    } else if (i == 10) {
      ptmp <- ggplot(data = dtmp) +
        scale_x_continuous(
          labels = dtmp$d2m[idx], breaks = dtmp$xx[idx],
          name = "Dist. to Give-way (m)"
        ) +
        scale_y_continuous(
          name = bquote(Acceleration(m / s^2)),
          sec.axis = sec_axis(trans = ~., name = "")
        )
    } else if (i %in% c(3, 6)) {
      ptmp <- ggplot(data = dtmp) +
        scale_x_continuous(
          labels = dtmp$d2m[idx], breaks = dtmp$xx[idx],
          name = ""
        ) +
        scale_y_continuous(
          name = "",
          sec.axis = sec_axis(trans = ~., name = "Speed (m/s)")
        )
    } else if (i == 8) {
      ptmp <- ggplot(data = dtmp) +
        scale_x_continuous(
          labels = dtmp$d2m[idx], breaks = dtmp$xx[idx],
          name = "Dist. to Give-way (m)"
        ) +
        scale_y_continuous(
          name = "",
          sec.axis = sec_axis(trans = ~., name = "")
        )
    } else {
      ptmp <- ggplot(data = dtmp) +
        scale_x_continuous(
          labels = dtmp$d2m[idx], breaks = dtmp$xx[idx],
          name = ""
        ) +
        scale_y_continuous(
          name = "",
          sec.axis = sec_axis(trans = ~., name = "")
        )
    }
  } else {
    ptmp <- ggplot(data = dtmp) +
      scale_x_continuous(
        labels = dtmp$d2m[idx], breaks = dtmp$xx[idx],
        name = ""
      ) +
      scale_y_continuous(
        name = "",
        sec.axis = sec_axis(trans = ~., name = "")
      )
  }


  p[[i]] <- ptmp + geom_point(aes(x = xx, y = acc), colour = pal[3]) +
    geom_line(aes(x = xx, y = acc, group = id)) +
    geom_point(aes(x = xx, y = Speed_ms), colour = pal[2]) +
    geom_line(aes(x = xx, y = Speed_ms, group = id)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = xint, linetype = "dotted") +
    geom_polygon(
      data = dzone, aes(x = x, y = y), alpha = .2,
      colour = "lightblue"
    )

  if (show_msg) {
    w <- diff(range(dtmp$xx))
    h <- diff(range(dtmp$acc, dtmp$Speed_ms))
    dmsg <- data.table(x = w / 2, y = 0, lab = msgv[[i]])
    p[[i]] <- p[[i]] +
      ggtitle(msgv[[i]]) +
      theme(plot.title = element_text(size = 8))
  }


  if (i %in% c(1, 5, 9, 13, 17)) {
    p[[i]] <- p[[i]] + theme(
      plot.margin = unit(margin2, "pt"),
      axis.text.x = element_text(size = lab_size, angle = 30),
      axis.text.y = element_text(size = lab_size),
      axis.title.y.right = element_text(colour = pal[2]),
      axis.title.y.left = element_text(colour = pal[3])
    )
  } else if (i %in% c(2:3, 6:7, 10:11, 14:15, 18:19)) {
    p[[i]] <- p[[i]] + theme(
      plot.margin = unit(margin3, "pt"),
      axis.text.x = element_text(size = lab_size, angle = 30),
      axis.text.y = element_text(size = lab_size),
      axis.title.y.right = element_text(colour = pal[2]),
      axis.title.y.left = element_text(colour = pal[3])
    )
  } else {
    p[[i]] <- p[[i]] + theme(
      plot.margin = unit(margin1, "pt"),
      axis.text.x = element_text(size = lab_size, angle = 30),
      axis.text.y = element_text(size = lab_size),
      axis.title.y.right = element_text(colour = pal[2]),
      axis.title.y.left = element_text(colour = pal[3])
    )
  }
}


cairo_pdf("tests/docs/first_submit/figs/shortstop_tmp3.pdf")
gridExtra::grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]],
  p[[6]], p[[7]], p[[8]], p[[9]], p[[10]],
  p[[11]], p[[12]],
  ncol = 3
)
dev.off()

png("tests/docs/first_submit/figs/shortstop_tmp3.png", 1024, 768)
gridExtra::grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]],
  p[[6]], p[[7]], p[[8]], p[[9]], p[[10]],
  p[[11]], p[[12]],
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
  Id <- paste0("d", l, "-p", j, "-v", k)
  load(fndat)
  distance_ped <- dped[1, "d2e"]$d2e
  if (l == 9) {
    speed_veh <- dveh[4, "Speed_ms"]$Speed_ms
    distance_veh <- dveh[4, "d2e"]$d2e
  } else {
    speed_veh <- dveh[1, "Speed_ms"]$Speed_ms
    distance_veh <- dveh[1, "d2e"]$d2e
  }

  speed_ped <- dped[1, "Speed_ms"]$Speed_ms
  dtmp <- data.frame(distance_ped, distance_veh, speed_ped, speed_veh, id = Id)
  dout <- rbind(dout, dtmp)
}


write.csv(dout,
  file = paste0("tests/extdata/4_short_stop.csv"),
  row.names = FALSE
)
