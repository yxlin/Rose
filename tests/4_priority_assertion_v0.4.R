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


# Section 1 ------------
# Data and software parameters
per <- records()
nday <- nrow(per)

# When the car is within 10 metres to the encounter samples, I examine
# if the car is at a high speed, does not decelerate and the pedestrian
# must cross later.
d2e_threshold <- 10
# m <- 1
# l <- 2
message("Capture thee priority assertion: ")
for (l in seq_len(nday)) {
  # Look up the interact figure folder to check if there is any interactions
  folder_name <- paste0("tests/interact_figs/day", l)
  ninteraction <- list.files(folder_name)
  # If no interaction is found, skip this day 
  if (length(ninteraction) == 0) {
    next
  }

  # Look up indexing data in pd.rda 
  file_name <- paste0("tests/extdata/day", l, "/pd.rda")
  load(file_name)
  nxi <- nrow(x1)

  # Load individual interaction pair 
  path2data <- "tests/interact_data"
  for (m in 1:nrow(x1)) {
    i <- x1[m, 1]
    j <- x1[m, 2]
    k <- x1[m, 3]
    fn <- paste0(path2data, "/day", i, "/p", j, "-v", k, ".rda")
    load(fn)
    encounter_point <- Rose::find_encounter(dped, dveh)
    xy <- encounter_point[[1]]
    site <- ifelse(i <= 7, "belle", "qw")
    hotzone <- check_hotzone(dveh, xy, site = site)

    if (nrow(hotzone) == 0) {
      cat("\nUndefined zone in interation: ", c(i, j, k), "\n")
      next
    }

    idx_vmin <- xy[1]
    idx_vthre <- which.max(dveh$d2e < d2e_threshold)

    # The indexs of the sample points leading to the encounter point
    crit_pts <- idx_vthre:idx_vmin
    ncrit <- length(crit_pts)

    # The percentage of deceleration in the critical points.
    ndeceleration <- sum(dveh$speed_change[crit_pts] <= 0, na.rm = TRUE)
    deceleration_percentage <- ndeceleration / ncrit
    # Find out who pass first
    tta <- Rose::who_cross_first(dped, dveh)

    # Sometimes the cameras does not capture any samples in the 10 seconds
    # leading to the encounter. For instance, a but can block the view. This was
    # found out by watching
    # the video.
    if (ncrit <= 5) {
      next
    }

    # Finally, look for the interaction where the car does not slow down.
    # Find the recorded encounter
    # When the percentage is 0, the driver keep its speed.
    if (deceleration_percentage == 0) {
      if (tta[[1]] == "Veh") {
        if (as.numeric(tta[[3]]) <= 3.8) {
          cat(c(i, j, k), "\n")
        }
      }
    }
  }
}

# Examine the moment to moment speeds leading to encounter points
#
# If a driver stepped on his/her braking pedal less than 10% of all
# recorded samples, this might be a "priority assertion"
#
# Find if the car keeps a (1) high speed?; (2) >= such a high speed; (3)
# (3) the pedestrian cross later (being denied the right of way).


# Section 2 -------
index_matrix <- matrix(
  c(
    2, 40, 4, # zone 1
    5, 649, 2, # zone 4
    5, 1069, 4, # zone 1
    5, 1131, 5, # zone mid-intersection
    6, 860, 3, # zone 2
    9, 72, 2, #  zone 5
    9, 982, 5, # zone 5
    10, 903, 4, # zone 5
    11, 1446, 4, # zone 5
    13, 249, 4 # zone 6
  ),
  ncol = 3, byrow = TRUE
)

# Aggregate the 
n <- nrow(index_matrix)
useId <- TRUE
x0 <- NULL # veh
x1 <- NULL # ped
msgv <- list()

for (l in 1:n) {
  data_index <- index_matrix[l, ]
  i <- data_index[1]
  j <- data_index[2]
  k <- data_index[3]
  fndat <- paste0("tests/interact_data/day", i, "/p", j, "-v", k, ".rda")
  load(fndat)
  site <- ifelse(i <= 7, "belle", "qw")

  dveh$acc <- dveh$speed_change / dveh$time_change
  dveh$d2e2 <- round(dveh$d2e, 1)

  tmp_xy <- find_encounter(dped, dveh) # Find the recorded encounter
  xy <- tmp_xy[[1]]
  hotzone <- check_hotzone(dveh, xy, site = site)

  idx_min <- which.min(dveh$d2e)
  # Remove the first sample , bcuz sometimes first sample often
  # shows abnormalities.
  dveh1 <- dveh[2:idx_min, ]

  vtta <- dveh1$otta
  ptta <- dped$otta

  if (any(is.infinite(vtta))) {
    test4 <- dped[which(dped$otta == 0), ]$Time -
      dveh1[which.min(otta), ]$Time
  } else if (any(is.infinite(ptta))) {
    test4 <- dped[which.min(ptta), ]$Time -
      dveh1[which(dveh1$otta == 0), ]$Time
  } else {
    test4 <- dped[which(dped$otta == 0), ]$Time -
      dveh1[which(dveh1$otta == 0), ]$Time
  }

  # who arrived the encounter earlier?
  msg <- ifelse(test4 <= 0, "Ped", "Veh")

  # if (l %in% c(6:10)) {
  # msglab <- paste0(msg, ", ", round(abs(test4), 2), " s (no video)")
  # } else {
  msglab <- paste0(msg, ", ", round(abs(test4), 2), " s")
  # }

  if (useId) {
    msgv[[l]] <- paste0(
      "d", data_index[1], "-p", data_index[2], "-v",
      data_index[3], ", ", msglab
    )
  } else {
    msgv[[l]] <- msglab
  }

  dveh1$xx <- 1:nrow(dveh1)
  dveh1$day <- data_index[1]
  dveh1$ped <- data_index[2]
  dveh1$veh <- data_index[3]
  dveh1$id <- paste0(dveh1$day, "-", dveh1$ped, "-", dveh1$veh)
  x0 <- rbind(x0, dveh1)
  
  dped$xx <- 1:nrow(dped)
  dped$acc <- dped$speed_change / dped$time_change
  dped$id <- paste0(data_index[1], "-", data_index[2], "-", data_index[3])
  idx_min_ped <- which.min(dped$d2e)
  dped1 <- dped[2:idx_min_ped, ]
  
  x1 <- rbind(x1, dped1)
  
}

# Visualisation 2 --------
dspeed_veh <- NULL
dacc_veh <- NULL
dspeed_ped <- NULL
dacc_ped <- NULL
d2e_threshold <- 10
ids <- unique(x0$id)
d2e_threshold_ped <- 4




for (i in 1:n) {
  dtmp0 <- x0[id %in% ids[i]]
  dtmp <- dtmp0[2:nrow(dtmp0)]
  
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
     file = "tests/pydata/old/5_empirical_priority_assertion_tmp.rda")


which(is.na(dnew$x))
dnew[ID == 1]

p0 <- ggplot(data = dnew[!is.na(x)]) +
  geom_point(aes(x = z, y = y), colour = pal[2]) +
  geom_line(aes(x = z, y = y, group = ID), colour = pal[2]) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_reverse() +
  scale_y_continuous(name = "Speed (m/s)") +
  theme(
    axis.text.x = element_text(size = lab_size),
    axis.text.y = element_text(size = lab_size),
    axis.title.y.left = element_text(colour = pal[2])
  )

p1 <- ggplot(data = dacc[!is.na(x)]) +
  geom_point(aes(x = z, y = y), colour = pal[3]) +
  geom_line(aes(x = z, y = y, group = ID), colour = pal[3]) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_reverse() +
  scale_y_continuous(name = bquote(Acceleration(m / s^2))) +
  theme(
    axis.text.x = element_text(size = lab_size),
    axis.text.y = element_text(size = lab_size),
    axis.title.y.left = element_text(colour = pal[3])
  )

p0
p1

# Section 3 ----------
# Combine the short stop and the priority assertion to facilitate a comparison
pal2 <- c("#e5f5f9", "#99d8c9", "#2ca25f")
pal3 <- c("#e0ecf4", "#9ebcda", "#8856a7")
pal1 <- c("#f0f0f0", "#bdbdbd", "#636363")
load(file = "tests/extdata/4_short_stop.rda")
dnew0 <- dnew[!is.na(x)]
dacc0 <- dacc[!is.na(x)]

ddnew0 <- dnew0[, .(M = mean(y)), .(z)]
setorder(ddnew0, z)
ddnew0$filteredM <- Kalman(ddnew0$M)

ddacc0 <- dacc0[, .(M = mean(y)), .(z)]
setorder(ddacc0, z)
ddacc0$filteredM <- Kalman(ddacc0$M)

dnew0$type <- "Short-slowing"
dacc0$type <- "Short-slowing"

ddacc0$type <- "Short-slowing"
ddnew0$type <- "Short-slowing"

load(file = "tests/extdata/5_priority_assertion.rda")
dnew1 <- dnew[!is.na(x)]
dacc1 <- dacc[!is.na(x)]

ddnew1 <- dnew1[, .(M = mean(y)), .(z)]
setorder(ddnew1, z)
ddnew1$filteredM <- Kalman(ddnew1$M)

ddacc1 <- dacc1[, .(M = mean(y)), .(z)]
setorder(ddacc1, z)
ddacc1$filteredM <- Kalman(ddacc1$M)

dnew1$type <- "Priority-assertion"
dacc1$type <- "Priority-assertion"

ddacc1$type <- "Priority-assertion"
ddnew1$type <- "Priority-assertion"


dnew <- rbind(dnew0, dnew1)
dacc <- rbind(dacc0, dacc1)

ddnew <- rbind(ddnew0, ddnew1)
ddacc <- rbind(ddacc0, ddacc1)



# unit(c(t, r, b, l), "cm")
margin1 <- c(3.5, 3.5, 3.5, 3.5)
margin2 <- c(3.5, 1, 3.5, 3.5)
margin3 <- c(3.5, .25, 3.5, .25)
ids <- unique(x0$id)
dtmp2 <- NULL
p <- vector("list", length = n)
d2e_thres <- 10

# Control how the figure looks
show_msg <- TRUE
msgsize <- 2
lab_size <- 6
nxlabel <- 4
# i <- 1
for (i in 1:n) {
  dtmp0 <- x0[id %in% ids[i]]
  dtmp <- dtmp0[2:nrow(dtmp0)]
  xint <- dtmp[d2e < d2e_thres, ]$xx[1]
  xend <- dtmp$xx[nrow(dtmp)]
  dtmp2 <- rbind(dtmp2, dtmp)
  if (nrow(dtmp) >= nxlabel) {
    idx0 <- seq(1, nrow(dtmp), length.out = nxlabel)
  } else {
    idx0 <- seq(1, nrow(dtmp), length.out = nrow(dtmp) / 2)
  }
  idx <- sort(c(floor(idx0), xint, xend))




  if (i %in% c(1, 4, 7)) {
    ptmp <- ggplot(data = dtmp) +
      scale_x_continuous(
        labels = dtmp$d2e2[idx], breaks = dtmp$xx[idx],
        name = ""
      ) +
      scale_y_continuous(
        name = bquote(Acceleration(m / s^2)),
        sec.axis = sec_axis(trans = ~., name = "")
      )
  } else if (i == 10) {
    ptmp <- ggplot(data = dtmp) +
      scale_x_continuous(
        labels = dtmp$d2e2[idx], breaks = dtmp$xx[idx],
        name = "Dist. to Encounter (m)"
      ) +
      scale_y_continuous(
        name = bquote(Acceleration(m / s^2)),
        sec.axis = sec_axis(trans = ~., name = "Speed (m/s)")
      )
  } else if (i %in% c(3, 6)) {
    ptmp <- ggplot(data = dtmp) +
      scale_x_continuous(
        labels = dtmp$d2e2[idx], breaks = dtmp$xx[idx],
        name = ""
      ) +
      scale_y_continuous(
        name = "",
        sec.axis = sec_axis(trans = ~., name = "Speed (m/s)")
      )
  } else if (i %in% c(2, 5, 7)) {
    ptmp <- ggplot(data = dtmp) +
      scale_x_continuous(
        labels = dtmp$d2e2[idx], breaks = dtmp$xx[idx],
        name = ""
      ) +
      scale_y_continuous(
        name = "",
        sec.axis = sec_axis(trans = ~., name = "")
      )
  } else if (i %in% c(8)) {
    ptmp <- ggplot(data = dtmp) +
      scale_x_continuous(
        labels = dtmp$d2e2[idx], breaks = dtmp$xx[idx],
        name = "Dist. to Encounter (m)"
      ) +
      scale_y_continuous(
        name = "",
        sec.axis = sec_axis(trans = ~., name = "")
      )
  } else if (i == 9) {
    ptmp <- ggplot(data = dtmp) +
      scale_x_continuous(
        labels = dtmp$d2e2[idx], breaks = dtmp$xx[idx],
        name = "Dist. to Encounter (m)"
      ) +
      scale_y_continuous(
        name = "",
        sec.axis = sec_axis(trans = ~., name = "Speed (m/s)")
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


  yup <- range(dtmp$Speed_ms, dtmp$acc)[1]
  ydown <- range(dtmp$Speed_ms, dtmp$acc)[2]
  dzone <- data.frame(x = c(xint, xint, xend, xend), y = c(yup, ydown, ydown, yup))


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

  p[[i]] <- p[[i]] + theme(
    plot.margin = unit(margin2, "pt"),
    axis.text.x = element_text(size = lab_size, angle = 30),
    axis.text.y = element_text(size = lab_size),
    axis.title.y.right = element_text(colour = pal[2]),
    axis.title.y.left = element_text(colour = pal[3])
  )


  # if (i %in% c(1, 5, 9, 13, 17)) {
  #   p[[i]] <- p[[i]] + theme(
  #     plot.margin = unit(margin2, "pt"),
  #     axis.text.x = element_text(size = lab_size),
  #     axis.text.y = element_text(size = lab_size),
  #     axis.title.y.right = element_text(colour = pal[2]),
  #     axis.title.y.left = element_text(colour = pal[3])
  #   )
  # } else if (i %in% c(2:3, 6:7, 10:11, 14:15, 18:19)) {
  #   p[[i]] <- p[[i]] + theme(
  #     plot.margin = unit(margin3, "pt"),
  #     axis.text.x = element_text(size = lab_size),
  #     axis.text.y = element_text(size = lab_size),
  #     axis.title.y.right = element_text(colour = pal[2]),
  #     axis.title.y.left = element_text(colour = pal[3])
  #   )
  # } else {
  #   p[[i]] <- p[[i]] + theme(
  #     plot.margin = unit(margin1, "pt"),
  #     axis.text.x = element_text(size = lab_size),
  #     axis.text.y = element_text(size = lab_size),
  #     axis.title.y.right = element_text(colour = pal[2]),
  #     axis.title.y.left = element_text(colour = pal[3])
  #   )
  # }
  # fn <- paste0("tests/tmp/d", ids[i], ".png")
  # png(filename = fn, 800, 800)
  # print(p[[i]])
  # dev.off()
}

cairo_pdf("tests/docs/first_submit/figs/priority_assertion_tmp.pdf")
gridExtra::grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]],
  p[[6]], p[[7]], p[[8]], p[[9]], p[[10]],
  ncol = 3
)
dev.off()


# labels = dtmp$d2e2[idx], breaks = dtmp$xx[idx],

bk <- c(8.8, 7.5, 5.0, 2.5, 0)

lab_size <- 10
p2 <- ggplot() +
  # geom_point(data = dnew, aes(x = z, y = y), colour = pal2[1]) +
  geom_line(data = dnew, aes(x = z, y = y, group = ID), colour = pal2[1]) +
  geom_line(
    data = ddnew, aes(x = z, y = filteredM), colour = pal2[3],
    size = 1.2
  ) +
  geom_line(data = dacc, aes(x = z, y = y, group = ID), colour = pal3[1]) +
  geom_line(
    data = ddacc, aes(x = z, y = filteredM), colour = pal3[3],
    size = 1.2
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(. ~ type) +
  xlab("Distance to give-way / encounter (m)") +
  theme_bw(base_size = 16) +
  scale_x_reverse() +
  # scale_x_reverse(breaks = bk) +
  scale_y_continuous(
    name = bquote(Acceleration(m / s^2)),
    sec.axis = sec_axis(trans = ~., name = "Speed (m/s)")
  ) +
  theme(
    axis.text.x = element_text(size = lab_size),
    axis.text.y = element_text(size = lab_size),
    axis.title.y.right = element_text(colour = pal2[3]),
    axis.title.y.left = element_text(colour = pal3[3])
  )


# cairo_pdf("tests/docs/first_submit/figs/car_profile.pdf")
# print(p2)
# dev.off()

load(file = "tests/extdata/dshort_stop_GM.rda")
dGMShort <- d
load(file = "tests/extdata/dpriority_GM.rda")
dGMPriority <- d


dshortSpeed <- dGMShort[, .(M = mean(S1)), .(x)]
dshortAcc <- dGMShort[, .(M = mean(A1)), .(x)]
dprioritySpeed <- dGMPriority[, .(M = mean(S1)), .(x)]
dpriorityAcc <- dGMPriority[, .(M = mean(A1)), .(x)]

dshortSpeed$type <- "Short-slowing"
dshortAcc$type <- "Short-slowing"

dprioritySpeed$type <- "Priority-assertion"
dpriorityAcc$type <- "Priority-assertion"


dGMSpeed <- rbind(dshortSpeed, dprioritySpeed)
dGMAcc <- rbind(dshortAcc, dpriorityAcc)



p3 <- ggplot() +
  geom_line(
    data = dGMSpeed, aes(x = x, y = M), colour = pal3[3],
    size = 1.2, linetype = "dashed"
  ) +
  geom_line(
    data = dGMAcc, aes(x = x, y = M), colour = pal1[3],
    size = 1.2, linetype = "dashed"
  ) +
  geom_line(
    data = dnew, aes(x = z, y = y, group = ID),
    colour = pal3[1]
  ) +
  geom_line(
    data = ddnew, aes(x = z, y = filteredM),
    colour = pal3[3], size = 1.2
  ) +
  geom_line(
    data = dacc, aes(x = z, y = y, group = ID),
    colour = pal1[1]
  ) +
  geom_line(
    data = ddacc, aes(x = z, y = filteredM),
    colour = pal1[3], size = 1.2
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_y_continuous(
    name = bquote(Acceleration(m / s^2)),
    sec.axis = sec_axis(trans = ~., name = "Speed (m/s)")
  ) +
  scale_x_reverse() +
  xlab("Distance to give-way / encounter (m)") +
  facet_grid(. ~ type) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(size = lab_size),
    axis.text.y = element_text(size = lab_size),
    axis.text.y.right = element_text(colour = pal3[3]),
    axis.text.y.left = element_text(colour = pal1[3]),
    axis.title.y.right = element_text(colour = pal3[3]),
    axis.title.y.left = element_text(colour = pal1[3])
  )


cairo_pdf("tests/docs/first_submit/figs/car_profile2.pdf")
print(p3)
dev.off()

png("tests/docs/first_submit/figs/car_profile2.png")
print(p3)
dev.off()

# Section 4 --------
index_matrix <- matrix(
  c(
    2, 40, 4,
    5, 649, 2,
    5, 1069, 4,
    5, 1131, 5,
    6, 860, 3,
    9, 72, 2,
    9, 982, 5,
    10, 903, 4,
    11, 1446, 4,
    13, 249, 4
  ),
  ncol = 3, byrow = TRUE
)

n <- nrow(index_matrix)

dout <- NULL
for (l in 1:n) {
  tmp <- index_matrix[l, ]
  i <- tmp[1]
  j <- tmp[2]
  k <- tmp[3]
  fndat <- paste0("tests/interact_data/day", i, "/p", j, "-v", k, ".rda")
  Id <- paste0("d", l, "-p", j, "-v", k)
  load(fndat)
  
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
  file = paste0("tests/extdata/5_priority_assertion.csv"),
  row.names = FALSE
)
