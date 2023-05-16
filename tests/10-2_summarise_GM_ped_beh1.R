rm(list = ls())
pkg <- c("data.table", "ggplot2", "Rose", "png")
sapply(pkg, require, character.only = TRUE)

core_path <- "[01]Projects/viscando/"
windows_path <- paste0("F:/", core_path)
unix_path <- paste0("/media/yslin/Avocet/", core_path)
wk <- ifelse(.Platform$OS.type == "windows", shortPathName(windows_path), 
             unix_path)
setwd(wk)

prepare_empirical_figure <- function(fn, is_pedestrian = TRUE) {
  
  # fn <- datalist[[1]]
  load(fn)
  # is_car <- TRUE
  if( is_pedestrian ) {
    dspeed0 <- dspeed_ped[!is.na(x)]
    dacc0   <- dacc_ped[!is.na(x)]
    dAvgSpeed <- dspeed0[, .(M = mean(y)), .(z)]
    dAvgAcceleration <-  dacc0[, .(M = mean(y)), .(z)]
    setorder(dAvgSpeed, z)
    setorder(dAvgAcceleration, z)
    
    dAvgSpeed$filteredM <- Kalman(dAvgSpeed$M)
    dAvgAcceleration$filteredM <- Kalman(dAvgAcceleration$M)
    
    
  } else {
    dspeed0 <- dspeed_veh[!is.na(x)]
    dacc0   <- dacc_veh[!is.na(x)]
    dAvgSpeed <- dspeed0[, .(M = mean(y)), .(z)]
    dAvgAcceleration <-  dacc0[, .(M = mean(y)), .(z)]
    setorder(dAvgSpeed, z)
    setorder(dAvgAcceleration, z)
    
    dAvgSpeed$filteredM <- Kalman(dAvgSpeed$M)
    dAvgAcceleration$filteredM <- Kalman(dAvgAcceleration$M)
    
  }
  
  return(list(dspeed0, dacc0, dAvgSpeed, dAvgAcceleration))
}

prepare_simulation_figure <- function(fn, data_length = 0, 
                                      is_pedestrian = TRUE) {
  # fn <- simlist[[1]]
  # data_length <- nrow(data0[[3]])
  # is_pedestrian <- TRUE
  if(data_length == 0) {
    stop("Must enter nrow of the data")
  }
  
  load(fn)
  if (is_pedestrian) {
    speed_cols <- c("x", "S0")
    acc_cols <- c("x", "A0")
    time_rng <- 4
  } else {
    speed_cols <- c("x", "S1")
    acc_cols <- c("x", "A1")
    time_rng <- 10
  }
  
  dSpeedSim <- d[, ..speed_cols]
  dAccSim <- d[, ..acc_cols] 

  if (is_pedestrian) {
    dAvgSpeedSim <- dSpeedSim[, .(M = mean(S0)), .(x)]
    dAvgAccSim <- dAccSim[, .(M = mean(A0)), .(x)]
  } else {
    dAvgSpeedSim <- dSpeedSim[, .(M = mean(S1)), .(x)]
    dAvgAccSim <- dAccSim[, .(M = mean(A1)), .(x)]
  }
  
  
  dAvgSpeedSimPlot  <- dAvgSpeedSim[x <= time_rng]
  dAvgSpeedSimPlot2 <- dAvgSpeedSimPlot[seq(1, nrow(dAvgSpeedSimPlot), 
                                            length.out = data_length)]
  
  dAvgAccSimPlot <- dAvgAccSim[x <= time_rng ]
  dAvgAccSimPlot2 <- dAvgAccSimPlot[seq(1, nrow(dAvgAccSimPlot), 
                                        length.out = data_length)]
  
  return(list(dAvgSpeedSimPlot2, dAvgAccSimPlot2))
}

themes <- function(bs = 14) {
  theme_minimal(base_size = bs) +
    theme (
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(size = lab_size),
      axis.text.y = element_text(size = lab_size),
      axis.ticks = element_line(),
      axis.text.y.right = element_text(colour = pal3[3]),
      axis.text.y.left = element_text(colour = pal1[3]),
      axis.title.y.right = element_text(colour = pal3[3]),
      axis.title.y.left = element_text(colour = pal1[3]))
  
}
# Overall figures --------
lab_size <- 10
pal1 <- c("#f0f0f0", "#bdbdbd", "#636363")
pal2 <- c("#e5f5f9", "#99d8c9", "#2ca25f")
pal3 <- c("#e0ecf4", "#9ebcda", "#8856a7")

datalist <- list(
  "tests/pydata/old/6_empirical_hesitation_tmp.rda", # <- replaced by hestitant_crossings
  "tests/pydata/old/7_empirical_early_yield_tmp.rda",
  "tests/pydata/old/4_empirical_short_stop_tmp.rda",
  "tests/pydata/old/5_empirical_priority_assertion_tmp.rda",
  "tests/pydata/old/6_hestitant_crossings_correct.rda"
)

simlist <- list(
  "tests/pydata/hesitation_GM.rda",
  "tests/pydata/early_yield_GM.rda",
  "tests/pydata/short_stop_GM.rda",
  "tests/pydata/priority_GM.rda",
  "tests/pydata/hestitant_crossings_correct_GM.rda"
)

# data_ped0 <- prepare_empirical_figure(datalist[[1]], is_pedestrian = TRUE)
data_ped0 <- prepare_empirical_figure(datalist[[5]], is_pedestrian = TRUE)
data_ped1 <- prepare_empirical_figure(datalist[[2]], is_pedestrian = TRUE)

sim_ped0 <- prepare_simulation_figure(simlist[[1]], nrow(data_ped0[[3]]), 
                                      is_pedestrian = TRUE)
sim_ped1 <- prepare_simulation_figure(simlist[[2]], nrow(data_ped1[[3]]), 
                                      is_pedestrian = TRUE)

# data_car0 <- prepare_empirical_figure(datalist[[1]], is_pedestrian = FALSE)
data_car0 <- prepare_empirical_figure(datalist[[5]], is_pedestrian = FALSE)
data_car1 <- prepare_empirical_figure(datalist[[2]], is_pedestrian = FALSE)

sim_car0 <- prepare_simulation_figure(simlist[[1]], nrow(data_ped0[[3]]), 
                                      is_pedestrian = FALSE)
sim_car1 <- prepare_simulation_figure(simlist[[2]], nrow(data_ped1[[3]]), 
                                      is_pedestrian = FALSE)


dspeed_ped0    <- data_ped0[[1]]
dacc_ped0      <- data_ped0[[2]]
davgspeed_ped0 <- data_ped0[[3]]
davgacc_ped0   <- data_ped0[[4]]
dspeed_ped1    <- data_ped1[[1]]
dacc_ped1      <- data_ped1[[2]]
davgspeed_ped1 <- data_ped1[[3]]
davgacc_ped1   <- data_ped1[[4]]

dspeed_car0    <- data_car0[[1]]
dacc_car0      <- data_car0[[2]]
davgspeed_car0 <- data_car0[[3]]
davgacc_car0   <- data_car0[[4]]
dspeed_car1    <- data_car1[[1]]
dacc_car1      <- data_car1[[2]]
davgspeed_car1 <- data_car1[[3]]
davgacc_car1   <- data_car1[[4]]

plab0 <- "Pedestrians crossed hesitantly"  
plab1 <- "Pedestrians crossed prematurely"  
vlab0 <- "The vehicle decelerated \nsuddenly"
vlab1 <- "The vehicle asserted \nits priority"

dspeed_ped0$type <- plab0
dspeed_ped1$type <- plab1
dacc_ped0$type   <- plab0  
dacc_ped1$type   <- plab1
davgspeed_ped0$type <- plab0
davgspeed_ped1$type <- plab1
davgacc_ped0$type <- plab0
davgacc_ped1$type <- plab1

dspeed_car0$type <- plab0
dspeed_car1$type <- plab1
dacc_car0$type   <- plab0  
dacc_car1$type   <- plab1
davgspeed_car0$type <- plab0
davgspeed_car1$type <- plab1
davgacc_car0$type <- plab0
davgacc_car1$type <- plab1


dEmSpeed_ped    <- rbind(dspeed_ped0, dspeed_ped1)
dEmAcc_ped      <- rbind(dacc_ped0, dacc_ped1)
dEmAvgSpeed_ped <- rbind(davgspeed_ped0, davgspeed_ped1)
dEmAvgAcc_ped   <- rbind(davgacc_ped0, davgacc_ped1)
dEmSpeed_car    <- rbind(dspeed_car0, dspeed_car1)
dEmAcc_car      <- rbind(dacc_car0, dacc_car1)
dEmAvgSpeed_car <- rbind(davgspeed_car0, davgspeed_car1)
dEmAvgAcc_car   <- rbind(davgacc_car0, davgacc_car1)

davgspeed_pedsim0 <- sim_ped0[[1]]
davgspeed_pedsim1 <- sim_ped1[[1]]
davgacc_pedsim0 <- sim_ped0[[2]]
davgacc_pedsim1 <- sim_ped1[[2]]

davgspeed_carsim0 <- sim_car0[[1]]
davgspeed_carsim1 <- sim_car1[[1]]
davgacc_carsim0 <- sim_car0[[2]]
davgacc_carsim1 <- sim_car1[[2]]

davgspeed_pedsim0$type <- plab0
davgspeed_pedsim1$type <- plab1
davgacc_pedsim0$type <- plab0
davgacc_pedsim1$type <- plab1

davgspeed_carsim0$type <- plab0
davgspeed_carsim1$type <- plab1
davgacc_carsim0$type <- plab0
davgacc_carsim1$type <- plab1

dGMSpeed_ped <- rbind(davgspeed_pedsim0, davgspeed_pedsim1)
dGMSpeed_car <- rbind(davgspeed_carsim0, davgspeed_carsim1)

dGMAcc_ped <- rbind(davgacc_pedsim0, davgacc_pedsim1)
dGMAcc_car <- rbind(davgacc_carsim0, davgacc_carsim1)

pped <- ggplot() +
  geom_line(data = dEmSpeed_ped, 
            aes(x = z, y = y, group = ID),
            colour = pal1[2]) +
  geom_line(data = dEmAvgSpeed_ped, 
            aes(x = z, y = filteredM),
            colour = pal1[3], linewidth = 1.2) +
  geom_line(data = dGMSpeed_ped,
            aes(x = x, y = M), colour = pal1[3],
            linewidth = 1, linetype = "longdash") +
  scale_x_reverse(breaks = c(4, 3, 2, 1, 0)) +
  facet_wrap(.~type) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_y_continuous (
    name = "Speed (m/s)") +
  xlab("") +
  facet_wrap(.~type) +
  themes()


pcar <- ggplot() +
  geom_line(data = dEmSpeed_car, 
            aes(x = z, y = y, group = ID),
            colour = pal1[2]) +
  geom_line(data = dEmAvgSpeed_car, 
            aes(x = z, y = filteredM),
            colour = pal1[3], linewidth = 1.2) +
  geom_line(data = dGMSpeed_car,
            aes(x = x, y = M), colour = pal1[3],
            linewidth = 1, linetype = "longdash") +
  scale_x_reverse() +
  facet_wrap(.~type) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_y_continuous (
    name = "Speed (m/s)") +
  xlab("Distance to encounter (m)") +
  facet_wrap(.~type) +
  theme_minimal(base_size = 14) +
  theme (
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = lab_size),
    axis.text.y = element_text(size = lab_size),
    axis.ticks = element_line(),
    axis.text.y.right = element_text(colour = pal3[3]),
    axis.text.y.left = element_text(colour = pal1[3]),
    axis.title.y.right = element_text(colour = pal3[3]),
    axis.title.y.left = element_text(colour = pal1[3]))

# cairo_pdf("tests/docs/first_submit/figs/ped_speed_profile-3.pdf")
gridExtra::grid.arrange(pped, pcar)
# dev.off()

# Acceleration
pacc_ped <- ggplot() +
  geom_line(data = dEmAcc_ped, 
            aes(x = z, y = y, group = ID),
            colour = pal1[2]) +
  geom_line(data = dEmAvgAcc_ped, 
            aes(x = z, y = filteredM),
            colour = pal1[3], linewidth = 1.2) +
  geom_line(data = dGMAcc_ped,
            aes(x = x, y = M), colour = pal1[3],
            linewidth = 1, linetype = "longdash") +
  scale_x_reverse(breaks = c(4, 3, 2, 1, 0)) +
  facet_wrap(.~type) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_y_continuous (
    limits = c(-.6, .6),
    name = bquote(Acceleration (m/s^2))) +
  xlab("") +
  facet_wrap(.~type) +
  themes()


pacc_car <- ggplot() +
  geom_line(data = dEmAcc_car, 
            aes(x = z, y = y, group = ID),
            colour = pal1[2]) +
  geom_line(data = dEmAvgAcc_car, 
            aes(x = z, y = filteredM),
            colour = pal1[3], linewidth = 1.2) +
  geom_line(data = dGMAcc_car,
            aes(x = x, y = M), colour = pal1[3],
            linewidth = 1, linetype = "longdash") +
  scale_x_reverse() +
  facet_wrap(.~type) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_y_continuous (
    limits = c(-4, 4),
    name = bquote(Acceleration (m/s^2))) +
  xlab("Distance to encounter (m)") +
  facet_wrap(.~type) +
  theme_minimal(base_size = 14) +
  theme (
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = lab_size),
    axis.text.y = element_text(size = lab_size),
    axis.ticks = element_line(),
    axis.text.y.right = element_text(colour = pal3[3]),
    axis.text.y.left = element_text(colour = pal1[3]),
    axis.title.y.right = element_text(colour = pal3[3]),
    axis.title.y.left = element_text(colour = pal1[3]))

# cairo_pdf("tests/docs/first_submit/figs/ped_acc_profile-3.pdf")
gridExtra::grid.arrange(pacc_ped, pacc_car)
dev.off()
