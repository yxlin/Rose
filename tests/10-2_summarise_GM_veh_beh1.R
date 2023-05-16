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
  # data_ped2 <- prepare_empirical_figure(datalist[[3]], is_pedestrian = TRUE)
  # is_pedestriasn  <- TRUE
  # fn <- datalist[[3]]
  load(fn)
  
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
  # sim_ped2 <- prepare_simulation_figure(simlist[[3]], nrow(data_ped2[[3]]), 
  #                                       is_pedestrian = TRUE)

  # sim_car2 <- prepare_simulation_figure(simlist[[3]], nrow(data_car2[[3]]), 
  #                                       is_pedestrian = FALSE)
  # fn <- simlist[[3]]
  # data_length <- nrow(data_car2[[3]])
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
  "tests/pydata/old/6_empirical_hesitation_tmp.rda",
  "tests/pydata/old/7_empirical_early_yield_tmp.rda",
  "tests/pydata/old/4_empirical_short_stop_tmp.rda",
  "tests/pydata/old/5_empirical_priority_assertion_tmp.rda",
  "tests/pydata/old/4-2_empirical_short_stop_correct_tmp.rda"
)

simlist <- list(
  "tests/pydata/hesitation_GM.rda",
  "tests/pydata/early_yield_GM.rda",
  "tests/pydata/short_stop_GM.rda",
  "tests/pydata/priority_GM.rda",
  "tests/pydata/short_stop_correct_GM.rda"
)


data_ped2 <- prepare_empirical_figure(datalist[[3]], is_pedestrian = TRUE)
data_car2 <- prepare_empirical_figure(datalist[[3]], is_pedestrian = FALSE)

sim_ped2 <- prepare_simulation_figure(simlist[[3]], nrow(data_ped2[[3]]), 
                                      is_pedestrian = TRUE)
sim_car2 <- prepare_simulation_figure(simlist[[3]], nrow(data_car2[[3]]), 
                                      is_pedestrian = FALSE)

data_ped3 <- prepare_empirical_figure(datalist[[4]], is_pedestrian = TRUE)
data_car3 <- prepare_empirical_figure(datalist[[4]], is_pedestrian = FALSE)

sim_ped3 <- prepare_simulation_figure(simlist[[4]], nrow(data_ped3[[3]]), 
                                      is_pedestrian = TRUE)
sim_car3 <- prepare_simulation_figure(simlist[[4]], nrow(data_car3[[3]]), 
                                      is_pedestrian = FALSE)


data_ped4 <- prepare_empirical_figure(datalist[[5]], is_pedestrian = TRUE)
data_car4 <- prepare_empirical_figure(datalist[[5]], is_pedestrian = FALSE)

sim_ped4 <- prepare_simulation_figure(simlist[[5]], nrow(data_ped4[[3]]), 
                                      is_pedestrian = TRUE)
sim_car4 <- prepare_simulation_figure(simlist[[5]], nrow(data_car4[[3]]), 
                                      is_pedestrian = FALSE)


dspeed_ped2    <- data_ped2[[1]]
dacc_ped2      <- data_ped2[[2]]
davgspeed_ped2 <- data_ped2[[3]]
davgacc_ped2   <- data_ped2[[4]]

dspeed_ped3    <- data_ped3[[1]]
dacc_ped3      <- data_ped3[[2]]
davgspeed_ped3 <- data_ped3[[3]]
davgacc_ped3   <- data_ped3[[4]]

dspeed_ped4    <- data_ped4[[1]]
dacc_ped4      <- data_ped4[[2]]
davgspeed_ped4 <- data_ped4[[3]]
davgacc_ped4   <- data_ped4[[4]]


dspeed_car2    <- data_car2[[1]]
dacc_car2      <- data_car2[[2]]
davgspeed_car2 <- data_car2[[3]]
davgacc_car2   <- data_car2[[4]]

dspeed_car3    <- data_car3[[1]]
dacc_car3      <- data_car3[[2]]
davgspeed_car3 <- data_car3[[3]]
davgacc_car3   <- data_car3[[4]]

dspeed_car4    <- data_car4[[1]]
dacc_car4      <- data_car4[[2]]
davgspeed_car4 <- data_car4[[3]]
davgacc_car4   <- data_car4[[4]]

vlab0 <- "The vehicle decelerated \nsuddenly"
vlab1 <- "The vehicle asserted \nits priority"
vlab2 <- "The vehicle stopped short"

dspeed_ped2$type <- vlab0
dspeed_ped3$type <- vlab1
dspeed_ped4$type <- vlab2

dacc_ped2$type   <- vlab0  
dacc_ped3$type   <- vlab1
dacc_ped4$type   <- vlab2

davgspeed_ped2$type <- vlab0
davgspeed_ped3$type <- vlab1
davgspeed_ped4$type <- vlab2

davgacc_ped2$type <- vlab0
davgacc_ped3$type <- vlab1
davgacc_ped4$type <- vlab2

dspeed_car2$type <- vlab0
dspeed_car3$type <- vlab1
dspeed_car4$type <- vlab2

dacc_car2$type   <- vlab0  
dacc_car3$type   <- vlab1
dacc_car4$type   <- vlab2

davgspeed_car2$type <- vlab0
davgspeed_car3$type <- vlab1
davgspeed_car4$type <- vlab2

davgacc_car2$type <- vlab0
davgacc_car3$type <- vlab1
davgacc_car4$type <- vlab2

dEmSpeed_ped    <- rbind(dspeed_ped2, dspeed_ped3, dspeed_ped4)
dEmAcc_ped      <- rbind(dacc_ped2, dacc_ped3, dacc_ped4)
dEmAvgSpeed_ped <- rbind(davgspeed_ped2, davgspeed_ped3, davgspeed_ped4)
dEmAvgAcc_ped   <- rbind(davgacc_ped2, davgacc_ped3, davgacc_ped4)


dEmSpeed_car    <- rbind(dspeed_car2, dspeed_car3, dspeed_car4)
dEmAcc_car      <- rbind(dacc_car2, dacc_car3, dacc_car4)
dEmAvgSpeed_car <- rbind(davgspeed_car2, davgspeed_car3, davgspeed_car4)
dEmAvgAcc_car   <- rbind(davgacc_car2, davgacc_car3, davgacc_car4)

davgspeed_pedsim2 <- sim_ped2[[1]]
davgspeed_pedsim3 <- sim_ped3[[1]]
davgspeed_pedsim4 <- sim_ped4[[1]]

davgacc_pedsim2 <- sim_ped2[[2]]
davgacc_pedsim3 <- sim_ped3[[2]]
davgacc_pedsim4 <- sim_ped4[[2]]

davgspeed_carsim2 <- sim_car2[[1]]
davgspeed_carsim3 <- sim_car3[[1]]
davgspeed_carsim4 <- sim_car4[[1]]

davgacc_carsim2 <- sim_car2[[2]]
davgacc_carsim3 <- sim_car3[[2]]
davgacc_carsim4 <- sim_car4[[2]]

davgspeed_pedsim2$type <- vlab0
davgspeed_pedsim3$type <- vlab1
davgspeed_pedsim4$type <- vlab2

davgacc_pedsim2$type <- vlab0
davgacc_pedsim3$type <- vlab1
davgacc_pedsim4$type <- vlab2

davgspeed_carsim2$type <- vlab0
davgspeed_carsim3$type <- vlab1
davgspeed_carsim4$type <- vlab2

davgacc_carsim2$type <- vlab0
davgacc_carsim3$type <- vlab1
davgacc_carsim4$type <- vlab2

dGMSpeed_ped <- rbind(davgspeed_pedsim2, davgspeed_pedsim3, davgspeed_pedsim4)
dGMSpeed_car <- rbind(davgspeed_carsim2, davgspeed_carsim3, davgspeed_carsim4)
dGMAcc_ped <- rbind(davgacc_pedsim2, davgacc_pedsim3, davgacc_pedsim4)
dGMAcc_car <- rbind(davgacc_carsim2, davgacc_carsim3, davgacc_carsim4)


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
  # geom_hline(yintercept = 0, linetype = "dotted") +
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


cairo_pdf("tests/docs/first_submit/figs/car_speed_profile-4.pdf")
gridExtra::grid.arrange(pped, pcar)
dev.off()



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
    limits = c(-.45, .45),
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


cairo_pdf("tests/docs/first_submit/figs/car_acc_profile-4.pdf")
gridExtra::grid.arrange(pacc_ped, pacc_car)
dev.off()



