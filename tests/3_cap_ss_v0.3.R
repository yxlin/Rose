## Description: Capture plausible short stopping observations
pkg <- c("data.table", "ggplot2", "Rose", "png")
sapply(pkg, require, character.only = TRUE)
unix_path <- "/media/yslin/Avocet/Projects/viscando/"
win_path <- "C:/Users/yslin/OneDrive - University of Leeds/Projects/viscando/"
wk <- ifelse(.Platform$OS.type == "windows", 
  shortPathName(win_path), unix_path)
setwd(wk)
rm(list = ls())

# Section 1 ----------
# Define the threshold of speed change. It has to be less than -3.5 m/s^2
threshold <- -3.5

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
for(i in seq_len(nday)) {

  site <- ifelse(i <= 7, "belle", "qw")
  load(paste0("tests/extdata/day", i, "/pd.rda"))
  d <- data.table(cbind(x1, pet, zoneid))
  if ( any(d$zoneid == 99) ) {
    idx <- which(d$zoneid == 99)
    cat("Day", i, ": \n")
    cat("pedestrian ", d[idx, ]$ped, "\nvehicle ", 
    d[idx, ]$veh, "has no zone id. \n")
  }
  cat("Day", i, "interaction: ", nrow(d), " \n")

  if (nrow(d) > 0) {
    day_with_interaction <- c(day_with_interaction, i)
  }
}

# I will test day 2 first to see whether the code works
# Starting from day 2 cuz day 1 has no interaction fulfilling the criteria.
i <- 2

for(i in day_with_interaction) {
  site <- ifelse(i <= 7, "belle", "qw")
  load(paste0("tests/extdata/day", i, "/pd.rda"))

## Fix the zone id for day 2, 3, 5, cuz I draw a narrow zone for them
  if (i == 2 || i == 3 || i == 5) {
    idx <- which( zoneid == 99 )
    zoneid[idx] <- 2
  } 

  d <- data.table(cbind(x1, pet, zoneid))
  # Zone 1 and 2 are unmarked crossings. I will skip them. 
  # Zone 3 and 4 have zebra crossings and stop lines
  izone <- 3
  l <- 1

  for(izone in 3:4) {
  
  dzonei <- d[zoneid == izone]
  nint <- nrow(dzonei)

    for(l in 1:nint) {
      j <- dzonei[l, ]$ped
      k <- dzonei[l, ]$veh
      fndat <- paste0("tests/interact_data/day", i, "/p", j, "-v",k, ".rda")
      load(fndat) 
      
      # dveh$Speed_ms <- dveh$Speed * (1000 / 3600)
      # Re-calculate time_change and speed_change
      dveh$time_change <- c(NA, diff(dveh$Time))
      dveh$speed_change <- c(NA, diff(dveh$Speed_ms))

      # Calculate the acceleration. Note that when a time sample jumps it will be 
      # incorrect
      dveh$acc <- dveh$speed_change / dveh$time_change

      # idx_of_time_jump <- which( round(dveh$time_change, 3) != 0.16 )
      # dveh$acc[idx_of_time_jump]

      # To make the x axis plots the approaching to the stop line, I will reverse axis.
      dveh$xx <- 1:nrow(dveh)  

      # plot(dveh$xx, dveh$acc)
      # points(dveh$xx[idx_of_time_jump], dveh$acc[idx_of_time_jump], col = "red")

      nped <- nrow(dped)
      nveh <- nrow(dveh)

      tmp_xy <- find_encounter(dped, dveh) # Find the recorded encounter
      xy <- tmp_xy[[1]]
      encounter_distance <- tmp_xy[[2]]
      hotzone <- check_hotzone(dveh, xy, site = site); hotzone

      if( nrow(hotzone) == 0 ) {
        next
        cat( c(i, j, k) )
      }
      
      zoneline <- load_stopping_lines(hotzone)

      xdist2 <- (p2metre(dveh$Xmpp) - p2metre(zoneline[3,]$x))^2
      ydist2 <- (p2metre(dveh$Ympp, FALSE, site = site) - 
                 p2metre(zoneline[3,]$y, FALSE, site = site))^2
      dveh$d2marking <- sqrt(xdist2 + ydist2) 
      
      idx_min <- which.min(dveh$d2marking)
      dveh$d2m <- round(dveh$d2marking, 2)  
      
      ## Extract the data less than 10 metre away from the stop line
      dveh_before <- dveh[1:idx_min, ]
      
      ## Find a speed change that can halt the car (an average length of car 4.2 m)
      test_set <- dveh_before[d2m < d2m_thres]$speed_change
      
      dveh1 <- dveh_before[2:nrow(dveh_before)]

      ## Set up many parameters to make an insightful plot
      idx_ss <- which.min(dveh1$acc)
      idx <- sort(c(seq(1, nrow(dveh1), jump), idx_ss))
      using_idx <- !(c(diff(idx), NA) %in% c(1))
      idx <- idx[using_idx]
      
      zonename <- paste0("Zone ", unique(hotzone$id))

      ## We can only examine the data we have, so any sample told me that 
      ## any speed change has a magnitude over threshold over 3.5 (less than negative 3.5)? 
      if (any( test_set < threshold, na.rm = T)) {
        message("A short stop: ", appendLF = F)
        cat(c(i, j, k), "\n")  
        
        xint <- dveh1[d2m < d2m_thres, ]$xx[1] 
        
        ptmp <- ggplot(data = dveh1) + 
          scale_x_continuous(labels = dveh1$d2m[idx], breaks = dveh1$xx[idx], 
                             name = "Distance to Give-way Line (m)") +
          scale_y_continuous(
            name = bquote(Acceleration(m/s^2)),  
            sec.axis = sec_axis(trans = ~., name = "Speed (m/s)")) 

        p2 <- ptmp + geom_point(aes(x = xx, y = acc), colour = pal[3], size = 5) +
          geom_line(aes(x = xx, y = acc, group = ID)) +
          geom_point(aes(x = xx, y = Speed_ms), colour = pal[2], size = 5) +
          geom_line(aes(x = xx, y = Speed_ms, group = ID)) +
          geom_hline(yintercept = 0, linetype = "dotted") +
          geom_vline(xintercept = xint, linetype = "dotted") +
          ggtitle(zonename) +
          theme_classic(base_size = 24) +
          theme(axis.text.x = element_text(angle = 30), 
                axis.text.y = element_text(), 
                axis.title.y.right = element_text(colour = pal[2]),
                axis.title.y.left = element_text(colour = pal[3]))

        # fn <- paste0("tests/tmp/d", i, "-p",j, "-v", k, "tmp.png")
        # png(filename = fn, 800, 800)
        print(p2)
        dev.off()
      }
      
      
    }    
  }
}

