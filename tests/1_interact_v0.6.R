isServer <- FALSE
pkg <- c("data.table", "ggplot2", "lubridate", "Rose", "png")
sapply(pkg, require, character.only = TRUE)
wpath <- ifelse(isServer, "~/文件/viscando/", 
                "/media/yslin/Avocet/Projects/viscando/"); wpath
wk <- ifelse(.Platform$OS.type == "windows", shortPathName("C:/"), wpath)
setwd(wk)
rm(list = ls())

# Set fixed parameters  ------------------------------------------------------
istest <- TRUE
pal <- load_basic_colour_palette()

## Data and software parameters
is_parallel <- FALSE
per <- records()
nday <- nrow(per)

# When the two closest samples in the two agents were fewer than 5-s apart,
# this pair might interact.
threshold_enc <- 5   

# When two agents are fewer than 3 metres apart, they are considered to interact.
enc_dist <- 3

# When pedestrian is 5 metres away from the encounter point, I assumed he / she
# started to watch for the car. 
watch_distance <- 5

# Look forward and backward 3 cloest agents on the output of table function.
rng <- 3 

# I excluded the interaction pair, when their encounter points were the 
# last recorded sample. This is because the constraint of the cameras and RNN 
# algorithm. When they recorded only the last samples, I cannot know what happened. 
exclude_last <- TRUE  

# The three are to control the look of figure and messages 
verbose <- TRUE
textsize1 <- 8
adj <- 2.5

# Project a list of index to enable parallel computing.
idx <- vector("list", length = nday)
for(l in 1:nday) { idx[[l]] <- l }

# i <- 2; j <- 288; k <- 4
# i <- 3; j <- 91; k <- 3
# i <- 3; j <- 419; k <- 6
# i <- 5; j <- 817; k <- 2
# i <- 5; j <- 1131; k <- 5
# i <- 9; j <- 213; k <- 4
i <- 9; j <- 913; k <- 3
run_day <- function(i, x0, enc_dist, threshold_enc, watch_distance, exclude_last) 
{
  if(istest) { 
    ## Veh stopped at the encounter, but the ped kept walking
    # if (i == 11 & j == 394 & k == 3) { next; }  
    i <- 2; j <- 59; k <- 6
    i <- 2; j <- 302; k <- 4
    i <- 2; j <- 589; k <- 6
    i <- 2; j <- 1102; k <- 3
    i <- 2; j <- 1102; k <- 4
    i <- 5; j <- 1131; k <- 5
    i <- 9; j <- 324; k <- 3
    i <- 9; j <- 594; k <- 5
    i <- 11; j <- 147; k <- 4
    i <- 11; j <- 394; k <- 3
    i <- 12; j <- 714; k <- 5
    i <- 12; j <- 911; k <- 4
    i <- 12; j <- 1487; k <- 4
    cat(c(i, j, k), "\n")
  }
  
  start_adj <- ifelse(i <= 7, 2*adj, adj)  ## to guess where to put the TTA label 
  load(paste0("tests/extdata/day", i, "/daday_.rda"))
  site <- ifelse(i <= 7, "belle", "qw")
  msg0 <- ifelse(i <= 7, "Belle Road", "QW")
  
  pngfile <- ifelse(i <= 7, "tests/figs/streets/ground - Belle_Isle.png", "tests/figs/queensway.png")
  img <- png::readPNG(pngfile) 
  
# image height and width
  h <- dim(img)[1] 
  w <- dim(img)[2] 

  interact_tbl <- table(daday$ID, daday$A)
  ped_idx    <- which(interact_tbl[, "Ped"]  > 0) 
  nroad_user <- nrow(interact_tbl)

# counting the number of two agents having crossing path
  ninteract <- 0 
  np_early <- nv_early <- 0
  x1 <- pet <- ipEarly <- NULL
  iTTAp <- iTTAv <- iSp <- iSv <- NULL
  iXp <- iXv <- iYp <- iYv <- NULL
  is_slow_down <- NULL
  zoneid <- is_marked <- NULL
  
  cat(length(ped_idx), "ped(s) in", msg0, "between", 
      as.character(ymd_hm(per[i, 1])), "to", as.character(ymd_hm(per[i, 2])), "\n")

  for (j in seq_len(length(ped_idx))) {
    # 1. Identify the 1st, 2nd, ... instance of non-zero ped count; ie the sensor
    # reported that it saw a pedestrian.
    self <- ped_idx[j]  
    if ( !unique(daday[ID == names(self)]$A) == "Ped" ) stop ("Not a pedestrian?")
    
    # 2. From the ped's perspective, looked forward and backward in time for 
    # rng (3) other road users 
    startrow <- ifelse((self - rng) < 1, 1, self - rng)
    endrow   <- ifelse((self + rng) >= nroad_user, nroad_user, self + rng)
    all <- startrow:endrow;    
    others <- all[all != self] # all other road users 
    
    # Get their Viscando assigned ID
    selID <- dimnames(interact_tbl)[[1]][self]
    othID <- dimnames(interact_tbl)[[1]][others]
    allID <- dimnames(interact_tbl)[[1]][all]

    # Separate their data & drop redundant factor levels associated with the A column
    dped <- daday[ID %in% selID] 
    doth <- daday[ID %in% othID & A != "Bic"] # excluding bicycles
    dped$A  <- droplevels(dped$A)
    doth$A  <- droplevels(doth$A)
    
    # Check all other agents who might interact with the pedestrian
    for (k in seq_len(length(others))) {
      
      dveh <- doth[ID == othID[k]]
      nped <- nrow(dped); 
      nveh <- nrow(dveh); 
      
      # We are not interested in the interaction between two pedestrians,
      is_vehicle <- unique(dveh$A) %in% c("Hgv", "Lgv") 
      if(!is_vehicle || nveh == 0) { 
        next 
      } 
      
      lab1 <- paste0(hour(dped$Time[1]), ":", minute(dped$Time[1]), ":", 
                     round(second(dped$Time[1]), 2))
      lab2 <- paste0(hour(dveh$Time[1]), ":", minute(dveh$Time[1]), ":", 
                     round(second(dveh$Time[1]), 2))
      
      # Criterion 1 checks whether a pedestrian and a vehicle have a 
      # linear crossing path by using their first and last recorded samples.
      is_encounter <- check_encounter(dped, dveh); is_encounter

      if (is_encounter[[1]]) {

        # Excluding the cases that the time stamps suggested no interaction. 
        # 1. Vehicle entered the scene earlier
        # test1 <- min(dveh$Time) < min(dped$Time); test1
        # 2. Vehicle left the scene earlier than the pedestrian entered the scene
        test2 <- max(dveh$Time) < min(dped$Time); test2
        # 3. Pedestrian left the scene earlier than the vehicle entered the scene
        test3 <- max(dped$Time) < min(dveh$Time); test3
        
        if (test3) {if(verbose) message("The P left the scene eariler. Abort."); next }
        if (test2) {if(verbose) message("The V left the scene eariler. Abort."); next }
        
        if (verbose) {
          tmp <- round(is_encounter[[2]], 2)
          message("Predictive crossing point: ", tmp[1], " ", tmp[2])
        }
        
        # Find the recorded encounter point
        # args(find_encounter)
        encounter_point <- find_encounter(dped, dveh) 
        encounter_xy <- encounter_point[[1]]
        encounter_distance <- encounter_point[[2]]
        
        hotzone <- check_hotzone(dveh, encounter_xy, site = site); 
        # hotzone

        dped$d2e <- sqrt( (dped$X - dped[xy[2], ]$X)^2 + (dped$Y - dped[xy[2], ]$Y)^2 )
        dveh$d2e <- sqrt( (dveh$X - dveh[xy[1], ]$X)^2 + (dveh$Y - dveh[xy[1], ]$Y)^2 )
        
        if (nrow(hotzone) != 0)  {
          is_marked_tmp <- unique(hotzone$z)
        } else {
          is_marked_tmp <- F
        }
        
        # Criterion 2: The encounter distance must be less than 3 metres.
        if (encounter_distance > enc_dist) {
          if (verbose) message("P and V had never come close. Abort.");
          next;  
        }
        
        # Exclusion 1: (limitation of the cameras)
        # If the first data point saw by the sensor is the encounter, we are
        # unsure whether they interacted. 
        if (any(xy == 1)) { 
          if (verbose) message("The first sample is the encounter. Abort.");
          next;  
        }
        
        # Exclusion 2: (Possibly taxi picked up a customer)
        # If the last or the one before the last recorded sample was the 
        # encounter, we are unsure whether this is an interaction or a taxi 
        # picked up someone (3-1162-6). (10-89-4)
        # xy[1] is the dveh; xy[2] is the dped
        if ( nveh <= (xy[1] + 1) | nped <= (xy[2] + 1) ) { next }

        # Exclusion 3: If the encounter points were the last recorded sample. 
        if ( isTRUE( all.equal(dveh[nveh, ]$d2e, 0) ) & exclude_last) { next }
        if ( isTRUE( all.equal(dped[nped, ]$d2e, 0) ) & exclude_last) { next }

        # Before the encounter, Veh has one 0 tta and Inf
        # if (i == 4 & j == 520 & k == 5) { next; }
        # The veh was seen by the cameras near the encounter at the beginning 
        # and it stayed there for 9 points
        # if (i == 5 & j == 817 & k == 2) { next; }
      
        ############### 10
        ## QW
        ############### 10
        # P and V come close and became parallel
        # if (i == 9 & j == 213 & k == 4) { next; }
        # Another taxi picked up a passenger, but not been screened out
        # if (i == 9 & j == 376 & k == 3) { next; }
        # A car parked on the road side and someone happened to walk on the side walk.
        # if (i == 12 & j == 714 & k == 5) { next; }
        # The ped stood and seemingly waited for the veh. They then went separate ways
        # if (i == 13 & j == 126 & k == 3) { next; }
      
        ptta <- get_tta(xy, dped, agent = "pedestrian")
        vtta <- get_tta(xy, dveh, agent = "vehicle")
        dped$otta <- c(ptta, rep(NA, nped - xy[2] ))
        dveh$otta <- c(vtta, rep(NA, nveh - xy[1] ))
        dped$tta <- round(c(ptta, rep(NA, nped - xy[2] )), 1)
        dveh$tta <- round(c(vtta, rep(NA, nveh - xy[1] )), 1)
        
        dped$abstime  <- as.numeric( dped$Time - min(dped$Time, dveh$Time) )
        dveh$abstime  <- as.numeric( dveh$Time - min(dped$Time, dveh$Time) )
        dveh$abstime_prn <- round(dveh$abstime, 1)
        dped$abstime_prn <- round(dped$abstime, 1)
        
        if ( dped[xy[2], ]$abstime < dveh[1, ]$abstime ) {
          if (verbose) message("Ped arrived at the encounter earlier than the sensor first saw the vehicle");
          next;  
        }
        
        if ( dveh[xy[1], ]$abstime < dped[1, ]$abstime ) {
          if (verbose) message("Veh arrived at the encounter earlier than the sensor first saw the ped");
          next;  
        }
        
        if (any(is.infinite(vtta))) {
          test4 <- dped[which(dped$otta == 0), ]$Time - dveh[which.min(otta), ]$Time
          if(test4 < 0 & verbose) {message("The V stopped to yield") }
        } else if ( any(is.infinite(ptta)) ) {
          test4 <- dped[which.min(ptta), ]$Time - dveh[which(dveh$otta == 0), ]$Time
          if (test4 > 0 & verbose) {message("The P stopped to yield") }
        } else {
          test4 <- dped[which(dped$otta == 0), ]$Time - dveh[which(dveh$otta == 0), ]$Time
        }
        
        msg <- ifelse(test4 <= 0, "Ped", "Veh") ## who arrived the encounter earlier?

        if ( abs(test4) < threshold_enc ) {
          
          msglab <- paste0(msg, " passed the encounter ", round(abs(test4), 2), " s earlier")
          message(msglab) 
          dtext <- data.table(x = c(dped[1, ]$Xmpp + 10*start_adj, 
                                  dveh[1, ]$Xmpp + 10*start_adj),
                              y = c(dped[1, ]$Ympp + start_adj, 
                                  dveh[1, ]$Ympp  + start_adj),
                              A = c(unique(dped$A), unique(dveh$A)),
                              AID = c(unique(dped$AID), unique(dveh$AID)),
                              lab = c(lab1, lab2))

          # 1. Find the index where the pedestrian is 5 metres away
          # from the encounter point. 
          # varying watch_distances?
          idxp_crit <- which.max( dped[1:xy[2]]$d2e <= watch_distance)
          
          ## 2. What time was it at this index? dped[idxp_crit,]$Time           
          ## 3. Prior this time point, how was the car's trajectory? 
          idxv_crit <- which(dveh$Time < dped[idxp_crit,]$Time)
          idxp_tta <- idxp_crit
          idxv_tta <- length(idxv_crit) 

          if (length(idxv_crit) == 0) {
            iTTAp <- c(iTTAp, dped[idxp_crit, ]$otta)

            cat(c(i, j, k), "\n")
            idxv_crit <- idxv_tta <- 1
            cat("The earliest Veh time is", dveh[1,]$abstime_prn, "s\n")
            cat("The Ped critical time is", dped[idxp_crit,]$abstime_prn, "s\n")
            message("None of the vehicle times was earlier than the ped's critical time\n")
            message("Use Veh 1st sample as the veh TTA")
            iTTAv <- c(iTTAv, dveh[idxv_crit, ]$otta)

          } else {
            iTTAv <- c(iTTAv, dveh[idxv_tta, ]$otta)
            iTTAp <- c(iTTAp, dped[idxp_tta,]$otta)
          }
          

          selected_veh3 <- seq(1, length(idxv_crit), by = 5)  
          selected_ped3 <- seq(1, idxp_crit, by = 7)
          
          if ( selected_veh3[length(selected_veh3)] < length(idxv_crit) ) {
            selected_veh3 <- c(selected_veh3, length(idxv_crit))
          }
          if ( selected_ped3[length(selected_ped3)] < idxp_crit) {
            selected_ped3 <- c(selected_ped3, idxp_crit)
          }
          
          # Was the car staying around one spot?
          if (length(idxv_crit) == 1) {
            adj_scale <- c(5, 15, 10, 10, 12, 12)  
          } else if ( mean( abs(diff(dveh[idxv_crit, ]$d2e)) ) < 1 ) {
            adj_scale <- 2*c(5, 15, 10, 10, 12, 12)  
          } else {
            adj_scale <- c(5, 15, 10, 10, 12, 12)
          }
          
          dvtext <- dveh[selected_veh3, ]
          dptext <- dped[selected_ped3, ]
          dptext$Y2 <- dptext$Ympp + adj_scale[1]*adj
          dptext$X2 <- dptext$Xmpp + adj_scale[2]*adj
          dvtext$X2 <- dvtext$Xmpp + adj_scale[3]*adj 
          dvtext$Y2 <- dvtext$Ympp + adj_scale[4]*adj #+ c(30, 15)

          if (msg == "Ped") {
            ipEarly <- c(ipEarly, 1)
            np_early <- np_early + 1 
          } else if (msg == "Veh") {
            ipEarly <- c(ipEarly, 0)
            nv_early <- nv_early + 1 
          } else {
            stop("An unknown result")
          }


          if ( nrow(hotzone) == 0) {
            zoneid <- c(zoneid, 99)
          } else {
            zoneid <- c(zoneid, unique(hotzone$id))
          }
          

          pet <- c(pet, test4)
          iSv <- c(iSv, dveh[idxv_tta, ]$Speed)
          iXv <- c(iXv, dveh[idxv_tta, ]$X)
          iYv <- c(iYv, dveh[idxv_tta, ]$Y)

          iSp <- c(iSp, dped[idxp_tta, ]$Speed)
          iXp <- c(iXp, dped[idxp_tta, ]$X)
          iYp <- c(iYp, dped[idxp_tta, ]$Y)
          
          is_marked <- c(is_marked, is_marked_tmp) 
          
          ninteract <- ninteract + 1
          x1 <- rbind(x1, c(i, j, k))
          colnames(x1) <- c("day", "ped", "veh")

          p0 <- ggplot() +
            annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"),
                                               height=unit(1,"npc")), 0, w, 0, -h) +
            scale_x_continuous(expand = c(0, 0), limits = c(0, w)) +
            scale_y_reverse(expand=c(0, 0), limits=c(h, 0)) +
            geom_point(data = dped, aes(x = Xmpp, y = Ympp, colour = A), shape=16,
                       size = 6, alpha = .4) +
            geom_point(data = dveh, aes(x = Xmpp, y = Ympp, colour = A),
                       shape = 16, size = 6, alpha = .4) +
            geom_point(data=dveh[xy[1], ], aes(x = Xmpp, y = Ympp, col = A),
                       size = 10, shape = 19) +
            geom_point(data=dped[xy[2], ], aes(x = Xmpp, y = Ympp, col = A),
                       size = 10, shape = 19) +
            geom_point(data=dveh[1], aes(x = Xmpp, y = Ympp, col = A),
                       size = 12, shape = 15) +
            geom_point(data=dped[1, ], aes(x = Xmpp, y = Ympp, col = A),
                       size = 12, shape = 15) +
            geom_text(data = dptext, aes(x=Xmpp, y=Ympp, label = tta),
                      size = textsize1, fontface = "bold") +
            geom_text(data = dvtext, aes(x=Xmpp, y=Ympp, label = tta),
                      size = textsize1, fontface = "bold") +
            geom_text(data = dtext, aes(x = x, y = y, label = lab),
                       size = textsize1, fontface = "bold", colour = "white") +
            geom_text(data = dtext, aes(x = x, y = y, label = lab),
                       size = textsize1, fontface = "bold", colour = "white") +
            geom_polygon(data = hotzone, aes(x=x, y=y, group = id),
                          alpha = .2, colour = "white", fill = "lightblue") +
            ggtitle(label = msglab) +

            coord_equal()  +
            scale_colour_manual(values = pal[c(3,7)]) +
            theme_bw(base_size = 24) +
            theme(legend.title = element_blank(),
                  legend.position = c(.9,.1),
                  legend.text = element_text(size = 30),
                  axis.title = element_blank())
           
          fn <- paste0("tests/interact_figs/day", i, "/p",j, "-v", k, ".png")
          png(filename = fn, 1600, 1600)
          print(p0)
          dev.off()

          fndat <- paste0("tests/interact_data/day", i, "/p",j, "-v",k, ".rda")
          save(dped, dveh, file = fndat)

        }   # threshold_enc
      }     # is_encounter 
    }       # k other vehicle
  }         # j pedestrian    

  fnrda <- paste0("tests/extdata/day", i, "/pd-tmp.rda")
  save(x1, pet, iTTAp, iTTAv, np_early, nv_early, 
       ninteract, iSv, iSp, iXv, iYv, iXp, iYp, ipEarly,
       zoneid, file = fnrda)
  
}


# Run the trajectory extraction ------------------------------------------------
if (is_parallel) {
  ncore <- nday
  res <- parallel::mclapply(idx, run_day, x0, enc_dist, 
                            threshold_enc, watch_distance, exclude_last, 
                            mc.cores = getOption("mc.cores", ncore))

} else {
  res <- lapply(idx, run_day, x0, enc_dist, threshold_enc, watch_distance,
                exclude_last)
}

