## Description: Plot the seven crossing zones
rm(list = ls())
pkg <- c("data.table", "ggplot2", "lubridate", "Rose", "png",
         "ggimage")
sapply(pkg, require, character.only = TRUE)

core_path <- "[01]Projects/viscando/"
windows_path <- paste0("F:/", core_path)
unix_path <- paste0("/media/yslin/Avocet/", core_path)
wk <- ifelse(.Platform$OS.type == "windows", shortPathName(windows_path), 
             unix_path)
setwd(wk)
pal <- c("#e41a1c", "#377eb8", "#4daf4a")

# Section 1 --------
# Load the two street images of Belle Isle Road and Queensway 

pngfile0 <- "tests/figs/streets/ground - Belle_Isle.png"
pngfile1 <- "tests/figs/streets/Queensway.png"
img0 <- png::readPNG(pngfile0) 
img1 <- png::readPNG(pngfile1) 

# Image sizes
h0 <- dim(img0)[1] 
w0 <- dim(img0)[2] 
h1 <- dim(img1)[1] 
w1 <- dim(img1)[2] 

# 1. Define the crossing zones in the pixel space. These number matches only
#    the png file, "ground - Belle_Isle.png" and "queensway.png"
#
# 2. Use the alternative one, if the png file, "belle.png" is used.
hotzone <- data.table( load_hotzone() )

# Calculate the centre of each zone to plot the zone number
hottext <- hotzone[, .(Mx = mean(x), 
                       My = mean(y)), .(id)]
hottext[3, ]$Mx <- hottext[3, ]$Mx + 50
hottext[3, ]$My <- hottext[3, ]$My - 50

hottext[5, ]$Mx <- hottext[5, ]$Mx - 50
hottext[5, ]$My <- hottext[5, ]$My - 50

# Separate the zones by site
dhotzone1 <- hotzone[id == 1]
dhotzone2 <- hotzone[id == 2]
dhotzone3 <- hotzone[id == 3]
dhotzone4 <- hotzone[id == 4]
dhotzone5 <- hotzone[id == 5]
dhotzone6 <- hotzone[id == 6]
dhotzone7 <- hotzone[id == 7]

zone_font_size <- 18
textsize1 <- 8

# Load one interaction example
rng <- 3  
i <- 4; j <- 119 ; k <- 4
# inteact_eg <- get_interaction_data(i, j, k, rng)
# dped <- inteact_eg[[1]]
# dveh <- inteact_eg[[2]]

load(paste0("tests/extdata/day", i, "/daday_.rda"))
interact_tbl <- table(daday$ID, daday$A)
ped_idx <- which(interact_tbl[, "Ped"] > 0)
nroad_user <- nrow(interact_tbl)
per <- records()
nday <- nrow(per)

self <- ped_idx[j]
startrow <- ifelse((self - rng) < 1, 1, self - rng)
endrow   <- ifelse((self + rng) >= nroad_user, nroad_user, self + rng)
all <- startrow:endrow;
others <- all[all != self] # all other road users

selID <- dimnames(interact_tbl)[[1]][self]
othID <- dimnames(interact_tbl)[[1]][others]
allID <- dimnames(interact_tbl)[[1]][all]


dped <- daday[ID %in% selID]
doth <- daday[ID %in% othID & A != "Bic"] # excluding bicycles
dped$A <- droplevels(dped$A)
doth$A <- droplevels(doth$A)

dveh <- doth[ID == othID[k]]


encounter_point <- find_encounter(dped, dveh)
encounter_xy <- encounter_point[[1]]
encounter_distance <- encounter_point[[2]]

nped <- nrow(dped)
nveh <- nrow(dveh)
ptta <- get_tta(encounter_xy, dped, agent = "pedestrian")
vtta <- get_tta(encounter_xy, dveh, agent = "vehicle")
dped$otta <- c(ptta, rep(NA, nped - encounter_xy[2]))
dveh$otta <- c(vtta, rep(NA, nveh - encounter_xy[1]))

# The followings are to be used for plotting.
watch_distance <- 4
dped$tta <- round(c(ptta, rep(NA, nped - encounter_xy[2])), 1)
dveh$tta <- round(c(vtta, rep(NA, nveh - encounter_xy[1])), 1)

dped$d2e <- sqrt((dped$X - dped[encounter_xy[2], ]$X)^2 + 
                   (dped$Y - dped[encounter_xy[2], ]$Y)^2)
dveh$d2e <- sqrt((dveh$X - dveh[encounter_xy[1], ]$X)^2 + 
                   (dveh$Y - dveh[encounter_xy[1], ]$Y)^2)


idxp_crit <- which.max(dped[1:encounter_xy[2]]$d2e <= watch_distance)
idxv_crit <- which.max(dveh[1:encounter_xy[1]]$d2e <= watch_distance)

selected_veh1 <- seq(1, idxv_crit, by = 5)
selected_ped1 <- seq(1, idxp_crit, by = 12)
dvtext <- dveh[selected_veh1, ]
dptext <- dped[selected_ped1, ]

selected_ped2 <- seq(1, nped, by = 10)
selected_veh2 <- seq(1, nveh, by = 3)


dped$image <- 'assets/walker2.png'
dveh$image <- 'assets/car1.png'


# Plot Belle Isle Road
p0 <- ggplot() +
  annotation_custom(grid::rasterGrob(img0,
                                     width  = unit(1, "npc"),
                                     height = unit(1, "npc")
  ), 0, w0, 0, -h0) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, w0)) +
  scale_y_reverse(expand = c(0, 0), limits = c(h0, 0)) +
  geom_polygon(data = dhotzone1, aes(x=x,y=y, fill = id, group = id),
               alpha = .2, colour = "white") +
  geom_polygon(data = dhotzone2, aes(x=x,y=y, fill = id, group = id),
               alpha = .2, colour = "white") +
  geom_polygon(data = dhotzone3, aes(x=x,y=y, fill = id, group = id),
               alpha = .2, colour = "white") +
  geom_polygon(data = dhotzone4, aes(x=x,y=y, fill = id, group = id),
               alpha = .2, colour = "white") +
  geom_text(data = hottext[id <= 4],
            aes(x = Mx, y = My, label = id), size = zone_font_size, 
            colour = "black") +
  geom_image(data = dped[selected_ped2, ], aes(x = Xmpp, y = Ympp, image = image),
             size = .018, color = pal[1]) + 
  geom_point(
    data = dped[1, ], aes(x = Xmpp, y = Ympp),
    shape = 2, size = 4, color = pal[1]
  ) +
  geom_point(
    data = dped[nrow(dped), ], aes(x = Xmpp, y = Ympp),
    shape = 0, size = 4, color = pal[1]
  ) +
  geom_point(
    data = dped[encounter_xy[2], ], aes(x = Xmpp, y = Ympp),
    size = 4, shape = 4, color = pal[1]
  ) +
  geom_text(
    data = dptext, aes(x = Xmpp, y = Ympp, label = tta),
    size = textsize1, fontface = "bold", color ="white",
  ) +
  
  
  geom_image(data = dveh[selected_veh2, ], 
             aes(x = Xmpp, y = Ympp, image = image),
             size = .018,
             col = pal[2]) +
  geom_point(
    data = dveh[1, ], aes(x = Xmpp, y = Ympp),
    shape = 2, size = 4, color = pal[2]
  ) +
  geom_point(
    data = dveh[nrow(dveh), ], aes(x = Xmpp, y = Ympp),
    shape = 0, size = 4, color = pal[2]
  ) +
  geom_point(
    data = dveh[encounter_xy[1], ], aes(x = Xmpp, y = Ympp),
    size = 4, shape = 4, color = pal[2]
  ) +
  geom_text(
    data = dvtext, aes(x = Xmpp, y = Ympp, label = tta),
    size = textsize1, fontface = "bold", color ="white",
  ) +

  coord_equal() +
  theme_classic(base_size = 30) +
  theme(legend.position ="none",
        axis.title = element_blank(),
        axis.text  = element_blank(), 
        axis.ticks = element_blank())

# This does not work in radian
fn <- paste0("tests/docs/first_submit/figs/belle_zone2_v4.png")
png(filename = fn, 1600, 1600)
print(p0)
dev.off()    


# Plot Queensway ----
# Load one interaction example
rng <- 3  
i <- 9; j <- 594 ; k <- 5
load(paste0("tests/extdata/day", i, "/daday_.rda"))
interact_tbl <- table(daday$ID, daday$A)
ped_idx <- which(interact_tbl[, "Ped"] > 0)
nroad_user <- nrow(interact_tbl)
per <- records()
nday <- nrow(per)

self <- ped_idx[j]
startrow <- ifelse((self - rng) < 1, 1, self - rng)
endrow   <- ifelse((self + rng) >= nroad_user, nroad_user, self + rng)
all <- startrow:endrow;
others <- all[all != self] # all other road users

selID <- dimnames(interact_tbl)[[1]][self]
othID <- dimnames(interact_tbl)[[1]][others]
allID <- dimnames(interact_tbl)[[1]][all]


dped <- daday[ID %in% selID]
doth <- daday[ID %in% othID & A != "Bic"] # excluding bicycles
dped$A <- droplevels(dped$A)
doth$A <- droplevels(doth$A)

dveh <- doth[ID == othID[k]]


encounter_point <- find_encounter(dped, dveh)
encounter_xy <- encounter_point[[1]]
encounter_distance <- encounter_point[[2]]

nped <- nrow(dped)
nveh <- nrow(dveh)
ptta <- get_tta(encounter_xy, dped, agent = "pedestrian")
vtta <- get_tta(encounter_xy, dveh, agent = "vehicle")
dped$otta <- c(ptta, rep(NA, nped - encounter_xy[2]))
dveh$otta <- c(vtta, rep(NA, nveh - encounter_xy[1]))

# The followings are to be used for plotting.
watch_distance <- 4
dped$tta <- round(c(ptta, rep(NA, nped - encounter_xy[2])), 1)
dveh$tta <- round(c(vtta, rep(NA, nveh - encounter_xy[1])), 1)

dped$d2e <- sqrt((dped$X - dped[encounter_xy[2], ]$X)^2 + 
                   (dped$Y - dped[encounter_xy[2], ]$Y)^2)
dveh$d2e <- sqrt((dveh$X - dveh[encounter_xy[1], ]$X)^2 + 
                   (dveh$Y - dveh[encounter_xy[1], ]$Y)^2)


idxp_crit <- which.max(dped[1:encounter_xy[2]]$d2e <= watch_distance)
idxv_crit <- which.max(dveh[1:encounter_xy[1]]$d2e <= watch_distance)

selected_veh1 <- seq(1, idxv_crit, by = 5)
selected_ped1 <- seq(1, idxp_crit, by = 12)

selected_veh2 <- seq(1, nveh, by = 5)
selected_ped2 <- seq(1, nped, by = 12)

dvtext <- dveh[selected_veh1, ]
dptext <- dped[selected_ped1, ]

dped$image <- 'assets/walker2.png'
dveh$image <- 'assets/car1.png'



p1 <- ggplot() +
  annotation_custom(grid::rasterGrob(img1, width=unit(1,"npc"),
                                     height=unit(1,"npc")), 0, w1, 0, -h1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, w1)) +
  scale_y_reverse(expand=c(0, 0), limits=c(h1, 0)) +
  geom_polygon(data = dhotzone5, aes(x=x,y=y, fill = id, group = id), 
               alpha = .2, colour = "white") + 
  geom_polygon(data = dhotzone6, aes(x=x,y=y, fill = id, group = id),
               alpha = .2, colour = "white") +
  geom_polygon(data = dhotzone7, aes(x=x,y=y, fill = id, group = id),
               alpha = .2, colour = "white") +
  geom_text(data = hottext[id > 4],
            aes(x = Mx, y = My, label = id), size = zone_font_size, 
            colour = "black") +
  geom_image(data = dped[selected_ped2, ], aes(x = Xmpp, y = Ympp, image = image),
             size = .018, color = pal[1]) +
  geom_point(
    data = dped[1, ], aes(x = Xmpp, y = Ympp),
    shape = 2, size = 4, col = pal[1]
  ) +
  geom_point(
    data = dped[nrow(dped), ], aes(x = Xmpp, y = Ympp),
    shape = 0, size = 4, col = pal[1]
  ) +
  geom_point(
    data = dped[encounter_xy[2], ], aes(x = Xmpp, y = Ympp),
    size = 4, shape = 4, col = pal[1]
  ) +
  geom_text(
    data = dptext, aes(x = Xmpp, y = Ympp, label = tta),
    size = textsize1, fontface = "bold", color ="white",
  ) +
  geom_image(data = dveh[selected_veh2, ], 
             aes(x = Xmpp, y = Ympp, image = image),
             size = .018, col = pal[2]) +
  geom_point(
    data = dveh[1, ], aes(x = Xmpp, y = Ympp),
    shape = 2, size = 4, col = pal[2]
  ) +
  geom_point(
    data = dveh[nrow(dveh), ], aes(x = Xmpp, y = Ympp),
    shape = 0, size = 4, col = pal[2]
  ) +
  geom_point(
    data = dveh[encounter_xy[1], ], aes(x = Xmpp, y = Ympp),
    size = 4, shape = 4, col = pal[2]
  ) +
  geom_text(
    data = dvtext, aes(x = Xmpp, y = Ympp, label = tta),
    size = textsize1, fontface = "bold", color ="white",
  ) +
  coord_equal()  +
  theme_classic(base_size = 30) +
  theme(legend.position ="none",
        axis.title = element_blank(),
        axis.text  = element_blank(), 
        axis.ticks = element_blank())


fn <- paste0("tests/docs/first_submit/figs/qw_zone2_v4.png")
png(filename = fn, 1600, 1600)
print(p1)
dev.off()    



# Section 2 --------
# Save Video meta data 
dp <- "tests/extdata/"
sites <- c("Site1_1", "Site1_2", "Site2_1", "Site2_2")

for(i in 1:4) {
  path2file <- paste0(dp, sites[i])
  files <- list.files(path2file);   
  x0 <- list()
  for(j in seq_len(length(files) )) {
    fn <- paste0(path2file, "/", files[j])
    dtmp      <- fread(fn)
    dtmp$fid  <- factor(j)
    dtmp$Time <- as.POSIXct(dtmp$V2, origin = '1970-01-01', tz = "UTC")
    x0 <- rbind(x0, dtmp[,c(6,7)])
  }
  site <- setorder(x0, Time)
  
  # Check if Time vector is in decreasing order, implying abnormality of cameras
  if ( sum(diff(site$Time) < 0)  ) { stop("Time decreases") }
  
  files2 <- strsplit(files, ".txt")
  files3 <- unlist(files2)
  cap <- files3
  
  fnrda <- paste0(dp, sites[i], ".rda")
  save(site, cap, file = fnrda)
  tools::resaveRdaFiles(fnrda, compress = "xz", compression_level = 9)
}

# Store meta data --------------
rm(list = ls())
dp <- "tests/extdata/"
sites <- c("Site1_1", "Site1_2", "Site2_1", "Site2_2")
load(paste0(dp, sites[1], ".rda"))
site1_1<- site
cap1_1 <- cap

load(paste0(dp, sites[2], ".rda"))
site1_2<- site
cap1_2 <- cap

load(paste0(dp, sites[3], ".rda"))
site2_1 <- site
cap2_1 <- cap

load(paste0(dp, sites[4], ".rda"))
site2_2 <- site
cap2_2 <- cap


site1 <- list(Camera1 = cap1_1, Camera2 = cap1_2, Set1 = site1_1, Set2 = site1_2)
site2 <- list(Camera1 = cap2_1, Camera2 = cap2_2, Set1 = site2_1, Set2 = site2_2)

fn <- "data/videometa3.rda"
save(site1, site2, file = fn)
tools::resaveRdaFiles(fn, compress = "xz", compression_level = 9)

# Section 3 --------
# Counting traffic

# Retrieve data-time records
per <- records()
nday <- nrow(per)
i <- 1
for(i in seq_len(nday)) {
  load(paste0("tests/extdata/day", i, "/daday_.rda"))
  
  interact_tbl <- table(daday$ID, daday$A)
  ped_idx <- which(interact_tbl[, "Ped"] > 0)
  nroad_user <- nrow(interact_tbl)
  nroad_ped <- length(ped_idx)
  load(paste0("tests/extdata/day", i, "/pd.rda"))
  cat(nroad_user, " ", nroad_ped, " ", ninteract, "\n")
}


count_mat <- matrix(c(944  ,   38  ,   0 ,
                      27331,   1717,   66, 
                      28364,   1600,   60, 
                      27624,   1553,   68, 
                      28945,   1535,   60, 
                      24409,   1377,   39, 
                      1991 ,   94  ,   2 ,
                      5    ,   0   ,   0 ,
                      12195,   2009,   45, 
                      12103,   2064,   63, 
                      13046,   1851,   50, 
                      11926,   2081,   54, 
                      7913 ,   1275,   28, 
                      70   ,   2   ,   0 ), ncol = 3, byrow = TRUE)

colnames(count_mat) <- c("Total_traffic", "Pedestrian", "Interactions")
count_mat

total_traffic_counts <- colSums(count_mat)
total_traffic_counts

# After discounting unuseable and ambiguous data
