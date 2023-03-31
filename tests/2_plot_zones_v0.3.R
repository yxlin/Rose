## Description: Plot the seven crossing zones
pkg <- c("data.table", "ggplot2", "Rose", "png")
sapply(pkg, require, character.only = TRUE)
unix_path <- "/media/yslin/Avocet/Projects/viscando/"
win_path <- "C:/Users/yslin/OneDrive - University of Leeds/Projects/viscando/"
wk <- ifelse(.Platform$OS.type == "windows", 
  shortPathName(win_path), unix_path)
setwd(wk)
rm(list = ls())

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

# 1. Define the crossing zones in the pixel space. These number matchs only
# the png file, "ground - Belle_Isle.png" and "Queensway.png"
# 2. Use the alternative one, if other png files are used.
hotzone <- data.table(load_hotzone())

# Calculate the centre of each zone to plot the zone number
hottext <- hotzone[, .(Mx = mean(x), 
                       My = mean(y)), .(id)]

# Separate the zones by site
dhotzone1 <- hotzone[id == 1]
dhotzone2 <- hotzone[id == 2]
dhotzone3 <- hotzone[id == 3]
dhotzone4 <- hotzone[id == 4]
dhotzone5 <- hotzone[id == 5]
dhotzone6 <- hotzone[id == 6]
dhotzone7 <- hotzone[id == 7]

# Plot Belle Isle Road
p0 <- ggplot() +
  annotation_custom(grid::rasterGrob(img0, width=unit(1,"npc"),
                                     height=unit(1,"npc")), 0, w0, 0, -h0) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, w0)) +
  scale_y_reverse(expand=c(0, 0), limits=c(h0, 0)) +
  geom_polygon(data = dhotzone1, aes(x=x,y=y, fill = id, group = id),
              alpha = .2, colour = "white") +
  geom_polygon(data = dhotzone2, aes(x=x,y=y, fill = id, group = id),
             alpha = .2, colour = "white") +
  geom_polygon(data = dhotzone3, aes(x=x,y=y, fill = id, group = id),
             alpha = .2, colour = "white") +
  geom_polygon(data = dhotzone4, aes(x=x,y=y, fill = id, group = id),
             alpha = .2, colour = "white") +
  geom_text(data = hottext[id <= 4],
            aes(x = Mx, y = My, label = id), size = 20, colour = "white") +
  coord_equal()  +
  theme_classic(base_size = 30) +
  theme(legend.position ="none")

# This does not work on Python radian
fn <- paste0("tests/figs/belle_zone2_v3.png")
png(filename = fn, 1600, 1600)
print(p0)
dev.off()    


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
            aes(x = Mx, y = My, label = id), size = 20, colour = "white") +
  coord_equal()  +
  theme_classic(base_size = 30) +
  theme(legend.position ="none")

# This does not work on Python radian
fn <- paste0("tests/figs/qw_zone2_v3.png")
png(filename = fn, 1600, 1600)
print(p1)
dev.off()   

