## Description: Freeze this file, considered it done. 
## https://stackoverflow.com/questions/32184252/how-to-select-columns-in-data-table-using-a-character-vector-of-certain-column-n
isServer <- FALSE
pkg <- c("Rose", "data.table", "dplyr", "lubridate")
sapply(pkg, require, character.only = TRUE)
wpath <- ifelse(isServer, "~/Documents/viscando/", 
                "/media/yslin/Avocet/Projects/viscando/"); wpath
wk <- ifelse(.Platform$OS.type == "windows", shortPathName("C:/"), wpath)
setwd(wk)
rm(list = ls())

## Belle Isle Road------------------------------------------------------
rawpath <- "tests/extdata/raw/"
# ~617MB
x0 <- extract_data(rawpath)
# object.size(x0) / 1024/1024
tibble::as_tibble(tail(x0))
# x1 <- extract_data(qpath)

cols <- c("Type", "Estimated", "fid"); cols
sapply(x0[, ..cols], table)
table(x0$Type, x0$fid)
#         1       2       3       4       5       6       7       8
# 0    4435  142556  140787  132728  133399  120089   75899    1592
# 1    2436   35867   34182   34338   33091   26350   16865     222
# 2   44863 1044432 1128341 1063156 1123429  919983  613123    3987
# 3    2532   74205   83698   75611   83675   41728   19903     813
# 
#         9      10      11      12      13      14      15
# 0  164643  179332  186407  174090  199000  119537   96366
# 1   38032   55318   44250   49197   46704   29123   25183
# 2  310260  371776  382412  442850  387210  262787  214529
# 3   33919   35672   38361   40660   31398   16381   25789
per <- records()
nday <- nrow(per)
cols <- c("ID", "Time", "X", "Y", "Speed", "A", "Xmpp", "Ympp")
for(i in seq_len(nday)) {
  tmp <- x0 %>% filter(Time > ymd_hm(per[i, 1]) & Time < ymd_hm(per[i, 2])) 
  msg0 <- ifelse(i <= 7, "Belle Road", "QW")
  pngfile <- ifelse(i <= 7, "tests/figs/belle.png", "tests/figs/queensway.png")
  img <- png::readPNG(pngfile) 

  # 594 = 49.54 m
  # 11.99031 pixel = 1 m
  h <- dim(img)[1] # image height
  w <- dim(img)[2] # image width
  xwidth <- ifelse(i <= 7, 24.77, 47.65)
  ywidth <- ifelse(i <= 7, 38.02, 42.27) 
  mppx <- w/(xwidth*2)
  mppy <- h/(ywidth*2)
  tmp$Xmpp <- mppx*(tmp$X + xwidth)
  tmp$Ympp <- mppy*(tmp$Y + ywidth)
  
  cat(msg0, "from", as.character(ymd_hm(per[i, 1])), "to", as.character(ymd_hm(per[i, 2])), "\n")  

  daday <- tmp[, ..cols]
  
  fn <- paste0("tests/extdata/day", i, "/daday.rda")
  save(daday, file = fn)
  tools::resaveRdaFiles(fn, compress = "xz", compression_level = 9)
  
}
