rm(list = ls())
pkg <- c("data.table", "ggplot2", "Rose", "png")
sapply(pkg, require, character.only = TRUE)

core_path <- "[01]Projects/viscando/"
windows_path <- paste0("D:/", core_path)
unix_path <- paste0("/media/yslin/Avocet/", core_path)
unix_path <- paste0("/home/yslin/", core_path)
wk <- ifelse(.Platform$OS.type == "windows",
             shortPathName(windows_path),
             unix_path
)
setwd(wk)

# Section 1 ------------------
# Load the data
pyrootlist <- list(
  "tests/pydata/short_stop/",
  "tests/pydata/priority_assertion/",
  "tests/pydata/hesitation/",
  "tests/pydata/early_yield/",
  "tests/pydata/short_stop_correct/"
)
savepathlist <- list(
  "tests/pydata/short_stop_GM.rda",
  "tests/pydata/priority_GM.rda",
  "tests/pydata/hesitation_GM.rda",
  "tests/pydata/early_yield_GM.rda",
  "tests/pydata/short_stop_correct_GM.rda"
)

i <- 5
input <- pyrootlist
output <- savepathlist
                          
pyroot <- input[[i]]
save2file <- output[[i]]

timefiles <- list.files(pyroot, pattern = "time_stamps(1[0-2]|[0-9])*")
a0files <- list.files(pyroot, pattern = "^(acceleration_case).*(0.txt)$")
a1files <- list.files(pyroot, pattern = "^(acceleration_case).*(1.txt)$")
s0files <- list.files(pyroot, pattern = "^speed(1[0-2]|[0-9]).*(0.txt)$")
s1files <- list.files(pyroot, pattern = "^speed(1[0-2]|[0-9]).*(1.txt)$")

s0_files <- list.files(pyroot, pattern = "^other_perceived_speed(1[0-2]|[0-9]).*(0.txt)$")
s1_files <- list.files(pyroot, pattern = "^other_perceived_speed(1[0-2]|[0-9]).*(1.txt)$")
dc0files <- list.files(pyroot, pattern = "^distance2encounter(1[0-2]|[0-9]).*(0.txt)$")
dc1files <- list.files(pyroot, pattern = "^distance2encounter(1[0-2]|[0-9]).*(1.txt)$")
dc0_files <- list.files(pyroot, 
                        pattern = "^other_distance2encounter(1[0-2]|[0-9]).*(0.txt)$")
dc1_files <- list.files(pyroot, 
                        pattern = "^other_distance2encounter(1[0-2]|[0-9]).*(1.txt)$")

dout <- NULL
nfile <- length(timefiles)

for (j in seq_len(nfile)) {
  xx <- scan(paste0(pyroot, timefiles[j]))
  a0 <- scan(paste0(pyroot, a0files[j]))
  a1 <- scan(paste0(pyroot, a1files[j]))
  s0 <- scan(paste0(pyroot, s0files[j]))
  s1 <- scan(paste0(pyroot, s1files[j]))
  
  s0_ <- scan(paste0(pyroot, s0_files[j]))
  s1_ <- scan(paste0(pyroot, s1_files[j]))
  dc0 <- scan(paste0(pyroot, dc0files[j]))
  dc1 <- scan(paste0(pyroot, dc1files[j]))
  dc0_ <- scan(paste0(pyroot, dc0_files[j]))
  dc1_ <- scan(paste0(pyroot, dc1_files[j]))
  dtmp <- data.frame(
    x = rev(xx),
    A0 = a0, A1 = a1,
    S0 = s0, S1 = s1,
    S0_ = s0_, S1_ = s1_,
    DC0 = dc0, DC1 = dc1, DC0_ = dc0_, DC1_ = dc1_,
    id = j)
  dout <- rbind(dout, dtmp)
}
  
d <- data.table(dout)
save(d, file = save2file)



