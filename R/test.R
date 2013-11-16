cors <- rbind(
  c(121687, 487484), # Amsterdam
  c( 92565, 437428), # Rotterdam
  c(176331, 317462)) # Maastricht

cors_wgs <- rbind(
  c(52.37422, 4.89801), # Amsterdam
  c(51.92183, 4.47959), # Rotterdam
  c(50.84660, 5.69006)) # Maastricht
cors_wgs <- cors_wgs[, c(2,1)]

# Test rd_to_wgs84
t <- rd_to_wgs84(cors[,1], cors[,2])
t1 <- abs(t$lambda - cors_wgs[,1]) < 1E-5
if (!all(t1)) stop("failed test 1")
t2 <- abs(t$phi - cors_wgs[,2]) < 1E-5
if (!all(t2)) stop("failed test 2")

t <- rd_to_wgs84(cors[1,])
t3 <- abs(t - cors_wgs[1,]) < 1E-5
if (!all(t3)) stop("failed test 3")

# Test wgs84_to_rd
t <- wgs84_to_rd(cors_wgs[,1], cors_wgs[,2])
t4 <- abs(t$x - cors[,1]) < 0.5
if (!all(t4)) stop("failed test 4")
t5 <- abs(t$y - cors[,2]) < 0.5
if (!all(t5)) stop("failed test 5")

t <- wgs84_to_rd(cors_wgs[1,])
t6 <- abs(t - cors[1,]) < 0.5
if (!all(t6)) stop("failed test 6")

