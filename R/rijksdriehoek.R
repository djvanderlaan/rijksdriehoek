
#' Translate coordinates in rijksdriehoek (Dutch National Grid) system to WGS84
#'
#' @param x vector of x-coordinates in rijksdriehoek system; or without y a 
#'   vector of length 2 containing the x and y coordinates.
#' @param y vector of y-coordinates in rijksdriehoek system
#'
#' @returns
#' A list with lamba (decimal degrees; longitude) and phi (decimal degrees; 
#' latitude), or if just x was given a numeric vector of length 2 with lambda
#' and phi.
#'
#' @export
rd_to_wgs84 <- function(x, y) {
  if (!is.numeric(x)) stop("x needs to be numeric.")
  if (nargs() == 1  && length(x) == 2) {
    y <- x[2]
    x <- x[1]
  } 
  if (!is.numeric(y)) stop("y needs to be numeric.")
  if (length(x) != length(y)) stop("x and y need to have same length.")

  x0 <- 155000.00
  y0 <- 463000.00
  phi0 <- 52.15517440
  lambda0 <- 5.38720621

  k <- list( 
    list(p=0, q=1, k=3235.65389), 
    list(p=2, q=0, k= -32.58297), 
    list(p=0, q=2, k=  -0.24750), 
    list(p=2, q=1, k=  -0.84978), 
    list(p=0, q=3, k=  -0.06550), 
    list(p=2, q=2, k=  -0.01709), 
    list(p=1, q=0, k=  -0.00738), 
    list(p=4, q=0, k=   0.00530), 
    list(p=2, q=3, k=  -0.00039), 
    list(p=4, q=1, k=   0.00033), 
    list(p=1, q=1, k=  -0.00012))
  l <- list(
    list(p=1, q=0, l=5260.52916), 
    list(p=1, q=1, l= 105.94684), 
    list(p=1, q=2, l=   2.45656), 
    list(p=3, q=0, l=  -0.81885), 
    list(p=1, q=3, l=   0.05594), 
    list(p=3, q=1, l=  -0.05607), 
    list(p=0, q=1, l=   0.01199), 
    list(p=3, q=2, l=  -0.00256), 
    list(p=1, q=4, l=   0.00128))
  dx <- (x - x0)/1E5
  dy <- (y - y0)/1E5
  phi <- rep(phi0, length(x))
  lambda <- rep(lambda0, length(x))

  for (i in seq_along(k)) {
    p <- k[[i]][["p"]]
    q <- k[[i]][["q"]]
    ki <- k[[i]][["k"]]
    phi <- phi + (ki * (dx^p) * (dy^q))/3600
  }
  for (i in seq_along(l)) {
    p <- l[[i]][["p"]]
    q <- l[[i]][["q"]]
    li <- l[[i]][["l"]]
    lambda <- lambda + (li * (dx^p) * (dy^q))/3600
  }
  if (nargs() == 1) {
    c(lambda[1], phi[1])
  } else list(lambda=lambda, phi=phi)
}


#' Translate coordinates in WGS84 to rijksdriehoek (Dutch National Grid)
#'
#' @param lambda numeric vector with longitutes (in decimal degrees), or 
#'   without phi a vector of length 2 containing both phi and lambda. 
#' @param phi numeric vector with latitudes (in decimal degrees).
#'
#' @returns
#' A list with x and y coordinates in Rijksdriehoek system coordinates, of if 
#' just lambda was given a numeric vector of length 2 with both coordinates.
#'
#' @export
wgs84_to_rd <- function(lambda, phi) {
  if (!is.numeric(lambda)) stop("lambda needs to be numeric.")
  if (nargs() == 1  && length(lambda) == 2) {
    phi <- lambda[2]
    lambda <- lambda[1]
  } 
  if (!is.numeric(phi)) stop("phi needs to be numeric.")
  if (length(lambda) != length(phi))
    stop("lambda and phi need to have same length.")
  x0 <- 155000.0;
  y0 <- 463000.0;
  phi0 <- 52.15517440;
  lambda0 <-  5.38720621;
  r <- list(
    list(p=0, q=1, r= 190094.945),
    list(p=1, q=1, r= -11832.228),
    list(p=2, q=1, r=   -114.221),
    list(p=0, q=3, r=    -32.391),
    list(p=1, q=0, r=     -0.705),
    list(p=3, q=1, r=     -2.340),
    list(p=1, q=3, r=     -0.608),
    list(p=0, q=2, r=     -0.008),
    list(p=2, q=3, r=      0.148))
  s <- list(
    list(p=1, q=0, s= 309056.544),
    list(p=0, q=2, s=   3638.893),
    list(p=2, q=0, s=     73.077),
    list(p=1, q=2, s=   -157.984),
    list(p=3, q=0, s=     59.788),
    list(p=0, q=1, s=      0.433),
    list(p=2, q=2, s=     -6.439),
    list(p=1, q=1, s=     -0.032),
    list(p=0, q=4, s=      0.092),
    list(p=1, q=4, s=     -0.054))
  dphi = 0.36*(phi - phi0)
  dlambda = 0.36*(lambda - lambda0)
  x <- rep(x0, length(phi))
  y <- rep(y0, length(phi))
  for (i in seq_along(r)) {
    p <- r[[i]][["p"]]
    q <- r[[i]][["q"]]
    ri <- r[[i]][["r"]]
    x <- x +  ri * (dphi^p) * (dlambda^q)
  }
  for (i in seq_along(s)) {
    p <- s[[i]][["p"]]
    q <- s[[i]][["q"]]
    si <- s[[i]][["s"]]
    y <- y +  si * (dphi^p) * (dlambda^q)
  }
  if (nargs() == 1) {
    c(x[1], y[1])
  } else list(x=x, y=y)
}


#' Reproject a geoJSON file to an other coordinate system
#'
#' @param filein the input file or connection.
#' @param fileout the output file or connection.
#' @param projection a function that projects coordinates from the original 
#'   coordinate system to the new system. The function has to accept numeric
#'   vectors of length two as its input and return a vector of length two.
#' @param inencoding when filein is a character string this encoding is used 
#'   when reading from the file. 
#'
#' @returns
#' A list containing the converted geoJSON. This output is usually not needed. 
#'
#' @export
reproject_geojson <- function(filein, fileout, projection, inencoding=getOption("encoding")) {
  library(rjson)
  trans <- function(coor) {
    if (is.numeric(coor)) {
      return(projection(coor))
    } else {
      res <- vector(length=length(coor), mode="list")
      for (i in seq_along(coor)) {
        res[[i]] <- trans(coor[[i]])
      }
      return(res)
    }
  }
  if (is.character(filein)) filein <- file(filein, "rt", encoding=inencoding)
  map <- fromJSON(file=filein)
  nfeatures <- length(map$features)
  for (i in seq_len(nfeatures)) {
    coor <- map$features[[i]]$geometry$coordinates
    map$features[[i]]$geometry$coordinates <- trans(coor)
  }
  json <- toJSON(map)
  writeLines(json, con=fileout)
  invisible(map)
}

