#' Making factor vectors
#' @description Changes character vectors to factor vectors
#' @param data A data set containing ....
#' @param binary Logical indicating if binary data should be considered as factor
#' @return Data frame with all character vectors changed to factor vectors
#' @export
Vectoriz <- function(data, binary = FALSE){
  for (i in 1:dim(data)[2]) {
    if(is.character(data[[i]]) || (binary && is.numeric(data[[i]]) && length(levels(as.factor(data[[i]]))) == 2)) data[[i]] <-  as.factor(data[[i]])
    
    else data[[i]] <- data[[i]]
  }
  return(as.data.frame(data))
}

#' Parameter Pearson's chi-squared test. Check over dispersion in a POISON GLM 
#' @param model A GLM model
#' @param method Method to use, 'pearson' is default
#' @return The probability value of the test
#' @details Parameter Pearson's chi-squared test. H0: le modele est adequat ; H1: le modele n'est pas adequat
#' @export
adequat <- function(model, method = "pearson"){
  cat("parameter Pearson's chi-squared test \nH1: le modele n'est pas adequat\n")
  r <- residuals(model, method)
  pearsonk=sum(r^2)
  p <- 1 - pchisq(pearsonk, df.residual(model))
  cat("p-value ")
  return(c(p)) #summary(model)$df.residual
}

#' Fibonacci numbers
#' @description Generates numbers from Fibonacci serie 
#' @param n Integer ...
#' @param Uo Integer ...
#' @param U1 Integer ...
#' @param PrintFib Logical, indicating if the series should be printed.
#' @return Either a real number, result of the function or a vector of al the series.
#' @examples fibonacci(n = 10, PrintFib = TRUE)
#' fibonacci(n = 10, Uo = 1, U1 = 3, PrintFib = FALSE)
#' @export
#' @author Narcisse Yehouenou
fibonacci <- function(n, PrintFib = FALSE, Uo = 0, U1 = 1){
  Un <- numeric(length = n)
  Un[1:2] <- c(Uo, U1)
  
  if(n < 2) return(Uo)
  
  else if (n > 2){
    for (i in 3:n){
      Un[i] <- Un[i-1] + Un[i-2]
    }
    Fib <- Un
  }
  else 
    Fib <- Un
  
  if(!PrintFib)
    Fib <- Fib[n]
  
  return(Fib)
}

#' Fibonacci series
#' @description Computes rates from Fibonacci series
#' @param n Integer ...
#' @param Uo Integer ...
#' @param U1 Integer ...
#' @param PrintSer Logical, indicating if the series should be printed.
#' @return Either a real number, result of the rate of nth and (n-1)th numbers in Fibonacci series.
#' @examples fiboRate(n = 100, PrintSer = FALSE, Uo = 0, U1 = 1)
#' ##(1+sqrt(5))/2
#' fiboRate(n = 10, PrintSer = TRUE, Uo = 0, U1 = 1)
#' @details The function returns golden number when Uo = 0, and U1 = 1. Larger n is, more precise the number (result) is.
#' @author Narcisse Yehouenou
#' @export

fiboRate <- function(n = 10, PrintSer = FALSE, Uo = 0, U1 = 1){
  a <- fibonacci(n = n, Uo = Uo, U1 = U1, PrintFib = TRUE)
  serie <- a[2:n]/a[1:n-1]
  if(PrintSer) return(serie)
  else return(serie[n-1])
}

#' Matrix and numeric square root
#' @description Computes Matrix and numeric number square root using eigen analysis.
#' @param x A matrix or numeric value 
#' @return Matrix square root of x.
#' @examples sqrtm(4) # returns numeric 
#' sqrtm(matrix(64)) # returns matrix
#' A <- matrix(data = c(1, 2, 3, 2, 20, 26, 3, 26, 70),
#' ncol = 3); A
#' a <- sqrtm(A); a
#' a %*% a ## ==A; matrix square 
#' a * a ## not equals A; simple square
#' @seealso \code{\link{sqrt}}
#' @export
sqrtm <- function(x)
{
  if(is.matrix(x) && (dim(x)[1] >1))
  {
    x.eig <- eigen(x)
    val <- x.eig$values
    vec <- x.eig$vectors
    s <- vec %*% diag(sqrt(val)) %*% solve(vec)
  }
  else s <- sqrt(x)
  return(s)
}

#' Radians to degrees
#' @description Converts angle values from  radian to degree.
#' @param radian A vector of degree values to be converted
#' @return Returns a vector of radian values.
#' @examples deg(pi/2)  
#' @seealso \code{\link{rad}}, the ‘complement’ of \code{deg}
#' @export

deg <- function (radian) 
{
  radian * (180/pi)
}

#' Degrees to radians
#' @description Converts angle values from  degree to radian.
#' @param degree A vector of radian values to be converted
#' @return Returns a vector of radian values.
#' @examples rad(180)  
#' @seealso \code{\link{deg}}, the the ‘complement’ of \code{rad}
#' @export
rad <- function (degree) 
{
  degree * (pi/180)
}

#' Height of tree or vertical object.
#' @description Computes the height of tree or vertical object. Allows all both slope and angle measures. No matter the relative position of the persons who measures angle/slope.
#' @param distance A numeric vector of distance between object and the person who measures angle.
#' @param vh,vb numeric vector of top angle and down angle respectively. 
#' @param type The type of 'vh' and 'vb' measures. Either "angle" or "slope". Default is "slope".
#' @param  angleUnit The unit of 'vh' and 'vb' measures when their are "angle". Either "deg", "rad". Defalt is "deg".
#' @return Returns a vector of heights.
#' @examples height(10, 80, 17)
#' height(17, vh = -18, vb = -113)
#' height(distance = 18, vh = 42, vb = -12, type = "angle", angleUnit = "deg")
#' height(distance = 18:21, vh = 42:45, vb = -12:-15, type = "angle", angleUnit = "deg")
#' ## Bellow shows warning messages 
#' height(distance = 18:21, vh = -42:-45, vb = -12:-15, type = "angle", angleUnit = "deg")
#' @author Narcisse Yehouenou
#' @export

height <- function(distance, vh, vb, type = c("angle", "slope"), 
                   angleUnit = c("deg", "rad")){
  if (prod(angleUnit == c("deg", "rad")) || angleUnit == "deg") {
    VH <- rad(vh)
    VB <- rad(vb)
  }
  else if (!(angleUnit %in% c("deg", "rad"))) {
    stop("angleUnit should be either  'deg' or 'rad'")
  }
  
  if (prod(type == c("angle", "slope"))) {
    type <- "slope"
  }
  else if (!(type %in% c("angle", "slope"))) {
    stop("type should be either  'angle' or 'slope'")
  }
  
  if (sum(vh <= vb))
    warning("One or more top angles are less than their down angles. 
          Please check your data")
  
  if (type ==  "slope") {
    h <- 0.01*distance * (vh - vb)
  }
  else
    h <- distance * (tan(VH) - tan(VB))
  return(h)
}


#' Angle to slope
#' @description Converts angle values to  slope values.
#' @param angle numeric vector of angle to be converted to slope. 
#' @param  angleUnit The unit of \code{angle}. Either "deg", "rad". Default is "deg".
#' @return Returns a vector of slope values.
#' @examples angle2slope(10)
#' angle2slope(angle = 45)
#' angle2slope(angle = 50, angleUnit = "deg")
#' angle2slope(1.047198, "rad") 
#' angle2slope(0.2617994, angleUnit = "rad") 
#' @seealso \code{\link{slope2angle}}, the the ‘complement’ of \code{angle2slope}.
#' @export
angle2slope <- function(angle, angleUnit = c("deg", "rad")){
  if (prod(angleUnit == c("deg", "rad")) || angleUnit == "deg") 
    angle <- rad(angle)
  
  else if (!(angleUnit %in% c("deg", "rad"))) 
    stop("angleUnit should be either  'deg' or 'rad'") 
  slp <- 100 * tan(angle)
  return(slp)
}

#' Slope to angle
#' @description Converts slope values to angle values.
#' @param slope numeric vector of slope to be converted to angle. 
#' @param  angleUnit The desired unit for the returned angle \code{Value}. Either "deg", "rad". Default is "deg".
#' @return A vector of angle values in specified unit.
#' @examples slope2angle(100)
#' slope2angle(17.6327)
#' slope2angle(angle2slope(30))
#' @seealso \code{\link{angle2slope}}, the the ‘complement’ of \code{slope2angle}
#' @export
slope2angle <- function(slope, angleUnit = c("deg", "rad")){
  if (prod(angleUnit == c("deg", "rad")) || angleUnit == "deg") 
    return(deg(atan(.01 * slope)))
  
  else if (!(angleUnit %in% c("deg", "rad"))) 
    stop("angleUnit should be either  'deg' or 'rad'") 
  
  return(atan(.01 * slope))
}

#' Main angle
#' @description \code{angleMain} returns the main angle of an angle value. Main angles range from -pi to pi for radian unit while they range from -180 to 180 for degree unit 
#' @param angle numeric vector of angle. 
#' @param  angleUnit The unit of \code{angle}. Either "deg", "rad". Default is "deg".
#' @return A matrix of main angle values in radian and in degree units.
#' @export 
#' @seealso \code{\link{rad}}, \code{\link{deg}}, \code{\link{slope2angle}}, \code{\link{angle2slope}}
angleMain <- function(angle, angleUnit = c("deg", "rad")){
  if (prod(angleUnit == c("deg", "rad")) || angleUnit == "deg") 
    angle <- rad(angle)
  
  else if (!(angleUnit %in% c("deg", "rad"))) 
    stop("angleUnit should be either  'deg' or 'rad'")
  radian <- Arg(complex(real = cos(angle), imaginary = sin(angle)))
  degree <- deg(radian)
  return(cbind(radian, degree))
}
