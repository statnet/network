#' Transform vector of values into color specification
#' 
#' Convenience function to convert a vector of values into a color
#' specification.
#' 
#' @param x vector of numeric, character or factor values to be transformed
#' @param opacity optional numeric value in the range 0.0 to 1.0 used to specify
#'   the opacity/transparency (alpha) of the colors to be returned. 0 means
#'   fully opaque, 1 means fully transparent.
#' 
#' Behavior of \code{as.color} is as follows: \itemize{ \item integer numeric
#' values: unchanged, (assumed to corespond to values of R's active
#' \code{\link{palette}}) \item integer real values: will be translated to into
#' grayscale values ranging between the max and min \item factor: integer
#' values corresponding to factor levels will be used \item character: if
#' values are valid colors (as determined by \code{is.color}) they will be
#' returned as is.  Otherwise converted to factor and numeric value of factor
#' returned. }
#' 
#' The optional \code{opacity} parameter can be used to make colors partially
#' transparent (as a shortcut for \code{\link{adjustcolor}}.  If used, colors
#' will be returned as hex rgb color string (i.e. \code{"#00FF0080"})
#' 
#' The \code{is.color} function checks if each character element of \code{x}
#' appears to be a color name by comparing it to \code{\link{colors}} and
#' checking if it is an HTML-style hex color code.  Note that it will return
#' FALSE for integer values.
#' 
#' These functions are used for the color parameters of
#' \code{\link{plot.network}}.
#' 
#' @return For \code{as.color}, a vector integer values (corresponding to color
#'   palette values) or character color name. For \code{is.color}, a logical
#'   vector indicating if each element of x appears to be a color
#' 
#' @export
#' 
#' @examples
#' 
#' 
#' as.color(1:3)
#' as.color(c('a','b','c'))
#' 
#' # add some transparency
#' as.color(c('red','green','blue'),0.5) # gives "#FF000080", "#00FF0080", "#0000FF80"
#' 
#' is.color(c('red',1,'foo',NA,'#FFFFFF55'))
as.color<-function(x,opacity=1.0){
  if(opacity > 1 | opacity < 0){
    stop('opacity parameter must be a numeric value in the range 0 to 1')
  }
  colors<-x
  #Numeric rule: if integer leave as-is, otherwise convert to grayscale
  if(is.numeric(x)){
    if(any(x!=round(x),na.rm=TRUE)){
      colors<-gray((x-min(x))/(max(x)-min(x)))
    }else
      colors<-x
  }
  #Factor rule: categorical colorings
  if(is.factor(x)){
    colors<-match(levels(x)[x],levels(x))
  }
  #Character rule: if colors, retain as colors; else categorical
  if(is.character(x)){
    if(all(is.color(x)))
      colors<-x
    else{
      colors<-match(x,sort(unique(x)))
    }
  }
  # add transparency if not 1
  if(opacity < 1){
    colors<-grDevices::adjustcolor(colors,alpha.f=opacity)
  }
  return(colors)
}
