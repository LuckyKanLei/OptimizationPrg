#' caculate the absolut abweichung
#' @param A vector, numica
#' @param B vector, numica
#' @return Num, sum(abs(A-B))
#' @examples
#' absolutabweichungfunk(c(1,5), c(2,3))
#' @export
absolutabweichungfunk <- function(A, B){  ##A,B sind moglichkeit
  return(sum(abs(A - B)))
}

#' optimisation with 0.618
#' @param paramekl numica, the small wert
#' @param paramegr numica, the big wert
#' @param DatenReferenz numica, the Ideal optimal solution
#' @param abweichfk Function, Deviation calculation function, default absolute error
#' @param processfk Function, Self function
#' @param umkriesn Num, how many search
#' @param ... other paramater for processfk
#' @return Num, the best wert Optimal solution
#' @export
eindim618funk <- function(processfk, paramekl, paramegr, DatenReferenz, abweichfk = absolutabweichungfunk, umkriesn, ...){
  paramelk <- paramekl
  paramerc <- paramegr
  abweischunglk <- abweichfk(DatenReferenz,processfk(paramelk, ...))
  abweischungrc <- abweichfk(DatenReferenz,processfk(paramerc, ...))
  for(i in 1:umkriesn){
    parametemrc <- paramelk + 0.618*(paramerc - paramelk)
    parametemlk <- paramerc  +0.618*(paramelk - paramerc)
    abweischungtemrc <- abweichfk(DatenReferenz, processfk(parametemrc, ...))
    abweischungtemlk <- abweichfk(DatenReferenz, processfk(parametemlk, ...))
    if(abweischungtemrc<abweischungtemlk){
      paramelk <- parametemlk
    }
    if(abweischungtemlk<abweischungtemrc){
      paramerc <- parametemrc
    }
  }
  return(mean(paramelk,paramerc))
}
