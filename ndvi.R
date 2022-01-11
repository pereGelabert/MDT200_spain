

ndiff <- function(b1,b2){
  return((b1-b2)/(b1+b2))
}

##################
#Crea funcion SAVI

nsavi <- function(b1,b2,L=0.5){
  return(((b1-b2)/(b1+b2+L))*(1+L))
}
