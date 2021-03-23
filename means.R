means <- function(var,grp) {
  return(tapply(var,grp,mean,na.rm=T)[grp])
}