group_center <- function(var,grp) {
  return(var-tapply(var,grp,mean,na.rm=T)[grp])
}