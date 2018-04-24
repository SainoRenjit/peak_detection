
peak_DETE<- function (x,m){
  
  thresh <- seq(1,20,length=20)
  ws <- lapply(thresh, function(k,x) wavShrink(x, wavelet="s8",shrink.fun="hard", thresh.fun="universal",
                                               thresh.scale=k, xform="modwt"), x=x)
  ecgvalue <- ws[[3]][1:length(ws[[3]])]
  input<-ecgvalue
  #-------------------------------------------------------------
  # input<-x
  shape <- diff(sign(diff(input, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i6){
    z <- i6 - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i6 + m + 1
    w <- ifelse(w < length(input), w, length(input))
    if(all(input[c(z : i6, (i6 + 2) : w)] <= input[i6 + 1])) return(i6 + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  return(pks)
}
