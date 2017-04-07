Bmsm_grad <- function(func, arg.list){
  
  #function(func, para, kbar, dat,n.vol)
  para <- arg.list$para
  kbar <- arg.list$kbar
  dat  <- arg.list$dat
  n.vol<- arg.list$n.vol
  
  new.arg1 <- arg.list
  new.arg2 <- arg.list
  
  para.size <- length(para)
  para      <- as.matrix(para)
  
  para.abs <- abs(para)
  
  if (!all(para==0)) {
    para2 <- para/para.abs
  } else {
    para2 <- 1
  }
  
  h1 <- cbind(para.abs, matrix(1,para.size,1)*1e-2)
  h  <- 1e-8*matrix(apply(h1,1,max),ncol=1)*para2
  
  para.temp <- para + h
  
  h <- para.temp - para
  
  ll.1 <- matrix(0,nrow(dat),para.size)
  ll.2 <- matrix(0,nrow(dat),para.size)
  
  for ( i in 1:para.size){
    x.temp <- matrix(rep(para,2),ncol=2)
    
    x.temp[i,1] <- x.temp[i,1] + h[i,1]
    x.temp[i,2] <- x.temp[i,2] - h[i,1]
    new.arg1$para <- x.temp[,1]
    new.arg2$para <- x.temp[,2]
    
    ll.1[,i] <- do.call(func,new.arg1) #do.call(func,list(para=x.temp[,1],kbar=kbar,dat = dat, n.vol = n.vol))
    ll.2[,i] <- do.call(func,new.arg2)
    
    #ll.1[,i] <- Msm_likelihood2(check_bmsm_para(x.temp[,1]),kbar,dat,n.vol)$LLs
    #ll.2[,i] <- Msm_likelihood2(check_bmsm_para(x.temp[,2]),kbar,dat,n.vol)$LLs
    
  }
  
  der <- (ll.1-ll.2)/(2*t(matrix(rep(h,nrow(dat)),ncol=nrow(dat))))
  return(der)
  
}