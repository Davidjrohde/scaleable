###############################
####
#### linregparser.R
#### Example of linear regression using paralell and serial processing 
#### 4 Sept 14, David Rohde
####
################################



P<-6 # dimension of beta
betaTrue <- matrix(c(1,-2,3,4,-5,6),P,1)
                                        
N<-1e5 # rows per file
M<-1e2  # number of files or blocks



# write out M batches of size N of data
if (FALSE) {    # after the first run you probably want to set this to FALSE,
    # but you must rerun with TRUE after modifying any of the above
    # if you don't the times calculated will be wrong
    for (ii in seq(M)) {
        set.seed(ii)
        X<-matrix(rnorm(P*N),N,P)
        y<-rnorm(N,X %*% betaTrue)

        save(list=c("X","y"),file=sprintf('%010d.RData',ii))
    }
}


C<-2 # number of cores



#version using the core parallel package using lapply idea

# first set up the function
loaddataandfit<-function(n) {
    load(file=sprintf('%010d.RData',n))
    XTX <- t(X) %*% X
    XTy<- t(X) %*% y
    return(list('XTX'=XTX,'XTy'=XTy))
}

# this is an alternative that draws the data (hence no disk access)
drawdataandfit<-function(n) {
    set.seed(n)
    X<-matrix(rnorm(P*N),N,P)
    y<-rnorm(N,X %*% betaTrue)
    XTX <- t(X) %*% X
    XTy<- t(X) %*% y
    return(list('XTX'=XTX,'XTy'=XTy))
}


#serial version
if (TRUE) {
    timelapply <- system.time({
        r<-lapply(matrix(seq(M)),loaddataandfit)

        XTX<-matrix(0,P,P)
        XTy<-matrix(0,P,1)
        for (ii in seq(M)) {        
            XTX<-XTX+r[[ii]][[1]]
            XTy<-XTy+r[[ii]][[2]]
        }
        print(solve(XTX,XTy))
    })[3]


    print(paste('lapply serial time ',timelapply))
    print(paste('time for 1e10 is ',1e10/M/N * timelapply/60,' minutes'))
}


#parallel version
if (TRUE) {
    library(parallel)

    timemclapply <- system.time({
        r<-mclapply(matrix(seq(M)),loaddataandfit,mc.cores=C)

        XTX<-matrix(0,P,P)
        XTy<-matrix(0,P,1)
        for (ii in seq(M)) {        
            XTX<-XTX+r[[ii]][[1]]
            XTy<-XTy+r[[ii]][[2]]
        }
        print(solve(XTX,XTy))
    })[3]


    print(paste('mclapply parallel ',timemclapply))
    print(paste('time for 1e10 is ',1e10/M/N * timemclapply /60,' minutes'))

    print(paste0('speed up is ',timelapply/timemclapply, ' theoretical max is ',C))
}




# no IO serial
if (TRUE) {
    timenoio<- system.time({
        r<-mclapply(matrix(seq(M)),drawdataandfit,mc.cores=1)
        XTX<-matrix(0,P,P)
        XTy<-matrix(0,P,1)
        for (ii in seq(M)) {       
            XTX<-XTX+r[[ii]][[1]]
            XTy<-XTy+r[[ii]][[2]]
        }
        print(solve(XTX,XTy))
    })[3]


    print(paste('no IO serial lapply ',timenoio))
    print(paste('time for 1e10 is ',1e10/M/N * timenoio /60,' minutes'))
    
}

# no IO paralell
if (TRUE) {
    timenoiomc <-system.time({
        r<-mclapply(matrix(seq(M)),drawdataandfit,mc.cores=C)
        XTX<-matrix(0,P,P)
        XTy<-matrix(0,P,1)
        for (ii in seq(M)) {        
            XTX<-XTX+r[[ii]][[1]]
            XTy<-XTy+r[[ii]][[2]]
        }
        print(solve(XTX,XTy))
    })[3]

    print(paste('no IO paralell mcapply ',timenoiomc))
    print(paste('time for 1e10 is ',1e10/M/N * timenoiomc /60,' minutes'))
    print(paste('no io speed up is ',timenoio/timenoiomc,' theoretical max is ',C))
}


