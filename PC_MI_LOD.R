# svlod<-3

PC_MI_LOD<-function(phen,gene,ps,svlod){
y<-phen  
# ps<-str
X<-t(gene)

set.seed(1)
X1<-X[3:nrow(X),]
sig<-seq(1:ncol(X1))
x<-data.frame(X1)
y<-as.matrix(y)
le<-length(sig)
xnew<-x[sig]

y1<-matrix(nrow=nrow(y),ncol=ncol(y))

if (is.null(ps)==FALSE)
{
  ps1<-cbind(matrix(1,nrow=nrow(y)),ps)
  vhat<-solve(crossprod(ps1,ps1))%*%crossprod(ps1,y)
  vhat1<-vhat[-1]
  y1<-y-ps%*%vhat1
}else{
  y1<-y 
}
# y1<-matrix(y1,nrow=199)

xxx<-xnew
matcor<-vector()

for(i in 1:le)
{
  if (var(xxx[,i])>0){
    matcor[i]<-abs(cor(xxx[,i],y1))
  }else{
    matcor[i]<-0
  }
}

n1<-nrow(y1)-1
ee<-as.vector(which(rank(matcor)>=(le-n1), arr.ind=T))

xxxnew<-X1[,sig[ee]]
yyy<-y1


cvfit1 <- ncvreg::cv.ncvreg(scale(xxxnew), yyy, family="gaussian",penalty="SCAD",gamma=3.7,warn=FALSE)


fit1 <- cvfit1$fit
obj11 <- (as.vector(fit1$beta[,cvfit1$min]))
obj1<-obj11[-1]

if (length(which(abs(obj1)!=0))!=0)
{
  sig1a<-which(abs(obj1)!=0)
  sig1b<-sig[-(sig[ee][sig1a])]                #é–«å¤Šåš?29æ¶??
  yyy1<-y1-(X1[,sig[ee][sig1a]]%*%as.matrix(obj1[sig1a]))
  xxx1<-X1[,sig1b]
  mat1<-vector()
  for (i in 1:length(sig1b))
  {
    if (var(xxx1[,i])>0){
      mat1[i]<-abs(cor(xxx1[,i],yyy1))
    }else{
      mat1[i]<-0 
    }
  }
  
  n2<-nrow(yyy1)-1
  ee1<-as.vector(which(rank(mat1)>=(ncol(xxx1)-n2), arr.ind=T))
  
  xxxnew1<-X1[,sig1b[ee1]]
  cvfit2 <- ncvreg::cv.ncvreg(scale(xxxnew1), yyy1, family="gaussian",penalty="SCAD",gamma=3.7,warn=FALSE)
  
  fit2 <- cvfit2$fit
  obj22 <- (as.vector(fit2$beta[,cvfit2$min]))
  obj2<-obj22[-1]
  sig1c<-sig1b[ee1][which(abs(obj2)!=0)]     #é–«å¤Šåš?60æ¶??
  sigg<-sort(c(sig[ee][sig1a],sig1c))
  
}else{
  
  sigg<-sig[ee]
}
le1<-length(sigg)      #ééâ‚¬å¤Šåš?89æ¶??


xxx<-xnew
xxxx<-as.matrix(xxx)
xxxx<-matrix(as.numeric(xxxx),nrow=nrow(xxxx))
mi1<-vector()


# library(R.matlab)
Matlab$startServer()
matlab <- Matlab()
# print(matlab)
isOpen <- open(matlab)
# print(matlab)

setVariable(matlab, y1=y1)
y1<- getVariable(matlab, "y1")

setVariable(matlab,xxxx=xxxx)
xxxx<- getVariable(matlab, "xxxx")

# evaluate(matlab,"makeosmex;")

evaluate(matlab, "mi1=new_mi_matrix(xxxx,y1);")
mi1<- getVariable(matlab, "mi1")
print(mi1)

y1<-as.matrix(unlist(y1[1]))
xxxx<-as.matrix(unlist(xxxx[1])) 
xxxx<-matrix(xxxx,nrow=nrow(y1))
mi1<-as.matrix(unlist(mi1[1])) 


# n1<-nrow(y1)-1
eee<-as.vector(which(rank(mi1)>=(le-n1), arr.ind=T))

xxxxnew<-X1[,sig[eee]]
yyy<-y1



cvfit3 <- ncvreg::cv.ncvreg(scale(xxxxnew), yyy, family="gaussian",penalty="SCAD",gamma=3.7,warn=FALSE)

fit3 <- cvfit3$fit
obj33 <- (as.vector(fit3$beta[,cvfit3$min]))
obj3<-obj33[-1]


  sig2a<-which(abs(obj3)!=0)
  sig2b<-sig[-(sig[eee][sig2a])]
  sig2c<-sig[eee][sig2a]
  yyyy1<-y1-(X1[,sig[eee][sig2a]]%*%as.matrix(obj3[sig2a]))
  xxxx1<-X1[,sig2b]
                                               #é–«å¤Šåš?15æ¶??
  
  mi2<-vector() 
  
  xxxx1<-as.matrix(xxxx1)
  xxxx1<-matrix(as.numeric(xxxx1),nrow=nrow(xxxx1))
    
  setVariable(matlab, yyyy1=yyyy1)
  yyyy1<- getVariable(matlab, "yyyy1")
    
  setVariable(matlab,xxxx1=xxxx1)
  xxxx1<- getVariable(matlab, "xxxx1")
    
  evaluate(matlab, "mi2=new_mi_matrix(xxxx1,yyyy1);")
  mi2<- getVariable(matlab,"mi2")
  print(mi2)
    
  yyyy1<-as.matrix(unlist(yyyy1[1]))
  xxxx1<-as.matrix(unlist(xxxx1[1])) 
  xxxx1<-matrix(xxxx1,nrow=nrow(yyyy1))
  mi2<-as.matrix(unlist(mi2[1]))
    
  
  eee1<-as.vector(which(rank(mi2)>=(ncol(xxxx1)-n2),arr.ind=T))
    
  xxxxnew1<-X1[,sig2b[eee1]]
    
  cvfit4 <- ncvreg::cv.ncvreg(scale(xxxxnew1), yyyy1, family="gaussian",penalty="SCAD",gamma=3.7,warn=FALSE)
    
  fit4 <- cvfit4$fit
  obj44 <- (as.vector(fit4$beta[,cvfit4$min]))
  obj4<-obj44[-1]
    
   
  sig3a<-which(abs(obj4)!=0)
  sig3c<-sig2b[eee1][sig3a]               #é™å ¥â‚¬å¤Šåš?21æ¶??
      
  sig3b<-sig[-c(sig3c,sig2c)]
     
  siggg<-sort(c(sig2c,sig3c))
     
  total<-unique(sort(c(sigg,siggg)))
 
  k<-length(total)

close(matlab)
# print(matlab)

if(k==1){
  xxxnew11<-matrix(X1[,total],1)
}else{
  xxxnew11<-X1[,total]
}

zz<-matrix()

if (is.null(ps)==TRUE)
{
  zz<-cbind(matrix(1,nrow(X1),1))
}else{
  zz<-cbind(matrix(1,nrow(X1),1),ps)
}

# source("multinormal.R")

u1<-ebayes_EM(zz,xxxnew11,y)
obj5<-u1$u 
result1<-matrix(0,ncol(X1)*1,ncol=1,nrow=ncol(X1))
for (i in 1: k)
{
  result1[(total)[i],1]=obj5[i]
}
Res<- t(as.matrix((rowSums(result1)/ncol(result1))))
Res1<-as.vector(Res)	
kk<-length(which(abs(Res1)>1e-5))    #18ä¸?

if(kk!=0){
  
  sig1<-which(abs(Res1)>1e-5)
  bbo<-matrix(0,kk,1)
  for (i in 1:kk){
    bbo[i,]=Res1[sig1[i]]
  }
  
  her1<-vector(length=kk)
  for (i in 1:kk){
    p1<-length(as.vector(which(X1[,sig1[i]]==1)))/length(X1[,sig1[i]])
    p2<-1-p1
    her1[i]=((p1+p2)-(p1-p2)^2)*(Res1[sig1[i]])^2
  }
  
  if(var(y)>=sum(her1)+u1$sigma2){
    her<-(her1/as.vector(var(y)))*100  
    
  }else{
    her<-(her1/(sum(her1)+u1$sigma2))*100 
  } 
  
  if(length(sig1)!= 0){
    
    if(length(sig1)==1){
      xxxxx<-as.matrix(X1[,sig1])
      
    }else{
      xxxxx<-X1[,sig1]
    }
    
    yn<-as.matrix(y)
    xxn<-zz
    
    lod<-likelihood(xxn,xxxxx,yn,bbo)
    slod<-cbind(sig1,lod,her)
 
    
    if(length(which(slod[,2]>=svlod))>=1){
      
      if(length(which(slod[,2]>=svlod))==1){
        sslod<-t(as.matrix(slod[which(slod[,2]>=svlod),]))
        sig1<-slod[which(slod[,2]>=svlod),1]
      }else if(length(which(slod[,2]>=svlod))>1){
        sslod<-slod[which(slod[,2]>=svlod),]
        sig1<-sslod[,1]
      }
      xxxxx<-as.matrix(X1[,sig1])
      lod<-sslod[,2]                 #lod>3çš„å…±17ä¸?
      her<-sslod[,3]
      
      ii<-as.vector(sig1)
      qqq<-matrix(0,nrow=length(ii),ncol=6)
      qqq[,1]=as.matrix(ii)
      for (j in 1:length(ii)){
        qqq[j,2]=X[1,ii[j]]
        qqq[j,3]=X[2,ii[j]]
        qqq[j,4]=result1[ii[j],]
        
        qqq[j,5]=lod[j]
        qqq[j,6]=her[j]
      }
    }else{
      qqq<-NULL
    }
    
}
# return(list(length=k,cor_feature=sigg,mi_feature=siggg,feature=total,EMEB=kk,lod=lod,final=qqq))
  
  return(list(final=qqq))
}
}





