library(SmarterPoland)
powiaty<-getMPpowiaty()


powiat_names <- powiaty$nazwa


city_powiats <-  grepl("^([[:upper:]]|Ą|Ć|Ę|Ł|Ń|Ó|Ś|Ź|Ż)", powiat_names)
powiat_names[city_powiats] <-paste0('m. ', tolower(substring(powiat_names[city_powiats],1,1))  , substring(powiat_names[city_powiats],2))  

str.leneq<-function(s="abcd",k=15,fill=" "){
  n<-nchar(s)
  if (n>=k)
    return(substr(s,1,k))
  do.call(paste0,as.list(c(s,rep(" ",k-n))))
}


sort.str.myOrder<-function(A=c("ade","abc","gih"),myOrderStr= ". abcdefghijklmnopqrstuvwxyzóąćęłńśźżABCDEFGHIJKLMNOPQRSTUVWXYZĄĆĘŁŃÓŚŹŻ",retord=FALSE,kstr=50) {
  myOrder<-as.vector(strsplit(myOrderStr,"")[[1]])                                        
  #myOrder
  match("c",myOrder)                    
  
  Amat<-do.call(rbind,sapply(A,function(v)strsplit(str.leneq(v,k=kstr,fill=" "),"")))
  #Amat
  AmatNum<-as.data.frame(matrix(Vectorize(function(l)match(l,myOrder))(Amat),nrow=dim(Amat)[1],ncol=dim(Amat)[2]))
  #colnames(Amat)                                                
  
  ord<-do.call(order,lapply(colnames(AmatNum),function(cn)AmatNum[,cn]))
  if(retord) return(ord)
  A[ord]
}                      

powiat_names <- powiat_names[-which(powiat_names == 'm. wałbrzych')] 
powiat_names[which(powiat_names == 'm. warszawa')] <- "m. st. warszawa"
powiat_names[which(powiat_names == 'bielski')] <- c("bielski (podlaskie)", "bielski (śląskie)")
powiat_names[which(powiat_names == 'brzeski')] <- c("brzeski (małopolskie)", "brzeski (opolskie)")
powiat_names[which(powiat_names == 'grodziski')] <- c("grodziski (mazowieckie)", "grodziski (wielkopolskie)")
powiat_names[which(powiat_names == 'krośnieński')] <- c("krośnieński (lubuskie)", "krośnieński (podkarpackie)")
powiat_names[which(powiat_names == 'nowodoworski')] <- c("nowodoworski (mazowieckie)", "nowodoworski (pomorskie)")
powiat_names[which(powiat_names == 'opolski')] <- c("opolski", "opolski (lubelskie)")
powiat_names[which(powiat_names == 'ostrowski')] <- c("ostrowski (mazowieckie)", "ostrowski (wielkopolskie)")
powiat_names[which(powiat_names == 'tomaszowski')] <- c("tomaszowski (lubelskie)", "tomaszowski (łódzkie)")
powiat_names[which(powiat_names == 'średzki')] <- c("średzki (dolnośląskie)", "średzki (wielkopolskie)")
powiat_names[which(powiat_names == 'świdnicki')] <- c("świdnicki (dolnośląskie)", "świdnicki (lubelskie)")


powiat_names <- sort.str.myOrder(powiat_names)




