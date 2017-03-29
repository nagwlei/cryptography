source("auxiliars/alfabeto.R")

LZW_como_en_clase <- function(mensaje, dic) {
  texto <-  unlist(strsplit(mensaje, ""))
  #dic = alfabeto(texto)
  auxdic <- dic
  palabra = texto[1]
  cont <- 0
  em = c()
  for (i in 1:(length(texto))) {
    if (any(dic == rep(palabra, length(dic)))) {
      pos <- which(dic == rep(palabra, length(dic)))
      dic[pos] <- paste(palabra, texto[1], sep="")
      em = c(em,(pos-1))
      for (j in 2:length(auxdic)) {
          dic <- c(dic,paste(palabra, auxdic[j], sep=""))
      }
      palabra = texto[i+1]
    } else{
      palabra = paste(palabra,texto[i+1],sep="")
    }
  }
  print(em)
  return(list(dic,em))
}

#SIS KISA SIGLIS GLIGLI GLAGLA LIGLAGLI KI LAGLA SA

#decimals <- c(3,5,11,4)
#m <- sapply(decimals,function(x){ as.integer(intToBits(x))})
#m
