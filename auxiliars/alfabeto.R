alfabeto<-function(mitexto)
{
  
  # El texto analizado es transcrito en un vector de talla N
  
  texto<-unlist(strsplit(mitexto,""))
  N=length(texto)
  tallagrupo=1
  
  # Cálculo del alfabeto, cada palabra del alfabeto estando constituida por paquetes de letras de talla tallagrupo. Cálculo tambien de las frecuencias en el texto
  # Inicialización del alfabeto con el primer paquete
  
  tallalfabeto=1
  alfabeto=vector("character",tallalfabeto)
  palabra<-texto[1]
  if (tallagrupo>1) {
    for (i in 2:tallagrupo) {
      palabra<-paste(palabra,texto[i],sep="")
    }
  }
  alfabeto[1]=palabra
  
  frecuencia=vector("numeric",tallalfabeto)
  frecuencia[1]=1
  
  # Cálculo del alfabeto :  
  
  for (i in 2:(N-(tallagrupo-1))) {
    palabra<-texto[i]
    if (tallagrupo>1) {
      for (j in (i+1):(tallagrupo+i-1)) {
        palabra<-paste(palabra,texto[j],sep="")
      }
    }
    indicador=0
    for (j in 1:tallalfabeto) {
      if (palabra==alfabeto[j]) {   # si la palabra es reconocida
        frecuencia[j]=frecuencia[j]+1  # su frecuencia es incrementada de 1
        indicador=1
      }
    }
    if (indicador==0) {
      tallalfabeto=tallalfabeto+1   # sino, la nueva palabra es añadida al alfabeto
      alfabeto[tallalfabeto]=palabra
      frecuencia[tallalfabeto]=1
    }
  }
  return(alfabeto)
}