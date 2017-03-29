source("auxiliars/alfabeto.R")

# msg <- 'banana_bandana'

LZWcompress <- function(msg, dic) {
  # Entrada: 
  # Dictionary: array que contiene las palabras del diccionario 
  #           (Asumimos que el código es la posición en el array)
  # Mensaje: String con el mensaje
  
  # Devuelve:
  # Una lista que contiene:
  # 1. Output: Contiene lo que se emite (números)
  # 2. Dic: Contiene el diccionario que se crea
  
  w <- ''
  text <-  unlist(strsplit(msg, ""))
  #dic <- alfabeto(text)
  
  output <- c()
  
  # Iterar sobre todas las letras del mensaje
  for (i in 1:length(text)) {
    aux <- paste(w, text[i], sep ='')
    
    isIn <- FALSE
    for (k in 1:length(dic)) {
      if (identical(dic[k], aux)) {
        isIn <- TRUE
      }
    }
    
    # Checkear si w + la actual letra está en el alfabeto
    if (isIn) {
      # Añadimos la letra a w
      w <- paste(w, text[i], sep='')
    } else {
      # Buscar el código numerico de w y enviarlo
      num <- 0
      for (k in 1:length(dic)) {
        if (identical(dic[k], w)) {
          num <- k
        }
      }
      output <- c(output, (num-1))
      
      # Añadir w + la actual letra alfabeto
      dic <- c(dic, aux)
      
      # Actualizar w a la letra actual
      w = text[i]
    }
  }
  
  # Este código es necesario, para enviar aquellas letras que se han 
  # quedado en w y no se han enviado, porque el mensaje se ha acabado
  num <- 0
  for (k in 1:length(dic)) {
    if (identical(dic[k], w)) {
      num <- k
    }
  }
  output <- c(output, (num-1))
  
  return(list(output, dic))
}