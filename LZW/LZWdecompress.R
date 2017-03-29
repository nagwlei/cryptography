
# dictionary <- c('a', 'b')
# men <- c(0, 1, 2, 4, 3, 6)

# We asume that the in the example above the code of 'a' is 1, the code of 'b' 2 
# and so on
LZWdecompress <- function(dictionary, mensaje) {
  # Entrada: 
  # Dictionary: Array que contiene las palabras del diccionario inicial
  #           (Asumimos que el código es la posición en el array)
  # Mensaje: Array que contiene el mensaje  en numeros
    
  # Devuelve: 
  # Una lista que contiene
  # 1. Output: Contiene lo que se emite (letras)
  # 2. Dic: Contiene el diccionario que se crea
  
  w <- dictionary[mensaje[1]+1]
  output <- c(w)
  dic <- dictionary
  
  # Iterar sobre todos los números del elemento
  for (i in 2:length(mensaje)) {
    # Checkear si el el indice del codigo está en el diccionario
    if (mensaje[i]+1<=length(dic)) {
      tmp <- w
      w <- dic[mensaje[i]+1]
      letter <- substr(w, 1, 1)
      entry <- paste(tmp, letter, sep ='')
      dic <- c(dic, entry)
    } else {
      letter <- substr(w, 1, 1)
      entry <- paste(w, letter, sep ='')
      dic <- c(dic, entry)
      w <- dic[mensaje[i]+1]
    }
    
    output <- c(output, w)
  }
  
  return(list(output, dic))
  
}