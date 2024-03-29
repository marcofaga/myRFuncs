#fun��o que cria um data.frame de dicion�rio das tabelas.
# vers�o 2.1

cDic <- function(tab, descr, fonte, max = 10) {
  
  #checa se o nome das vari�veis tem um n de caracteres menor ou igual a 10
  names <- nchar(names(tab))
  
  if(any(names > max)) {
    
    pos <- which(names > max)
    print(pos)
    varsProb <- paste(names(tab)[pos], "\n", sep="", collapse = " ")
    
    msgErr <- paste("As vari�veis:\n\n", 
                    varsProb, 
                    "\npossuem n�mero de caracteres maior que 10.")
    
    stop(msgErr)
    
    
  }
  
  #checa se text ou fonte � um character
  
  if(class(descr) != "character" | 
     class(fonte) != "character") {
    
    
    stop("Descri��o ou fonte precisam ser vetores de caracteres")
  
  }
  
  nrow <- length(names)
  
  if(length(descr) != nrow) {
    
    stop(paste("Voc� precisa inserir ", nrow, "descri��es"))
    
  }
  
  #montando o dic
  pos <- c(1:nrow)
  vars <-  names(tab)
  tipo <- unname(sapply(tab, class))
  
  x <- 1
  cont <- c()
  
  for(each in tipo) {
    
    if(each %in% c("numeric", "integer")) {
      
      maximo <- max(tab[,x], na.rm = TRUE)
      minimo <- min(tab[,x], na.rm = TRUE)
      
      str <- paste("min: ", minimo, "/ max: ", maximo, sep="")
      
    } else {

      conteudo <- pull(unique(tab[,x]))
      
      if(length(conteudo) < 100) {
        
        conteudo <- paste(conteudo, collapse = "/ ")
      
      } else {
        
        conteudo <- paste(conteudo[c(1:100)], collapse = "/ ")
        
      }
      nchar <- nchar(conteudo)
      
      if(nchar > 47) {
      
        conteudo <- substr(conteudo, 1, 47)
        conteudo <- paste(conteudo, "...", sep="")
      
      }
      
      str <- conteudo
      
    }
    

    x <- x + 1
    cont <- c(cont, str)
    
    
  }

  
  dic <- tibble(pos = pos,
                vars = vars,
                descr = descr,
                tipo = tipo,
                content = cont,
                fonte = fonte) 
  
  
  return(dic)
  
  
}