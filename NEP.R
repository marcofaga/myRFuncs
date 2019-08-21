#onde x é um vetor numérico de proporções ou absolutos.
#Retorna o NEP a partir do cálculo Laaskso Taagepera ou Rae.
#Ver em https://www.academia.edu/7485835/arquivo_excel_-_template_%C3%ADndice_de_fragmenta%C3%A7%C3%A3o_partid%C3%A1ria

nep <- function(vetor, type = "Laakso-Taagepera") {
  
  x <- vetor/sum(vetor)
  x <- x^2
  
  if(type == "Laakso-Taagepera") {
  
    x <- 1/sum(x)
  
  } else if(type == "Rae") {
    
    
    x <- 1-sum(x)
    
  } else if(type == "HH") {
    
   x <- sum(x) 
    
    
  } else if(type == "Golosov") {
    
    #fórmula: sum(1/(1+((s1^2)/si)-si))
    vetor <- vetor/sum(vetor)
    maxVet <- which(vetor == max(vetor))[1]
    x <- sum(1/(1+((vetor[maxVet]^2)/vetor)-vetor)) 
    
    
    
  }else if(type == "Frag") {
    
    
    f <- 1-sum(x)
    
    if(f == 0){
      
      x <- 1
      
    } else {
      
      vot <- sum(vetor)
      nPart <- length(vetor)
      fmax <- (vot*(nPart-1))/(nPart*(vot-1))
      x <- f/fmax
      
    }
    
  } else {
    
    
    stop("Tipo não Reconhecido - Tem de ser Laakso_Taagepera, HH, Golosov, Rae ou Frag")
    
  }
  
  return(x)
  
  
}