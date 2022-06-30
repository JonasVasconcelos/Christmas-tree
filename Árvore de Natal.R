##############
### Arvore de Natal do Estatisticamente_Falando
##############

# Funcao das camadas do pinheiro
Triangulo <- function(Topo, Altura, Largura, Cor, Cores) {
  p1 <- c(Topo[1] - (Largura/2), Topo[2] - Altura)
  p2 <- c(Topo[1] + (Largura/2), Topo[2] - Altura)
  
  lines(c(p1[1], p2[1]),
        c(p1[2], p2[2]),
        lwd = 2)
  lines(c(p1[1], Topo[1]),
        c(p1[2], Topo[2]))
  lines(c(p2[1], Topo[1]),
        c(p2[2], Topo[2]))
  
  polygon(c(Topo[1], p1[1], p2[1]),
          c(Topo[2], p1[2], p2[2]),
          col = Cor)
  
  LuzesX <- seq(p1[1], p2[1], 4)
  points(LuzesX,
         rep(p1[2], length(LuzesX)),
         pch = 8, col = Cores, cex = 1.2)
  points(LuzesX,
         rep(p1[2], length(LuzesX)),
         pch = 16, col = Cores, cex = 1)
}

# Funcao do caule
Caule <- function(AlturaCaule, LarguraCaule, CorCaule) {
  lines(c(Topo[1], Topo[1]),
        c(0, AlturaCaule),
        lwd = LarguraCaule,
        col = CorCaule)
}

x <- 100
y <- 2 * x

plot(0:x, 0:x,
     xlab = '', ylab = '',
     xlim = c(0, x), ylim = c(0, y),
     frame.plot = F, axes = F, ty = 'n')

Camadas <- 5
Topo <- c(x/2, (y/Camadas)*2)
Altura <- 50
Largura <- Altura
Cor <- '#336600'

LarguraCaule <- 20
AlturaCaule <- 50
CorCaule <- '#666633'

Cores <- c('#66CCFF', '#FFFF00',
           '#FF0000', '#0066CC')

Caule(AlturaCaule, LarguraCaule, CorCaule)

for(i in 1:Camadas){
  Triangulo(Topo, Altura, Largura, Cor, Cores)
  Topo[2] <- Topo[2] + (Altura/2)
  Largura <- Largura * 0.8
}


