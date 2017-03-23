rm(list=ls())
library ('plot3D')
library ('boxplotdbl')


rm(list=ls())
library("corpcor") #pacote para achar a matriz H'->pseudoinversa
#terei que achar o polinomio que gerou os pontos
#o problema consiste ein w tal que:
#Hw=y
#w=H`y

#geraracao dos dados
#n?mero de amostras/pontos
N<-200
#grau m?ximo de polin?mios
p<-10
#n?mero de simula??es
nsim<-20
#numero de amostragem do erro
amostras<-50

#declara??o de vari?veis
varerro<-matrix(nrow=nsim,ncol=p)
erroaux<-matrix(nrow=nsim,ncol=p)
erro<-matrix(nrow=amostras,ncol=1)


H<-1
Hgrid<-1
ns<-0
for (i in (1:nsim)){
  ns<-ns+1
  np<-0
  for (j in (1:p)){
    x<-runif(n = N, min=-15,max=10)
    xgrid<- seq(-15,25,40/(N-1)) #os valores com grid sao valores ordenados sem ruido
    yr<-(0.5*(x^2)+3*x+10) + 10*rnorm(length(x))
    ygrid<-(0.5*(xgrid^2)+3*xgrid+10)
  
    
    #aproximacao de mais graus
    H<-cbind(H,x^np)
    w<-pseudoinverse(H) %*% yr
    
    
    #plotando a projecao H
    
    #aproximacao
    Hgrid<-cbind(Hgrid,xgrid^np)
    yhatgrid <- Hgrid %*% w      #e' a projecao com ruido, usei os valores xgrid e ygrid com
    #os valores de w encontrados
      
    #calculando o erro
    for (i in 1:amostras){
      am<-round(runif(n=1,min=1,max=200))
      erro[i,1]<- yr[am]-yhatgrid[am]
      }
    np<-np+1
    erroaux[ns,np]<-(t(erro)%*%erro)/amostras
    varerro[ns,np]<-var(erro)
    }
  
}
 
 boxplot((erroaux),col = "indianred1",xlab="Grau do Polinômio (p)",ylab="Erro médio quadrático")
 boxplot((varerro),col = "lightslateblue",xlab="Grau do Polinômio (p)",ylab="Variância do erro")

 
 