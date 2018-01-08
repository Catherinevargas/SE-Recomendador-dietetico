### LÓGICA DIFUSA EN R SISTEMA EXPERTO  RECOMENDADODR PARA PERDER PESO  ###
recomendadorparabajardepesoTSK=function(dieta,ejercicio){
  
ligera  = c(1, 3, 10,20);
moderada = c(18, 20, 25, 30);
intensa  = c(28, 32, 38, 40);

poco = c(1, 2, 3, 5);
regular = c(4, 6, 8, 10);
mucho = c(9, 12, 13, 15);
  
pierdermuchoapeso          = 30;
pierderaregularpeso   = 10;
perderapocopeso       = 5;
perderamuypocopeso = 2;
noperderapeso        = 0;

mu_ligera  = calcula_Mu(ligera,dieta);
mu_moderada = calcula_Mu(moderada,dieta);
mu_intensa  = calcula_Mu(intensa,dieta);

mu_poco    = calcula_Mu(poco,ejercicio);
mu_regular = calcula_Mu(regular,ejercicio);
mu_mucho   = calcula_Mu(mucho,ejercicio);

basereglas = matrix(c(Y(mu_ligera,mu_poco),noperderapeso,
                      Y(mu_ligera,mu_regular),perderamuypocopeso,
                      Y(mu_ligers,mu_mucho),perderapocopeso,
                      Y(mu_moderada,mu_poco),perderapocopeso,
                      Y(mu_moderada,mu_regular),pierderaregularpeso,
                      Y(mu_moderada,mu_mucho),pierdermuchoapeso,
                      Y(mu_intensa,mu_poco),pierderaregularpeso,
                      Y(mu_intensa,mu_regular),pierdermuchoapeso,
                      Y(mu_intensa,mu_mucho),pierdermuchoapeso),nrow=9,ncol=2,byrow=TRUE)


antecedentes = basereglas[,1];
consecuentes = basereglas[,2];

salida = sum(antecedentes*consecuentes)/sum(antecedentes);
print(paste0('dieta=',dieta,' y ejercicios=',ejercicio,' -> Seleccionar=',salida))
}

Y = function(valor1,valor2){
  salida = min(valor1,valor2);
}

calcula_Mu = function(difuso,valor){
  a = difuso[1];
  b = difuso[2];
  c = difuso[3];
  d = difuso[4];
  
  if      (valor<a){
    salida=0;
  }
  else{
    if  (valor<b){
      salida = (valor-a)/(b-a);
    }
    else{ 
      if  (valor<c){
        salida = 1;
      }
      else{
        if  (valor<d){
          salida = (d-valor)/(d-c);
        }
        else{
          salida = 0;
        }
      }
    }
  }  
  
  
  
}

