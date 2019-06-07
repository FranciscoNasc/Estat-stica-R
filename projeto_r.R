#questao 1
got = read.csv("planilha.csv",header = TRUE)
got

#questao 2

#media,desvio padrao e moda das notas dos episodios

mean(got[,3])
sd(got[,3])
freq= table(got$Nota)
names(table(got$Nota))[table(got$Nota) == max(table(got$Nota))]

#questao 3

#media desvio padrao e a mediana da audiencia dos episodios

mean(got[,5])
sd(got[,5])
median(got[,5])

#questao 4

#FaÃÂÃÂ§a uma funÃÂÃÂ§ÃÂÃÂ£o que retorna apenas os nomes dos episÃÂÃÂ³dios que possuem notas maiores ou iguais a nove (9).

fun = function(list) {
  x = list[,3]
  u = 1
  popeye = NULL
  for (i in x) {
    if (list[u,3] >= 9.0) {
      triste = as.character(list[u,2])
      popeye = c(popeye,triste)
                      
    }
    u = u +1
  }
  return (popeye)
}

fun(got);

#questao 5

#FaÃÂÃÂ§a uma funÃÂÃÂ§ÃÂÃÂ£o que retorna o nome dos episÃÂÃÂ³dios com menor e maior notas, nessa ordem para cada uma das temporadas. 
#Por fim, faÃÂÃÂ§a um dataframe com cada episÃÂÃÂ³dio encontrado com as colunas TÃÂÃÂTULO, NOTA, TEMPORADA ordenados de forma 
#crescente por temporada (de 1 atÃÂÃÂ© 8).



func = function(list) {
  y = NULL
  a = 0  
  b = 0
  c = 0
  for (i in list[,1]) {##pega o nÃÂºmero de temporadas
    if (i > c) {
       c = i
    }
  }
  
  for(j in 1:c){
    a1 = 15
    b1 = 0
    u = 1;
    for(k in list[,1]){
      if(j==k){
        if(list[u,3] > b1){
          b1 = list[u,3]
          b = u
        }
        if(list[u,3] < a1){
          a1 = list[u,3]
          a = u
        }
      }
      u = u +1
    }
    y = c(y,a)
    y = c(y,b)
   
  }
  return(y)
}

fi = function(list){
  r = func(list)
  x = length(r)
  y = NULL
  for(i in  1:x){
    y = c(y,as.character(list[r[i],2]))
  }
  return(y)
}

qw = fi(got)
qw



funci = function(list){
  r = func(list)
  titulo = NULL
  nota = NULL
  temporada = NULL
  x = length(r)
  v = NULL
  for(i in 1:x){
    titulo = c(titulo,as.character(list[r[i],2]))
  }
  for(i in 1:x){
    nota = c(nota,as.character(list[r[i],3]))
  }
  for(i in 1:x){
    temporada = c(temporada,as.character(list[r[i],1]))
  }
  pq = data.frame(titulo,nota,temporada)
}



r = funci(got)
r

#Questao 6
#FaÃÂ§a uma funÃÂ§ÃÂ£o que retorne qual a temporada com o menor desvio padrÃÂ£o na audiÃÂªncia.

funct = function(list) {
  tempnumber = 0
  for (i in list[,1]) {
    if (i > tempnumber) {
      tempnumber=i
    }
  }
  desvios = NULL
  for(x in 1:tempnumber) {
    portemp = NULL
    indice = 1
    for (i in list[,5]) {
      if (x == list[indice,1]) {
        a = list[indice,5]
        portemp = c(portemp,a)
      }
      indice = indice+1
    }
    toadd = sd(portemp)
    desvios = c(desvios,toadd)
  }
  qualatemp = 0
  menordesvio = desvios[1]
  lala = 0
  for(i in desvios) {
    qualatemp = qualatemp+1
    if (i<menordesvio) {
      menordesvio = i
      lala = qualatemp
    }
  }
  return(lala)
}

funct(got)


##Questao 7:

tl = function(nome,list){
  if(grepl("Brienne of Tarth",as.character(nome))){
    nome = "Brienne of Tarth"
  }
  x = length(list[,4])
  resp = NULL
  for(i in 1:x){
    if(grepl(nome,as.character(list[i,4]))){
      resp = c(resp,list[i,3])
    }
  }
  ##print(resp)
  return(mean(resp))
}

nome = "Brienne of Tarth(Gwendoline Christie)"
tl(nome,got)


#questao 8
#FaÃ§a uma funÃ§Ã£o que retorne uma lista com os personagens que sÃ³ apareceram em um 
#Ãºnico episÃ³dio na quarta (4Âª) temporada.


qua = function(list){
  v = NULL
  x = length(list[,1])
  for(i in 1:x){
    if(list[i,1] == 4){
      v = c(v,unlist(strsplit(as.character(list[i,4]),",")))
      l = table(v)
    }
  }
  tu = as.data.frame(l)
  
  s = length(tu[,2])
  resp = NULL
  for(k in 1:s){
    if(tu[k,2] == 1){
      resp = c(resp,as.character(tu[k,1]))
    }
  }

  
  return(resp)
}
g = qua(got)
g




##Questao 9 :FaÃÂ§a uma funÃÂ§ÃÂ£o que dado o nome de um personagem, cria um histograma 
##onde mostra a frequÃÂªncia de apariÃÂ§ÃÂ£o desse personagem a cada temporada.
##NÃÂ£o esqueÃÂ§a de dar um tÃï¿½tulo e fazer ele de forma colorida, facilitando a visualizaÃÂ§ÃÂ£o.
##Um exemplo para o personagem Bran Stark(Isaac Hempstead) seria:


nome = "Bran Stark"

tyr = function(a,b){
  resp = NULL
  f = 1
  g = 0
  s = length(b[,4])
  for(i in 1:s){
    
    if(grepl(nome,b[i,4])){
      resp = c(resp,b[i,1])
    }
  }
  
  c = 0
  for (i in b[,1]) {##pega o nÃÂºmero de temporadas
    if (i > c) {
      c = i
    }
  }
  
  return(hist(resp,main = nome,xlab = "temporada" ,ylab = "ocorrencia",breaks = c(0,1,2,3,4,5,6,7,8),freq = TRUE,xlim =c(0,c), ylim = c(1,8),col = "green",border = "black"))
}

t = tyr(nome,got)
t




