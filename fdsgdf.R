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

#Faça uma função que retorna apenas os nomes dos episódios que possuem notas maiores ou iguais a nove (9).

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

#Faça uma função que retorna o nome dos episódios com menor e maior notas, nessa ordem para cada uma das temporadas. 
#Por fim, faça um dataframe com cada episódio encontrado com as colunas TÍTULO, NOTA, TEMPORADA ordenados de forma 
#crescente por temporada (de 1 até 8).



func = function(list) {
  y = NULL
  a = 0  
  b = 0
  c = 0
  for (i in list[,1]) {##pega o n�mero de temporadas
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
    ##print(a)
    ##print(b)
    ##y = c(y,as.character(list[a,2]))
    ##y = c(y,as.character(list[b,2]))
    y = c(y,a)
    y = c(y,b)
   
  }
  return(y)
}

fi = function(list){
  r = func(list)
  print(r)
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
  print(x)
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
  print(pq)
}



r = funci(got)
r
print(r[2])


n = c(2, 3, 5) 
s = c("aa", "bb", "cc") 

tr = NULL
tr =c(tr,n)
tr =c(tr,s)
tr[1]

got[12,2]

