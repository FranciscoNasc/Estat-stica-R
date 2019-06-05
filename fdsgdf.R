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
  x = list[,1]
  y = NULL
  a = 15
  b = 0
  c = 0
  for (i in list[,1]) {
    if (i > c) {
       c = i
    }
  }
  for(j in 1:c){
    u = 1;
    for(k in list[,1]){
      if(j==k){
        if(list[u,3] >= b){
          b = k
        }else{
          a = k
        }
      }else{
        break;
      }
      u = u +1
    }
    print(a)
    print(b)
  }
}
func(got)
