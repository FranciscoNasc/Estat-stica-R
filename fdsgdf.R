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

got[,2]

fun = function(foda) {
  x = foda[,3]
  u = 1
  for (i in x) {
    if (foda[u,3] >= 9.0) {
      triste = unlist(strsplit(as.character(foda[u,2]),"73")
      print(triste[1])
    }
    u = u +1
  }
}

fun(got);