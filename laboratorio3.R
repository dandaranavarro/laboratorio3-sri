library(tm)
library(FastKNN)
library(SnowballC)

dir_legendas <- "C:/Users/Dandara/Documents/UFCG/SRI/laboratorio3-sri/legendas/"


filmes.names <- dir(dir_legendas)

#mudar para arquivo com todos os generos
label.training <- read.table("C:/Users/Dandara/Documents/UFCG/SRI/laboratorio3-sri/generos1.txt", sep="&")

generos.matrix <- as.matrix(label.training)

#Separando somente os gêneros, pois irei mostrar os gêneros existentes para que o usuário escolha
#qual tipo de filme irá classificar
just.genres <- c(generos.matrix[1:636,2])
just.genres <- unique(just.genres)

#Mostra os gêneros existentes para  usuário
for(i in 1: length(just.genres)) {
  print(paste(i,just.genres[i]))
  
}
answer <- readline(prompt="Qual dos gêneros acima você gostaria de classificar? [1,2,3 ...]")

#genero escolhido
chosen.genre <- just.genres[as.numeric(answer)]

liked.films <- c()

#perguntando quais filmes o usuário gosta (apenas os do mesmo gênero que ele escolheu)
for (i in 1:636){
  if(generos.matrix[i,2] == chosen.genre){
    question <- paste0("Você gosta do filme ", generos.matrix[i],"? [s/n]")
    answer2 <- readline(prompt=question)
    if(answer2=="s"){
      liked.films <- c(liked.films,i)
    }
  }
}

conj.filmes <- c()

#Todos os filmes
for(i in 1:length(filmes.names)){
  filename <- paste(dir_legendas, filmes.names[i], sep="")
  doc <- readChar(filename,file.info(filename)$size)
  conj.filmes <- c(conj.filmes, doc)
}

my.docs <- VectorSource(c(conj.filmes))

# Corpus dos filmes sendo tratado
my.docs.corpus <- Corpus(my.docs)
my.docs.corpus <- tm_map(my.docs.corpus, removePunctuation)
my.docs.corpus <- tm_map(my.docs.corpus, removeWords, stopwords("portuguese"))
my.docs.corpus <- tm_map(my.docs.corpus, stemDocument, language="portuguese")
my.docs.corpus <- tm_map(my.docs.corpus, removeNumbers)
my.docs.corpus <- tm_map(my.docs.corpus, stripWhitespace)

matrix.docs.stm <- DocumentTermMatrix(my.docs.corpus)

matrix.docs <- as.matrix(matrix.docs.stm)

tfidf.matrix.docs = weightTfIdf(matrix.docs.stm)

user.profile <- tfidf.matrix.docs[liked.films, ]
treino <- tfidf.matrix.docs[-liked.films, ]

distance <- Distance_for_KNN_test(user.profile, treino)

recomendacoes <- k.nearest.neighbors(1,distance_matrix = distance, k=10)

for (i in 1:length(recomendacoes)) { 
  print(filmes.names[recomendacoes[i]]) 
}
