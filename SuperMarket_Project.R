### Projeto com finalidade de estudo de modelos de Regressão Linear

# Dataset de Lojas de Supermercados

# Baixando e carregando pacotes
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","Rcpp","equatiomatic", 'data.table')

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


#Visualizando diretório
getwd()

#Carregando e visualizando os dados
df <- read.csv('Stores.csv')
glimpse(df)
dim(df) # Shape do nosso dataset
View(df)


#Transformando a Variável Id para char
df$Store.ID <- as.character(df$Store.ID)
summary(df)


###Tratamento das variáveis

#Verificando valores NA
sum(is.na(df)) # Não há valores NA
any(sapply(df, is.na)) # Outra maneira

#Valores duplicados
sum(duplicated(df)) # Não há observações duplicadas
any(duplicated(df)) # Outra maneira


#Transformar a coluna Store.ID no index do dataset
head(df)
df <- column_to_rownames(.data = df, var = 'Store.ID')
head(df)
View(df)

# Visualizando todas as variáveis por boxplot
par(mfrow = c(2,2))

boxplot(df$Store_Area, main='Store Area')
boxplot(df$Items_Available, main= 'Items Available')
boxplot(df$Daily_Customer_Count, main = 'Daily Customer Count')
boxplot(df$Store_Sales, main= 'Store Sales')

# Outra opção de visualização usando a função gather do pacote tidyr
df_long <- gather(data=df, key = 'Variaveis', value = 'Valores')
View(df_long)

ggplotly(
  ggplot(data = df_long, aes(x=Variaveis, y = Valores)) +
    geom_boxplot(aes(x = Variaveis, y = Valores), color = 'black', fill = 'darkgreen', outlier.color = 'red')+
    labs(x= 'Variaveis', y = 'Valores', title = 'Boxplots de todas as variáveis do dataset')+
    facet_wrap(facets = 'Variaveis', scale = 'free')
)

#Notou-se a presença de alguns poucos outliers. Irei remove-los usando o método da distancia interquartil (IQR)
summary(df)



#Função para detectar os outliers
detect_outlier <-  function(x) { # x vai ser os valores de uma coluna
  
  # Primeiro quartil
  q1 <-  quantile(x, probs=0.25)
  
  # terceiro quartil
  q3 <-  quantile(x, probs=0.75)
  
  # calculate inter quartile range
  IQR = q3-q1
  
  # retorna true or false
  x > q3 + (IQR*1.5) | x < q1 - (IQR*1.5)
}



# Função para remover os outliers
remove_outlier <-  function(df,
                            columns=names(df)) {
  
  # looping nas variáveis do dataset
  for (col in columns) {
    
    # remove observaçoes que foram consideradas outliers pela funçãao criada previamente
    df <-  df[!detect_outlier(df[[col]]), ]
  }
  clean_df <- df
  # retorna o dataset limpo
  print("Outliers removidos")
  return(clean_df)
}

# Aplicando a função no dataset
clean_df <- remove_outlier(df)

# Verificando se os outliers foram removidos
sort(clean_df$Daily_Customer_Count, partial = 1:1)[1:1] #Foram!

# Verificando o plot do novo dataset
clean_df_long <- gather(clean_df, key = 'Variaveis', value = 'Valores')

ggplotly(
  ggplot(data = clean_df_long, aes(x=Variaveis, y = Valores)) +
    geom_boxplot(aes(x = Variaveis, y = Valores), color = 'black', fill = 'darkgreen', outlier.color = 'red')+
    labs(x= 'Variaveis', y = 'Valores', title = 'Boxplots de todas as variáveis do dataset')+
    facet_wrap(facets = 'Variaveis', scale = 'free')
) # Dataset sem outliers!!!

