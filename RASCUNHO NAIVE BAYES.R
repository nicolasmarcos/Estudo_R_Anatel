## caminhos alternativos

unique(df_anatel$CanalEntrada)
unique(df_anatel$GrupoEconNorm)
unique(df_anatel$Condicao)
unique(df_anatel$Tipo)
unique(df_anatel$Servico)
unique(df_anatel$Modalidade)
sum(df_anatel$Motivo == "acessibilidade")
length(unique(anatel_compl$Motivo))

## substituindo motivo por Motivo_Aglutinado a taxa de acerto caiu para 42,59%

modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + CanalEntrada + Tipo + Servico + Modalidade + Motivo_Aglutinado + QtdeSolic, anatelTreino)

## retirando a variavel modalidade a performance ficou em 44,18%

modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + CanalEntrada + Servico + Motivo + Motivo_Aglutinado + QtdeSolic, anatelTreino)

## retirando a variavel tipo a performance ficou em 45,60%

modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + CanalEntrada + Servico + Modalidade + Motivo + Motivo_Aglutinado + QtdeSolic, anatelTreino)

## retirando a variavel QtdeSolic e substituindo motivo por Motivo_Aglutinado a performance continua estavel em 77,99%
## porem o numero de acertos em reabertura cai drasticamente

modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + CanalEntrada + Tipo + Servico + Modalidade + Motivo_Aglutinado, anatelTreino)


## outras tentativas que ficaram na mesma margem de variacao (entre 76 e 78%) sem a variavel QtdeSolic

## retirando canal de entrada e o tipo 

modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + Servico + Modalidade + Motivo + Motivo_Aglutinado, anatelTreino)

## retirando modalidade, canal de entrada, motivo e o tipo

modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + Servico +  Motivo_Aglutinado, anatelTreino)

## retirando modalidade e servico

modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + CanalEntrada + Tipo + Motivo + Motivo_Aglutinado, anatelTreino)

## retirando modalidade, servico e CanalEntrada

modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + Tipo + Motivo + Motivo_Aglutinado, anatelTreino)

## testes com todas variaveis

## A taxa de acerto com todas as variaveis (exceto data, ano, mes, uf) ficou em 46,28%
## retirando modalidade houve aumento para 44,47%
## retirando tipo foi para 46%

## testes com a retirada da variavel QtdeSolic

## retirando a variavel QtdeSolic a performance fica emtorno de 78%
## retirando tipo e QtdeSolic a performace sobe para 72,77% porem o numero de acertos em reabertura cai em torno 
## de 15%
## retirando qualquer outra variavel restante a performace aumenta para em torno de 76% porem o numero de acertos em
## reabertura cai bruscamente

## nitidamente a variavel QtdeSolic altera a performance do modelo

## tentando melhorar o modelo analisando casos especificos sem a variavel QtdeSolic tendo como base fixa os grupos economicos

## deixando somente o grupo e uma variavel servico a performace fica em 77% porem o acerto em reabertura é minimo
## e isso ocorre para todas as outras combinacoes. Ficam vaiando entre 70 e 73% com pouco acerto em reabertura

modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + CanalEntrada, anatelTreino)
modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + Servico, anatelTreino)
modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + Modalidade, anatelTreino)
modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + Motivo, anatelTreino)
modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + Fator_motivo, anatelTreino)
modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + QtdeSolic, anatelTreino)
modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + QtdeSolic, anatelTreino)

## Problemas da variavel "QtdeSolic" no funcionamento do naive bayes


## A limitação do Naive Bayes a suposição de preditores independentes. Na vida real é quase impossível que ter 
## um conjunto de indicadores que sejam completamente independentes. por este motivo que devemos dar importancia 
## no pré-processamento de dados e na seleção dos recursos antes de aplicar o algoritmo Naive Bayes.

