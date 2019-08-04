
## instalação dos pacotes

install.packages("e1071")

## import

library(e1071)

## colocar as referências

# IMPORTANDO O DATASET EM .CSV

# A EQUIPE REALIZOU O MERGE DOS DATASETS EM AMBIENTE LINUX (TRATANDO O LOCALE PARA pt_BR) E RETIRANDO OS ESPAÇOS QUE VEM NOS ARQUIVOS ORIGINAIS.
# NÃO FORAM RETIRADOS OU INCLUSOS DADOS, OU SEJA, SOMENTE ADEQUAÇÃO A FIM DE GARANTIR A IMPORTAÇÃO DE DADOS LIMPOS DE FORMA BEM SUCEDIDA NO R.

Anatel <- read_delim("arquivao.csv", ";", 
                     escape_double = FALSE, col_types = cols(DataExtracao = col_date(format = "%Y-%m-%d")), 
                          locale = locale(encoding = "ISO-8859-1"), 
                          trim_ws = TRUE)

# VISUALIZANDO O DATASET RECENTEMENTE IMPORTADO
View(Anatel)

# TESTANDO O DATASET (RECENTE) A FIM DE VERIFICAR SE NÃO HÁ REPETIÇÃO DE 'HEADER' AO LONGO DO DF.
unique(Anatel$Condicao)

# CRIAÇÃO DE UM DATAFRAME PARA MANIPULAÇÃO DOS DADOS PELA EQUIPE

Anatel_Consolidado<-Anatel

# VISUALIZAÇÃO DO NOVO DATAFRAME CRIADO
View(Anatel_Consolidado)

# COMANDOS PARA CRIAR (O CAMPO 'Motivos_Aglutinados') E 'AGLUTINAR' OS MOTIVOS #
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cancelamento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Serviços Adicionais" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Bloqueio" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fistel" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Indevido"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de Acessos Individuais" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Indevido"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Atendimento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Reparo" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Clonagem" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Perda e Roubo"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Completamento de Chamadas" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Código de acesso" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Habilitação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Planos de serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Área de Cobertura" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Outros" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Outros"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Desbloqueio" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Recusa de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Mudança de endereço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Recusa de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Solicitação (ID)" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Recusa de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cartão pré-pago" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Inclusão no SPC/Serasa" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Indevido"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Promoções" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Código de acesso" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Recusa de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Bloqueio" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cartão Telefônico" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Licenciamento e instalação" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Tarifas" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Interferência" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Transferência de titularidade" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Recusa de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Outros" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Outros"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Licença / Autorização" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Central de Atendimento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade do atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Central de Atendimento" & Anatel_Consolidado$Tipo == "Elogio" ] <- "Qualidade do atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Planos de serviço" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Atuação da Anatel" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Anatel"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Rede telefônica" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Legislação" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Tarifas" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Códigos não-geográficos" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Regulamentação" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Atendimento" & Anatel_Consolidado$Tipo == "Elogio" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de Telefone de Uso Público - TUP" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Solicitação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Prestadoras" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fale Conosco" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Canais de relacionamento" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cartão pré-pago" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Lista telefônica" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Programação" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Outorga" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Código de acesso" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Dados cadastrais" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade do atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cópia de Documentos" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade do atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Antenas/ERBs" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade da rede"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Desbloqueio" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Recusa de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Outros" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Outros"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Área de Cobertura" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Atendimento" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Produtos" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Atendimento" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cartão Telefônico" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Outorga" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Atuação da Anatel" & Anatel_Consolidado$Tipo == "Elogio" ] <- "Elogio"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Serviços Adicionais" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Códigos não-geográficos" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Freqüência/Potência das emissoras" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Interferência" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "TV Analógica x TV Digital" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Serviços Adicionais" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Dados cadastrais" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Dados cadastrais" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Interrupções no Serviço de Localidades" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Reparo" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Central de Atendimento" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Outros" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Outros"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fale Conosco" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Anatel"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Programação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de TUP" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Antenas/ERBs" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Prestadores de Serviço" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Sala do Cidadão" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Anatel"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Contrato de prestação de serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Jurídico"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Promoções" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Reparo" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Licença / Autorização" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Ações" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Ações" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Habilitação" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Rádio Pirata / Clandestina" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cartão Telefônico" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Atuação da Anatel" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Completamento de Chamadas" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fundamentação Legal" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Serviço de Valor Adicionado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Clonagem" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Perda e Roubo"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Tarifas" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Auxílio à Lista" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de Acessos Individuais" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Serviço de Valor Adicionado" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Habilitação" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Interrupções no Serviço de Localidades" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Lista telefônica" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Contrato de prestação de serviço" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Promoções" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Transferência de titularidade" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Mudança de endereço" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Atuação da Anatel" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Planos de serviço" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Prestadoras" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Canais de relacionamento" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Equipamento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Inclusão no SPC/Serasa" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Provedor de acesso" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Licença / Autorização" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Canais de relacionamento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Definição" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Link Anatel" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Interferência" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fistel" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fust" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Contribuições" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Freqüência/Potência das emissoras" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Captação de sinais via satélite" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Produtos" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Plano Básico de Rádio" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Certificação / Homologação" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Prestadoras" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Prestadores de Serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Cancelamento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Acompanhamento" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cópia" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Solicitação (ID)" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Provedor de acesso" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Central de Atendimento" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Regulamentação" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Programação" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Completamento de Chamadas" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Clonagem" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Perda e Roubo"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Prestadores de Serviço" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Freqüência/Potência das emissoras" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Licença / Autorização" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Captação de sinais via satélite" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Outros" & Anatel_Consolidado$Tipo == "Elogio" ] <- "Outros"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Inclusão no SPC/Serasa" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cartão pré-pago" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Laudo técnico" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Jurídico"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Canais de relacionamento" & Anatel_Consolidado$Tipo == "Elogio" ] <- "Elogio"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de Acessos Individuais" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Bloqueio" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cópia de Documentos" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de TUP" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "TV Analógica x TV Digital" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Resposta de solicitação" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Problemas Técnicos" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Outorga" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Licenciamento e instalação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Provedor de acesso" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Auxílio à Lista" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Recepção do sinal" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Programação" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Outorga" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Certificação / Homologação" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Documentos emitidos pela Anatel" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Jurídico"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Viabilização do canal" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Auxílio à Lista" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Produtos" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Correspondência" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Serviço de Valor Adicionado" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Desbloqueio" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Recusa de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cópia de Documentos" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Documentos emitidos pela Anatel" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Correspondência" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Certificação / Homologação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Link Anatel" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Frequência" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Sala do Cidadão" & Anatel_Consolidado$Tipo == "Elogio" ] <- "Elogio"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Plano Básico de TV" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Produtos" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Contrato de prestação de serviço" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Provedor de acesso" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Regulamentação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fale Conosco" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Antenas/ERBs" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Plano Básico de TV" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Atendimento" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Lista telefônica" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Acompanhamento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Bloqueio" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Reparo" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Área de Cobertura" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Plano Básico de Rádio" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Link Anatel" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Mudança de endereço" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Licitação" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Licenciamento e instalação" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Acompanhamento" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Viabilização do canal" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Recepção do sinal" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cópia" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Legislação" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Equipamento" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Certificação / Homologação" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Problemas Técnicos" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Laboratórios de Ensaios" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Sala do Cidadão" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Contribuições" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Códigos não-geográficos" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Resposta de solicitação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Correspondência" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Interferência" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Captação de sinais via satélite" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Ações" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Solicitação (ID)" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Problemas Técnicos" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Definição" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Serviços Adicionais" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Legislação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Cancelamento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fust" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Funttel" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Serviço de Valor Adicionado" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Equipamento" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Equipamento" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Transferência de titularidade" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Sala do Cidadão" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Contribuições" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Frequência" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Viabilização do canal" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Organismos de Certificação Designados - OCD's" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Contrato de prestação de serviço" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Jurídico"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Interferência" & Anatel_Consolidado$Tipo == "Elogio" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Regulamentação" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Interrupções no Serviço de Localidades" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Alteração" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Organismos de Certificação Designados - OCD's" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Interrupções no Serviço de Localidades" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "STFC" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança - Co-billing" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Rede telefônica" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "TV Analógica x TV Digital" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Rádio Pirata / Clandestina" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Alteração" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Recepção do sinal" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Documentos emitidos pela Anatel" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Plano de Referência RADCOM" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fundamentação Legal" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Viabilização de canal" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Aplicação do FUST" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Viabilização de canal" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Plano de Referência RADCOM" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Licença / Autorização" & Anatel_Consolidado$Tipo == "Elogio" ] <- "Elogio"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Conselho de Usuários" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Rádio Comunitária (Outorga)" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Conselho de Usuários" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Mudança de endereço" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Tarifas" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Legislação" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Prestadoras" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Frequência" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Definição" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Inclusão no SPC/Serasa" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Funttel" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fistel" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fistel" & Anatel_Consolidado$Tipo == "Elogio" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Desbloqueio" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Recusa de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Dados cadastrais" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Produto não certificado / homologado pela Anatel" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fundamentação Legal" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Licitação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cadastramento de Pessoa Jurídica" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cópia" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Rádio Pirata / Clandestina" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Habilitação" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Serviço"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Programa de Medição da Qualidade da Banda Larga Fixa" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Sistemas" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Organismos de Certificação Designados - OCD's" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "SIC" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Sistemas" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "SMP" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Laboratórios de Ensaios" & Anatel_Consolidado$Tipo == "Denúncia" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação inadequada do serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Alteração" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Produto não certificado / homologado pela Anatel" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Gravação" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "COQL" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Demandas de Órgãos e Entidades Relativas a Consumidores" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Solicitações de Audiência/Intimações Relativas a Consumidores" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Demandas de Órgãos e Entidades Relativas a Consumidores" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança de serviço, produto ou plano contratado, mas não fornecido" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cadastramento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Oferta com informações divergentes, incompletas ou omissas" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "RGQ - Regulamento de Gestão de Qualidade" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Certificação / Homologação" & Anatel_Consolidado$Tipo == "Elogio" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Ressarcimento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Automático" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Anatel"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Oferta" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Acompanhamento de Processo" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Verificação de Entidade" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue cancelar o serviço por meio do atendente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança de valores ou taxas não informadas anteriormente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança de serviço, produto ou plano não contratado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Sem Conexão" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Linha muda ou sem sinal" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue cancelar o serviço no menu da central de atendimento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Reparo não realizado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Mudança de endereço não realizada até o momento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Lentidão ou velocidade reduzida de conexão" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança de serviços adicionais não contratados" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Condições contratuais alteradas sem prévio aviso" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Jurídico"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue desbloquear o serviço a pedido" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Dados do Setor de Telecomunicações" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Problema nos dados cadastrais do consumidor junto à prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Web" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Crédito pré-pago cobrado de maneira indevida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança de valores que já foram pagos" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "instalação ou habilitação não realizada até o momento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Fornecimento de informações divergentes, incompletas ou omissas" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não funcionamento de facilidade ou serviço adicional" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Crédito que foi pago, mas não inserido" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Sem imagem ou sem sinal" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Bloqueio ou suspensão indevido" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Mudança de endereço não efetuada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento efetuado fora do prazo" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança em desacordo com o contratado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não entrega do documento de cobrança" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue alterar o plano de serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança indevida de multa por fidelização (multa rescisória)" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança de ligações com valor incorreto" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não fornecimento de serviço adicional" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não reativação de linha ou serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Queda de conexão" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Interrupção do serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Promoção incluída ou alterada indevidamente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue desbloquear aparelho" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Inclusão indevida ou não exclusão no Serviço de Proteção ao Crédito" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Persistência do problema após reparo" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não fornecimento da 2ª via de documento de cobrança" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Detalhamento ou desmembramento da fatura não efetuado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Plano de serviço alterado indevidamente pela prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não recebimento de chip e/ou aparelho" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não recebimento de gravação de atendimento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Dificuldade de completamento de chamada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Tratamento desrespeitoso do atendente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação ou habilitação indevida ou não solicitada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança durante a suspensão ou bloqueio do serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Consumidor não consegue falar com atendente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Atraso na entrega do documento de cobrança" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança após cancelamento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Consumidor não consegue negociar a dívida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Reparo não realizado no prazo" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não notificação prévia sobre cancelamento do serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança de consumo de banda larga (dados) com valor incorreto" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Produto ou serviço diferente do que foi ofertado pela prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Ressarcimento de valores não efetuado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Portal Anatel" & Anatel_Consolidado$Tipo == "Sugestão" ] <- "Sugestão"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Prevenção e Correção" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Portabilidade não realizada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Utilização indevida de dados cadastrais" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Consumidor não obtém resposta da contestação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de ponto adicional não efetuada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento indevido ou não solicitado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação ou habilitação não efetuada por questões técnicas" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue aderir à promoção" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança de ligações não efetuadas pelo consumidor" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não possibilidade de forma do ressarcimento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Mudança ou transferência de titularidade não efetuada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue bloquear ou suspender parcialmente o serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Falha ou ruídos na chamada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não recebimento de fatura corrigida ou da parcela em débito" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Bloqueio ou suspensão parcial indevido" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não cumprimento de agendamento de reparo" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Recebimento inoportuno de ligações de oferta" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue cancelar serviço adicional" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Oferta não disponível para antigos consumidores" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Desistência da solicitação de cancelamento automático" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue bloquear ou suspender serviço a pedido" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança após portabilidade" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Portabilidade não realizada no prazo" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Contestação de valor não respondida no prazo" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Consumidor não consegue contestar a cobrança" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Prestadora liga ou envia mensagens indevidas de cobrança" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Prazo de validade do crédito não informado no momento da recarga" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Ressarcimento a menor" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Consumidor não consegue registrar reclamação na prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança durante contestação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Queda de chamada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não disponibilização de bônus promocionais" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Alteração na forma ou data de pagamento não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento feito por meio de atendente efetuado fora do prazo" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue inserir crédito ou fazer recarga" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Troca de equipamento não efetuada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "SCM - Banda Larga" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Recebimento de mensagens publicitárias não autorizadas" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Alteração de linha não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Alteração não solicitada ou perda de número da linha" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança durante o período de interrupção do serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue alterar a forma de recebimento da fatura" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Mudança de endereço não solicitada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento do pedido de portabilidade não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Problema no funcionamento do serviço após portabilidade" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Serviço não foi desbloqueado após pagamento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Documento de cobrança de difícil entendimento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento parcial indevido ou não solicitado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Qualidade ruim de imagem ou de sinal" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Indisponibilidade do canal de atendimento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Alteração não solicitada de linha" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Inexistência ou dificuldade de atendimento nas lojas" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Dificuldade de recebimento de chamada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Oferta em venda casada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não notificação prévia do consumidor sobre bloqueio ou suspensão" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue contratar por inadimplência com terceiros" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Longo tempo de espera para início do atendimento telefônico" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Produto ou serviço indisponível no endereço do consumidor" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue cancelar o serviço pelo site" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não retorno de ligação interrompida ao falar com atendente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não foi informado que a validade dos créditos estava por acabar" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue cancelar parcialmente pacote" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Portabilidade indevida ou não solicitada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Consumidor não recebe protocolo no contato com atendimento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue cancelar a solicitação de instalação do serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Mudança ou transferência indevida de titularidade" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de serviço na área rural não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Créditos acabam antes do prazo de validade" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Funcionamento e conservação de Orelhão" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Aumento da velocidade da conexão de dados na Escola - PBLE" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança de consumo de banda larga (dados) não efetuado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não retirada dos equipamentos após o cancelamento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Desmembramento não solicitado de fatura" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não recebimento de contrato de prestação de serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Jurídico"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Validade de crédito anterior não foi renovada com nova recarga" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue cancelar solicitação de mudança de endereço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Problemas no funcionamento da área reservada do site da prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Queda de ligação no atendimento telefônico da prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não foi informado que os créditos estavam próximos de acabar" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não funcionamento de Telefone de Utilidade Pública" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de Banda Larga na Escola - PBLE" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Reparo na Banda Larga da Escola - PBLE" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de Orelhão em área rural não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Alteração na forma ou data de pagamento não solicitada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Ativação de conexão de dados em escolas rurais" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Oferta com fidelização obrigatória" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança e Documento de Cobrança" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação Telefone Popular (AICE) não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Retirada de Orelhão" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança indevida de ponto extra ou aluguel de equipamento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento de plano ou promoção" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Acessibilidade" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Habilitação ou mudança de plano ou promoção" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de Orelhão em área urbana não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de Orelhão em estabelecimentos públicos não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Não consegue cancelar o plano no menu da central de atendimento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Não consegue bloquear ou suspender serviço a pedido" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Bloqueio ou supensão indevido" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Cobrança em desacordo com o plano contratado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Funcionamento, conservação ou retirada de Orelhão ou TAP" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento solicitado por meio de atendente não efetuado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Inclusão indevida no Serviço de Proteção ao Crédito" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança de serviço, produto ou plano contratado e não disponibilizado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Queda de conexão de dados" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Sem Conexão de dados" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue registrar pedido de cancelamento por meio do atendente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Não consegue bloquear ou suspender parcialmente o serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Não consegue alterar a forma de recebimento da fatura" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Produto ou serviço fornecido diferente do que foi ofertado pela prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue bloquear ou suspender temporariamente o serviço a pedido" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação ou habilitação não realizada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Consumidor não consegue negociar a dívida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Não consegue cancelar o plano por meio do atendente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Mudança ou transferência de titularidade não efetuada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Reparo recusado pela prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Medição incorreta do consumo de internet (dados)" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Cobrança de ligações com valor incorreto" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Fornecimento de informações divergentes, incompletas ou omissas" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Alteração na forma ou data de pagamento não solicitada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Cobrança após cancelamento do plano" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Longo tempo de espera para início do atendimento telefônico" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Cobrança de serviço, produto ou plano contratado, mas não fornecido" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Não notificação prévia sobre cancelamento do plano" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Detalhamento ou desmembramento da fatura não efetuado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue registrar o pedido de cancelamento de serviço adicional" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Cobrança de serviço, produto ou plano não contratado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Longo tempo de espera para falar com o atendente no call center" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Recebimento de mensagens publicitárias não autorizadas no seu telefone fixo ou móvel" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de Orelhão para portadores de deficiência não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Tratamento desrespeitoso do atendente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Cobrança durante contestação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "LDN/LDI - Não consegue aderir à promoção" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Ressarcimento menor que o valor cobrado indevidamente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Indisponibilidade da opção de cancelamento do serviço pelo site" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não possibilidade de escolha da forma do ressarcimento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação ou habilitação recusada por questões técnicas" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento de serviço adicional não efetuado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Solicitação de alteração na forma ou data de pagamento não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança de Internet (dados) com valor incorreto" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Interrupção do serviço em um bairro, localidade, região geográfica" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Mudança de endereço não realizada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Ausência de notificação prévia do consumidor sobre bloqueio ou suspensão" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Crédito cobrado diferente do contratado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Promoção alterada indevidamente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Longo tempo de espera para atendimento presencial" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue bloquear ou suspender o serviço no combo" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não fornecimento de declaração de quitação de débito anual." & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Perda de número da linha" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Divergência nos dados cadastrais do consumidor junto à prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação ou habilitação de serviço cancelada indevidamente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Alteração do número da linha solicitada e não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não devolução em dobro dos valores cobrados indevidamente e pagos pelo consumidor" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança de internet (dados) não contratada ou utilizada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento sem intervenção de atendente não efetuado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Venda casada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue cancelar uma parte do pacote de serviços" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Reparo na Banda Larga das Escolas - PBLE não realizado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não recebimento de conta corrigida ou da parcela em débito" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Mudança de endereço para recebimento de conta não realizada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Mudança de endereço recusada pela prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não devolução de valores devidos por interrupção do serviço" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não exclusão do Serviço de Proteção ao Crédito" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação ou habilitação recusada por inadimplência com terceiros" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Desmembramento da conta não efetuado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Alteração do número de linha não solicitada" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Detalhamento da conta não disponibilizado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de Banda Larga na Escola - PBLE não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Problemas nas funcionalidades da área reservada do site da prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Inclusão indevida em promoção" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Dificuldade de atendimento nas lojas" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Qualidade ruim de imagem ou má qualidade do sinal" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue alterar a forma de recebimento da conta" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Relatório detalhado não disponibilizado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue registrar pedido de cancelamento pelo site da prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança indevida de aluguel de equipamento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Desistência da solicitação de cancelamento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Desmembramento não solicitado de conta" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Portabilidade recusada pela prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Indisponibilidade da opção de cancelamento sem intervenção do atendente" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Qualidade dos servicos"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança indevida de ponto extra" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento de plano ou promoção não solicitado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cobrança indevida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue registrar pedido de cancelamento no menu da central de atendimento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue contato com serviços de utilidade pública" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Instalação de telefone fixo na área rural não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Inexistência de atendimento nas lojas" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Falta de atendimento especializado para pessoas com deficiência" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não consegue atendimento pelos canais de atendimento da prestadora" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Indisponibilidade de contato com a Central de Intermediação de Comunicação - CIC  às pessoas com deficiência" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Cancelamento de plano ou promoção não realizado" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Não disponibilização de informações em formatos acessíveis à pessoas com deficiência" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Intermediação inadequada na Central de Intermediação de Comunicação - CIC às pessoas com deficiência" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Ativação de conexão de dados em escolas rurais não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Aumento da velocidade da conexão de dados na Escola - PBLE não atendida" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Documento de cobrança" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Jurídico"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Produto não certificado / homologado pela Anatel" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "CODI" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Aplicativo Celular" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Portal Anatel" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Laboratórios de Ensaios" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Problemas nos sistemas interativos de outorga e licenciamento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Baixa de Pagamentos Efetuados" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Laboratórios de Ensaio" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Impressão de Boletos" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Restituição e Compensação" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Parcelamento Administrativo" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Produtos Retidos" & Anatel_Consolidado$Tipo == "Pedido de Informação" ] <- "Informação"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Vista de Processo" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Consulta Pública" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Acompanhamento de processos de licenciamentos de estações" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Nada Consta/Certidão Negativa de Débitos" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Cobranca indevida"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "SFUST - Sistema de Acolhimento da Declaração do FUST" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Acesso aos sistemas interativos de outorga e licenciamento" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Produtos Retidos" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Jurídico"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Resolução de Conflitos envolvendo Prestadora de Serviço de Telecomunicações" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Acompanhamento de processos de outorga" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Dados disponibilizados insuficientes" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Canal de atendimento"
Anatel_Consolidado$Motivo_Aglutinado[ Anatel_Consolidado$Motivo == "Procedimento inadequado de fiscalização" & Anatel_Consolidado$Tipo == "Reclamação" ] <- "Termo regulatório"

# ORDENAÇÃO DE COLUNA 'Motivo_Algutinado' A FIM DE MELHORAR A VISUALIZAÇÃO DOS DADOS
Anatel_Consolidado<-Anatel_Consolidado[ ,c(1:10,13,11,12)]

# Exporta DF, se necessário
write.table(Anatel_Consolidado, file = "Anatel_Consolidado.csv",row.names=FALSE, na="", sep=";")

#### Análises sob leitura do arquivo inicial ####
str(Anatel_Consolidado)
summary(Anatel_Consolidado)
class(Anatel_Consolidado)

## fazendo uma cópia do dataframe

df_anatel <- Anatel_Consolidado

## substituindo o atributo "Reencaminhada" para "Nova"

df_anatel$Condicao[df_anatel$Condicao == "Reencaminhada"] <- "Nova" # substitutindo

## para analisar os dados da Anatel resolvi escolher o algoritmo naive bayes pois é um algoritmo apropriado para este tipo
## de dataframe por tratar-se de uma classificação além de analisar bem problemas com caractere numérico.

## sua análise é feita através de probabilidades considerando que as variáveis vão influenciar o evento de forma
## independente. por isso o data frame e o resultado final do algoritmo deve ser bem analisado pois na vida real dificilmente
## os fatos irão ocorrer de forma independente.

## Aplicando o algoritmo. Dividindo os dados de teste e treino

library(e1071)
set.seed(1)
amostra <- sample(2,4613209, replace = T, prob = c(0.7, 0.3))
anatelTreino <- df_anatel[amostra == 1,]
anatelteste <- df_anatel[amostra ==2,]

## Aplicando o algoritmo

modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + CanalEntrada + Tipo + Servico + Modalidade + Motivo + Motivo_Aglutinado + UF + QtdeSolic, anatelTreino)

modelo
class(modelo)
predicao <- predict(modelo, anatelteste[-5])
predicao
confusao <- table(anatelteste$Condicao, predicao)
confusao
taxaacerto <- (confusao[1] + confusao[4]) / sum(confusao)
taxaacerto


## tive problemas de coerção neste ponto. Tive que transformar as variáveis em factor. isso resolveu o problema

## Transformando os dados em factor exceto QtdeSolic

df_anatel$Ano <- factor(as.factor(df_anatel$Ano))
df_anatel$Mes <- factor(as.factor(df_anatel$Mes))
df_anatel$CanalEntrada <- factor(as.factor(df_anatel$CanalEntrada))
df_anatel$Condicao <- factor(as.factor(df_anatel$Condicao))
df_anatel$GrupoEconNorm <- factor(as.factor(df_anatel$GrupoEconNorm))
df_anatel$Tipo <- factor(as.factor(df_anatel$Tipo))
df_anatel$Servico <- factor(as.factor(df_anatel$Servico))
df_anatel$Motivo <- factor(as.factor(df_anatel$Motivo))
df_anatel$Modalidade <- factor(as.factor(df_anatel$Modalidade))
df_anatel$UF <- factor(as.factor(df_anatel$UF))
df_anatel$Motivo_Aglutinado <- factor(as.factor(df_anatel$Motivo_Aglutinado))

str(df_anatel)

## Aplicando o modelo com todas as variáveis a taxa de acerto ficou em 46,28%

## Retirando a variavel QtdeSolic a performance ficou em 77,86%

modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + CanalEntrada + Tipo + Servico + Modalidade + Motivo + Motivo_Aglutinado + UF, anatelTreino)

## retirando a variável QtdeSolic e tipo a performance fica em 77,90% porém o número de acertos em reabertura cai em torno de 30%

modelo <- naiveBayes(as.factor(Condicao) ~ GrupoEconNorm + CanalEntrada + Servico + Modalidade + Motivo + Motivo_Aglutinado + UF, anatelTreino)

## Após vários testes observamos que retirando qualquer outra variável a performance vai ficar em torno de 78%.
## A maior variação fica concentrada nos acertos em reabertura 

## Para alcançar o nosso objetivo devemos adotar o modelo sem a variável QtdeSolic pois o algoritmo apresenta uma performance maior 

## variável QtdeSolic

## Ela indica a quantidade de abertura de chamados durante o dia e fornece uma repetição de um mesmo problema pelos 
## mesmos atributos. Ao retirá-la estamos considerando importante somente os motivos que levaram a abertura de um 
## chamado e não a quantidade de chamados do dia. Pois se considerarmos a quantidade poderemos estar cometendo um 
## erro pois vários chamados podem ter sido abertos e reabertos pelo mesmo problema o que daria um peso maior porém 
## falseando a análise.

## Problemas específicos em uma determinada região, município ou bairro pode gerar vários chamados em um só dia para o mesmo problema para
## uma determinada operadora e criar outliers. Se os dados da anatel indicassem bairros ou cidades poderíamos 
## identificar os ouliers mas como só estão disponíveis os dados dos estados, fica inviável a identificação e isso torna
## um problema micro em um problema macro demais.

## Dentre os problemas que devemos levar em consideração podemos citar: rompimento de cabos, furtos de cabos, 
## falta de manutenção, desastres ambientais (queda de árvores, alagamentos, incêndios etc), apagão de luz, queda 
## de raios, manutenções preventivas, ampliações da rede entre outros.

## Outro motivo para desconsiderar a variável QtdeSolic esta no cálculo de probabilidade condicional do naive bayes 
## que atribui um peso desproporcional em relação as outras variáveis
## A probabilidade condicional calculada pelo algoritmo para situação "Nova" foi de 53.53 sendo que para os demais
## atributos variaram entre 0.0 a 9.6
## Esse peso influencia diretamente no resultado final do cálculo pois no cálculo de probabilidade posterior a classe 
## indicada sera a de maior valor.

## Problemas da variável "QtdeSolic" no funcionamento do naive bayes com relação as outras variáveis pois ele vai ajustar a quantidade de abertura 
## do dia com peso para as variáveis vinculadas, o que não é verdade se considerarmos fatores externos.

## Dando importância a quantidades de solicitações durante um dia para um certo motivo, estamos reforçando a 
## teoria do naive bayes sobre a independência dos atributos, o que prejudica a performance do algoritmo.

## O classificador Naive Bayes possui uma adaptação para dados numéricos que é referida como Gaussian Naive Bayes. 
## Para este tipo de variável, o algoritmo assume que a mesma possui uma distribuição Gaussiana ou normal.
## A distribuição gaussiana é uma curva simétrica em torno do seu ponto médio, apresentando assim seu famoso 
## formato de sino, que é uma forte suposição.

## Com o histograma podemos ver a distribuição dos dados coletados a cada dia e que irá representar uma distribuição 
## diferente da normal. 
## Para transformar em uma distribuição normal, os dados podem ser agrupados mensalmente, calculando a média 
## de aberturas de chamados por dia utilizando o valor da média diária para gerar um novo histograma. Desta vez 
## a distribuição será normal.

## Por este motivo sugiro não considerar a quantidade de ocorrências dos dias e sim as causas.

