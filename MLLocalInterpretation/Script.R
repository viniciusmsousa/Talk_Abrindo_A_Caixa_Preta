
# Script Setup ------------------------------------------------------------

# Paper https://arxiv.org/pdf/1602.04938.pdf
# Link: http://uc-r.github.io/lime


lista_pacotes <- c("lime","vip","pdp","ggplot2","caret","h2o","tidyverse","drake","rsample")
nao_instalados <- lista_pacotes[!(lista_pacotes %in% installed.packages()[,"Package"])]
if(length(nao_instalados)) install.packages(nao_instalados)
lapply(lista_pacotes,function(x){library(x,character.only=TRUE)}) 
rm(lista_pacotes,nao_instalados)
gc()

h2o.init()
#h2o.no_progress()





# Work Flow ---------------------------------------------------------------



workFlow <- drake_plan(
  # Preparando os Dados
  dfAtrito = attrition %>% 
    dplyr::mutate_if(is.ordered, factor, ordered = FALSE) %>%
    dplyr::mutate(Attrition = factor(Attrition, levels = c("Yes", "No"))),
  indexOBSLocais = c(1:6),
  obsTreino = dfAtrito[-indexOBSLocais,],
  obsLocais = dfAtrito[indexOBSLocais,] %>% 
    mutate(Attrition = NA),
  
  # Preparando para h2o
  resposta = "Attrition",
  preditores = setdiff(names(obsTreino),resposta),
  obsTreino_h2o = as.h2o(obsTreino),
  #obsLocais = as.h2o(obsLocais)
  
  # Estimando com automl
  automl = h2o.automl(
    y = resposta,
    x = preditores,
    training_frame = obsTreino_h2o,
    balance_classes = T,
    exclude_algos = "DeepLearning",
    max_runtime_secs = 1800 # 3600 (1hr)
  ),
  melhorModelo = automl@leader,
  
  # lime
  explicador = lime(obsTreino,melhorModelo),
  explicacao = lime::explain(
    x = obsLocais,
    explainer = explicador,
    n_permutations = 500,
    dist_fun = "gower",
    kernel_width = .75,
    n_features = 10, 
    feature_select = "highest_weights",
    labels = "Yes"
  )
  
)

config <- drake_config(workFlow) 
vis_drake_graph(config)
make(workFlow)




# Analise na Seção --------------------------------------------------------



dfAtrito <- readd("dfAtrito")
indexOBSLocais <- readd("indexOBSLocais")
automl <- readd("automl")
obsTreino <- readd("obsTreino")
obsLocais <- readd("obsLocais")
melhorModelo <- readd("melhorModelo")
explicador <- readd("explicador")
explicacao <- readd("explicacao")


plot_features(explicacao)
plot_explanations(explicacao)
