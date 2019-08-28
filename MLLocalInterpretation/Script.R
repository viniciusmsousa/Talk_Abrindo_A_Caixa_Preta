
# Script Setup ------------------------------------------------------------

# Paper https://arxiv.org/pdf/1602.04938.pdf
# Link: http://uc-r.github.io/lime

listaPacotes <- c("tidyverse","lime","rsample","ggplot2","h2o","drake")
naoInstalados <- listaPacotes[!(listaPacotes %in% installed.packages()[,"Package"])]
if(length(naoInstalados)) install.packages(naoInstalados)
lapply(listaPacotes,function(x){library(x,character.only=TRUE)}) 
rm(listaPacotes,naoInstalados)
gc()

h2o.init()
#h2o.no_progress()



# Preparando dados --------------------------------------------------------

dfAtrito = attrition %>% 
  dplyr::mutate_if(is.ordered, factor, ordered = FALSE) %>%
  dplyr::mutate(Attrition = factor(Attrition, levels = c("Yes", "No")))

indexOBSLocais = c(1:6)

obsTreino = dfAtrito[-indexOBSLocais,]

obsLocais = dfAtrito[indexOBSLocais,] %>% 
  mutate(Attrition = NA)


# Preparando para h2o -----------------------------------------------------
resposta = "Attrition"
preditores = setdiff(names(obsTreino),resposta)
obsTreino_h2o = as.h2o(obsTreino)
#obsLocais = as.h2o(obsLocais)


# Estimando com automl ----------------------------------------------------

automl = h2o.automl(
  y = resposta,
  x = preditores,
  training_frame = obsTreino_h2o,
  balance_classes = T,
  exclude_algos = "DeepLearning",
  max_runtime_secs = 60 # 3600 (1hr)
)

melhorModelo = automl@leader

# Lime --------------------------------------------------------------------

explicador = lime(obsTreino,melhorModelo)

explicacao = lime::explain(
  x = obsLocais,
  explainer = explicador,
  n_permutations = 500,
  kernel_width = .75,
  dist_fun = "gower",
  n_features = 10, 
  feature_select = "highest_weights",
  labels = "Yes"
)


plot_features(explicacao)
plot_explanations(explicacao)
