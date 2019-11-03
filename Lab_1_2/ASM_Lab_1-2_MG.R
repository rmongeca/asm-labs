#--------------------Homework 1: Linear Models -----------------------------#
#PROCEDURE....= 0. Load csv data                                            #
#               1. Exploratory Data Analysis                                #
#               2. Fit complete model                                       #
#               3. Evaluation First Order Interactions                      # 
#               4. Automatic Variable Selection process                     #
#               5. Model comparison                                         #
#               5. Validate model's assumptions                             # 
#               6. Model interpretation                                     #
#INPUT DATA...= JYB.csv                                                     #
#R VERSION....= R version 3.5.1 (2018-07-02) Feather Spray                  # 
#AUTHOR.......= Maria Gkotsopoulou                                          #
#CREATED......= October 2019                                                #
#---------------------------------------------------------------------------#

############################################################
###############  libraries  & Functions      ############### 
############################################################

### load packages ###
requireorinstall=function(package=""){
  reqpac=parse(text=paste("require(",as.character(package),")"))
  if(eval(reqpac)){
    print(paste(as.character(package), "has been loaded correctly"))
  } else {
    print(paste("trying to install" ,as.character(package)))
    eval(parse(text=paste("try(install.packages(",as.character(package),"))")))
    if(eval(reqpac)){
      print(paste(as.character(package) ,"has been installed and loaded correctly"))
    } else {
      warning(paste("could not install",as.character(package)))
    }
  }
}


requireorinstall(c("knitr","kableExtra","ggplot2","ggsci","dplyr","tidyr","lmtest",
                   "tidyverse","broom","purrr","magrittr","grid","gridExtra","scales",
                   "GGally","fBasics","viridis", "ggmosaic"))

## theme definition for ggplot 
ggstyle = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_blank(),
                axis.title.x = element_text(size = 10,
                                            vjust=0.1),
                axis.title.y = element_text(size = 10,
                                            vjust=1.5),
                axis.text.y = element_text(size=9),
                axis.text.x = element_text(size=9,
                                           angle = 45,
                                           vjust = 1,
                                           hjust=1),
                plot.title = element_text(size=11),
                legend.text = element_text( size=8),
                legend.title = element_text( size=9))

ggstyleFonts = theme(axis.title.x = element_text(size = 10,
                                                 vjust=0.1),
                     axis.title.y = element_text(size = 10,
                                                 vjust=1.5),
                     axis.text.y = element_text(size=9),
                     axis.text.x = element_text(size=9,
                                                angle = 45,
                                                vjust = 1,
                                                hjust=1),
                     plot.title = element_text(size=11),
                     legend.text = element_text( size=8),
                     legend.title = element_text( size=9))


############################################################
###############       DATA LOAD             ############### 
############################################################

jyb <- read.csv("JYB.csv", sep=";")

dfNum <- jyb %>% dplyr::select_if(is.numeric) %>% dplyr::select(-id)

data.frame(variable = names(dfNum),
           class = sapply(dfNum, class),
           min = sapply(dfNum, function(x) min(x)),
           mean = sapply(dfNum, function(x) mean(x)),
           median =  sapply(dfNum, function(x) median(x)),
           max = sapply(dfNum, function(x) max(x)),
           row.names = NULL) 

jyb %>% 
  dplyr::select_if(is.factor) %>% 
  map(levels) %>% 
  map(length) %>% 
  unlist(recursive = FALSE) %>% 
  enframe() 

plyr::ldply(jyb %>% dplyr::select_if(is.factor) %>% 
              map(levels) , rbind) %>% 
  mutate_all(funs(fct_explicit_na(., na_level = ""))) 

############################################################
###############  MISSINGS   ############### 
############################################################

missings <- sapply(jyb, function(x) is.na(x))
col.missings <- apply(missings, 2, sum)

col.missings %>% as.data.frame() 

colSums(jyb =="") 

rm(missings)
# No missings nor attributes without a value

############################################################
################  Exploratory Data Analysis ############### 
############################################################

jyb %>% 
  dplyr::select(c("y",colnames(jyb %>% 
                                 dplyr::select_if(is.numeric) %>% 
                                 dplyr::select(-id)))) %>%
  gather(-y, key = "some_var_name", value = "some_value_name") %>%
  ggplot(aes(x = some_value_name, fill = y)) +
  geom_density( alpha = .2) +
  scale_fill_viridis(name="y",option="plasma",  discrete = TRUE) +
  facet_wrap(~ some_var_name, scales = "free")+
  ggstyleFonts +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = '#ffffff'),
        legend.position="bottom")


jyb %>% 
  dplyr::select(c("y",colnames(jyb %>% 
                                 dplyr::select_if(is.numeric) %>% 
                                 dplyr::select(-id)))) %>%
  gather(-y, key = "some_var_name", value = "some_value_name") %>%
  ggplot(aes(x = y, y = some_value_name ,fill = y)) +
  geom_boxplot(outlier.size=0.05) +
  scale_fill_viridis(name="y",option="plasma",  discrete = TRUE) +
  facet_wrap(~ some_var_name, scales = "free")+
  ggstyleFonts +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = '#ffffff'),
        legend.position="bottom")


mosaicYplusX <- function(factVar){
  ggplot(data = jyb) +
    geom_mosaic(aes_string(x = paste0("product(y,", factVar, ")"),
                           fill="y"), na.rm=TRUE) + 
    scale_fill_viridis(name="y",option="plasma",  discrete = TRUE)+
    ggstyleFonts +
    theme(panel.grid.major = element_blank(),
          panel.background = element_rect(fill = '#ffffff'),
          axis.text.y = element_text(size=7),
          axis.text.x = element_text(size=7),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none")
}

factorNames <- colnames(jyb %>% dplyr::select_if(is.factor) %>% dplyr::select(-y))

p = list()
for(i in 1:(length(factorNames))) {
  p[[i]] = mosaicYplusX(factVar =factorNames[i])
}

do.call(grid.arrange, p )

rm(p)


catdesOutput <- catdes(jyb,21)

catdesOutput$test.chi2

catdesOutput$quanti.var

############################################################
###############  Complete Model ############### 
############################################################


############################################################
###############  Evaluation First Order Interactions #######
############################################################


############################################################
###############  Automatic Variable Selection process ######
############################################################


############################################################
###############  Model comparison             ##############
############################################################


############################################################
###############  Model validation             ##############
############################################################


############################################################
###############  Model interpretation         ##############
############################################################

