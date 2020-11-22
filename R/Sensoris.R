library(readr)

chocolates <- 
  read_csv(
    "https://dqlab-dataset.s3-ap-southeast-1.amazonaws.com/chocolates.csv",
    col_types = cols(
      panelist = col_factor(),
      session = col_factor(),
      rank = col_factor(),
      product = col_factor(levels = paste0("choc", 1:6)),
      .default = col_integer()
    )
  )
chocolates

install.packages("skimr",repos = "http://cran.us.r-project.org")
library(skimr)
skim(chocolates)

library(dplyr)

chocolates %>% 
  summarise(
    sample = toString(levels(product)),
    n_sample = n_distinct(product),
    n_panelist = n_distinct(panelist)
  )

n_sample <- 6
n_panelist <- 29

ncol(chocolates) - 4
atribut_sensoris <- colnames(chocolates[-c(1, 2, 3, 4)])
atribut_sensoris

install.packages('skimr')

install.packages("skimr",repos = "http://cran.us.r-project.org")
library(skimr)
library(dplyr)

chocolates %>% 
  select(atribut_sensoris) %>% 
  skim_without_charts()

batas_bawah <- 0
batas_atas <- 10


model_bitterness <- aov(bitterness ~ product + panelist + session + panelist:product + panelist:session + product:session + rank, data = chocolates)

model_bitterness


anova(model_bitterness)


summary.lm(model_bitterness)
-1.74 # dua digit di belakang koma/titik

install.packages('FactoMineR')
install.packages('AovSum')

library(FactoMineR)

res_bitterness <- AovSum(model_bitterness)

anova(model_bitterness)

res_bitterness$Ftest


res_bitterness$Ttest[1:7, 1:2]
c("choc1", "choc4", "choc2", "choc5", "choc6" , "choc3")



install.packages('agricolae', repos="http://cran.rstudio.com/")
library(agricolae)

posthoc_bitterness <- HSD.test(model_bitterness, trt = "product")
posthoc_bitterness$groups


install.packages('agricolae', repos="http://cran.rstudio.com/")
library(agricolae)

posthoc_bitterness$groups

plot.group(posthoc_bitterness, variation = "SE")


library(dplyr)
library(corrplot)

chocolates2 <- chocolates %>% 
  select(atribut_sensoris) %>% 
  cor() %>% 
  corrplot(
    type = "upper",
    method = "square",
    diag = FALSE,
    addgrid.col = FALSE,
    order = "FPC", 
    tl.col = "gray30", 
    tl.srt = 30
  )
chocolates2


chocolates_adjmean <- readRDS("chocolates_adjmean.rds")
chocolates_adjmean
dim(chocolates_adjmean)


library(FactoMineR)
chocolates_pca <- PCA(chocolates_adjmean, graph = FALSE)
names(chocolates_pca)
chocolates_pca$eig


library(factoextra)

fviz_eig(chocolates_pca, choice = "eigenvalue", addlabels = TRUE)
fviz_eig(chocolates_pca, choice = "variance", addlabels = TRUE)


library(factoextra)

fviz_pca_ind(chocolates_pca, repel = TRUE)


library(factoextra)
fviz_pca_ind(chocolates_pca, repel = TRUE)

choc2 <- FALSE
choc3 <- TRUE
choc4 <- FALSE
choc5 <- FALSE
choc6 <- TRUE


library(factoextra)
fviz_pca_var(chocolates_pca, repel = TRUE)


library(factoextra)
fviz_pca_var(chocolates_pca, repel = TRUE)
"sticky"
"crunchy"


library(factoextra)
fviz_pca_biplot(chocolates_pca, repel = TRUE, title = "Peta Persepsi Produk Cokelat Komersial")