# title: "Chapter 5. Dimensionality reduction techniques"
# author: "Tuure Parviainen"
# date: "November 30, 2017"



require("easypackages") # for loading and installing many packages at one time
packages_to_load <- c("broom", "dplyr", "tidyverse", "corrplot", "ggplot2", "GGally", "devtools", "ggthemes", "stringr", "FactoMineR", "factoextra")
packages(packages_to_load, prompt = TRUE) # lock'n'load install/load

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # to set directory to source file

human <- read.table("data/humaf.csv", sep  =",", header = T, row.names = 1)


ggpairs(
  human, 1:8,
  lower = list(combo = wrap("facethist", binwidth = 0.5
)))


ggduo(
  human, 5, c(1:4,6:8),
  types = list(continuous = "smooth_loess", mapping = aes(color = GNI)),
  title = "GNI constituents loess smooth",
  xlab = "",
  ylab = ""
)

human_std <- scale(human) %>% as.data.frame()

#x11() # to open another graphical device for comparison
ggpairs(
  human_std, 1:8,
  lower = list(combo = wrap("facethist", binwidth = 0.5
  )))

ggduo(
  human_std, 5, c(1:4,6:8),
  types = list(continuous = "smooth_loess", mapping = aes(color = GNI)),
  title = "GNI constituents loess smooth",
  xlab = "",
  ylab = ""
)

# Running the principal component analysis'
pca_human       <- prcomp(human)
pca_human_std   <- prcomp(human_std)

# comparison summaries
s_pca_human     <- summary(pca_human)
s_pca_human_std <- summary(pca_human_std)
s_pca_human
s_pca_human_std

# Generating labels
pca_pr      <- round(100*s_pca_human$importance[2, ], digits = 1)
pca_pr_std <- round(100*s_pca_human_std$importance[2, ], digits = 1)
pca_pr_lab <- paste0(names(pca_pr), " (", pca_pr, "%)")
pca_pr_lab_std <- paste0(names(pca_pr_std), " (", pca_pr_std, "%)")

Plotting 2:1
par(mfrow=c(2,1))
biplot(
  pca_human, cex = c(0.8, 1),
  col = c("grey40", "deeppink2"),
  xlab = pca_pr_lab[1],
  ylab = pca_pr_lab[2],
  main= "Principal component analysis of GNI components (unstandardized)"
)
biplot(
  pca_human_std,
  cex = c(0.8, 1),
  col = c("black", "green2"),
  xlab = pca_pr_lab_std[1],
  ylab = pca_pr_lab_std[2],
  main= "Principal component analysis of GNI components (standardized)"
)
dev.off()
# Loading tea data
# http://sebastien.ledien.free.fr/unofficial_factominer/classical-methods/hierarchical-clustering-on-principal-components.html
data(tea)

keep_columns <- c("Tea", "How", "how", "sugar", "where", "lunch")

# select the 'keep_columns' to create a new dataset
tea_time <- select(tea, one_of(keep_columns))

# look at the summaries and structure of the data
tea_time %>% summary()
#str(tea_time)

# visualize the dataset
gather(tea_time) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
 mca <- MCA(tea_time, graph = FALSE)

# summary of the model
summary(mca)

# visualize MCA
 plot(mca, invisible=c("ind"), habillage = "quali")

 plot(mca, habillage = "quali")
 
 # Screeplot dimension explaining for variance
 fviz_screeplot(mca, addlabels = TRUE, ylim = c(0, 50))
 
 #Strongest 1 dimension
 fviz_mca_ind(mca,  habillage = "how",
              addEllipses = TRUE, repel = TRUE)
 #Stronggest 2 dimension
 fviz_mca_ind(mca,  habillage = "where",
           addEllipses = TRUE, repel = TRUE)
 fviz_mca_var(mca, repel = TRUE) 
 
 # Multiple Correspondence Analysis
 res.mca = MCA(tea, ncp=20, quanti.sup=19, quali.sup=c(20:36), graph=FALSE)
 # par(mfrow=c(2,2))
 res.hcpc = HCPC(res.mca, nb.clust=3)
 
 res.hcpc$desc.var$test.chi2
 res.hcpc$desc.var$category
 res.hcpc$desc.axes
 res.hcpc$desc.ind
 
 #FactoMinerExtra http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/

# Cluster Dendrogram
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.1      # Augment the room for labels
)

fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

fviz_cluster(mca,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)




