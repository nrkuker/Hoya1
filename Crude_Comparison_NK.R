# Visualizing Crude Characteristics

pacman::p_load(
  pacman, rio, tidyverse, magrittr, janitor,
  tidyselect, tidyquant, lubridate,
  stargazer, data.table, gridExtra,
  forecast, scales, tseries, tibbletime,
  lmtest, itsmr, here, fpp2,
  vars, MTS, car, caret,
  MLmetrics,
  psych,        # EDA
  visdat,       # missingness
  NbClust,
  corrplot,     # correlation plot
  FactoMineR,   # EDA, PCA, MFA
  factoextra    # extract and visualize PCA/MFA
)

data <- import("data/CrudeAssayDetails_short.xlsx", 
               sheet = "Crude") %>% clean_names()



describe(data[,3:15])



ggplot(data, aes(fct_infreq(origin))) + geom_bar() + 
  labs(x = "Crude Country of Origin", y = "Count")

ggplot(data, aes(wcp2, wcp3, color = origin)) + geom_point() +
  geom_text(label = data$crude, nudge_x = 1.5, nudge_y = -0.009, check_overlap = T) +
  # geom_text(label = data$crude) + 
  labs(x = "API Gravity", y = "Total Sulfur (% wt)",
       title = "Distribution of Crudes by Specific Gravity & Sulfur Content")




# PCA & CLUSTERING

# perform PCA
pca <- prcomp(data[,-c(1:2)], 
              center = TRUE,
              scale. = TRUE)
summary(pca)


# screeplot
fviz_eig(pca, 
         choice = "variance", 
         addlabels = T) + 
  labs(title = "Scree Plot of Variance Explained by PCA Dimension") +
  theme(plot.title = element_text(hjust=0.5, size=12, face="bold"))

fviz_eig(pca, 
         choice = "eigenvalue",
         addlabels = T) + 
  labs(title = "Scree Plot of Eigenvalues by PCA Dimension") +
  theme(plot.title = element_text(hjust=0.5, size=12, face="bold"))

# extract results at variable level
var <- get_pca_var(pca)

# display as a correlation plot
var$cor[,1:5] %>% as.data.frame() %>% #arrange(-Dim.1) %>% 
  # filter(Dim.1<0.7) %>% dim()
  as.matrix() %>% 
  corrplot(is.corr = F, tl.col = "black",
           cl.align.text = "l",
           mar = c(0,0,2,0),
           tl.cex = 0.5, #tl.pos = "n",
           title = "Correlations Between Variables & Dimensions")


# Contributions of variables to all PCs kept
fviz_contrib(pca, choice = "var", axes = 1:5)
# dashed line is expected value if contributions were uniform

# Contributions of variables to individual PCs
fviz_contrib(pca, choice = "var", axes = 1)
fviz_contrib(pca, choice = "var", axes = 2)


# Correlation Plots
fviz_pca_var(pca, col.var = "cos2",
             labelsize = 3,
             # select.var = list(cos2 = 0.3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoids text overlapping
) + labs(title = "Variable Correlation - Dims 1 & 2",
         subtitle = "colored by 'quality of representation'")

fviz_pca_var(pca, col.var = "contrib",
             labelsize = 3,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
) + labs(title = "Variable Correlation - Dims 1 & 2",
         subtitle = "colored by variable contribution")

fviz_pca_var(pca, col.var = "cos2",
             axes = c(1,2),
             labelsize = 3,
             # select.var = list(cos2 = 0.3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


# extract results at individual level
ind <- get_pca_ind(pca)
rownames(ind$coord) <- data$crude
rownames(ind$cos2) <- data$crude
rownames(ind$contrib) <- data$crude
ind$coord


# Contributions of individuals to all PCs kept
fviz_contrib(pca, choice = "ind", axes = 1:5)
# dashed line is expected value if contributions were uniform

# Contributions of individuals to individual PCs
fviz_contrib(pca, choice = "ind", axes = 1)
fviz_contrib(pca, choice = "ind", axes = 2)


# Correlation Plots
fviz_pca_ind(pca, col.ind = "cos2", geom = c("text"),
             # select.ind = list(cos2 = 0.3),
             gradient.cols = c("#0073C2FF", "#00AFBB", "#E7B800", "#FC4E07"),
             label = "ind"
) + labs(title = "Individual Correlation - Dims 1 & 2",
         subtitle = "colored by 'quality of representation'")





eig.vec <- pca$rotation %>% as_tibble() %>% 
  mutate(feature = row.names(pca$rotation))

# influence of each var on PC1
ggplot(eig.vec, aes(PC1, reorder(feature, PC1))) + geom_point()

ggplot(eig.vec, aes(PC1, PC2, label = feature)) + geom_text()








nb <- NbClust(data = data[,-c(1:2)], distance = "euclidean", diss = NULL,
              min.nc = 2, max.nc = 15, method = "kmeans")

fviz_nbclust(data[,-c(1:2)], kmeans, k.max = 10, method = "wss") +
  labs(subtitle = "Elbow method")

fviz_nbclust(data[,-c(1:2)], kmeans, k.max = 10, method = "silhouette") +
  labs(subtitle = "Silhouette method")

fviz_nbclust(data[,-c(1:2)], kmeans, k.max = 10, method = "gap_stat", 
             nboot = 100, iter.max = 50) +
  labs(subtitle = "Gap statistic method")






set.seed(7852)
km <- kmeans(data[,-c(1:2)], centers = 5, nstart = 25)
# km <- pam(data3, 9, metric = "euclidean")

clus <- cbind(data, cluster = km$cluster)

ggplot(clus, aes(wcp2, wcp3, color = factor(cluster))) + #geom_point(size = 3) + 
  geom_text(label = clus$crude) + 
  labs(x = "API Gravity", y = "Total Sulfur (% wt)")

as.data.frame(cbind(ind$coord, cluster = km$cluster)) %>% 
  ggplot(aes(Dim.1, Dim.2, color = factor(cluster))) + geom_text(label = clus$crude)
