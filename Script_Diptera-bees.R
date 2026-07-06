### READ THIS ###
#To work, this script must be placed in the same folder with:
  #Diptera-bees.xlsx
  #Bee-Tree.nwk



# 0 - PREPARE THE ENVIRONMENT ----
#You can collapse and run this whole section as its only purpose is to
#prepare the R-environment

.start_time <- Sys.time() #Set the clock!

packages <- c(
  # Operate with data
  "openxlsx", "readxl", "writexl", "dplyr", "tidyr", "purrr", "tibble", "scales",
  
  # Models
  "ape", "caper", "phylolm", "phytools", "coda", "glmmTMB", "bipartite",
  
  # Plots
  "ggplot2", "ggbreak", "viridis", "circlize", "fmsb", "igraph",
  
  # Wake up!
  "beepr", "here" 
)

# Install and load missing packages
install.packages(setdiff(packages, rownames(installed.packages())))
invisible(lapply(packages, library, character.only = T))


#Create folders to save results and figures
directory <- c("figures", "results")
for(name in directory) {
  if (!dir.exists(name)){
    dir.create(name)
  }else{
    print("dir already exists")
  }
}


#Clean the environment
rm(list = ls())


#Is the directory correct?
#The folder in which BOTH the .R and .xlsx are stored
here()


### YOU SHOULD NOW BE ABLE TO RUN THE SCRIPT ###
beep()
cat("Required packages installed, now the script should run.\n\n\n")





#---------------------------------------------------------------------------#





#LOAD DATASETS
df <- read.xlsx("Diptera-bees.xlsx", sheet = "Data")


#Create interaction matrix
interaction_matrix <- df %>%
  mutate(value = 1) %>% 
  distinct(B_Species, D_Genus, value) %>%   # Avoid duplicates
  pivot_wider(
    names_from = D_Genus,
    values_from = value,
    values_fill = 0
  )

interaction_matrix <- interaction_matrix %>%
  column_to_rownames(var = "B_Species")

#Alphabetically order rows and columns
interaction_matrix <- interaction_matrix[
  order(rownames(interaction_matrix)),
  order(colnames(interaction_matrix))]

#Create the actual numeric matrix
interaction_matrix <- as.matrix(interaction_matrix)

write.xlsx(interaction_matrix, file = "results/Inter_Matrix.xlsx", rowNames = T)


#Create tables for each Diptera trait x bee species
diptera_traits <- df %>%
  group_by(B_Species, Parasitism_Type) %>%
  summarise(n = n_distinct(D_Genus), .groups = "drop") %>%
  pivot_wider(names_from = Parasitism_Type,
              values_from = n,
              values_fill = 0)
head(diptera_traits)

diptera_host <- df %>%
  group_by(B_Species, Host) %>%
  summarise(n = n_distinct(D_Genus), .groups = "drop") %>%
  pivot_wider(names_from = Host,
              values_from = n,
              values_fill = 0)
head(diptera_host)

diptera_enter <- df %>%
  group_by(B_Species, Entry) %>%
  summarise(n = n_distinct(D_Genus), .groups = "drop") %>%
  pivot_wider(names_from = Entry,
              values_from = n,
              values_fill = 0)
head(diptera_enter)

diptera_families <- df %>%
  group_by(B_Species, D_Family) %>%
  summarise(n = n_distinct(D_Genus), .groups = "drop") %>%
  pivot_wider(names_from = D_Family,
              values_from = n,
              values_fill = 0)
head(diptera_families)

diptera_strategy <- df %>%
  group_by(B_Species, Strategy) %>%
  summarise(n = n_distinct(D_Genus), .groups = "drop") %>%
  pivot_wider(names_from = Strategy,
              values_from = n,
              values_fill = 0)

diptera_strategy <- diptera_strategy %>%
  mutate(Str_Richness = rowSums(across(-B_Species, ~ . != 0)))

head(diptera_strategy)


#Bee traits df
bee_info <- df %>%
  group_by(B_Species) %>%
  summarise(
    B_Family = first(B_Family),
    B_Genus = first(B_Genus),
    B_Sociality = first(B_Sociality),
    B_Nest = first(B_Nest),
    Koppen = first(Koppen),
    animal = first(animal),
    .groups = "drop"
  )
head(bee_info)


#Merge the tables in a single df
df_phylo <- bee_info %>%
  left_join(diptera_traits, by = "B_Species") %>%
  left_join(diptera_host, by = "B_Species") %>%
  left_join(diptera_enter, by = "B_Species") %>%
  left_join(diptera_families, by = "B_Species")  %>%
  left_join(diptera_strategy, by = "B_Species")

df_phylo$animal     <- as.factor(df_phylo$animal)
df_phylo$B_Species  <- as.factor(df_phylo$B_Species)
df_phylo$B_Genus    <- as.factor(df_phylo$B_Genus)
df_phylo$B_Family   <- as.factor(df_phylo$B_Family)
df_phylo$B_Sociality  <- as.factor(df_phylo$B_Sociality)
df_phylo$B_Nest    <- as.factor(df_phylo$B_Nest)
df_phylo$Koppen       <- as.factor(df_phylo$Koppen)
df_phylo$Par_Richness <- df_phylo$Parasitoid + df_phylo$Cleptoparasite

vars_to_bin <- c("Parasitoid_Larva_Enter",
                 "Parasitoid_Larva_NoEnter",
                 "Parasitoid_Adult_NoEnter",
                 "Cleptoparasite_Pollen_Enter")

df_phylo <- df_phylo %>%
  mutate(across(all_of(vars_to_bin),
                ~ ifelse(. > 0, 1, 0)))


df_phylo <- as.data.frame(df_phylo)

str(df_phylo)

write.xlsx(df_phylo, file = "results/Dataset-Collapsed.xlsx", rowNames = T)


#Finish
beep()
rm(vars_to_bin)
cat("All the DFs have been prepared.\n\n\n")




#---------------------------------------------------------------------------#




#LOAD TREE

myTree <- read.tree("Tree_Diptera-Bees.nwk")
myTree <- myTree[[1]]

#Keep only our species
sp <- df_phylo$animal
remove <- setdiff(myTree$tip.label, sp)
mt <- drop.tip(myTree, remove)

#Some checks
is.ultrametric(mt) #Check 1 --> FALSE
mt <- chronos(mt) #If FALSE, then do this

is.ultrametric(mt) #Check 1 --> T
is.rooted(mt) #Check 2 --> T
mt$edge.length <= 0 #Check 3 --> ALL FALSE

#Tree and df should be the same
setdiff(mt$tip.label, df_phylo$animal) # --> 0
all(df_phylo$animal %in% mt$tip.label) # --> T

#Remove big tree
rm(myTree, sp, remove)


#Plot a first basic tree
df1 <- df_phylo[match(mt$tip.label, df_phylo$animal), ] #Reorder species as in the tree
tree_plot <- mt
tree_plot$tip.label <- as.character(df1$B_Species) #Correct tip labels


#Set my palette based on the family
family_colors <- c(
  "Andrenidae"  = "purple4",
  "Apidae"      = "blue",
  "Colletidae"  = "forestgreen",
  "Halictidae"  = "gold3",
  "Megachilidae"= "red3",
  "Melittidae" = "grey20"
)

family_vec <- df1$B_Family[match(tree_plot$tip.label, df1$B_Species)]

#Plot the tree
pdf("figures/Basic-tree.pdf", width = 10, height = 20) #Size is in inches
plot(tree_plot,
     type = "phylogram",
     cex = 0.4,
     font = 3,
     edge.width = 2,
     show.tip.label = T,
     tip.color = family_colors[family_vec],
     edge.color = "black",
     label.offset = 0.02)
dev.off()


# Finish
rm(df1)
beep()
cat("Tree loaded and saved.\n\n\n")




#---------------------------------------------------------------------------#




#1 - NETWORK ----

##1.1 - Bipartite network ----

#Create the matrix to plot
matrix_gen <- df %>%
  count(B_Genus, D_Genus) %>%
  pivot_wider(
    names_from = D_Genus,
    values_from = n,
    values_fill = 0
  )


matrix_gen <- matrix_gen %>%
  column_to_rownames(var = "B_Genus")
write.xlsx(matrix_gen, "results/Matrix_Gen.xlsx")

#Alphabetically order rows and columns
matrix_gen <- matrix_gen[
  order(rownames(matrix_gen)),
  order(colnames(matrix_gen))]

#Create the actual numeric matrix
matrix_gen <- as.matrix(matrix_gen)

#Set Diptera colour
tmp <- df %>%
  distinct(D_Genus, Parasitism_Type, Host, Entry)

df %>% distinct(Parasitism_Type, Host, Entry)

tmp$color <- ifelse(tmp$Parasitism_Type == "Parasitoid" & 
                      tmp$Host == "Adult" & 
                      tmp$Entry == "NoEnter", "purple4",
                    ifelse(tmp$Parasitism_Type == "Parasitoid" & 
                             tmp$Host == "Larva" & 
                             tmp$Entry == "Enter", "forestgreen",
                           ifelse(tmp$Parasitism_Type == "Cleptoparasite" & 
                                    tmp$Host == "Pollen" & 
                                    tmp$Entry == "Enter", "skyblue",
                                  ifelse(tmp$Parasitism_Type == "Parasitoid" & 
                                           tmp$Host == "Larva" & 
                                           tmp$Entry == "NoEnter", "gold",
                                         NA))))

diptera_colors <- setNames(tmp$color, tmp$D_Genus)

pdf("figures/Network_bipartite.pdf", width = 7, height = 20)

plotweb(
  matrix_gen,
  sorting = "dec",
  higher_italic = T,
  lower_italic = T,
  higher_border = "black",
  lower_border = "black",
  lower_color = "grey50",
  higher_color = diptera_colors,
  curved_links = T,
  text_size = 1,
  horizontal = T)

dev.off()



##1.2 - Modules ----
set.seed(123)
g <- graph_from_incidence_matrix(matrix_gen > 0)
com <- cluster_louvain(g)

mod <- computeModules(matrix_gen)
mod_obs <- slot(mod, "likelihood")
mod_obs 
null_mod <- nullmodel(matrix_gen, N = 20, method = "r2d")
null_mod_lik <- sapply(null_mod, function(x) {
  res <- computeModules(x)
  return(slot(res, "likelihood"))
})

beep()

p_value <- sum(null_mod_lik >= mod_obs) / length(null_mod_lik)
z_score <- (mod_obs - mean(null_mod_lik)) / sd(null_mod_lik)

cat("Observed:", mod_obs, "\n")
cat("Null:", mean(null_mod_lik), "\n")
cat("P-value:", p_value, "\n")
cat("Z-score:", z_score, "\n")


#Names for the plot
vertex_names <- c(rownames(matrix_gen), colnames(matrix_gen))
V(g)$name <- vertex_names
is_row <- V(g)$type == T

#Colors
colors <- viridis(max(membership(com)))
colors_alpha <- sapply(colors, function(col) adjustcolor(col, alpha.f = 0.6))
V(g)$color <- colors_alpha[membership(com)]

#Symbols
V(g)$shape <- ifelse(is_row, "circle", "square")
V(g)$size <- 2

#Lables
V(g)$label.color <- "black"
V(g)$label.cex <- 1.4
V(g)$label.dist <- 0.7
V(g)$label.font <- 3


set.seed(123)

cairo_pdf("figures/Network.pdf", width = 17, height = 17, family="Arial")
plot(g)
dev.off()



##1.3 - Specialisation Network ----
set.seed(123)
null.t.test(matrix_gen, index = "H2", N = 99)



##1.4 - Diptera specialisation ----
d_col <- dfun(t(matrix_gen)) %>%
  as.data.frame() %>%
  rownames_to_column("B_Genus")
write.xlsx(d_col, "results/Specialisation-Diptera.xlsx", rownames = T)

#Create a df
diptera_d <- d_col %>%
  rename(D_Genus = B_Genus) %>%
  left_join(
    df %>% distinct(D_Genus, D_Family, Strategy),
    by = "D_Genus"
  ) %>%
  left_join(
    df %>%
      group_by(D_Genus) %>%
      summarise(Species = n_distinct(D_Species), .groups = "drop"),
    by = "D_Genus"
  )

#Merge with the types of bee attacked
df_unique <- df %>%
  distinct(D_Genus, B_Species, .keep_all = T)


nest_counts <- df_unique %>%
  count(D_Genus, B_Nest) %>%
  pivot_wider(
    names_from = B_Nest,
    values_from = n,
    values_fill = 0,
    names_prefix = "Nest_"
  )


social_counts <- df_unique %>%
  count(D_Genus, B_Sociality) %>%
  pivot_wider(
    names_from = B_Sociality,
    values_from = n,
    values_fill = 0,
    names_prefix = "Soc_"
  )


diptera_d <- diptera_d %>%
  left_join(nest_counts, by = "D_Genus") %>%
  left_join(social_counts, by = "D_Genus") %>%
  mutate(across(starts_with(c("Nest_", "Soc_")), ~replace_na(.x, 0))) %>%
  rename(
    Soc_H_Eusocial = `Soc_H-Eusocial`,
    Soc_P_Eusocial = `Soc_P-Eusocial`)

diptera_d <- diptera_d %>%
  mutate(
    Total_hosts = Nest_Aerial + Nest_Fossorial,
    prop_above = Nest_Aerial / Total_hosts,
    prop_solitary = Soc_Solitary / Total_hosts,
    prop_H_eusocial = Soc_H_Eusocial / Total_hosts)





##1.5 - Bee specialisation ----
d_row <- dfun(matrix_gen)
d_row <- as.data.frame(d_row)
d_row <- dfun(matrix_gen) %>%
  as.data.frame() %>%
  rownames_to_column("B_Genus")

#Merge with phylum
df_phylo_d <- df_phylo %>%
  left_join(d_row, by = "B_Genus")

bee_specialisation <- df_phylo_d %>%
  group_by(B_Genus) %>%
  summarise(
    dprime = first(dprime),
    B_Nest = first(B_Nest),
    B_Sociality = first(B_Sociality),
    Koppen = first(Koppen),
    Species = n_distinct(B_Species),
    .groups = "drop"
  )

write.xlsx(bee_specialisation, "results/Specialisation-Bees.xlsx", rownames = FALSE)


# Summary table
summary_table <- bind_rows(
  df_phylo %>% filter(B_Nest      == "Fossorial")  %>% summarise(Bee_Traits = "Fossorial",  n = n_distinct(B_Species), Cleptoparasite = round(mean(Cleptoparasite, na.rm=T),2), Par_Adult = round(mean(Parasitoid_Adult_NoEnter, na.rm=T),2), Par_Larva_Enter = round(mean(Parasitoid_Larva_Enter, na.rm=T),2), Par_Larva_NoEnter = round(mean(Parasitoid_Larva_NoEnter, na.rm=T),2)),
  df_phylo %>% filter(B_Nest      == "Aerial")     %>% summarise(Bee_Traits = "Aerial",     n = n_distinct(B_Species), Cleptoparasite = round(mean(Cleptoparasite, na.rm=T),2), Par_Adult = round(mean(Parasitoid_Adult_NoEnter, na.rm=T),2), Par_Larva_Enter = round(mean(Parasitoid_Larva_Enter, na.rm=T),2), Par_Larva_NoEnter = round(mean(Parasitoid_Larva_NoEnter, na.rm=T),2)),
  df_phylo %>% filter(B_Sociality == "Solitary")   %>% summarise(Bee_Traits = "Solitary",   n = n_distinct(B_Species), Cleptoparasite = round(mean(Cleptoparasite, na.rm=T),2), Par_Adult = round(mean(Parasitoid_Adult_NoEnter, na.rm=T),2), Par_Larva_Enter = round(mean(Parasitoid_Larva_Enter, na.rm=T),2), Par_Larva_NoEnter = round(mean(Parasitoid_Larva_NoEnter, na.rm=T),2)),
  df_phylo %>% filter(B_Sociality == "P-Eusocial") %>% summarise(Bee_Traits = "P-Eusocial", n = n_distinct(B_Species), Cleptoparasite = round(mean(Cleptoparasite, na.rm=T),2), Par_Adult = round(mean(Parasitoid_Adult_NoEnter, na.rm=T),2), Par_Larva_Enter = round(mean(Parasitoid_Larva_Enter, na.rm=T),2), Par_Larva_NoEnter = round(mean(Parasitoid_Larva_NoEnter, na.rm=T),2)),
  df_phylo %>% filter(B_Sociality == "H-Eusocial") %>% summarise(Bee_Traits = "H-Eusocial", n = n_distinct(B_Species), Cleptoparasite = round(mean(Cleptoparasite, na.rm=T),2), Par_Adult = round(mean(Parasitoid_Adult_NoEnter, na.rm=T),2), Par_Larva_Enter = round(mean(Parasitoid_Larva_Enter, na.rm=T),2), Par_Larva_NoEnter = round(mean(Parasitoid_Larva_NoEnter, na.rm=T),2)),
  df_phylo %>% filter(Koppen      == "Tropical")   %>% summarise(Bee_Traits = "Tropical",   n = n_distinct(B_Species), Cleptoparasite = round(mean(Cleptoparasite, na.rm=T),2), Par_Adult = round(mean(Parasitoid_Adult_NoEnter, na.rm=T),2), Par_Larva_Enter = round(mean(Parasitoid_Larva_Enter, na.rm=T),2), Par_Larva_NoEnter = round(mean(Parasitoid_Larva_NoEnter, na.rm=T),2)),
  df_phylo %>% filter(Koppen      == "Arid")       %>% summarise(Bee_Traits = "Arid",       n = n_distinct(B_Species), Cleptoparasite = round(mean(Cleptoparasite, na.rm=T),2), Par_Adult = round(mean(Parasitoid_Adult_NoEnter, na.rm=T),2), Par_Larva_Enter = round(mean(Parasitoid_Larva_Enter, na.rm=T),2), Par_Larva_NoEnter = round(mean(Parasitoid_Larva_NoEnter, na.rm=T),2)),
  df_phylo %>% filter(Koppen      == "Temperate")  %>% summarise(Bee_Traits = "Temperate",  n = n_distinct(B_Species), Cleptoparasite = round(mean(Cleptoparasite, na.rm=T),2), Par_Adult = round(mean(Parasitoid_Adult_NoEnter, na.rm=T),2), Par_Larva_Enter = round(mean(Parasitoid_Larva_Enter, na.rm=T),2), Par_Larva_NoEnter = round(mean(Parasitoid_Larva_NoEnter, na.rm=T),2)),
  df_phylo %>% filter(Koppen      == "Cold")       %>% summarise(Bee_Traits = "Cold",       n = n_distinct(B_Species), Cleptoparasite = round(mean(Cleptoparasite, na.rm=T),2), Par_Adult = round(mean(Parasitoid_Adult_NoEnter, na.rm=T),2), Par_Larva_Enter = round(mean(Parasitoid_Larva_Enter, na.rm=T),2), Par_Larva_NoEnter = round(mean(Parasitoid_Larva_NoEnter, na.rm=T),2))
)

write.xlsx(summary_table, "results/SumTab.xlsx")

# Finish
#Clean the environment
rm(summary_table, com, g, tmp, colors, colors_alpha, diptera_colors, is_row, vertex_names,
   mod_obs, d_row, d_col, df_unique, nest_counts, social_counts, df_phylo_d,
   null_mod, mod, null_mod_lik, p_value, z_score)
beep()
cat("Network plots saved.\n\n\n")





#---------------------------------------------------------------------------#




#2 - MODELS ----
df_phyloglm <- df_phylo
rownames(df_phyloglm) <- df_phylo$animal


##2.1 - Richness ----
model <- phylolm(Par_Richness ~ B_Nest + B_Sociality + Koppen, 
                 data = df_phyloglm,
                 phy = mt,
                 model = "lambda",
                 REML = T,
                 boot = 999)

#Save results
sink("results/Par_Richness.txt")

cat("\n\n ----- MODEL ----- \n\n")
summary(model)

sink()


##2.2 - Cleptoparasites ----

model <- phyloglm(Cleptoparasite_Pollen_Enter ~ B_Nest + B_Sociality + Koppen, 
                  data = df_phyloglm,
                  phy = mt,
                  method = "logistic_MPLE",
                  boot = 999)

comp_data <- comparative.data(mt, df_phyloglm, animal)

#Save results
sink("results/Clepto.txt")

cat("\n\n ----- MODEL ----- \n\n")
summary(model)

cat("\n\n ----- D (1 - Lamda) ----- \n\n")
phylo.d(comp_data, binvar = Parasitoid_Larva_NoEnter)

sink()



##2.3 - Parasitoid Adult ----
model <- phyloglm(Parasitoid_Adult_NoEnter ~ B_Nest + B_Sociality + Koppen, 
                  data = df_phyloglm,
                  phy = mt,
                  method = "logistic_MPLE",
                  boot = 999) 

comp_data <- comparative.data(mt, df_phyloglm, animal)


#Save results
sink("results/Adult.txt")

cat("\n\n ----- MODEL ----- \n\n")
summary(model)

cat("\n\n ----- D (1 - Lamda) ----- \n\n")
phylo.d(comp_data, binvar = Parasitoid_Larva_NoEnter)

sink()



##2.4 - Parasitoid Larva Enter ----

model <- phyloglm(Parasitoid_Larva_Enter ~ B_Nest + B_Sociality + Koppen, 
                  data = df_phyloglm,
                  phy = mt,
                  method = "logistic_MPLE",
                  boot = 999) 

comp_data <- comparative.data(mt, df_phyloglm, animal)


#Save results
sink("results/Larva_Enter.txt")

cat("\n\n ----- MODEL ----- \n\n")
summary(model)

cat("\n\n ----- D (1 - Lamda) ----- \n\n")
phylo.d(comp_data, binvar = Parasitoid_Larva_NoEnter)

sink()



##2.5 - Parasitoid Larva NoEnter ----
  
model <- phyloglm(Parasitoid_Larva_NoEnter ~ B_Nest + B_Sociality + Koppen, 
                    data = df_phyloglm,
                    phy = mt,
                    method = "logistic_MPLE",
                    boot = 999) 

comp_data <- comparative.data(mt, df_phyloglm, animal)
  
  
#Save results
sink("results/Larva_NoEnter.txt")
  
cat("\n\n ----- MODEL ----- \n\n")
summary(model)
  
cat("\n\n ----- D (1 - Lamda) ----- \n\n")
phylo.d(comp_data, binvar = Parasitoid_Larva_NoEnter)
  
sink()


##2.7 - Strategy richness ----

model <- phylolm(Str_Richness ~ B_Nest + B_Sociality + Koppen, 
                 data = df_phyloglm,
                 phy = mt,
                 model = "lambda",
                 REML = T,
                 boot = 999)


#Save results
sink("results/Str_Richness.txt")

cat("\n\n ----- MODEL ----- \n\n")
summary(model)

sink()



##2.8 - Specialisation bees ----

tmp <- lm(dprime ~ B_Nest + B_Sociality + Koppen + Species,
          data = bee_specialisation)


#Save results
sink("results/Specialisation_Bees.txt")
car::Anova(tmp)
sink()


##2.9 - Specialisation Diptera ----
tmp <- lm(dprime ~ Strategy + prop_above + prop_solitary + prop_H_eusocial + Species,
          data = diptera_d)

sink("results/Specialisation_Diptera.txt")
car::Anova(tmp)
sink()

#Comparison Richnesses
sink("results/Richness-Strategy.txt")
cor.test(df_phylo$Par_Richness, df_phylo$Str_Richness)
sink()


#Finish
beep(2)
gc()
rm(model, comp_data, tmp)
cat("Phylogenetic models run.\n\n\n")




#---------------------------------------------------------------------------#




#3 - PLOTS ----

##3.1 - Parasite composition ----

#Select the diptera
paras <- unique(df$D_Family)

#Df to plot
df_long <- df_phylo %>%
  dplyr::select(B_Family, dplyr::all_of(paras)) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(paras),
    names_to = "Family",
    values_to = "Count")


df_plot <- df_long %>%
  group_by(B_Family, Family) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  group_by(B_Family) %>%
  mutate(Total = sum(Count),
         Proportion = Count / Total)


#Plot
ggplot(df_plot, aes(x = B_Family, y = Proportion, fill = Family)) +
  geom_bar(stat = "identity", colour = "black") +
  ylab("Proportion of parasitic families") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(color="black")) +
  scale_fill_viridis_d()

ggsave("figures/Barplot.pdf", width = 15, height = 25, units = "cm")

dev.off()
rm(df_long, df_plot, paras)




##3.2 - Spiderplots ----
vars <- c("Parasitoid_Adult_NoEnter", "Parasitoid_Larva_Enter",
          "Parasitoid_Larva_NoEnter", "Cleptoparasite_Pollen_Enter")

labels <- c(
  "Parasitoid of Adults",
  "Nest-entering larval parasitoid",
  "Non-nest-entering larval parasitoid",
  "Cleptoparasite")

names(labels) <- vars

df_sum <- df_phylo %>%
  group_by(B_Family) %>%
  summarise(across(all_of(vars), sum))


#Plot
data_num <- df_sum %>% dplyr::select(-B_Family)

max_row <- apply(data_num, 2, max)
min_row <- rep(0, ncol(data_num))

radar_data <- as.data.frame(rbind(max_row, min_row, data_num))
rownames(radar_data) <- c("Max", "Min", as.character(df_sum$B_Family))
colnames(radar_data) <- labels[colnames(radar_data)]

wrapped_labels <- sapply(colnames(radar_data),
                         function(x) paste(strwrap(x, width = 10), collapse="\n"))

fam_rows <- rownames(radar_data)[-c(1,2)]
fill_colors <- alpha(family_colors[fam_rows], 0)
line_colors <- family_colors[fam_rows]

pdf("figures/Spiderplot_Merged.pdf", width = 9, height = 9)
radarchart(
  radar_data,
  seg = 3,
  caxislabels = NA,
  pcol = line_colors,
  pfcol = fill_colors,
  plwd = 2,
  plty = rep(1, length(line_colors)),
  cglcol = "grey30",
  axislabcol = "white",
  vlabels = wrapped_labels,
  vlcex = 1)

legend("topright", legend = df_sum$B_Family, col = line_colors, 
       lty = 1, lwd = 3, bty = "n")

dev.off()


#Second plot
plot_spider_family <- function(df, family_name) {
  
  data_fam <- df_sum %>%
    dplyr::filter(B_Family == family_name) %>%
    dplyr::select(-B_Family)
  
  # totale teorico (uguale per tutti i blocchi)
  total <- data_fam$Parasitoid_Adult_NoEnter + data_fam$Parasitoid_Larva_Enter +
    data_fam$Parasitoid_Larva_NoEnter + data_fam$Cleptoparasite_Pollen_Enter
  
  # righe richieste da fmsb
  max_row <- rep(total, ncol(data_fam))
  min_row <- rep(0,     ncol(data_fam))
  
  radar_data <- rbind(max_row, min_row, data_fam)
  colnames(radar_data) <- labels[colnames(radar_data)]
  
  radarchart(
    radar_data,
    seg = 3,
    caxislabels = pretty(c(0, total), n = 6),
    pcol = "darkblue",
    pfcol = scales::alpha("darkblue", 0.3),
    plwd = 2,
    cglcol = "darkgrey",
    axislabcol = "white",
    vlabels = wrapped_labels,
    title = paste0(family_name, " (n = ", total, ")")
  )
}


pdf("figures/Spiderplot.pdf", width = 6, height = 9)

par(mfrow = c(3, 2),
    mar = c(2, 2, 2, 2))

for (fam in unique(df_sum$B_Family)) {
  plot_spider_family(df_sum, fam)
}

dev.off()


#Finish
rm(df_sum, radar_data, fam, fam_rows, fill_colors, labels, line_colors,
   max_row, min_row, vars, wrapped_labels, plot_spider_family, data_num)




##3.3 - Significative plots ----

ggplot(df_phylo, aes(x = B_Nest, y = Str_Richness, fill = B_Nest)) +
  geom_boxplot(alpha = 0.2, outliers = F) +
  geom_jitter(width = 0.1, height = 0, size = 3, alpha = 0.7, shape = 21) +
  labs(x ="",
       y = "Strategy richness") +
  theme_bw() +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none") +
  scale_fill_manual(values = c("skyblue", "gold2"))

ggsave("figures/Str_Richness.pdf", width = 13, height = 20, unit = "cm")


# Binary plots
prop_summary <- function(df, group_var, response_var) {
  df %>%
    group_by({{ group_var }}) %>%
    summarise(
      n    = n(),
      prop = mean({{ response_var }}, na.rm = T),
      se   = sqrt(prop * (1 - prop) / n),
      lo   = pmax(prop - 1.2 * se, 0),
      hi   = pmin(prop + 1.96 * se, 1),
      .groups = "drop"
    )
}


p1 <- prop_summary(df_phylo, B_Nest, Cleptoparasite) %>%
  ggplot(aes(x = B_Nest, y = prop, fill = B_Nest)) +
  geom_col(width = 0.5, alpha = 0.85, col = "black") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, linewidth = 0.7) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.75), expand = c(0, 0)) +
  scale_fill_manual(values = c("skyblue", "gold2")) +
  labs(x = "", y = "Genera of Cleptoparasites") +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none")

ggsave("figures/Cleptoparasite_Nest.pdf", p1, width = 13, height = 20, units = "cm")


p2 <- prop_summary(df_phylo, B_Nest, Parasitoid_Larva_NoEnter) %>%
  ggplot(aes(x = B_Nest, y = prop, fill = B_Nest)) +
  geom_col(width = 0.5, alpha = 0.85, col = "black") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, linewidth = 0.7) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.75), expand = c(0, 0)) +
  scale_fill_manual(values = c("skyblue", "gold2")) +
  labs(x = "", y = "Genera of Non-nest-entering larval parasitoids") +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none")

ggsave("figures/Parasitoid_Larva_NoEnter_Nest.pdf", p2, width = 13, height = 20, units = "cm")


p3 <- prop_summary(df_phylo, B_Sociality, Parasitoid_Larva_NoEnter) %>%
  ggplot(aes(x = B_Sociality, y = prop, fill = B_Sociality)) +
  geom_col(width = 0.5, alpha = 0.85, col = "black") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, linewidth = 0.7) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.75), expand = c(0, 0)) +
  scale_fill_manual(values = c("purple3", "orange3", "lightgreen")) +
  labs(x = "", y = "Genera of Non-nest-entering larval parasitoids") +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15, color = "black"),
        legend.position = "none")

ggsave("figures/Parasitoid_Larva_NoEnter_Sociality.pdf", p3, width = 13, height = 20, units = "cm")

ggplot(bee_specialisation, aes(x = B_Nest, y = dprime, fill = B_Nest)) +
  geom_boxplot(alpha = 0.2) +
  geom_jitter(width = 0.2, height = 0, size = 3, alpha = 0.7, shape = 21) +
  labs(x ="",
       y = "Bee d'") +
  theme_bw() +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none") +
  scale_fill_manual(values = c("skyblue", "gold2"))

ggsave("figures/Bee_d1.pdf", width = 13, height = 20, unit = "cm")


ggplot(diptera_d, aes (x = prop_above, y = dprime)) +
  geom_point(shape = 21, size = 4,  alpha = 0.5, fill = "skyblue") +
  geom_smooth(method = "lm", alpha = 0.15, size = 1, se = T, color = "black") +
  labs(x ="Aerial-nesting bees attacked",
       y = "Diptera d'") +
  theme_bw() +
  theme(text = element_text(size = 20)
        , axis.text = element_text(size = 15, color = "black")
        , legend.position = "none")

ggsave("figures/Diptera_d1.pdf", width = 13, height = 20, unit = "cm")



##3.4 - ANC STATES ----

###3.4.1 - Parasitism ----

traits <- c("Cleptoparasite", "Parasitoid_Adult_NoEnter", 
            "Parasitoid_Larva_Enter", "Parasitoid_Larva_NoEnter")

trait_colors <- c("skyblue", "purple4", "forestgreen", "gold")

# Funzione per estrarre lik.anc da ace
get_lik <- function(trait_name) {
  tv <- setNames(df_phylo[[trait_name]], df_phylo$B_Species)
  tv <- tv[tree_plot$tip.label]
  tv <- na.omit(tv)
  tmp <- ace(tv, tree_plot, type = "discrete", method = "ML", marginal = T)
  tmp$lik.anc[, 2]  # probabilità dello stato "1" per ogni nodo
}

lik_matrix <- sapply(traits, get_lik)

# Normalisation
lik_norm <- lik_matrix / rowSums(lik_matrix)

pdf("figures/AncState_Parasitism.pdf", width = 10, height = 20)
plot(tree_plot, cex = 0.5, label.offset = 0.02)
nodelabels(
  node   = 1:tree_plot$Nnode + Ntip(tree_plot),
  pie    = lik_norm,
  piecol = trait_colors,
  cex    = 0.5
)
legend("bottomleft",
       legend = c("Cleptoparasite", "Par. Adult", "Par. Larva (Enter)", "Par. Larva No-Enter"),
       fill   = trait_colors,
       cex    = 0.5,
       bty    = "n")
dev.off()



###3.4.5 - Sociality ----

#Create named numeric vector for the trait
trait_vector <- setNames(df_phylo$B_Sociality, df_phylo$B_Species)

#Reorder to match the tree tip order
trait_vector <- trait_vector[tree_plot$tip.label]
trait_vector <- na.omit(trait_vector)
species <- names(trait_vector)

#Check alignment
all(names(trait_vector) == tree_plot$tip.label)

#Run Ancestral state
tmp <- ace(trait_vector, tree_plot, type = "discrete", method = "ML",
           marginal = T)
ancestral_states <- apply(tmp$lik.anc, 1, which.max) #Extract the states

#Plot the reconstructions

state_colors <- c("purple3", "orange3", "lightgreen")
names(state_colors) <- c(1, 2, 3)  # match with ancestral_states values

pdf("figures/AncState_Sociality.pdf", width = 10, height = 20)

plot(tree_plot, cex = 0.5, label.offset = 0.02)

nodelabels(node = 1:tree_plot$Nnode + Ntip(tree_plot),
           pie = tmp$lik,
           piecol = c("purple3", "orange3", "lightgreen"),
           cex = 0.5)

dev.off()



###3.4.6 - Nesting ----
#Create named numeric vector for the trait
trait_vector <- setNames(df_phylo$B_Nest, df_phylo$B_Species)

#Reorder to match the tree tip order
trait_vector <- trait_vector[tree_plot$tip.label]
trait_vector <- na.omit(trait_vector)
species <- names(trait_vector)

#Check alignment
all(names(trait_vector) == tree_plot$tip.label)

#Run Ancestral state
tmp <- ace(trait_vector, tree_plot, type = "discrete", method = "ML",
           marginal = T)
ancestral_states <- apply(tmp$lik.anc, 1, which.max) #Extract the states

#Plot the reconstructions

state_colors <- c("skyblue", "gold2")
names(state_colors) <- c(1, 2)  # match with ancestral_states values

pdf("figures/AncState_Nest.pdf", width = 10, height = 20)

plot(tree_plot, cex = 0.5, label.offset = 0.02)

nodelabels(node = 1:tree_plot$Nnode + Ntip(tree_plot),
           pie = tmp$lik,
           piecol = c("skyblue", "gold2"),
           cex = 0.5)

dev.off()




#---------------------------------------------------------------------------#




#FINISH
gc()
dev.off()
beep(3)
.end_time <- Sys.time()
elapsed <- difftime(.end_time, .start_time, units = "mins")
cat("Script run in", round(elapsed, 2), "minutes \n\n\n")
rm(list = ls())