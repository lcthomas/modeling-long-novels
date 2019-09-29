library(igraph) 
library(lattice)
library(reshape2)
library(ggplot2)

# This is really rough and will only work with 1 text at a time, i.e., it's not generalizable.
# Doubtless there are better/more efficient ways to do these simple tasks.
# A lot of this is drawn from here: https://github.com/kateto/R-Network-Visualization-Workshop/blob/master/Polnet%202018%20R%20Network%20Visualization%20Workshop.R

# The steps below take the social network edge list produced using the lit-cascades software
# (https://github.com/vuw-sim-stia/lit-cascades) to re-create a text's social network graph
# for further analysis. You can also use the edges and nodes lists from the dynamic
# network produced by lit-cascades to re-create that graph.

# Can load variable with source name to help keep things straight, or you can save
# each text's environment separately in RStudio for easy re-loading.
theSource = "brief-history" #customize with a short name for your text.

# Create nodes and links objects from file.
# Can use just edge list for social network graphs.
# CSV must have 3 columns: id1,id2,label. No quotes.
# If adapting from list of links obtained via lit-cascades, take out quotes and index numbers (extra 1st column),
# and use commas instead of tabs as separators.
# nodes <- read.csv("The_Intuitionist_socialnetwork_nodes.csv", header=T, as.is=T)
links <- read.csv("Brief_History_socialnetwork_links.csv", header=T, as.is=T)

# Converting the data to an igraph object:
# The graph_from_data_frame() function can take two different data frames: 'd' and 'vertices'.
# 'd' describes the edges of the network - it should start with two columns 
# containing the source and target node IDs for each network tie.
# 'vertices' should start with a column of node IDs.
# Any additional columns in either data frame are interpreted as attributes.
# I have saved my edges list as 'links' above, so that's the argument I will use.
net <- graph_from_data_frame(links, directed=F) 

# Get summary of net object
summary(net)

# Can plot the graph if you want. It's not likely to be pretty.
# Because lit-cascades already creates a good social network graph as one of its outputs,
# I don't focus on making this visualization legible/useful here.
plot(net) # not pretty!

### Strength, total strength, and intimacy index

# Get strength or weighted vertex degree of all of the nodes in the graph.
# The strength function sums up the edge weights of the adjacent edges for each vertex.
all_strength <- strength(net, vids=V(net))
#sum all strength: This adds up all of the strength values for all of the vertices in graph.
sum_all_strength <- sum(all_strength)

# Finding the total number of edges.
size <- gsize(net)

# Character co-occurrence, or the average weight of each edge (for the social network.)
# Lit-cascade's dynamic network is not weighted.
charco <- sum_all_strength / size

# Saving values to a file
gstat <- c(size, sum_all_strength, charco)
write.table(c(theSource,gstat),paste0(theSource,"_table2.csv"),append = T,col.names = F,row.names = F)

# Plotting weights of each degree as a barchart (similar to degree distribution barchart).
barchart(strength(net, vids=V(net)))#messy

### Sorted strength values. Creating bar plot of the "heaviest" nodes in each text's social network.
# Adjusted for number of characters.

# What follows is what I did for each text, changing out variable names for each text to save them all separately in the environment.
# This example is for the text A Brief History of Seven Killings.
# Sorted strength for brief history, highest to lowest.
brief_history_strength <- sort(strength(net, vids=V(net)), decreasing=T)
# Modify this to only show those nodes with strength values >= 10
bh_strength_mod <- brief_history_strength[1:41]
# Use this to produce barchart for Fig 2.
bh_bar <- barchart(bh_strength_mod, xlab="Strength of the adjacent edges of each node") # can create new variables for each text to save individually
bh_bar
# Summed strength for one text.
brief_history_sum <- sum(brief_history_strength)
# Values for top 10, 15, 20% of nodes in terms of weight
brief_history_p10 <- sum(brief_history_strength[1:7]) # The number 7 represents approximately 10% of A Brief History's characters.
brief_history_p15 <- sum(brief_history_strength[1:11]) # ~15% of its characters
brief_history_p20 <- sum(brief_history_strength[1:14]) # ~20% of its characters
#proportion of top degrees by weight in relation to weights of the rest of the degrees
brief_history_normp10 <- (brief_history_p10 / brief_history_sum)*100
brief_history_normp15 <- (brief_history_p15 / brief_history_sum)*100
brief_history_normp20 <- (brief_history_p20 / brief_history_sum)*100

#sorted strength for crypto, highest to lowest
crypto_strength <- sort(strength(net, vids=V(net)), decreasing=T)
#summed strength for one text
crypto_sum <- sum(crypto_strength)
#values for top 10, 15, 20% of nodes in terms of weight
crypto_p10 <- sum(crypto_strength[1:12])
crypto_p15 <- sum(crypto_strength[1:19])
crypto_p20 <- sum(crypto_strength[1:25])
#values for top 5 degrees in terms of weight, top 10, etc
crypto_top10 <- sum(crypto_strength[1:10])
crypto_top5 <- sum(crypto_strength[1:5])
#proportion of top 10/top 5 degrees by weight in relation to weights of the rest of the degrees
crypto_norm5 <- (crypto_top5 / crypto_sum)*100
crypto_norm10 <- (crypto_top10 / crypto_sum)*100
crypto_normp10 <- (crypto_p10 / crypto_sum)*100
crypto_normp15 <- (crypto_p15 / crypto_sum)*100
crypto_normp20 <- (crypto_p20 / crypto_sum)*100

#sorted strength for Infinite Jest, highest to lowest
inf_jest_strength <- sort(strength(net, vids=V(net)), decreasing=T)
#summed strength for one text
inf_jest_sum <- sum(inf_jest_strength)
#values for top 10, 15, 20% of nodes in terms of weight
inf_jest_p10 <- sum(inf_jest_strength[1:11])
inf_jest_p15 <- sum(inf_jest_strength[1:17])
inf_jest_p20 <- sum(inf_jest_strength[1:22])
#values for top 5 degrees in terms of weight, top 10, etc
inf_jest_top10 <- sum(inf_jest_strength[1:10])
inf_jest_top5 <- sum(inf_jest_strength[1:5])
#proportion of top 10/top 5 degrees by weight in relation to weights of the rest of the degrees
inf_jest_norm10 <- (inf_jest_top10 / inf_jest_sum)*100
inf_jest_norm5 <- (inf_jest_top5 / inf_jest_sum)*100
inf_jest_normp10 <- (inf_jest_p10 / inf_jest_sum)*100
inf_jest_normp15 <- (inf_jest_p15 / inf_jest_sum)*100
inf_jest_normp20 <- (inf_jest_p20 / inf_jest_sum)*100

#sorted strength for A Little Life, highest to lowest
life_strength <- sort(strength(net, vids=V(net)), decreasing=T)
#summed strength for one text
life_sum <- sum(life_strength)
#values for top 10, 15, 20% of nodes in terms of weight
life_p10 <- sum(life_strength[1:19])
life_p15 <- sum(life_strength[1:28])
life_p20 <- sum(life_strength[1:37])
#values for top 5 degrees in terms of weight, top 10, etc
life_top10 <- sum(life_strength[1:10])
life_top5 <- sum(life_strength[1:5])
#proportion of top 10/top 5 degrees by weight in relation to weights of the rest of the degrees
life_norm5 <- (life_top5 / life_sum)*100
life_norm10 <- (life_top10 / life_sum)*100
life_normp10 <- (life_p10 / life_sum)*100
life_normp15 <- (life_p15 / life_sum)*100
life_normp20 <- (life_p20 / life_sum)*100

#sorted strength for Goon Squad, highest to lowest
goon_strength <- sort(strength(net, vids=V(net)), decreasing=T)
#summed strength for one text
goon_sum <- sum(goon_strength)
#values for top 10, 15, 20% of nodes in terms of weight
goon_p10 <- sum(goon_strength[1:6])
goon_p15 <- sum(goon_strength[1:8])
goon_p20 <- sum(goon_strength[1:11])
#values for top 5 degrees in terms of weight, top 10, etc
goon_top10 <- sum(goon_strength[1:10])
goon_top5 <- sum(goon_strength[1:5])
#proportion of top 10/top 5 degrees/percentages of nodes by weight in relation to weights of the rest of the degrees
goon_norm10 <- (goon_top10 / goon_sum)*100
goon_norm5 <- (goon_top5 / goon_sum)*100
goon_normp10 <- (goon_p10 / goon_sum)*100
goon_normp15 <- (goon_p15 / goon_sum)*100
goon_normp20 <- (goon_p20 / goon_sum)*100


#sorted strength for Intuitionist, highest to lowest
intuition_strength <- sort(strength(net, vids=V(net)), decreasing=T)
#summed strength for one text
intuition_sum <- sum(intuition_strength)
#values for top 10, 15, 20% of nodes in terms of weight
intuition_p10 <- sum(intuition_strength[1:3])
intuition_p15 <- sum(intuition_strength[1:5])
intuition_p20 <- sum(intuition_strength[1:7])
#values for top 5 degrees in terms of weight, top 10, etc
intuition_top10 <- sum(intuition_strength[1:10])
intuition_top5 <- sum(intuition_strength[1:5])
#proportion of top 10/top 5 degrees by weight in relation to weights of the rest of the degrees
intuition_normp20 <- (intuition_p20 / intuition_sum)*100
intuition_normp10 <- (intuition_p10 / intuition_sum)*100
intuition_normp15 <- (intuition_p15 / intuition_sum)*100


# After finding these values for all of the texts I was comparing, I created a barplot to visually compare them all.
# Plotting all p10/p15/p20 values for each text together.
# Make a data frame.
topscoresp <- data.frame(
  title = c("Infinite Jest","Cryptonomicon","A Little Life","A Brief History of Seven Killings","The Intuitionist","A Visit from the Goon Squad"),
  top_10  = c(inf_jest_normp10,crypto_normp10,life_normp10,brief_history_normp10,intuition_normp10,goon_normp10),
  top_15  = c(inf_jest_normp15,crypto_normp15,life_normp15,brief_history_normp15,intuition_normp15,goon_normp15),
  top_20  = c(inf_jest_normp20,crypto_normp20,life_normp20,brief_history_normp20,intuition_normp20,goon_normp20)
)

# Bring the data to long format as needed by ggplot2.
topscoresp.molten <- melt(topscoresp, value.name="percentage", variable.name="top", na.rm=TRUE)
# Brief argument explanation:
# topscoresp is the dataframe I created above; value.name is the name of the variable I want to store values I will be plotting;
# variable.name is the name of the variable I want to store measured variable names (the p10, p15, p20 rows from above).
# na.rm=TRUE to remove N/A values (although there shouldn't be any for this dataframe).

# Factor the title values so that they appear in a specific order in the plot (ggplot defaults to alphabetical order otherwise).
topscoresp.molten$title <- factor(topscoresp.molten$title,levels = c("Infinite Jest", "Cryptonomicon", "A Little Life", "A Brief History of Seven Killings", "The Intuitionist", "A Visit from the Goon Squad"))

# Colorblind-friendly palatte for graphing.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Plot the values as a grouped barchart.
p <- ggplot(topscoresp.molten, aes(x=title, y=percentage)) + # use the "melted" dataframe, x-axis is titles, y-axis is percentage values
  geom_bar(aes(fill = top),width = 0.4, position = position_dodge(width=0.5), stat="identity", colour="black") +  # make it barchart using variable.name defined above as fill value
  labs(x = "Novels", y = "Percentage of Network's Total Strength") +
  scale_fill_manual(values=cbPalette, name = "Characters",labels = c("Top 10%", "Top 15%", "Top 20%")) + # use the color-blind palatte defined above for colors
  theme(legend.position="top", legend.title = element_text(size=10), axis.title.x=element_text(size = 15), 
        axis.title.y=element_text(size = 15))
# Can also add titles after the fact
#p + labs(y="Percentage of Total Strength")
#p + labs(title = "Percentage of total strength of the top 5 and top 10 nodes in each graph")
p

### Correlation tests

# Create matrix for biserial correlation test with long, characters, size, total_strength, int_ind, and density columns
corpus_int2 <- matrix(c(1,138,972,1944,2,0.156370656370656,1,128,835,1670,2,0.109493836873853,1,196,1930,3860,2,0.165884194053208,1,76,424,2954,6.96698113207547,0.165884194053208,0,35,223,1632,7.318,0.3975,0,57,291,1924,6.61168384879725,0.188961038961039), ncol=6, byrow=T)
colnames(corpus_int2) <- c("long", "characters", "size", "total_strength", "int_ind","density")
rownames(corpus_int2) <- c("Infinite Jest", "Cryptonomicon", "A Little Life", "A Brief History of Seven Killings", "The Intuitionist", "A Visit from the Goon Squad")
corpus_int2

# Turn this matrix into a dataframe
corpus_int_df3 <- as.data.frame(corpus_int2)

# Correlation tests -- biserial (continuous, dichotomous [artificial dichotomy w/ underlying continuity])
res <- cor.test(corpus_int_df3$long, corpus_int_df3$int_ind) # testing the relationship btwn length (x) and intimacy index (y), when length is treated as dichotomous
res
res2 <- cor.test(corpus_int_df3$long, corpus_int_df3$characters) # between length (x) and characters (y)
res2
# Can repeat for other comparisons

# Extract p-value and correlation coefficient from first comparison above (res)
intind_p <- res$p.value
intind_cor <- res$estimate

# Slightly different test; values should be similar to ones obtained above
polyserial(corpus_int_df3$int_ind, corpus_int_df3$long, std.err = T)

# Because I was dealing with a small dataset, I simply transcribed the values into a csv file.
# Ideally, would want to have R do this.
