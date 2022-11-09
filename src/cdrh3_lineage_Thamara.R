# Load packages
suppressPackageStartupMessages(
  c(
    require(treeio),
    require(ggplot2),
    require(ggtree),
    require(ggtreeExtra),
    require(RColorBrewer),
    require(dplyr),
    require(stringr),
    require(ggimage)
  )
)

# Load tree file
tree_obj <- read.tree(
  "../data/all_wks_combined_igphyml.tree"
)

# Extract weeks from tip labels
weeks <- as.numeric(
  # Extract weeks from name # Used later for data extraction
  str_extract(tree_obj$tip.label, "^\\d+")
)

# Create groups for colouring the phylo tree
groupInfo <- split(
  # Access tip labels NB: Only works before attaching external data
  tree_obj$tip.label,
  weeks
)
# Grouping step
tree_obj <- groupOTU(tree_obj, groupInfo)

# Create data.frame with information
extracted.data <- tibble(
  label = tree_obj$tip.label,
  period = weeks,
  # tipsize
)

extracted.data <- extracted.data %>%
  mutate(
    # Modify tip labels
    to.label = str_extract(
      label,
      "(\\d+wk_\\d+)|([\\w]*[C5|G3|UCA]$)|IGHV4-34\\*02"
    ),
    # Extract specific tip labels labels
    to.label = replace(
      to.label,
      !grepl(
        paste(
          c("^IGHV4", "480727", "1160181",
            "2643", "4383382", "2395", "4990",
            "UCA$", "G3$", "C5$"),
          collapse = "|"), to.label),
      "")
    )

tree_obj <- full_join(
  tree_obj, extracted.data, by=c("label")
)

tree_obj@data <- tree_obj@data %>%
  mutate(
    # Direct conversion from factor to integer does some weired stuff
    group = as.integer(
      as.character(group)
      )
  )

# Specific data to emphasise
dt <- data.frame(
  node = c(101, 135),
  image = c("../images/ac1.png", "../images/ac3.png"),
  name = c("G3", "C5"),
  group = c(1, 2)
)

breaks <- sort(unique(weeks))

# Clade labels
cladelab.phylo.tree <- ggtree(tree_obj, layout = "dendrogram") +
  aes(color=group) +
  scale_color_continuous(
    name = "Period (Weeks)",
    type = "viridis",
    limits = c(15, 150), breaks = breaks,
    guide = guide_legend(reverse = TRUE)) +
  geom_tippoint(show.legend = F) +
  geom_cladelab(
    data = dt,
    align = T,
    show.legend = F,
    offset = 0.04,
    offset.text=0.03,
    # geom = "image",
    mapping = aes(
      node = node,
      # image = image,
      label = name,
      # color = group
      )
    ) +
  # geom_tiplab(aes(hjust=.5) +
  geom_tiplab(
    aes(label = to.label, angle = if_else(grepl("G3|C5$", to.label), 0, 90)),
    size=2.5, offset = -0.002,
    hjust=1.02,
    show.legend=FALSE
    ) +
  # theme_dendrogram(plot.margin=margin(10,10,120,10)) +
  theme(legend.position=c(.05, 0.3))

ggsave("../plots/cladelabeled_G3_C5_tree.png",
       cladelab.phylo.tree,
       width = 16,
       height = 8
)

# Regular
phylo.tree <- ggtree(tree_obj, layout = "dendrogram") +
  aes(color=group) +
  scale_color_continuous(
    name = "Period (Weeks)",
    type = "viridis",
    limits = c(15, 150), breaks = breaks,
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_tippoint(show.legend = F) +
  geom_tiplab(
    aes(
      label = to.label,
      angle = if_else(grepl("G3|C5$", to.label), 0, 90),
      # hjust = if_else(grepl("G3|C5$", to.label), 1.02, 1.02)
    ),
    size=2.5, offset = -0.002,
    hjust=1.02,
    show.legend=FALSE
  ) +
  theme(legend.position=c(.05, 0.3))

ggsave(
  "../plots/G3_C5_tree.png",
  phylo.tree,
  width = 16,
  height = 8
)













