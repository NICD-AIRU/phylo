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

p <- ggtree(tree_obj, size=1.2) +
  layout_dendrogram() +
  aes(color=group) +
  scale_color_continuous(
    name = "Period (wpi)",
    type = "viridis",
    direction=-1,
    limits = c(17, 149),
    breaks = breaks,
    guide = guide_legend(reverse = FALSE)
    ) +
  # geom_tiplab(aes(hjust=.5) +
  geom_tiplab(
    aes(
      label = to.label,
      angle = if_else(grepl("G3|C5$", to.label), 0, 90)
    ),
    colour = "black",
    size=2.5,
    offset = -0.02,
    hjust=1.02,
    show.legend=FALSE
    ) +
  geom_tippoint(
    show.legend = F,
    size=2
  ) +
  geom_treescale(
    0.01,
    width = 0.1,
    offset = -1.4,
    # label = "ED",
    # offset.label = -1.4
    # show.legend = F
    )
  # theme_dendrogram(plot.margin=margin(10,10,120,10)) +
  # theme(legend.position=c(.04, 0.5))


# Clade labels
cladelab.phylo.tree <- p +
  geom_cladelab(
    data = dt,
    align = T,
    show.legend = F,
    offset = 0.06,
    offset.text = 0.03,
    fontface = 2,
    # geom = "image",
    mapping = aes(
      node = node,
      # image = image,
      label = name,
      # color = group
    )
  ) +
  theme(legend.position=c(.04, 0.5))

ggsave(
  "../plots/cladelabeled_G3_C5_tree.png",
  cladelab.phylo.tree,
  width = 10,
  height = 5
)

# Regular
phylo.tree <- p +
  theme(legend.position=c(.05, 0.5))

ggsave(
  "../plots/G3_C5_tree.png",
  phylo.tree,
  width = 10,
  height = 5
)













