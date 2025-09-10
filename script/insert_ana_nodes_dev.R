


## function ----

library(ape)
library(ggtree)
library(dplyr)

insert_node_ggtree_data <- function(tree, inserts, node_area_data = NULL) {
  p <- ggtree(tree)
  gdata <- p$data
  new_data <- gdata
  max_node <- max(gdata$node)
  
  for (i in seq_len(nrow(inserts))) {
    ins <- inserts[i, ]
    parent <- ins$parent
    child <- ins$child
    pos_from_tip <- ins$position
    trait_val <- ins$trait
    
    # Get the child row
    child_row <- new_data %>% filter(node == child)
    parent_row <- new_data %>% filter(node == !!parent)
    
    bl <- child_row$branch.length
    if (pos_from_tip >= bl || pos_from_tip <= 0) {
      stop("Position must be > 0 and < branch length")
    }
    
    diff_bl <- bl - pos_from_tip
    
    # Create new node
    max_node <- max_node + 1
    new_node <- max_node
    new_x <- child_row$x - diff_bl
    
    new_row <- child_row
    new_row$node <- new_node
    new_row$x <- new_x
    new_row$branch.length <- diff_bl
    new_row$parent <- parent
    new_row$label <- NA
    new_row$isTip <- FALSE
    new_row$trait <- trait_val
    
    # Modify child_row to now descend from new node
    new_data <- new_data %>%
      mutate(
        parent = ifelse(node == child, new_node, parent),
        branch.length = ifelse(node == child, pos_from_tip, branch.length),
        x = ifelse(node == child, child_row$x - pos_from_tip, x)
      )
    
    new_data <- bind_rows(new_data, new_row)
  }
  
  # Update trait data if provided
  trait_df <- if (!is.null(node_area_data)) {
    bind_rows(node_area_data, new_data %>% filter(node > max(tree$edge)) %>% select(node, trait))
  } else {
    new_data %>% filter(!is.na(trait)) %>% select(node, trait)
  }
  
  # Rebuild tree (if needed for further modeling)
  new_tree <- as.phylo(new_data)
  
  return(list(data = new_data, phylo = new_tree, traits = trait_df))
}

## end function ----

set.seed(42)
tree <- rcoal(5)

# Create trait table
node_area <- tibble(
  node = 1:(length(tree$tip.label) + tree$Nnode),
  trait = LETTERS[1:(length(tree$tip.label) + tree$Nnode)]
)

# Find edge to insert on (e.g., tip 3)
gdata <- ggtree(tree)$data
ins_data <- gdata %>% filter(label == "t5" | node == 8) 
#parent <- gdata %>% filter(node == child) %>% pull(parent)

# Define insertion
inserts <- tibble(
  parent = ins_data$parent,
  child = ins_data$node,
  position = rep(0.02, 2),
  trait = c("Z", "DS")
)

# Run function
result <- insert_node_ggtree_data(tree, inserts, node_area_data = node_area)

# Get ggtree layout data
tree_data <- ggtree(result$phylo)$data

# Join your traits onto the tree layout
node_area_df <- left_join(result$traits, tree_data, by = "node")

# Now plot
p <- ggtree(result$phylo) +
  geom_point2(aes(subset = node %in% result$traits$node), color = "red", size = 3) +
  geom_text2(data = node_area_df, aes(label = trait), hjust = -0.2)

print(p)


# Confirm tree height remains the same
cat("Original tree height:", max(node.depth.edgelength(tree)), "\n")
cat("Modified tree height:", max(node.depth.edgelength(result$phylo)), "\n")
