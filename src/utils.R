
# Function to manage tree data
# @description
# Assigns values based on condition matching tree tip label
# Assigns NA if there is no match
# Example:
# > control.aes(tip_label = tree$tip.label, output_1 = "blue")
# [1] "blue"
# [2] NA
control.aes <- function(tip_label, output_1=NA, output_2=NA, output_3=NA, output_4=NA, output_5=NA){
  if (str_starts(tip_label, "\\d")){
   return(output_1)
  } else if (str_ends(tip_label, "G3")) {
    return(output_2)
  } else if (str_ends(tip_label, "C5")){
    return(output_3)
  } else if (str_ends(tip_label, "UCA")){
    return(output_4)
  } else if (str_starts(tip_label, "IGHV4")){
    return(output_5)
  }
}

# Example usage
# tipsize <- sapply(
#   tree_obj$tip.label, control.aes,
#   output_1=0.2,
#   output_2=others.size,
#   output_3=others.size,
#   output_4=others.size,
#   output_5=others.size,
#   simplify = T, USE.NAMES = F # Necessary to avoid clutter
# )
