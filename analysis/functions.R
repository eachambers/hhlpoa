
lake_colors <- function(raw_data = FALSE, full_names = FALSE) {
  if (!raw_data) lake_cols <- c("halls" = "#448cbc", "bighawk" = "#58ae50", "littlehawk" = "#b4d6e7", "kennisis" = "#bde49c")
  if (raw_data) lake_cols <- c("HALLS LAKE" = "#448cbc", "BIG HAWK LAKE" = "#58ae50", "LITTLE HAWK LAKE" = "#b4d6e7")
  if (full_names) lake_cols <- c("Halls Lake" = "#448cbc", "Big Hawk Lake" = "#58ae50", "Little Hawk Lake" = "#b4d6e7")
  return(lake_cols)
}

data_colors <- function(type = "assorted") {
  if (type == "assorted") data_cols <- c("#206c24", "#208c74", "#186484", "#704c14", "#f07434", "#a82c94", "#e0840c")
  if (type == "warms") data_cols <- c("#704c14", "#f07434", "#a82c94", "#e0840c", "#bd724b", "goldenrod1")
  return(data_cols)
}

