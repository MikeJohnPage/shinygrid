# ---- Parse Grid ----
parse_grid <-
  function(...) {
    grid_vector <- c(...)
    grid_vector_split <- strsplit(grid_vector, split = " ", fixed = TRUE)
    grid_matrix <- matrix(
      unlist(grid_vector_split, use.names = FALSE),
      nrow = length(grid_vector),
      byrow = TRUE
    )
  }

# ---- Check for stacks ----
check_stacks <- function(grid_matrix) {
  stack_rows <- data.frame()

  for (col in seq_len(ncol(grid_matrix))) {
    groups_by_col <- with(
      rle(grid_matrix[, col]),
      data.frame(
        stacked_elements = values,
        row_start = cumsum(lengths) - lengths + 1,
        row_end = cumsum(lengths),
        group_size = lengths
      )
    )

    stacks_by_col <- groups_by_col[groups_by_col$group_size > 1, ]
    stacks_by_col$group_size <- NULL

    stack_rows <- rbind(stack_rows, stacks_by_col)
  }

  return(stack_rows)
}
