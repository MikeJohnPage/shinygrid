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

    stack_rows <- stack_rows[!duplicated(stack_rows[,c('row_start','row_end')]),]
  }

  return(stack_rows)
}

# Both type of stacks need an individual fluidRow()
# 1. super stacks
# 2. stacks

check_super_stacks <- function(stack_rows){

  super_stacks <- data.frame()

  for(row in seq_len(nrow(stack_rows))){

    for(i in seq_len(nrow(stack_rows))){

      if(stack_rows$stacked_elements[row] != stack_rows$stacked_elements[i]){

        if(stack_rows$row_start[row] >= stack_rows$row_start[i] &
           stack_rows$row_start[row] <= stack_rows$row_end[i]){

          confirmed_super_stacks <- data.frame(
            stacked_elements = paste0(
              stack_rows$stacked_elements[row], "-", stack_rows$stacked_elements[i]),
            row_start = min(stack_rows$row_start[row], stack_rows$row_start[i]),
            row_end = max(stack_rows$row_end[row], stack_rows$row_end[i])
          )

          super_stacks <- rbind(super_stacks, confirmed_super_stacks)

          super_stacks <- super_stacks[!duplicated(super_stacks[,c('row_start','row_end')]),]

        }
      }
    }

  }

  return(super_stacks)
}
