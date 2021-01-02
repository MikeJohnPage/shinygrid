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

# ---- To develop / test ----
t <- parse_grid(
  "a b c",
  "a d e",
  "f d g",
  "h i g",
  "j k l",
  "j m n",
  "o m p"
)

ts <- check_stacks(t)

# problem, when you run check_super_stacks on ts, the functions needs to be run
# again to identify to new super stacks. This process needs to be run iteratively

# solution
# At the end of check_super_stacks, just check if stacked_elements share letters,
# if they do, combine into one super stack.
# Whats happens if free super stacks share letters but different ones, e.g.,
# "a-b", "b-c", and "c-d"? Perhaps when joining stacks and finding min and max
# Row numbers, grow the stacked_elements by including all the letters, e.g.,
# after joining "a-b", and "b-c", store that as an intermediary variable called
# "a-b-c". This can be the matched to "c-d". The order doesn't matter.
# Questions: duplicates are removed in check_stack(), is this going to mean
# some stacked_elements are missed using this strategy?

