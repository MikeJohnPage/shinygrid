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
    stack_rows <- stack_rows[!duplicated(stack_rows[, c("row_start", "row_end")]), ]
  }

  return(stack_rows)
}

# ---- Check for super stacks (stacked stacks) ----
check_super_stacks <- function(stack_rows) {
  stack_rows$id <- seq_along(stack_rows[, 1])

  super_stacks <- data.frame()

  super_stack_tracker <- vector()

  for (row in seq_len(nrow(stack_rows))) {
    for (i in seq_len(nrow(stack_rows))) {
      if (stack_rows$stacked_elements[row] != stack_rows$stacked_elements[i]) {
        if (stack_rows$row_start[row] >= stack_rows$row_start[i] &
          stack_rows$row_start[row] <= stack_rows$row_end[i]) {
          confirmed_super_stacks <- data.frame(
            stacked_elements = paste0(
              stack_rows$stacked_elements[row], "-", stack_rows$stacked_elements[i]
            ),
            row_start = min(stack_rows$row_start[row], stack_rows$row_start[i]),
            row_end = max(stack_rows$row_end[row], stack_rows$row_end[i])
          )
          super_stacks <- rbind(super_stacks, confirmed_super_stacks)
          super_stacks <- super_stacks[!duplicated(super_stacks[, c("row_start", "row_end")]), ]
          super_stack_tracker <- c(super_stack_tracker, stack_rows$id[row], stack_rows$id[i])
        }
      }
    }
  }

  not_a_super_stack_ids <- setdiff(stack_rows$id, unique(super_stack_tracker))
  not_a_super_stack_df <- subset(stack_rows, id %in% not_a_super_stack_ids)
  not_a_super_stack_df$id <- NULL

  complete_stack <- rbind(super_stacks, not_a_super_stack_df)

  return(complete_stack)
}

# ---- Iterate over super stacks ----
iterate_stacks <- function(stack) {
  current_stack <- check_super_stacks(stack)
  current_stack_size <- nrow(current_stack)

  while (nrow(check_super_stacks(current_stack)) < current_stack_size) {
    current_stack <- check_super_stacks(current_stack)
    current_stack_size <- nrow(current_stack)
  }

  return(current_stack)
}

# ---- Rows covered by stacks ----
rows_covered_by_stacks <- function(stack) {
  rows_covered <- vector()

  for (row in seq_len(nrow(stack))) {
    rows_covered <- c(rows_covered, stack$row_start[row]:stack$row_end[row])
  }

  return(rows_covered)
}

rows_not_covered_by_stacks <- function(rows_covered, grid_matrix) {
  rows_not_covered <- setdiff(1:nrow(grid_matrix), rows_covered)
}

# ---- append single rows ----
append_single_rows <- function(rows_not_covered, current_stack) {
  single_rows_df <- data.frame()

  for (i in seq_along(rows_not_covered)) {
    single_rows <- data.frame(
      stacked_elements = "No stack",
      row_start = rows_not_covered[i],
      row_end = rows_not_covered[i]
    )

    single_rows_df <- rbind(single_rows_df, single_rows)
  }

  all_rows <- rbind(current_stack, single_rows_df)

  return(all_rows)
}

# ---- To develop / test ----
# Multiple super stacks and stacks
t <- parse_grid(
  "a b c",
  "a d e",
  "f d g",
  "h i g",
  "y z z", # random row with no stacks to separate super stacks
  "j k l",
  "j m n",
  "o m p",
  "q r s",
  "q t u",
  "v w x" # random row with no stacks to track back
)

ts <- check_stacks(t)

its <- iterate_stacks(ts)

its_c <- rows_covered_by_stacks(its)

its_nc <- rows_not_covered_by_stacks(rows_covered = its_c, t)

append_single_rows(its_nc, current_stack = its)

t <- parse_grid(
  "a a b",
  "a a c",
  "d e c"
)

ts <- check_stacks(t)

its <- iterate_stacks(ts)

its_c <- rows_covered_by_stacks(its)

its_nc <- rows_not_covered_by_stacks(rows_covered = its_c, t)

append_single_rows(its_nc, current_stack = its)

t <- parse_grid(
  "a b c c",
  "d d c c"
)

ts <- check_stacks(t)

its <- iterate_stacks(ts)

its_c <- rows_covered_by_stacks(its)

its_nc <- rows_not_covered_by_stacks(rows_covered = its_c, t)

append_single_rows(its_nc, current_stack = its)

# One huge super stack
v <- parse_grid(
  "a b c v",
  "a d e v",
  "f d g v",
  "h i g v",
  "j k l v",
  "j m n v",
  "o m p v",
  "q r s v",
  "q t u v"
)

vs <- check_stacks(v)

ivs <- iterate_stacks(vs)

ivs_c <- rows_covered_by_stacks(ivs)

ivs_nc <- rows_not_covered_by_stacks(rows_covered = ivs_c, v)

append_single_rows(ivs_nc, current_stack = ivs)

# # No stacks (i.e., df returns zero)
# w <- parse_grid("a b c",
#                 "d e f")
#
# wa <- check_stacks(w)
#
# iwa <- iterate_stacks(wa)
#
# w_c <- rows_covered_by_stacks(iwa)

# ---- Columns sorting ----
# On a fluidRow() by fluidRow() basis:
# 1. Perform the same procedure that was done for fluidRows to find the column
#    stacks and super stacks
# What happens in a case like this?
# "a a b c"
# "a a d e"
# "f g g e"
# Column super stack of a-a-g-g and row super stack of a-a-e-e?
