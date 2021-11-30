
# Quick sorting with R

**Author**: Xiurui Zhu<br /> **Modified**: 2021-11-30 23:10:34<br />
**Compiled**: 2021-11-30 23:10:40

## Introduction

Quick sorting includes a series of techniques that quickly sorts a
numeric vector. In this paper, two common quick-sorting techniques were
introduced: pivot sorting and heapsort.

## Question

Given a numeric vector `input_vec`, sort it by increasing order.

``` r
set.seed(999L)
input_vec <- sample.int(1000L, size = 100L)
input_vec
#>   [1] 923 324  61 583 361 654 833 186  10 662 291 279 952 207 581 391 678 574
#>  [19] 403 491  82 577 421 915 890 460 393 671 181 375 549 441 762 378 580 632
#>  [37] 249 719 223 887  16 541 363 258   9 353 411 754 344 552 346 708 604 836
#>  [55]  41 199 866 397 793 386 461 822 487 343 569 706  45 670 780 205 108 159
#>  [73] 770  91 785 172 669 467 148 807  73 644 962 809 404 938 535 891 979 741
#>  [91]  27 296 936 371 610  70  79 117  33  29
```

## Solutions

``` r
library(tidyverse)
```

### Common function

Sorting involves a basic option of swapping numbers. We first defined a
helper function with swapping.

``` r
swap <- function(vec, i, j) {
  stopifnot(
    is.integer(i) == TRUE,
    is.integer(j) == TRUE,
    dplyr::between(i, 1L, length(vec)) == TRUE,
    dplyr::between(j, 1L, length(vec)) == TRUE
  )
  if (i != j) {
    tmp <- vec[i]
    vec[i] <- vec[j]
    vec[j] <- tmp
  }
  vec
}

# Test
swap(1L:5L, 2L, 5L)
#> [1] 1 5 3 4 2
```

### Pivot sorting

Pivot sorting selects a pivot in the original vector and all the other
elements are compared with the pivot. Then the sequence is ordered
increasingly or decreasingly around the pivot. Recursively call the
function on both sides of the sequence until there is at most 1 element
on each side.

``` r
pivot_sort <- function(vec, decreasing = FALSE, ...) {
  stopifnot(anyDuplicated(vec) == 0, length(vec) > 0)
  arg_list <- list(...)
  # Initialize operation count
  if ("opn" %in% names(arg_list) == TRUE) {
    opn <- arg_list[["opn"]]
  } else {
    opn <- 0L
  }
  # Take first element as pivot
  pivot <- vec[1L]
  # Compare the rest with pivot
  larger_lgl <- vec[-1L] > pivot
  # Add operation count
  opn <- opn + length(vec) - 1L
  # Get elements on both sides
  left_vec <- vec[-1L][larger_lgl == decreasing]
  right_vec <- vec[-1L][larger_lgl != decreasing]
  # Recursively order elements on both sides
  if (length(left_vec) > 1L) {
    left_res <- pivot_sort(left_vec, decreasing, opn = opn)
    left_vec <- left_res[["vec"]]
    opn <- left_res[["opn"]]
  }
  if (length(right_vec) > 1L) {
    right_res <- pivot_sort(right_vec, decreasing, opn = opn)
    right_vec <- right_res[["vec"]]
    opn <- right_res[["opn"]]
  }
  # Output ordered vector
  list(vec = c(left_vec, pivot, right_vec), opn = opn)
}

# Test
pivot_sort(input_vec, decreasing = FALSE)
#> $vec
#>   [1]   9  10  16  27  29  33  41  45  61  70  73  79  82  91 108 117 148 159
#>  [19] 172 181 186 199 205 207 223 249 258 279 291 296 324 343 344 346 353 361
#>  [37] 363 371 375 378 386 391 393 397 403 404 411 421 441 460 461 467 487 491
#>  [55] 535 541 549 552 569 574 577 580 581 583 604 610 632 644 654 662 669 670
#>  [73] 671 678 706 708 719 741 754 762 770 780 785 793 807 809 822 833 836 866
#>  [91] 887 890 891 915 923 936 938 952 962 979
#> 
#> $opn
#> [1] 675
```

### Heapsort

Heapsort builds a binary tree out of the vector. It tries to keep the
root larger than the leaves (when sorting increasingly). After
traversing all roots, the top root is removed as the largest element in
the vector. Repeat the process recursively until the length of the
vector is shortened to 2.

``` r
heap_sort <- function(vec, decreasing = FALSE, ...) {
  stopifnot(anyDuplicated(vec) == 0, length(vec) > 0)
  arg_list <- list(...)
  # Initialize finished elements
  if ("tail" %in% names(arg_list) == TRUE) {
    tail <- arg_list[["tail"]]
  } else {
    tail <- 0L
  }
  if (tail > 0L) {
    vec_tail <- vec[(length(vec) - tail + 1L):length(vec)]
  } else {
    vec_tail <- NULL
  }
  vec <- vec[1L:(length(vec) - tail)]
  # Initialize operation count
  if ("opn" %in% names(arg_list) == TRUE) {
    opn <- arg_list[["opn"]]
  } else {
    opn <- 0L
  }
  # Set stop criterion for recursion
  if (length(vec) == 1L) return(list(vec = c(vec, vec_tail), opn = opn))
  # Compare root with leaves from bottom to top
  order_res <- floor(length(vec) / 2L):1L %>%
    purrr::reduce(
      ~ {
        vec_order <- .x[["vec"]]
        root <- vec_order[.y]
        left_leaf <- vec_order[2L * .y]
        opn <- .x[["opn"]]
        if ((vec_order[.y] > vec_order[2L * .y]) == decreasing) {
          vec_order <- swap(vec_order, .y, 2L * .y)
        }
        opn <- opn + 1L
        if (2L * .y + 1L <= length(vec_order)) {
          if ((vec_order[.y] > vec_order[2L * .y + 1L]) == decreasing) {
            vec_order <- swap(vec_order, .y, 2L * .y + 1L)
          }
          opn <- opn + 1L
        }
        list(vec = vec_order, opn = opn)
      },
      .init = list(vec = vec, opn = opn)
    )
  # Move top root to end of vector
  order_res[["vec"]] <- c(order_res[["vec"]][-1L], order_res[["vec"]][1L])
  heap_sort(c(order_res[["vec"]], vec_tail),
            decreasing,
            opn = order_res[["opn"]],
            tail = tail + 1L)
}

# Test
heap_sort(input_vec, decreasing = FALSE)
#> $vec
#>   [1]   9  10  16  27  29  33  41  45  61  70  73  79  82  91 108 117 148 159
#>  [19] 172 181 186 199 205 207 223 249 258 279 291 296 324 343 344 346 353 361
#>  [37] 363 371 375 378 386 391 393 397 403 404 411 421 441 460 461 467 487 491
#>  [55] 535 541 549 552 569 574 577 580 581 583 604 610 632 644 654 662 669 670
#>  [73] 671 678 706 708 719 741 754 762 770 780 785 793 807 809 822 833 836 866
#>  [91] 887 890 891 915 923 936 938 952 962 979
#> 
#> $opn
#> [1] 4950
```
