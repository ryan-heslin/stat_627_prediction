vec2DF <- function(x) list2DF(as.list(x))

table_head <- function(..., .n = 6) {
  table(...) |>
    sort(decreasing = TRUE) |>
    head(n = .n)
}

fit_to_scale <- function(x, min, max, increment, transform = c("round", "floor", "ceiling")) {
  transform <- match.arg(transform)
  remainder <- x %% increment
  x <- (x %/% increment) * increment
  if (transform != "floor") {
    if (transform == "ceiling") {
      x <- x + increment
    } else if (transform == "round") {
      round_up <- remainder > (increment / 2)
      x[round_up] <- x[round_up] + increment
    } else {
      stop("Unknown transform ", transform, ". How did you get past match.arg?")
    }
  }
  clamp(x, min, max)
}

clamp <- function(x, lower, upper) pmin(pmax(x, lower), upper)

extract_sentiments <- function(.data, ...) {
  modify(.data, ~ str_remove_all(.x, "\\.") |>
    sentimentr::get_sentences() |>
    sentimentr::sentiment(...) |>
    select("sentiment")) |>
    rowSums()
}

partialize_metric <- function(metric_fn, ...) {
  dots <- list(...)
  mapply(assign, value = dots, x = names(dots))
  new_metric <- as.function(alist(truth, estimate))
  function(truth, estimate) {
    metric_fn(truth, estimate)
  }
}

interval_error_fn <- function(truth, estimate, min, max, increment) {
  mean(abs(truth - fit_to_scale(estimate,
    min = min,
    max = max, increment = increment
  )) / increment)
}

# Defining a custom metric. See https://yardstick.tidymodels.org/articles/custom-metrics.html

# Workaround to pass interval function to metric function without that argument in its signature
partialize_from_dots <- function(fn, ...) {
  dots <- list(...)
  # enclose <- new.env(parent = globalenv())
  enclose <- list2env(dots, envir = NULL, parent = globalenv())
  environment(fn) <- enclose
  fn
}

interval_error_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  interval_error_impl <- partialize(interval_error_fn, partial_args = list(min = 1, max = 4, increment = .25))
  yardstick::metric_vec_template(
    metric_impl = interval_error_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

interval_error <- function(data, ...) {
  UseMethod("interval_error")
}

partialize <- function(fn, partial_args) {
  args <- formals(fn)
  to_replace <- intersect(names(args), names(partial_args))
  remaining_args <- args[setdiff(names(args), to_replace)]
  args[to_replace] <- partial_args[to_replace]
  no_defaults <- sapply(args, identical, quote(expr = ))
  args[no_defaults] <- lapply(names(args[no_defaults]), as.symbol)
  # remaining_args <- remaining_args[!names(remaining_args) %in% names(partial_args)]
  as.function(c(
    remaining_args,
    substitute(do.call(
      fn,
      args
    ))
  ))
}

interval_error <- yardstick::new_numeric_metric(interval_error, direction = "minimize")

interval_error.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  yardstick::metric_summarizer(
    metric_nm = "interval_error",
    metric_fn = interval_error_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    ...,
  )
}
# class(interval_error) <- c("numeric_metric", class(interval_error))
# attr(interval_error, "direction") <- "minimize"

test_preds <- function(model, test) {
  predict(model, new_data = test) |>
    bind_cols(test)
}

fitted_actual_plot <- function(.data, to_pivot) {
  to_pivot <- enquo(to_pivot)
  pivot_longer(.data, cols = !!to_pivot, names_to = "model", values_to = "predicted") |>
    group_by(model, actual, predicted) |>
    mutate(n = n()) |>
    ungroup() |>
    ggplot(aes(x = predicted, y = actual, color = model, size = n)) +
    geom_point(alpha = .5) +
    geom_abline() +
    facet_wrap(~model, ncol = 1) +
    guides(size = guide_legend(override.aes = list(alpha = 1))) +
    theme(legend.position = "bottom")
}


# Creates dummy columns for presence of most commonly used words in input columns
#
create_term_dummies <- function(columns, match_terms = NULL, n = 10) {
  if (is.null(match_terms)) {
    match_terms <-
      unlist(columns, use.names = FALSE) |>
      table() |>
      sort(decreasing = TRUE) |>
      head(n) |>
      names()
  }
  out <- apply(as.matrix(columns), MARGIN = 1, `%in%`, x = match_terms) |>
    t()
  class(out) <- "integer"
  colnames(out) <- match_terms
  as.data.frame(out)
}

step_term_dummy <- function(recipe,
                            ...,
                            role = NA,
                            trained = FALSE,
                            match_terms = NULL,
                            n = 10,
                            skip = FALSE,
                            id = rand_id("term_dummy")) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_term_dummy_new(
      terms = terms,
      trained = trained,
      role = role,
      match_terms = match_terms,
      n = n,
      skip = skip,
      id = id
    )
  )
}
step_term_dummy_new <-
  function(terms, role, trained, n = 10, match_terms = NULL, options, skip, id, col_names = NULL) {
    step(
      subclass = "term_dummy",
      terms = terms,
      role = role,
      trained = trained,
      match_terms = match_terms,
      n = n,
      skip = skip,
      id = id,
      col_names = col_names
    )
  }
# x == step_term_dummy_new object
prep.step_term_dummy <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  # Automatically compute most common terms from selected columns if not provided
  columns <- train[, col_names, drop = FALSE]
  if (isTRUE(ncol(columns) == 0)) stop("No valid columns selected")
  if (!all(purrr::map_chr(columns, class) %in% c("character", "factor"))) {
    stop("All selected columns must be character vectors")
  }
  if (is.null(x$match_terms)) {
    x$match_terms <- table_head(unlist(columns),
      .n = x$n
    ) |>
      names()
  }

  # Update object with prep information - here just selected columns
  step_term_dummy_new(
    terms = x$terms,
    trained = TRUE,
    match_terms = x$match_terms,
    n = x$n,
    role = x$role,
    skip = x$skip,
    id = x$id,
    col_names = col_names
  )
}

# object is step_term_dummy
bake.step_term_dummy <- function(object, new_data, ...) {
  # col_names <- recipes::recipes_eval_select(x$terms, new_data, info = NULL)
  new_columns <- create_term_dummies(columns = new_data[, object$col_names, drop = FALSE], match_terms = object$match_terms, n = object$n)
  tibble(new_data, new_columns, .name_repair = "universal")
}
