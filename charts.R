# The functions that draw the charts of kiadas.qmd (category_chart(),
# plan_actual_chart(), plan_actual_heatmap(), country_lines_chart(),
# country_heatmap(), country_chart() and country_stack_chart()), and their
# helpers. They use data.table, echarts4r, RawData, HURawData and CategoryPaths, and
# CountryTable and country_groups (see countries.R), so kiadas.qmd sources this after
# attaching the packages and defining the data (plan_actual_labels and country_names
# below read HURawData and CountryTable as soon as this is sourced). Their JavaScript
# is in the files of js/ (see kiadas_dependency)

# The page's own JavaScript, the `kiadas` object that the charts and their filters
# use, in the files of js/: kiadas.js makes it, with what the charts share, and the
# others add the code of their parts to it (each part's R function tells which of
# its functions it calls). They're loaded in the head of the page once, however
# many charts attach them, in this order, so that they're there before the charts
# are rendered and before the scripts of the filters run
kiadas_dependency <- htmltools::htmlDependency(
  "kiadas", "1.0", src = normalizePath("js", mustWork = TRUE),
  script = c("kiadas.js", "filters.js", "layout.js", "download.js", "category.js",
             "plan-actual.js", "country.js"),
  all_files = FALSE
)

# The ids of the elements of JSON (see json_element()) that the page being knitted
# already has
json_embedded <- new.env()

# A script element of the JSON `json` (a string), with the id `id`: data of a chart
# or of a filter, which it reads with kiadas.json() (see js/kiadas.js), so that the
# browser doesn't parse them until they're first needed. As several charts or filters
# may need the same data, the page only has one copy: the first one that's knitted
# embeds it, and for the others this is NULL, as they read it from there. (So the
# copy comes before them all, which the scripts of the filters need, as they read
# their data as the page is read.) Outside knitting, as when a chunk is run in
# RStudio, each chart is shown on a page of its own, so each embeds it
json_element <- function(id, json) {
  if (isTRUE(getOption("knitr.in.progress"))) {
    if (!is.null(json_embedded[[id]])) return(NULL)
    json_embedded[[id]] <- TRUE
  }
  # So that nothing in the data could end the element early, as a "</script>" would
  # ("<\/" reads the same in JSON)
  htmltools::tags$script(type = "application/json", id = id,
                         htmltools::HTML(gsub("</", "<\\/", json, fixed = TRUE)))
}

# The JSON `json`, which several filters may share: a list of the id of its element,
# made from its hash, so that the same data are in the same element, and the element
# (see json_element())
shared_json <- function(json) {
  id <- paste0("kiadas_data_", digest::digest(as.character(json), algo = "xxhash64",
                                              serialize = FALSE))
  list(id = id, element = json_element(id, json))
}

# Radio buttons with the values `choices`, labelled by its names if it has any
# (as in shiny::radioButtons). `compact = TRUE` shows the choices as a group of
# small toggle buttons (stretched to the available width) with a small label and
# no margin below, for placing it next to other filters; `inline` is then ignored
radio_buttons <- function(id, label, choices, selected = choices[[1]], inline = FALSE,
                          compact = FALSE) {
  labels <- if (is.null(names(choices))) choices else names(choices)
  radios <- lapply(seq_along(choices), \(i) {
    input_id <- paste0(id, "_", i)
    input <- htmltools::tags$input(
      class = if (compact) "btn-check" else "form-check-input", type = "radio",
      name = id, id = input_id, value = choices[[i]],
      checked = if (choices[[i]] == selected) NA
    )
    if (compact) {
      # Bootstrap draws the label right after a .btn-check input as its button
      htmltools::tagList(input, htmltools::tags$label(
        class = "btn btn-sm btn-outline-primary", `for` = input_id, labels[[i]]
      ))
    } else {
      htmltools::div(
        class = "form-check", class = if (inline) "form-check-inline", input,
        htmltools::tags$label(class = "form-check-label", `for` = input_id, labels[[i]])
      )
    }
  })
  if (compact) radios <- htmltools::div(class = "btn-group d-flex", radios)
  
  htmltools::tags$fieldset(
    id = id, class = if (!compact) "mb-3",
    htmltools::tags$legend(
      class = "form-label", class = if (compact) "small mb-1" else "fs-6", label
    ),
    radios
  )
}

# A single-choice dropdown (without an "(All)" option) whose choices depend on other
# controls: it only offers the values of `group` occurring in the rows of the data
# frame `data` that match the current choice of each parent control, the radio
# buttons (see radio_buttons() and filter_radio_dependent()) or dropdowns (as this
# one) in `parent_id`, with their grouping variables in `parent_group` (a list if
# there are several). `group` and `parent_group` are formulas, evaluated in `data`,
# or the values themselves. Without parents (the default), it offers the values of
# all rows. Values are offered in their order of appearance, and when the parents
# change, the dropdown keeps its choice if it's still offered, else it falls back to
# `selected`, or to the first value offered. The charts read the choices of their
# controls themselves (see kiadas.categoryData() in js/category.js). `compact = TRUE`
# makes the dropdown and its label smaller, without margin below, as in
# radio_buttons()
filter_select_dependent <- function(id, label, data, group, parent_id = character(),
                                    parent_group = list(), selected = NULL,
                                    compact = FALSE) {
  eval_group <- \(g) {
    as.character(if (inherits(g, "formula")) eval(g[[2]], data, environment(g)) else g)
  }
  if (!is.list(parent_group)) parent_group <- list(parent_group)
  values <- eval_group(group)
  # Joined the same way as the parents' current choices in kiadas.parentKey()
  # (js/filters.js). Without parents, all rows are under the key "all" (not "", which
  # jsonlite would turn into a number)
  parents <- if (length(parent_group)) {
    do.call(paste, c(lapply(parent_group, eval_group), sep = "\u001f"))
  } else rep("all", length(values))
  rows <- split(seq_along(values), parents)
  # In an element of its own, which other controls may share (see shared_json())
  choices <- shared_json(jsonlite::toJSON(lapply(rows, \(i) unique(values[i]))))

  # Set up at once, as the page is read, so that the dropdown is filled before the
  # charts are rendered (see defer_chart()), which start from its choice
  js <- sprintf(
    "kiadas.filterSelectDependent({choices: %s, selected: %s, id: %s, parents: %s});",
    jsonlite::toJSON(choices$id, auto_unbox = TRUE),
    jsonlite::toJSON(selected, auto_unbox = TRUE, null = "null"),
    jsonlite::toJSON(id, auto_unbox = TRUE),
    jsonlite::toJSON(parent_id)
  )

  htmltools::tagList(
    htmltools::div(
      class = if (!compact) "mb-3",
      htmltools::tags$label(
        class = "form-label", class = if (compact) "small mb-1", `for` = id, label
      ),
      htmltools::tags$select(
        id = id, class = "form-select", class = if (compact) "form-select-sm"
      )
    ),
    choices$element,
    htmltools::tags$script(htmltools::HTML(js))
  )
}

# Radio buttons (see radio_buttons(), which describes `inline` and `compact`) whose
# choices depend on other controls, as those of filter_select_dependent() do (which
# describes `data`, `group`, `parent_id`, `parent_group` and how the choice is kept
# when the parents change): they only offer the values of `group` occurring in the
# rows that match the parents' current choices, in the order of its levels if it's a
# factor, else sorted, and show the buttons even if they offer a single value
filter_radio_dependent <- function(id, label, data, group, parent_id, parent_group,
                                   selected = NULL, inline = FALSE, compact = FALSE) {
  eval_group <- \(g) if (inherits(g, "formula")) eval(g[[2]], data, environment(g)) else g
  if (!is.list(parent_group)) parent_group <- list(parent_group)
  values <- eval_group(group)
  lvls <- as.character(
    if (is.factor(values)) levels(droplevels(values)) else sort(unique(values))
  )
  values <- as.character(values)
  # Joined the same way as the parents' current choices in kiadas.parentKey()
  # (js/filters.js)
  parents <- do.call(paste, c(lapply(parent_group, \(g) as.character(eval_group(g))),
                              sep = "\u001f"))
  fieldsets <- lapply(split(seq_along(values), parents), \(i) {
    offered <- intersect(lvls, values[i])
    radio_buttons(id, label, offered,
                  selected = if (isTRUE(selected %in% offered)) selected else offered[[1]],
                  inline = inline, compact = compact)
  })

  # Set up at once, as in filter_select_dependent()
  js <- sprintf(
    "kiadas.filterRadioDependent({contents: %s, selected: %s, id: %s, parents: %s});",
    # What goes inside the fieldset (the legend and the buttons) for each choice of
    # the parents
    jsonlite::toJSON(lapply(fieldsets, \(f) as.character(htmltools::tagList(f$children))),
                     auto_unbox = TRUE),
    jsonlite::toJSON(selected, auto_unbox = TRUE, null = "null"),
    jsonlite::toJSON(id, auto_unbox = TRUE),
    jsonlite::toJSON(parent_id)
  )

  htmltools::tagList(
    # The buttons for the parents' first combination of values, until the
    # JavaScript reads their current choices
    fieldsets[[1]],
    htmltools::tags$script(htmltools::HTML(js))
  )
}

# Puts `chart` in a box with a download button (a dropdown with the id `id`) over
# its top right corner, whose menu saves the chart as a PNG, a JPG or an SVG, or
# the data it plots, as shown, in an Excel or a CSV file: a column of the x values,
# then the plotted column of each series left by the filters and the legend, headed
# by its name and `unit` (unless it's ""). A heatmap has a column for each of its
# rows instead, of the values its cells are coloured by, headed by the row's name in
# `row_names` (by its value on the y axis). A scatter plot, with `point_columns`,
# has a row for each of its points instead: the name of its series, then the
# columns of the data in `point_columns`, headed by their names (which give the
# units, as `unit` isn't added to them). The images show the whole legend, even
# where the chart scrolls it, and are taller for it. The files are named after the
# title of the chart and, if it has the radio buttons `choice_id` (which choose the
# plotted column, or the type of the axes), the chosen one. With
# `download_axis_name = TRUE`, both data files start with a line of the name of the
# y axis, as shown (in one line). The menu's code, kiadas.downloadMenu() in
# js/download.js, puts the Excel file together itself (as a zip of a few XML files),
# so that the page needs no library for it. As the result is no longer a widget,
# this comes after everything else done to the chart
add_download_menu <- function(chart, unit, id, choice_id = NULL,
                              download_axis_name = FALSE, row_names = NULL,
                              point_columns = NULL) {
  chart <- htmlwidgets::onRender(
    chart, "kiadas.downloadMenu",
    data = list(unit = unit, menuId = id, choiceId = choice_id,
                downloadAxisName = download_axis_name, rowNames = row_names,
                points = if (!is.null(point_columns)) {
                  list(headers = as.list(names(point_columns)),
                       columns = as.list(unname(point_columns)))
                })
  )
  
  item <- \(format, label) htmltools::tags$li(htmltools::tags$button(
    # Taller than Bootstrap's default, to be easier to tap
    type = "button", class = "dropdown-item py-2", `data-format` = format, label
  ))
  htmltools::div(
    class = "position-relative",
    chart,
    # After the chart, so that it's drawn over it
    htmltools::div(
      id = id, class = "dropdown position-absolute", style = "top: 0; right: 10px",
      htmltools::tags$button(
        type = "button", class = "btn btn-outline-secondary p-0",
        class = "d-flex align-items-center justify-content-center",
        # Of a set size, as the title of the chart is wrapped to clear it, possibly
        # before the font of the icon has loaded
        style = "width: 2.5rem; height: 2.5rem",
        `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
        `aria-label` = "Letöltés", title = "Letöltés",
        htmltools::tags$i(class = "bi bi-download", `aria-hidden` = "true")
      ),
      htmltools::tags$ul(
        class = "dropdown-menu dropdown-menu-end",
        item("png", "Mentés képként (PNG)"),
        item("jpg", "Mentés képként (JPG)"),
        item("svg", "Mentés képként (SVG)"),
        htmltools::tags$li(htmltools::tags$hr(class = "dropdown-divider")),
        item("xlsx", "Adatok letöltése (XLSX)"),
        item("csv", "Adatok letöltése (CSV)")
      )
    )
  )
}

# The filters and the chart of the column `var` of HURawData in "Eredmények", with
# `axis_name` on the y axis and `unit` (unless it's "") after the values in the
# tooltip. The chart has no title if `title` is NULL, else `title` followed by the
# name of the chosen main group and, unless it's "Összesen", of the subgroup. If
# `var` has several columns, the chart plots the one chosen with radio buttons
# labelled `var_label`, which offer them by their names (the first one by
# default), and `axis_name` may give a different name for each. With
# `stack = TRUE`, the chart instead shows what the chosen main group is made of, as
# the stacked areas of its subgroups (or, for "Összesen", of the main groups), for
# the type of data chosen with radio buttons in place of the subgroup dropdown,
# which only offer the types of data that come in the chosen classification. The
# title then names the main group (unless it's "Összesen") and the type, and the
# tooltip only shows the hovered area. With a `base_year` (relative mode), the
# chart shows the same parts, with the same controls, but as lines, each divided
# by its own value in the base year (and missing if that isn't positive), chosen
# with a slider, at first `base_year`. The title then also names the base year,
# and the tooltip only shows the hovered line. With `heatmap = TRUE` as well, the
# parts are instead the rows of a heatmap, labelled by their codes (and named in
# full in the legend and the tooltip), with a column for each year, and each cell
# coloured by its ratio (see add_category_heatmap()). The heights of the rows follow
# the parts' weights, their shares of the whole's spending in the base year: they're
# proportional to the square roots of the weights with `row_scale = "sqrt"`, so
# that the small parts stay readable, or to the weights themselves with "linear".
# Showing parts (in any mode), the columns of `var` relative to the main group
# (named ...Parent, like propParent) are only offered when a main group is chosen,
# not for "Összesen", and the radio buttons are hidden while they offer only one
# column. With
# `download_axis_name = TRUE`, the downloadable CSV and Excel files start with a
# line of the name of the y axis, which tells what the values are (e.g. which
# column is plotted). Each call has its own element ids (made from `id`), so its
# controls only affect its own chart. The chart is only drawn once it comes near
# the viewport, and then shows the rows that the current choices of its controls
# pick (see defer_chart() and add_category_data()). The helpers below make its
# parts
category_chart <- function(var, axis_name, unit, title = NULL, var_label = NULL,
                           stack = FALSE, base_year = NULL, heatmap = FALSE,
                           row_scale = c("sqrt", "linear"), download_axis_name = FALSE,
                           id = paste0(var[[1]], if (stack) "_stack",
                                       if (!is.null(base_year)) "_relative",
                                       if (heatmap) "_heatmap",
                                       if (heatmap && row_scale == "linear") "_linear")) {
  # Before `id` is first used, as its default reads it
  row_scale <- match.arg(row_scale)
  relative <- !is.null(base_year)
  if (stack && relative) stop("A chart can't be both stacked and relative to a base year")
  if (heatmap && !relative) stop("A heatmap shows the parts relative to a base year")
  # The chart shows the parts of a whole, stacked or relative to the base year
  parts <- stack || relative
  # Showing parts, the columns of `var` relative to the main group (named
  # ...Parent, like propParent), which put each part of "Összesen" (a main group)
  # at 100%, so they're only offered when a main group is chosen
  parent_var <- if (parts) var[endsWith(var, "Parent")] else character()
  axis_name <- rep_len(axis_name, length(var))
  ids <- category_ids(id)
  data <- category_data(var, parts, heatmap)
  # After the values, in the tooltip and in the description for screen readers
  unit_suffix <- unit_suffix_for(unit)
  # The decimals of the values there: the ratios of relative mode need two
  fraction_digits <- if (relative) 2L else 1L
  row_names <- if (heatmap) heatmap_row_names(data$parts)

  chart <- data_chart(ids$chart, data$years) |>
    add_category_data(ids, data, var, stack, heatmap, base_year)
  chart <- if (heatmap) {
    add_category_heatmap(chart, var, axis_name, row_names, unit_suffix, fraction_digits,
                         heatmap_sizes, row_scale, data$slots)
  } else {
    add_category_lines(chart, data, var, axis_name, unit_suffix, fraction_digits, stack,
                       relative)
  }
  chart <- chart |>
    add_chart_frame() |>
    add_chart_description(
      if (heatmap) {
        paste("Hőtérkép, a sorok magassága a kategóriák bázisévi",
              if (row_scale == "sqrt") "súlyának négyzetgyökével" else "súlyával",
              "arányos")
      } else if (stack) "Halmozott területdiagram" else "Vonaldiagram",
      unit_suffix, fraction_digits, row_names
    ) |>
    add_column_choice(ids, var, axis_name, parent_var, parts, heatmap)
  if (parts && !heatmap) chart <- add_part_lines(chart, stack, unit)
  chart <- add_chart_layout(chart, title, ids, data$paths, relative,
                            row_height = if (heatmap) heatmap_sizes$row,
                            min_plot_height = if (heatmap) heatmap_sizes$plot)

  chart_block(
    htmltools::tagList(
      data$elements,
      category_filters(ids, data$filter_paths, var, var_label, parent_var, parts, relative)
    ),
    chart, ids, unit, choice_id = if (length(var) > 1) ids$var,
    download_axis_name = download_axis_name, row_names = row_names
  )
}

# The ids of the elements of the category_chart() with the id `id`
category_ids <- function(id) {
  list(
    classification = paste0(id, "_classification_filter"),
    main = paste0(id, "_main_group_filter"),
    sub = paste0(id, "_subgroup_filter"),
    type = paste0(id, "_type_filter"),
    var = paste0(id, "_var_choice"),
    base = paste0(id, "_base_year"),
    download = paste0(id, "_download"),
    chart = paste0(id, "_chart")
  )
}

# An empty chart with the id `id`, which its data hook draws, with the data that its
# controls pick (see add_category_data() and add_plan_actual_data()), with a category
# axis of the years `years` (unless it's NULL). Hovering the legend highlights
# nothing if the chart had more than hoverLayerThreshold (by default 3000) elements
# when last redrawn, as a chart of many categories might have, with those shown
# before still fading out, so that's raised
data_chart <- function(id, years = NULL) {
  chart <- e_chart(elementId = id, hoverLayerThreshold = 1e6, draw = FALSE)
  if (!is.null(years)) {
    chart$x$opts$xAxis <- list(list(type = "category", data = as.list(years),
                                    boundaryGap = TRUE))
  }
  chart
}

# What follows the values of a chart in its tooltip and in its description for
# screen readers: `unit`, after a space, but not after one if it's a percent sign
# (or if there's no unit)
unit_suffix_for <- function(unit) if (unit %in% c("", "%")) unit else paste0(" ", unit)

# The formatter of a tooltip of values, kiadas.lineTooltip() in js/kiadas.js (which
# describes it): with at most `fraction_digits` decimals, and `unit_suffix` after
# the values. `value`, `title` and `label` are the paths of the properties of the
# points that give their values, the title and their names (as "value.1", the
# second element of a point's data), by default (NULL) the column its series plots,
# its name and the name of its series
line_tooltip <- function(fraction_digits, unit_suffix, value = NULL, title = NULL,
                         label = NULL) {
  cfg <- list(digits = fraction_digits, unit = unit_suffix)
  # Assigning NULL leaves them out
  cfg$value <- value
  cfg$title <- title
  cfg$label <- label
  htmlwidgets::JS(sprintf("kiadas.lineTooltip(%s)", jsonlite::toJSON(cfg, auto_unbox = TRUE)))
}

# The formatter of the labels of a value axis, in Hungarian, with all their decimals
# (see kiadas.axisNumber() in js/kiadas.js)
axis_number_label <- htmlwidgets::JS("kiadas.axisNumber")

# The options of the labels of an axis of categories that set the label of the
# category `highlight` (none if NULL) in bold: the rich text style `highlight`, and
# the formatter that gives it to that label (see kiadas.highlightLabel() in
# js/kiadas.js)
highlight_axis_labels <- function(highlight) {
  list(rich = list(highlight = list(fontWeight = "bold")),
       formatter = htmlwidgets::JS(sprintf(
         "kiadas.highlightLabel(%s)",
         jsonlite::toJSON(highlight, auto_unbox = TRUE, null = "null")
       )))
}

# The sizes in a heatmap (see add_category_heatmap() and add_country_heatmap()), in
# px: the average height of a row, the least height of the plot, and the height of
# the colour scale beside it, from its top. The plot is at least 20px taller than
# the scale, as the name of the x axis is beside its bottom
heatmap_sizes <- list(row = 24, plot = 150, scale = 130)

# The full names of the categories in `paths` (by their `code` and `nameLabel`), by
# code: a heatmap labels its rows by the codes, so its legend, its tooltip, its
# description for screen readers and its data files name them by these
heatmap_row_names <- function(paths) {
  with(unique(paths[, .(code, nameLabel = as.character(nameLabel))]),
       as.list(setNames(nameLabel, code)))
}

# The data of a category_chart() (see there) of the columns `var` of HURawData: the
# `values` of hu_values(), of `var` and, in a heatmap (with `heatmap = TRUE`), of the
# spending (value), by which its rows are sized; the `years` of the x axis, all
# those of HURawData; the `elements` of JSON that the page needs for them; and the
# rows of the filters (`filter_paths`). Without parts, also the types of data
# (`types`), each a series of the chart, and the filters are on CategoryPaths.
# Showing parts (with `parts = TRUE`), the paths of the parts (`paths`, which the
# filters are on, with "Összesen" first), the parts themselves (`parts`), which the
# page has in the element of JSON with the id `parts_id`, and how many parts a whole
# has at most, of a type of data (`slots`), as the chart draws each part shown in a
# slot of its own (see add_category_data())
category_data <- function(var, parts = FALSE, heatmap = FALSE) {
  values <- hu_values(unique(c(unname(var), if (heatmap) "value")))
  years <- sort(unique(HURawData$yearchr))
  if (!parts) {
    return(list(values = values, years = years, elements = values$elements,
                types = levels(droplevels(HURawData$typeLabel)),
                filter_paths = CategoryPaths))
  }
  # A row of HURawData in both classifications (e.g. Egészségügy) is a part of a
  # different whole in each, so here a path is a row of HURawData in one
  # classification. The main group dropdown picks the whole: a main group, made up
  # of its subgroups, or "Összesen", made up of the main groups. Each type of data
  # only appears in the classification it comes in (the budget's cash-based data in
  # the Hungarian one, Eurostat's accrual-based data in COFOG), as only there do the
  # parts add up to the whole: the categories mapped from the other classification
  # leave gaps
  native <- c(original_plan = "Magyar", actual = "Magyar",
              actual_cofog = "Nemzetközi (COFOG)")
  paths <- CategoryPaths[classification == native[HURawData$type[key]], .(
    classification, main = fifelse(sub == "Összesen", "Összesen", main),
    typeLabel = HURawData$typeLabel[key], nameLabel, code = node
  )]
  # A part for each category in each classification, in the order of the codes
  # (stacked in that order), with the whole it's a part of (`main`) and its code
  # (unique across the classifications), which labels its row in a heatmap. Its
  # rows are those of its category of the type of data chosen (see
  # kiadas.categoryData() in js/category.js)
  parts <- unique(paths[, .(classification, main, nameLabel, code)])
  parts_json <- shared_json(jsonlite::toJSON(list(
    classification = parts$classification, main = parts$main,
    category = as.character(parts$nameLabel), code = parts$code
  )))
  list(values = values, years = years, paths = paths, parts = parts,
       parts_id = parts_json$id,
       slots = paths[, .(parts = uniqueN(code)), by = .(classification, typeLabel, main)][
         , max(parts)],
       elements = htmltools::tagList(values$elements, parts_json$element),
       # "Összesen" first in the dropdown
       filter_paths = paths[order(main != "Összesen")])
}

# The values of the columns `columns` of HURawData for the charts of the Hungarian
# categories, which pick the rows they show from them in the browser (see
# kiadas.categoryData() in js/category.js), as echarts4r would embed every row of
# their data in the page, with the names of the columns: in an element of JSON of the
# rows of HURawData, the year of each row (`yearchr`) and, for each category (by its
# nameLabel) and type of data (by its typeLabel), the indices of its rows (from 0, in
# the order of the years: `groups`); and in an element of each column, its values in
# the order of the rows, rounded (see round_for_chart()), NA as null. Charts of the
# same columns share their elements (see shared_json()). The ids of the elements
# (`rows`, and `columns`, by column), and the elements themselves (`elements`)
hu_values <- function(columns) {
  index <- data.table(i = seq_len(nrow(HURawData)) - 1L,
                      category = as.character(HURawData$nameLabel),
                      type = as.character(HURawData$typeLabel))
  groups <- lapply(split(index, by = "category", keep.by = FALSE), \(rows) {
    lapply(split(rows, by = "type", keep.by = FALSE), \(type) type$i)
  })
  rows <- shared_json(jsonlite::toJSON(list(yearchr = HURawData$yearchr, groups = groups)))
  rounded <- round_for_chart(HURawData[, columns, with = FALSE], columns)
  values <- lapply(setNames(columns, columns), \(column) {
    shared_json(jsonlite::toJSON(rounded[[column]], na = "null", digits = 16,
                                 use_signif = TRUE))
  })
  list(rows = rows$id, columns = lapply(values, `[[`, "id"),
       elements = htmltools::tagList(rows$element, lapply(values, `[[`, "element")))
}

# Draws a category_chart() (see there), with the ids `ids` (see category_ids()), with
# the rows of HURawData that its controls pick (see category_data() for `data`): at
# first, and again whenever they change, in the browser (see kiadas.categoryData() in
# js/category.js). Without parts, the rows of the category chosen with the dropdowns
# (the main group itself if the subgroup dropdown is at "Összesen"), a dataset for
# each type of data; with parts, those of the parts of the whole chosen, of the type
# of data chosen, each part in a slot of its own. The chart has as many slots as a
# whole has parts at most (`slots` of category_data()), each with a dataset and its
# series, and the parts shown take up the first ones, in their order, with the
# series of each slot named after its part (those of the empty slots after none).
# That's much faster than a series for every part of every whole, mostly empty, as
# ECharts goes through all the series whenever anything changes. Each row has its
# year (`yearchr`) and its values in the columns `var`, rounded (see
# round_for_chart()). In stack mode (with
# `stack = TRUE`), each column is split into its negative part, in a column of its
# name with "_neg" appended, and its positive part, in the column itself, as the two
# are stacked separately (see add_category_lines()): the negative part is missing
# except around the years when it's below 0 (with 0 in the years just before and
# after, so that its area tapers off), as a series of zeros would still draw its line
# along 0. In relative mode (with a `base_year`), the values of each part are divided
# by its value in the base year (missing if that isn't positive), chosen with the
# slider ids$base among the years of the rows shown, at first `base_year`: when the
# controls change, the slider keeps its year if the rows have it, else it moves to
# the nearest year they have. The year is also shown beside the slider, and kept in
# its data-year attribute, for the title, which add_chart_layout() sets after this.
# A heatmap (with `heatmap = TRUE`) has the rows of all the slots in a single dataset,
# in the order of the years, each also with the code of its part (`code`) and its
# spending (`spending`, from the column value), which size the rows of the heatmap.
# The other parts of the chart learn which rows are shown from kiadas.onFilter(), so
# this comes before them. (echarts4r doesn't draw the chart, so that it's first drawn
# with its data)
add_category_data <- function(chart, ids, data, var, stack = FALSE, heatmap = FALSE,
                              base_year = NULL) {
  columns <- unname(var)
  parts <- !is.null(data$parts)
  dimensions <- c("yearchr", if (heatmap) "code", columns,
                  if (stack) paste0(columns, "_neg"), if (heatmap) "spending")
  chart$x$opts$dataset <- rep(list(list(dimensions = as.list(dimensions))),
                              if (heatmap) 1L else if (parts) data$slots else
                                length(data$types))
  htmlwidgets::onRender(chart, "kiadas.categoryData",
    data = list(
      rowsId = data$values$rows, valuesIds = data$values$columns,
      columns = as.list(columns), types = if (!parts) as.list(data$types),
      partsId = data$parts_id, slots = data$slots,
      classificationId = ids$classification, typeId = if (parts) ids$type,
      mainId = ids$main, subId = if (!parts) ids$sub, stack = stack, heatmap = heatmap,
      baseId = if (!is.null(base_year)) ids$base,
      year = if (!is.null(base_year)) as.character(base_year)
    ))
}

# Rounds the columns `cols` of the chart data `dt` (see hu_values() and
# plan_actual_data()) by reference, so `dt` must be a copy of HURawData's columns,
# which the page would otherwise have with 16 significant digits: the amounts in
# Mrd Ft to 4 decimals, the precision of the source data (0.1 million Ft), so they
# stay exact, and the percentages to 6 significant digits rather than to a number of
# decimals, so that their ratios to the base year (in relative mode) stay exact
# enough even for the smallest categories. The tooltips and the axes show fewer
# digits anyway: only the downloaded data files show the difference. Other
# columns are left as they are
round_for_chart <- function(dt, cols) {
  mrd <- intersect(cols, c("value", "valuedeflGDP", "valuedeflP3S13", "valueinfl",
                           "spending"))
  pct <- intersect(cols, c("prop", "propParent", "propgdp"))
  if (length(mrd)) dt[, (mrd) := lapply(.SD, round, 4), .SDcols = mrd]
  if (length(pct)) dt[, (pct) := lapply(.SD, signif, 6), .SDcols = pct]
  dt
}

# The filters of a category_chart() (see there) of the columns `var`, with the ids
# `ids` (see category_ids()), on the rows of `paths` (`filter_paths` of
# category_data()). They share a row on wide screens, but the dropdowns are at least
# 20rem wide, so on narrower screens they wrap onto rows of their own. (A Bootstrap
# .row would overflow, as Quarto sets overflow-x: auto on the cell output)
category_filters <- function(ids, paths, var, var_label, parent_var,
                             parts = FALSE, relative = FALSE) {
  htmltools::div(
    class = "d-flex flex-wrap align-items-end row-gap-2 mb-3",
    # Not Bootstrap's column-gap-3: Quarto mistakes any class starting with
    # "column-" for a layout class like column-page, and turns the section into a
    # grid that confines its content to the narrower body column
    style = "column-gap: 1rem",
    htmltools::div(
      # Full width on phones, where it's alone in its row
      class = "flex-grow-1 flex-md-grow-0",
      radio_buttons(
        id = ids$classification,
        label = "Osztályozás:",
        choices = sort(unique(paths$classification)),
        selected = "Magyar",
        compact = TRUE
      )
    ),
    if (parts) htmltools::div(
      class = "flex-grow-1 flex-md-grow-0",
      # Before the main group dropdown, so that when the classification changes,
      # this is updated first, and the dropdown never looks for the main groups of
      # a type that the new classification doesn't have (which would lose its
      # choice)
      filter_radio_dependent(
        id = ids$type,
        label = "Adat:",
        data = paths,
        group = ~typeLabel,
        parent_id = ids$classification,
        parent_group = ~classification,
        selected = "Tervezet (pénzforgalmi)",
        compact = TRUE
      )
    ),
    htmltools::div(
      style = "flex: 1 1 20rem",
      filter_select_dependent(
        id = ids$main,
        label = "Főcsoport:",
        data = paths,
        group = ~main,
        # Showing parts, only the main groups with subgroups of the chosen type
        parent_id = if (parts) c(ids$classification, ids$type) else ids$classification,
        parent_group = if (parts) list(~classification, ~typeLabel) else ~classification,
        selected = if (parts) "Összesen" else "Egészségügy (Magyar: F05, COFOG: GF07)",
        compact = TRUE
      )
    ),
    if (!parts) htmltools::div(
      style = "flex: 1 1 20rem",
      filter_select_dependent(
        id = ids$sub,
        label = "Csoport:",
        data = paths,
        group = ~sub,
        parent_id = c(ids$classification, ids$main),
        parent_group = list(~classification, ~main),
        selected = "Összesen",
        compact = TRUE
      )
    ),
    if (length(var) > 1) htmltools::div(
      class = "flex-grow-1 flex-md-grow-0",
      # While the buttons offer only one column, as they may at first, for
      # "Összesen" (see add_column_choice())
      hidden = if (length(setdiff(var, parent_var)) < 2) NA,
      radio_buttons(id = ids$var, label = var_label, choices = var, compact = TRUE)
    ),
    if (relative) htmltools::div(
      style = "flex: 1 1 20rem",
      htmltools::tags$label(
        class = "form-label small mb-1", `for` = ids$base, "Bázisév: ",
        # The chosen year, filled in by the chart (see add_category_data()), which
        # also gives it to screen readers as the value of the slider
        htmltools::tags$span(id = paste0(ids$base, "_value"), `aria-hidden` = "true")
      ),
      # Its value is the position of the year among those offered
      htmltools::tags$input(type = "range", class = "form-range", id = ids$base,
                            min = 0, max = 0, step = 1, value = 0)
    )
  )
}

# The lines of a category_chart() (see there) of the column var[[1]] (the one
# plotted at first) or, with `stack = TRUE`, its stacked areas, with
# `axis_name[[1]]` on the y axis, and a tooltip of the values in the hovered year,
# followed by `unit_suffix`, with at most `fraction_digits` decimals: a series for
# each type of data, named after it, or showing parts, for each slot (see
# add_category_data()), which is named after the part it shows as it's drawn (see
# category_data() for `data`), each of the dataset of its rows. In stack mode, the
# columns with "_neg" appended to their names have the negative parts
add_category_lines <- function(chart, data, var, axis_name, unit_suffix, fraction_digits,
                               stack = FALSE, relative = FALSE) {
  column <- var[[1]]
  parts <- !is.null(data$parts)
  n <- if (parts) data$slots else length(data$types)
  # The series of a slot have ids, by which the data hook renames them: ECharts would
  # merge a renamed series into the one that has its name already, if any, rather than
  # into the one at its place. In stack mode, "-" ends the id of the series of the
  # negative part, and "+" that of the positive part
  series <- \(k, y, sign = "", ...) c(
    list(datasetIndex = k - 1L, encode = list(x = "yearchr", y = y)),
    if (parts) list(id = paste0("slot", k - 1L, sign)) else list(name = data$types[[k]]),
    list(type = "line", coordinateSystem = "cartesian2d", xAxisIndex = 0L,
         yAxisIndex = 0L, emphasis = list(focus = "series"), ...)
  )
  chart$x$opts$series <- if (stack) {
    # Each part (in its slot) as two series, of its negative and of its positive
    # parts, each stacked only on the series of the same sign, so the negative parts
    # go below 0 and the positive ones above. (ECharts' default would stack a value
    # on the values of its sign, which draws a category that is negative in a year
    # as an area diving from its place in the stack to below 0 and back, and stacks a
    # 0 on the negative values if no category before it is positive in that year.)
    # The series of the negative parts come first, with a symbol only where the
    # part is below 0, not at the zeros around it. triggerLineEvent: hovering an
    # area, not only its points, tells which one it is, for the tooltip
    c(lapply(seq_len(n), \(k) series(
        k, paste0(column, "_neg"), "-", areaStyle = list(), stack = "total",
        stackStrategy = "negative", triggerLineEvent = TRUE,
        symbolSize = htmlwidgets::JS("kiadas.negativeSymbolSize")
      )),
      lapply(seq_len(n), \(k) series(
        k, column, "+", areaStyle = list(), stack = "total", stackStrategy = "positive",
        triggerLineEvent = TRUE
      )))
  } else {
    # In relative mode, as above, hovering a line, not only its points, tells which
    # one it is, for the tooltip
    lapply(seq_len(n), \(k) series(k, column, triggerLineEvent = relative))
  }
  # Showing parts, the legend lists those shown (see add_part_legend())
  chart$x$opts$legend <- list(data = if (parts) list() else as.list(data$types))
  chart |>
    # On phones, the tooltip is often wider than the room beside the finger, or
    # even than the chart (with the long names of the categories), and ECharts
    # would then push it partly off the screen. So it's kept within the chart, and
    # its lines wrap if it's still too wide (but not within a value and its unit).
    # The tooltip of the legend, with the names cut short there, does the same
    e_tooltip(trigger = "axis", confine = TRUE, extraCssText = "white-space: normal;",
              formatter = line_tooltip(fraction_digits, unit_suffix)) |>
    e_y_axis(name = axis_name[[1]],
             axisLabel = list(formatter = axis_number_label))
}

# The heatmap of a category_chart() or a plan_actual_heatmap() (see there), of the
# dataset of the rows shown (see add_category_data() and add_plan_actual_data()): a
# row for each part shown, labelled by its code (in the column `code`), from the top
# in the order of the codes, with a height that follows its weight, its share of the
# whole's spending (in the column `spending`) in the base year or, with
# `base_weights = FALSE`, summed over the years shown (proportional to the weight's
# square root with `row_scale = "sqrt"`, or to the weight itself with "linear": see
# relayout() in js/category.js), a column for each year from the first to the last
# shown, and the cells coloured by the column var[[1]] (or that chosen with
# add_column_choice()), with `axis_name[[1]]` on the y axis, as it tells what the
# colours show. The colours of a ratio (as relative to the base year) run
# continuously on a log scale, so that halving looks as strong as doubling, from red
# at 1/2 through light grey at 1 (no change) to blue at 2, and the ratios beyond take
# the colours at the ends: over a wider range, most ratios would be barely tinted.
# Light grey, so that no change can be told from the cells without a ratio, which are
# left empty. The colour scale beside the plot is `sizes$scale` px tall, labelled at
# 1/2, 0.8, 1, 1.25 and 2 (symmetric around 1 on the log scale, as 0.8 = 1/1.25). The
# columns in `linear` (as differences) are coloured in the same colours on a linear
# scale symmetric around 0 instead (see restyle() in js/category.js). As the y axis
# only shows the codes, the legend lists the full names of the rows shown,
# `row_names` (by code), of which there are `slots` at most (see the legend below),
# and so does the tooltip, with the year and the value of the hovered cell,
# followed by `unit_suffix`, with at most `fraction_digits` decimals or, with
# `plan_actual` (the names of the plan and the actual spending), with what the
# tooltip of a plan_actual_chart() shows of a point (see kiadas.planActualTooltip()),
# from the columns `plan` and `actual`, with the same unit and decimals; and then
# with the weight of the row, as `weight_label` says. `sizes` also gives the average
# height of a row (`row`) and the least height of the plot (`plot`), in px, as
# add_chart_layout() lays it out
add_category_heatmap <- function(chart, var, axis_name, row_names, unit_suffix,
                                 fraction_digits, sizes, row_scale, slots,
                                 linear = character(), base_weights = TRUE,
                                 weight_label = "Súlya a bázisévben",
                                 plan_actual = NULL) {
  # ColorBrewer's RdBu, from decrease (red) to increase (blue), but light grey in the
  # middle (see above), spread evenly over the log2 of the ratios from -1 to 1, and
  # blended in the CIELAB colour space, where the lightness changes evenly
  ramp <- grDevices::colorRamp(c("#b2182b", "#ef8a62", "#fddbc7", "#e0e0e0", "#d1e5f0",
                                 "#67a9cf", "#2166ac"), space = "Lab")
  color_at <- \(log_ratio) grDevices::rgb(ramp((log_ratio + 1) / 2), maxColorValue = 255)
  # ECharts' visualMap only maps values linearly, so the cells are coloured by pieces
  # 1/32 wide on the log2 scale (or on a linear one, see restyle() in
  # js/category.js), too narrow to tell apart, each as its middle (the ones at the
  # ends also taking every value beyond). Its own legend can't label a gradient
  # between its ends, so it's hidden, and the colour scale drawn by hand
  middles <- seq(-1, 1, by = 1 / 32)
  number <- \(x) sub(".", ",", as.character(x), fixed = TRUE)
  ticks <- c(0.5, 0.8, 1, 1.25, 2)
  scale <- list(
    height = sizes$scale,
    # The colours of the bar, from the bottom up
    stops = Map(\(offset, color) list(offset = offset, color = color),
                (middles + 1) / 2, color_at(middles)),
    # From the bottom up, at their positions as fractions of the height of the bar
    ticks = Map(\(position, label) list(position = position, label = label),
                (log2(ticks) + 1) / 2,
                c(paste("\u2264", number(ticks[1])), number(ticks[2:4]),
                  paste("\u2265", number(ticks[5]))))
  )

  # A cell for each row of the dataset, set up as a heatmap, but drawn by hand, as a
  # custom series (see renderItem() in js/category.js), as the rows of a heatmap are
  # all equally tall. The legend lists series without data, without icons, so one
  # name in each line, so that it's clear where one ends and the next one starts:
  # one for each row shown, as many as there may be (`slots`), which are named after
  # the rows by their ids as they're laid out (see relayout() in js/category.js), and
  # the others after none. (Rather than one for each name, which ECharts would go
  # through whenever anything changes.) Its names can be switched off, as ECharts makes
  # the items of a legend ignore the mouse otherwise, but they're switched back on at
  # once (see js/category.js)
  chart$x$opts$series <- c(
    list(list(
      datasetIndex = 0L, encode = list(x = "yearchr", y = "code", value = var[[1]]),
      type = "custom", coordinateSystem = "cartesian2d",
      # So that it can be updated by itself (see add_column_choice())
      id = "heatmap",
      # Nothing to draw until the rows are laid out
      renderItem = htmlwidgets::JS("function() { return null; }")
    )),
    lapply(seq_len(slots), \(k) {
      list(id = paste0("legend", k - 1L), name = " ", type = "line", data = list())
    })
  )
  chart$x$opts$legend <- list(data = list(), orient = "vertical", icon = "none",
                              itemWidth = 0)
  # Its pieces are set by restyle() (in js/category.js), before any cell is drawn
  chart$x$opts$visualMap <- list(list(
    type = "piecewise", show = FALSE, seriesIndex = 0, dimension = var[[1]]
  ))
  # The rows take up their weights' shares of the y axis, from 0 at the top to 1 at
  # the bottom, labelled with their codes instead of numbers (see relayout() in
  # js/category.js). Its name starts above the rows: centred on the axis, as in the
  # other modes, it would push the plot to the right on phones (ECharts keeps it
  # within the chart)
  chart$x$opts$yAxis <- list(list(
    type = "value", min = 0, max = 1, inverse = TRUE, name = axis_name[[1]],
    nameLocation = "start", nameTextStyle = list(align = "left"),
    axisLine = list(show = FALSE), axisTick = list(show = FALSE),
    splitLine = list(show = FALSE), axisLabel = list(customValues = list())
  ))

  chart |>
    # Room on the left for the codes
    e_grid(left = 60) |>
    # Kept within the chart, with its lines wrapped, as the tooltip of the lines
    e_tooltip(trigger = "item", confine = TRUE, extraCssText = "white-space: normal;") |>
    # The cells line the axes, which would only show beside the empty ones. (Not
    # at the 0 of the y axis, which is at the top)
    e_x_axis(axisLine = list(show = FALSE, onZero = FALSE)) |>
    htmlwidgets::onRender("kiadas.categoryHeatmap",
      data = list(codes = as.list(names(row_names)), names = row_names, slots = slots,
                  unit = unit_suffix, digits = fraction_digits, scale = scale,
                  sizes = sizes, rowScale = row_scale, colors = as.list(color_at(middles)),
                  columns = as.list(unname(var)), linear = as.list(linear),
                  baseWeights = base_weights, weightLabel = weight_label,
                  planActual = if (!is.null(plan_actual)) as.list(plan_actual)))
}

# The credit below a category_chart() or a plan_actual_chart() and the x axis,
# named `x_name` (by default as the axis of the years)
add_chart_frame <- function(chart, x_name = "Év") {
  chart <- chart |>
    e_title(subtext = "Ferenci Tamás (medstat.hu)", bottom = 0, right = 10,
            sublink = "https://www.medstat.hu/", subtarget = "blank") |>
    # The first and last years (or values) are always labelled, not only when
    # ECharts' step between labels happens to land on them. A label that would
    # collide with them is hidden instead (with hideOverlap, also one that would
    # come within a few pixels)
    e_x_axis(name = x_name,
             axisLabel = list(showMinLabel = TRUE, showMaxLabel = TRUE,
                              hideOverlap = TRUE))
  # ECharts shows the tooltip where it last was (at first, the middle of the chart)
  # before moving it into place, so it may stick out of the chart for a moment,
  # which is enough for phones to widen the page, and leave it widened. The chart
  # clips it, which hides nothing, as the tooltip always ends up within the chart
  htmlwidgets::onRender(chart, "function(el) { el.style.overflow = 'hidden'; }")
}

# For screen readers, a category_chart() or a plan_actual_chart() (see there) is
# an image, described by its title, its axes and the values shown, formatted as in
# the tooltip (with `unit_suffix` and at most `fraction_digits` decimals, each either
# one for all the values, or a list of one for each column, by column). `type`
# names the kind of chart (or, as a list, the kind for each type of the x axis, as
# a scatter plot's axes may be linear or logarithmic). A heatmap's values are
# listed by row, under the rows' names in `row_names` (by their values on the y
# axis), and its y axis is said to show `rows_name`. A scatter plot's, with
# `points`, are listed by point, under the name of its series: each labelled by
# its column points$label in the data, with its values on the x and the y axis,
# named points$x and points$y. (ECharts' own description, with e_aria(), would be
# in English and list every column of the data, for every series, even the ones
# filtered out)
add_chart_description <- function(chart, type, unit_suffix, fraction_digits,
                                  row_names = NULL, points = NULL,
                                  rows_name = "kategóriák") {
  chart |>
    htmlwidgets::onRender("kiadas.chartDescription",
      data = list(type = type, unit = unit_suffix, digits = fraction_digits,
                  rowNames = row_names, points = points, rowsName = rows_name))
}

# With several columns in `var`, a category_chart() (see there) plots the one
# chosen with its radio buttons (see category_ids() for `ids`), with its name in
# `axis_name` on the y axis. `parent_var` are the columns relative to the main
# group, which can't be chosen while it's "Összesen", showing `parts`. A heatmap
# colours its cells by the chosen column
add_column_choice <- function(chart, ids, var, axis_name, parent_var, parts = FALSE,
                              heatmap = FALSE) {
  if (length(var) > 1) {
    # Plots the chosen column, with its axis name. Also run at the start, in case
    # the browser restored an earlier choice when reloading the page. Showing
    # parts, while the main group dropdown is at "Összesen" (or empty), the buttons
    # don't offer the columns relative to the main group: if one of those is chosen,
    # the first column offered is plotted (and checked)
    # instead, until a main group is chosen again. The buttons are hidden while
    # they offer only one column
    chart <- chart |>
      htmlwidgets::onRender("kiadas.columnChoice",
        data = c(list(id = ids$var, axisNames = as.list(setNames(axis_name, var)),
                      parentColumns = as.list(unname(parent_var)), heatmap = heatmap),
                 if (parts) list(mainId = ids$main)))
  }
  chart
}

# Showing parts, a category_chart() (see there) colours its lines or stacked areas,
# lists only the parts shown in the legend (see add_part_legend()), and shows only
# the years from their first to their last. With `stack = TRUE` and `unit = "%"`,
# the stacked areas are kept within 0 and 100% (see below)
add_part_lines <- function(chart, stack = FALSE, unit = "") {
  if (stack && unit == "%") {
    # The positive parts of a whole stack up to 100%, but rounding errors can put
    # the top a hair above it, and negative parts too small to see push it up as
    # much as they go below 0. ECharts would then run the axis to 120, or down to
    # -20, so an overshoot of less than 0.5% of the axis is cut off, above 100 and
    # below 0. (When the top stays at or below 100, so does ECharts' own maximum.)
    # See kiadas.stackMax() and kiadas.stackMin() in js/kiadas.js
    chart <- e_y_axis(chart, max = htmlwidgets::JS("kiadas.stackMax"),
                      min = htmlwidgets::JS("kiadas.stackMin"))
  }

  chart |>
    add_part_legend() |>
    htmlwidgets::onRender("kiadas.partLines")
}

# The colours of the parts in a chart of parts (see add_part_legend()) or in a
# country_stack_chart(), and of the lines of a country_lines_chart(): ECharts'
# default palette has 9 colours, so this
# adds 7 for the 16 Hungarian main groups, each picked (among colours of moderate
# lightness and chroma) to differ as much as possible from all the colours before it,
# by CIEDE2000
chart_palette <- c("#5070dd", "#b6d634", "#505372", "#ff994d", "#0ca8df", "#ffd10a",
                   "#fb628b", "#785db0", "#3fbe95", "#666018", "#a04024", "#c3b0e4",
                   "#ab2965", "#167a4e", "#ae9d69", "#027588")

# Colours the series of a chart of parts (a category_chart() showing parts, or a
# plan_actual_chart(), whose parts are its categories), each of the dataset of a part
# (in stack mode, two series), or of a slot that shows one (see add_category_data()
# and add_plan_actual_data()), and named after the part. The legend only lists the
# parts shown, those with rows, coloured as if they were the only ones (from the
# start, and whenever the rows shown change). Series added after this (as the line
# of a plan_actual_chart()) are left out of the legend, and keep their own colours
add_part_legend <- function(chart) {
  datasets <- vapply(chart$x$opts$series, \(s) {
    if (is.null(s$datasetIndex)) NA_integer_ else as.integer(s$datasetIndex)
  }, 0L)
  chart |>
    e_color(chart_palette) |>
    htmlwidgets::onRender("kiadas.partLegend",
      data = list(datasets = as.list(datasets)))
}

# The title of a category_chart() (see there), if any, from `title` and the choices
# of its filters (see category_ids() for `ids`), and the room for it, for the legend
# and, in a heatmap, for its rows, `row_height` px each on average, with the plot
# at least `min_plot_height` px tall: see below. `paths` are the parts shown, if any
# (see category_data()). With `second = FALSE`, as in a plan_actual_chart(), whose
# title only follows the main group dropdown, `paths` are the rows of its choices
# (`choices`, see plan_actual_data()), and the title only names the main group
# (unless it's "Összesen"). The chart is also laid out again whenever the controls
# with the ids `watch_ids` change, as they change the legend (as the radio buttons
# of the colouring of a plan_actual_chart()), and whenever its data hook shows other
# rows (see kiadas.showView() in js/kiadas.js). Unless `year_label` is NULL, the title
# ends with the year chosen with the slider `ids$base` (kept in its data-year
# attribute), after `year_label`: in relative mode by default, the base year, after
# "bázisév: ". A chart with several legends (as a country_stack_chart(), with one of
# the parts and one of the countries) has them one above the other at the bottom, the
# first one at the top
add_chart_layout <- function(chart, title, ids, paths = NULL, relative = FALSE,
                             row_height = NULL, min_plot_height = 0, second = TRUE,
                             watch_ids = NULL, year_label = if (relative) "bázisév: ") {
  parts <- !is.null(paths)
  # The end of the title for each choice of the main group dropdown and of the
  # subgroup dropdown or, showing parts, of the type (or for each choice of the
  # main group dropdown alone, "" for "Összesen")
  if (!is.null(title)) {
    name_of <- \(label) HURawData$name[match(label, HURawData$nameLabel)]
    ends <- if (!second) {
      unique(paths[, .(main)])[, end := fifelse(main == "Összesen", "", name_of(main))]
    } else if (parts) {
      unique(paths[, .(main, second = as.character(typeLabel))])[, end := fifelse(
        main == "Összesen", second, paste0(name_of(main), ", ", second)
      )]
    } else {
      unique(CategoryPaths[, .(main, second = sub)])[, end := fifelse(
        second == "Összesen", name_of(main),
        paste0(name_of(main), ", ", name_of(second))
      )]
    }
  }

  # The title, if any, with room for it above the plot, and room for the legend
  # below it: ECharts puts the legend at the bottom, and wraps it into more lines
  # when the chart is too narrow for one (as on phones, or showing parts, when it
  # has many items), but only leaves room for one, so the others would cover the
  # horizontal axis. If a name is too long for a line of the legend (as on
  # phones), the names are cut short with an ellipsis, and shown whole on hover.
  # With several legends, each is laid out so, and room is left for all of them
  # (see legendLayout() in js/layout.js)
  chart |>
    htmlwidgets::onRender("kiadas.chartLayout",
      data = list(
        title = title, mainId = ids$main,
        secondId = if (second) { if (parts) ids$type else ids$sub },
        baseId = if (!is.null(year_label)) ids$base, yearLabel = year_label,
        downloadId = ids$download, rowHeight = row_height,
        minPlotHeight = min_plot_height, watchIds = as.list(watch_ids),
        ends = if (!is.null(title)) {
          if (second) {
            lapply(split(setNames(ends$end, ends$second), ends$main), as.list)
          } else as.list(setNames(ends$end, ends$main))
        }
      ))
}

# Renders the chart with the id `id` (a category_chart()) only once it comes within a
# screen's height of the viewport, or before printing (see kiadas.defer() in
# js/kiadas.js), so that loading the page only draws the charts in view. Comes
# after the chart and its download menu, which the chart's code uses
defer_chart <- function(id) {
  htmltools::tags$script(htmltools::HTML(
    sprintf("kiadas.defer(%s);", jsonlite::toJSON(id, auto_unbox = TRUE))
  ))
}

# The HTML of a chart, as the chart functions return it: first `controls` (its
# filters and other controls, and whatever they need), then the chart in its box
# with its download menu, with the id ids$download (see add_download_menu(), which
# `unit` and `...` go to), and the script that has the chart rendered once it comes
# near the viewport (see defer_chart()), all with the page's own JavaScript (see
# kiadas_dependency)
chart_block <- function(controls, chart, ids, unit, ...) {
  htmltools::attachDependencies(
    htmltools::tagList(
      controls,
      add_download_menu(chart, unit, id = ids$download, ...),
      defer_chart(ids$chart)
    ),
    kiadas_dependency
  )
}

# The names of the two types of data that a plan_actual_chart() and a
# plan_actual_heatmap() compare, the planned and the actual spending, as elsewhere
# on the page
plan_actual_labels <- as.character(
  HURawData$typeLabel[match(c("original_plan", "actual"), HURawData$type)]
)

# The filters and the scatter plot of the planned (on the x axis) against the
# actual (on the y axis) spending in "Eredmények", in the column `var` of HURawData,
# with a point for each category in each year: for the main groups if the main
# group dropdown is at "Összesen", else for the subgroups of the chosen main group
# or, if it has none, for the main group itself, coloured by category. Only the
# Hungarian classification has both types of data (COFOG's data are all actual),
# and only the years with both count. The dashed line marks where the actual
# spending equals the plan. `axis_name` names the x and the y axis, and `unit`
# (unless it's "") follows the values in the tooltip, which also gives the
# deviation from the plan. Radio buttons switch both axes between linear and
# logarithmic (at first logarithmic with `log_scale = TRUE`): on log axes,
# categories of very different sizes all show, and the vertical distance from the
# line shows the relative deviation, but the points whose plan or actual value
# isn't positive are left out. Further radio buttons colour the points by category
# (at first) or by year (see add_plan_actual_colors()). The chart has no title if
# `title` is NULL, else `title` followed by the name of the chosen main group
# (unless it's "Összesen").
# As in category_chart(), each call has its own element ids (made from `id`), and
# the chart is only drawn once it comes near the viewport, starting from the
# choices of its controls. The helpers below and those of category_chart() make its
# parts
plan_actual_chart <- function(axis_name, unit, title = NULL, var = "value",
                              log_scale = TRUE, id = paste0(var, "_plan_actual")) {
  ids <- c(category_ids(id), scale = paste0(id, "_scale_filter"),
           color = paste0(id, "_color_choice"))
  # The choices of the radio buttons of the axes
  scales <- c(linear = "Lineáris", log = "Logaritmikus")
  data <- plan_actual_data(var)
  unit_suffix <- unit_suffix_for(unit)

  # A series for each slot (see add_plan_actual_data()), of the dataset of the points
  # of the category in it, in the order of the years, so that the downloaded data and
  # the description list them so. They have ids, by which the data hook names them
  # after their categories (see add_category_lines()). The legend lists the
  # categories shown (see add_part_legend())
  chart <- data_chart(ids$chart)
  chart$x$opts$xAxis <- list(list(type = "value"))
  chart$x$opts$series <- lapply(seq_len(data$slots), \(k) list(
    datasetIndex = k - 1L, encode = list(x = "plan", y = "actual", itemId = "key"),
    id = paste0("slot", k - 1L), type = "scatter", coordinateSystem = "cartesian2d",
    xAxisIndex = 0L, yAxisIndex = 0L, symbolSize = 8, emphasis = list(focus = "series")
  ))
  chart$x$opts$legend <- list(data = list())
  chart <- chart |>
    add_plan_actual_data(ids, data, points = TRUE, log_value = scales[["log"]]) |>
    # Before the line is added (see add_plan_actual_axes()), which isn't a category
    add_part_legend() |>
    add_chart_frame(x_name = axis_name[[1]]) |>
    add_plan_actual_axes(axis_name[[2]], plan_actual_labels, unit_suffix, ids,
                         scales[["log"]], log_scale) |>
    add_plan_actual_colors(ids, data$point_years) |>
    add_chart_description(
      # For each type of the axes
      lapply(c(log = "logaritmikus", value = "lineáris"), \(type) paste0(
        "Pontdiagram ", type, " tengelyekkel, a szaggatott vonalon a tényleges ",
        "érték megegyezik a tervezettel"
      )),
      unit_suffix, 1L,
      points = list(label = "yearchr", x = plan_actual_labels[[1]],
                    y = plan_actual_labels[[2]])
    ) |>
    add_chart_layout(title, ids, data$choices, second = FALSE, watch_ids = ids$color)

  chart_block(
    htmltools::tagList(
      data$element,
      plan_actual_filters(
        ids, data$choices,
        radio_buttons(id = ids$scale, label = "Tengelyek:", choices = unname(scales),
                      selected = scales[[if (log_scale) "log" else "linear"]],
                      compact = TRUE),
        # It only changes the colours (see add_plan_actual_colors())
        radio_buttons(id = ids$color, label = "Színezés:",
                      choices = c("Kategória" = "category", "Év" = "year"), compact = TRUE)
      )
    ),
    chart, ids, unit, choice_id = ids$scale,
    point_columns = setNames(
      c("yearchr", "plan", "actual"),
      c("Év", paste0(plan_actual_labels, if (unit != "") paste0(" [", unit, "]")))
    )
  )
}

# The data of a plan_actual_chart() or a plan_actual_heatmap() (see there) of the
# column `var` of HURawData, rounded (see round_for_chart()), of the Hungarian
# categories with both a plan and an actual value in some year: the `categories`, in
# the order of their codes, with their main groups (`main`, as in category_data():
# "Összesen" for the main groups), their `nameLabel` and `code`; the `years` from
# the first to the last with both for any category, and those with both for some
# category (`point_years`); in an element of JSON (`element`, with the id
# `values_id`), those `years`, the `codes` and the `labels` (their nameLabel) of the
# categories, and for each category, in each of those years (null where it's
# missing), its plan (`plan`), its actual value (`actual`), their ratio (`ratio`,
# missing where the plan isn't positive) and difference (`difference`), rounded as
# in round_for_chart(), and the plan's share of all the planned spending of its year
# (`spending`, in %), which weights the rows of a plan_actual_heatmap(); `choices`, a
# row for each choice of the main group dropdown (`main`) and each category it shows
# (by its position in `categories`, from 0: `k`), and the same as a list of the
# positions by choice (`shown`); and the most categories a choice shows (`slots`),
# as the charts draw each in a slot of its own (see add_plan_actual_data() and
# add_category_heatmap()). "Összesen" shows the main groups, and a main group its
# subgroups or, if it has none, itself
plan_actual_data <- function(var) {
  types <- c(original_plan = "plan", actual = "actual")
  long <- CategoryPaths[
    classification == "Magyar" & HURawData$type[key] %chin% names(types), .(
      main = fifelse(sub == "Összesen", "Összesen", main), nameLabel, code = node,
      yearchr = HURawData$yearchr[key], type = unname(types[HURawData$type[key]]),
      value = HURawData[[var]][key]
    )
  ]
  # Named as the column, which tells how it's rounded
  setnames(long, "value", var)
  round_for_chart(long, var)
  points <- dcast(long, main + nameLabel + code + yearchr ~ type, value.var = var)
  both <- points[!is.na(plan) & !is.na(actual)]
  years <- range(as.integer(both$yearchr))
  years <- as.character(seq(years[1], years[2]))
  points <- points[unique(both[, .(main, nameLabel, code)])[, .(yearchr = years),
                                                            by = .(main, nameLabel, code)],
                   on = .(main, nameLabel, code, yearchr)]
  setorder(points, code, yearchr)
  shares <- CategoryPaths[
    classification == "Magyar" & HURawData$type[key] == "original_plan",
    .(code = node, yearchr = HURawData$yearchr[key], spending = HURawData$prop[key])
  ]
  points <- shares[points, on = .(code, yearchr)]
  points[, `:=`(ratio = signif(fifelse(plan > 0, actual / plan, NA_real_), 6),
                difference = round(actual - plan, 4), spending = signif(spending, 6))]
  categories <- unique(points[, .(main, nameLabel, code)])
  by_category <- \(column) {
    unname(split(points[[column]], factor(points$code, levels = categories$code)))
  }
  values <- shared_json(jsonlite::toJSON(
    list(years = years, codes = categories$code,
         labels = as.character(categories$nameLabel), plan = by_category("plan"),
         actual = by_category("actual"), ratio = by_category("ratio"),
         difference = by_category("difference"), spending = by_category("spending")),
    na = "null", digits = 16, use_signif = TRUE
  ))
  # "Összesen" first in the dropdown, then the main groups in the order of their codes
  categories[, k := .I - 1L]
  choices <- rbind(
    categories[, .(main, code, k)],
    categories[main == "Összesen" & !as.character(nameLabel) %chin% categories$main,
               .(main = as.character(nameLabel), code, k)]
  )[order(main != "Összesen", code)]
  list(categories = categories, years = years, point_years = sort(unique(both$yearchr)),
       values_id = values$id, element = values$element, choices = choices,
       shown = lapply(split(choices$k, choices$main), as.list),
       slots = max(table(choices$main)))
}

# Draws a plan_actual_chart() or a plan_actual_heatmap() (see there), with the ids
# `ids` (see there), with the rows of `data` (see plan_actual_data()) of the
# categories that the main group dropdown shows: at first, and again whenever its
# controls change, in the browser (see kiadas.planActualData() in js/plan-actual.js).
# With `points = TRUE`, as in a plan_actual_chart(), the rows of each category are a
# dataset, in a slot of its own, as the parts of a category_chart() (see
# add_category_data()): the chart has as many slots as the dropdown shows categories
# at most (`slots` of plan_actual_data()), and the categories with rows take up the
# first ones, in their order, with the series of each slot named after its category.
# The rows of a category are those with both a plan (`plan`) and an actual value
# (`actual`) and, while the radio buttons of the axes are at `log_value`, only those
# with positive ones, which log axes can show; each with its year (`yearchr`) and a
# `key`, by which ECharts pairs the points before and after a change as it animates
# it (by default it would pair them by their order, so that when some leave, as when
# a year is switched off in the legend, the others would slide into their places).
# The points of the years switched off in the legend of the years are left out (see
# add_plan_actual_colors()). Otherwise, as in a plan_actual_heatmap(), all the rows
# of the categories shown are a single dataset, in the order of the years (each
# year's rows in the order of the categories), each of a cell, with the category's
# `code`, and the `ratio`, the `difference` and the `spending` of plan_actual_data().
# The other parts of the chart learn which rows are
# shown from kiadas.onFilter(), so this comes before them. (echarts4r doesn't draw
# the chart, so that it's first drawn with its data)
add_plan_actual_data <- function(chart, ids, data, points = FALSE, log_value = NULL) {
  dimensions <- if (points) c("yearchr", "plan", "actual", "key") else
    c("yearchr", "code", "ratio", "difference", "plan", "actual", "spending")
  chart$x$opts$dataset <- rep(list(list(dimensions = as.list(dimensions))),
                              if (points) data$slots else 1L)
  htmlwidgets::onRender(chart, "kiadas.planActualData",
    data = list(valuesId = data$values_id, choices = data$shown, slots = data$slots,
                mainId = ids$main, scaleId = if (points) ids$scale, logValue = log_value,
                points = points))
}

# The filters of a plan_actual_chart() or a plan_actual_heatmap() (see there),
# with the ids `ids` (see there), laid out as in category_filters(): the main group
# dropdown, on the rows of `choices` (see plan_actual_data()), at "Összesen" at
# first, followed by the radio buttons in `...` (leaving out NULLs)
plan_actual_filters <- function(ids, choices, ...) {
  htmltools::div(
    class = "d-flex flex-wrap align-items-end row-gap-2 mb-3",
    # Not Bootstrap's column-gap-3, as in category_filters()
    style = "column-gap: 1rem",
    htmltools::div(
      style = "flex: 1 1 20rem",
      filter_select_dependent(
        id = ids$main,
        label = "Főcsoport:",
        data = choices,
        group = ~main,
        selected = "Összesen",
        compact = TRUE
      )
    ),
    lapply(Filter(Negate(is.null), list(...)), \(buttons) {
      # Full width on phones, where it's alone in its row
      htmltools::div(class = "flex-grow-1 flex-md-grow-0", buttons)
    })
  )
}

# The axes, the tooltip and the dashed line of a plan_actual_chart() (see there),
# with the ids `ids` (see there): the x axis of the plans (named by
# add_chart_frame()), and the y axis, named `y_name`, of the actual values, both
# logarithmic if the radio buttons of the axes are at `log_value`, else linear (at
# first as `log_scale` says, until the buttons are read). Both run over the same
# range, so that the line, where the actual value equals the plan, runs from corner
# to corner: from the smallest to the largest value of the points shown (from the
# start, and whenever they change, as with the type of the axes), with those of the
# years switched off in the legend, on log axes widened to 1, 2 or 5 times a power of
# 10, on linear ones from 0 (unless a value is negative) to the first tick at or
# above the largest value, with the ticks spaced as ECharts would space them. The
# tooltip shows the hovered point's category and year, its plan and actual value
# (named by `type_labels`, followed by `unit_suffix`), and its deviation from the
# plan (see kiadas.planActualTooltip())
add_plan_actual_axes <- function(chart, y_name, type_labels, unit_suffix, ids,
                                 log_value, log_scale = TRUE) {
  axis <- list(
    type = if (log_scale) "log" else "value",
    axisLabel = list(
      showMinLabel = TRUE, showMaxLabel = TRUE, hideOverlap = TRUE,
      formatter = axis_number_label
    )
  )
  chart$x$opts$xAxis[[1]] <- modifyList(chart$x$opts$xAxis[[1]], c(axis, list(
    # Below the labels, as at the end of the axis (where the years' name is) it
    # would stick out of the chart
    nameLocation = "middle", nameGap = 30
  )))
  chart$x$opts$yAxis[[1]] <- modifyList(chart$x$opts$yAxis[[1]],
                                        c(axis, list(name = y_name)))
  # Under the points, left out of the tooltip, and drawn once the range is known
  # (see js/plan-actual.js). Its colour is its own, not ECharts' colour of its place
  # among the series
  chart$x$opts$series <- c(chart$x$opts$series, list(list(
    id = "diagonal", type = "line", data = list(), symbol = "none", silent = TRUE,
    z = 1, color = "#888", lineStyle = list(type = "dashed", width = 1)
  )))

  chart |>
    # Kept within the chart, with its lines wrapped, as the tooltip of the lines
    e_tooltip(trigger = "item", confine = TRUE, extraCssText = "white-space: normal;") |>
    # Room for the name of the x axis, between its labels and the legend
    e_grid(bottom = 100) |>
    htmlwidgets::onRender("kiadas.planActualAxes",
      data = list(scaleId = ids$scale, logValue = log_value,
                  labels = as.list(type_labels), unit = unit_suffix))
}

# The colouring of a plan_actual_chart() (see there), with the ids `ids` (see
# there), chosen with its radio buttons: by category, the series as
# add_part_legend() colours them, or by year, each point in the colour of its year
# among `years` (from dark to light along viridis, without its lightest yellows,
# which would hardly show on white), with the legend then listing the years of the
# points shown. Hovering a year in the legend, or a point, then highlights the
# points of the year and fades the others, as hovering a point highlights its
# category when coloured by category. Clicking a year in the legend switches its
# points off and on, as clicking a category does: they're then left out of the
# description and the data files too (see add_plan_actual_data()), but the axes stay
# as they are. Categories and years switched off stay off while the colouring
# changes, so that, e.g., the points of a few categories can be coloured by year
add_plan_actual_colors <- function(chart, ids, years) {
  colors <- setNames(
    grDevices::colorRampPalette(grDevices::hcl.colors(9, "viridis")[1:8])(length(years)),
    years
  )
  # Series without data, one for each year, which give the legend its years, as
  # in add_category_heatmap(). Highlighting one fades all the points (see
  # js/plan-actual.js)
  chart$x$opts$series <- c(chart$x$opts$series, lapply(years, \(year) {
    list(name = year, type = "scatter", data = list(), color = colors[[year]],
         emphasis = list(focus = "self"))
  }))

  htmlwidgets::onRender(chart, "kiadas.planActualColors",
    data = list(colorId = ids$color, colors = as.list(colors)))
}

# The filters and the heatmap of how much the actual spending in "Eredmények", in
# the column `var` of HURawData, deviated from the plan over the years, as the
# heatmap of category_chart() (see add_category_heatmap()): a row for each
# category, for the main groups if the main group dropdown is at "Összesen", else
# for the subgroups of the chosen main group or, if it has none, for the main group
# itself, labelled by its code (and named in full in the legend and the tooltip),
# and a column for each year. The cells are coloured by one of `metrics`, chosen
# with radio buttons labelled `metric_label`, which offer them by their names (the
# first one at first): the ratio of the actual spending to the plan ("ratio",
# missing where the plan isn't positive), on the log scale of ratios, or their
# difference ("difference"), on a linear scale around 0, with `axis_name` on the y
# axis for each, as it tells what the colours show. The tooltip shows the hovered
# cell as that of a plan_actual_chart() shows a point: its category and year, its
# plan and actual value, followed by `unit` (unless it's ""), and its deviation
# from the plan, and then the weight of its row. The heights of the rows follow the
# categories' weights, their shares of the planned spending summed over the years
# shown (proportional to the weights' square roots with `row_scale = "sqrt"`, or
# to the weights themselves with "linear"). As in plan_actual_chart(), only the
# Hungarian classification has both types of data, and a year without both leaves
# the cell empty (as 2002, without a plan). The chart has no title if `title` is
# NULL, else `title` followed by the name of the chosen main group (unless it's
# "Összesen"). As in category_chart(), each call has its own element ids (made from
# `id`), and the chart is only drawn once it comes near the viewport
plan_actual_heatmap <- function(axis_name, unit, title = NULL, var = "value",
                                metrics = c("Hányados" = "ratio",
                                            "Különbség" = "difference"),
                                metric_label = "Mérőszám:",
                                row_scale = c("sqrt", "linear"),
                                id = paste0(var, "_plan_actual_heatmap")) {
  row_scale <- match.arg(row_scale)
  ids <- category_ids(id)
  data <- plan_actual_data(var)
  columns <- unname(metrics)
  # By column, in the description for screen readers: a ratio has no unit, and
  # needs two decimals, as in relative mode in category_chart()
  unit_suffix <- list(ratio = "", difference = unit_suffix_for(unit))
  fraction_digits <- list(ratio = 2L, difference = 1L)
  row_names <- heatmap_row_names(data$categories)

  chart <- data_chart(ids$chart, data$years) |>
    add_plan_actual_data(ids, data) |>
    # Its tooltip gives the plan and the actual value, with at most one decimal, as
    # that of plan_actual_chart()
    add_category_heatmap(columns, axis_name, row_names, unit_suffix_for(unit), 1L,
                         heatmap_sizes, row_scale, data$slots,
                         linear = intersect(columns, "difference"), base_weights = FALSE,
                         weight_label = "Átlagos súlya a tervekben",
                         plan_actual = plan_actual_labels) |>
    add_chart_frame() |>
    add_chart_description(
      paste("Hőtérkép, a sorok magassága a kategóriák tervekben mért átlagos",
            if (row_scale == "sqrt") "súlyának négyzetgyökével" else "súlyával",
            "arányos"),
      unit_suffix, fraction_digits, row_names
    ) |>
    add_column_choice(ids, columns, axis_name, character(), heatmap = TRUE) |>
    add_chart_layout(title, ids, data$choices, second = FALSE,
                     row_height = heatmap_sizes$row,
                     min_plot_height = heatmap_sizes$plot)

  chart_block(
    htmltools::tagList(
      data$element,
      plan_actual_filters(
        ids, data$choices,
        if (length(metrics) > 1) {
          radio_buttons(id = ids$var, label = metric_label, choices = metrics,
                        compact = TRUE)
        }
      )
    ),
    # Without a unit in the headers of the data files, as they start with the name of
    # the y axis, which gives the unit of the metric chosen
    chart, ids, "", choice_id = if (length(metrics) > 1) ids$var,
    download_axis_name = TRUE, row_names = row_names
  )
}

# The Hungarian names of the countries in RawData, by their codes (geo): the member
# states of the EU (see countries.R)
country_names <- setNames(CountryTable$name, CountryTable$geo)
# And those of the groups of countries, their codes (see countries.R), whose prop,
# propgdp and propParent are the averages of their members', weighted by their GDP. A
# country_chart() shows them among the countries, in grey
group_names <- setNames(names(country_groups), names(country_groups))

# The ids of the elements of a chart of the countries with the id `id`, named as
# category_ids() names them, as add_chart_layout() reads them: of its main group
# dropdown, of its subgroup dropdown (unless `sub` is FALSE), of the radio buttons of
# its columns, of the slider of its year (`base`, unless `year` is FALSE), of its
# download menu, and of the chart itself
country_ids <- function(id, sub = TRUE, year = TRUE) {
  c(list(main = paste0(id, "_main_group")),
    if (sub) list(sub = paste0(id, "_subgroup")),
    list(var = paste0(id, "_var_choice")),
    if (year) list(base = paste0(id, "_year")),
    list(download = paste0(id, "_download"), chart = paste0(id, "_chart")))
}

# The names of the countries `countries` (those with data: see country_data()) in
# the order the charts of the countries list them: first those of `first` (by their
# codes, which must have names: see country_names and group_names), in their order,
# then the others, in alphabetical order by the Hungarian rules (in which É goes with
# E). A list of the names of `first` with data (`first`), and of all the countries,
# in that order (`all`)
country_order <- function(first, countries) {
  geo_names <- c(country_names, group_names)
  absent <- setdiff(first, names(geo_names))
  if (length(absent) > 0) stop("No name for the countries ", toString(absent))
  first <- intersect(geo_names[first], countries)
  list(first = first,
       all = c(first, stringi::stri_sort(setdiff(countries, first), locale = "hu")))
}

# The HTML of a chart of the countries (see chart_block()) with the ids `ids` (see
# country_ids()), of the `data` of country_data(): the element of the values of the
# countries, unless the page already has it (see country_values_element()), and the
# `controls` (see country_filters()), which choose the plotted column with radio
# buttons if `var` has several, then the chart, whose data files start with the name
# of the y axis, which tells what the values are relative to (as the headers of their
# columns only name the countries, or the category and the year). `...` go to
# add_download_menu()
country_block <- function(controls, chart, ids, data, unit, var, ...) {
  chart_block(
    htmltools::tagList(country_values_element(data$values_id, data$values), controls),
    chart, ids, unit, choice_id = if (length(var) > 1) ids$var, download_axis_name = TRUE,
    ...
  )
}

# The filters and the bar chart of how much each country spent on a category of
# COFOG in a year, by Eurostat's accrual-based data (type "actual_cofog") in
# RawData, in the column `var`: a bar for each country (or group of countries: see
# group_names) with data, from the largest value on the left to the smallest on the
# right, with `axis_name` on the y axis and `unit` (unless it's "") after the values
# in the tooltip. The category is chosen with two dropdowns, as in category_chart():
# one of the main groups (at first Egészségügy) and one of its subgroups, after
# "Összesen" for the main group as a whole (at first), and the year with a slider,
# among the years with data for the category (at first the last one with data for
# the most countries, as the latest years may only have data for a few). If `var`
# has several columns, the chart plots the one chosen with radio buttons labelled
# `var_label`, which offer them by their names (the first one at first), and
# `axis_name` may give a different name for each. The columns relative to the main
# group (named ...Parent, like propParent) are only offered while a subgroup is
# chosen, not for "Összesen" (see country_filters()). With `unit = "%"`, the y axis
# ends at 100 at most, unless a value shown is above it (see percent_axis_max()).
# The bar of the country `highlight` (by its code; none if NULL) stands out in
# another colour, with its name in bold, and those of the groups of countries are
# grey. The chart has no title if `title` is NULL, else `title` followed by the
# names of the main group and of the subgroup (unless it's "Összesen"), as in
# category_chart(), and the year. As in category_chart(), each call has its own
# element ids (made from `id`), and the chart is only drawn once it comes near the
# viewport, then starting from the current choices of its controls. The helpers
# below and those of category_chart() make its parts
country_chart <- function(var, axis_name, unit, title = NULL, var_label = NULL,
                          highlight = "HU", id = paste0(var[[1]], "_country")) {
  ids <- country_ids(id)
  columns <- unname(var)
  axis_name <- rep_len(axis_name, length(var))
  data <- country_data(columns)
  # After the values, in the tooltip and in the description for screen readers,
  # with at most this many decimals, as in category_chart()
  unit_suffix <- unit_suffix_for(unit)
  fraction_digits <- 1L
  highlight <- if (!is.null(highlight)) country_names[[highlight]]
  
  # At first Egészségügy as a whole, in the first column, in the last year with
  # data for the most countries
  category <- data$categories[code == "GF07", label]
  by_year <- data$values[[category]][[columns[[1]]]]
  counts <- vapply(by_year, \(values) sum(!is.na(values)), 0L)
  years <- names(by_year)[counts > 0]
  year <- max(names(by_year)[counts == max(counts)])
  
  # Drawn as it is at first, though add_country_bars() draws it again at once,
  # in case the browser restored other choices of the controls
  chart <- data.table(country = data$countries, value = by_year[[year]])[
    !is.na(value)][order(-value)] |>
    e_chart(country, elementId = ids$chart) |>
    e_bar(value) |>
    # A single series, which the title names
    e_legend(show = FALSE) |>
    # The country and its value, as in the tooltip of the lines in
    # add_category_lines(): kept within the chart, with its lines wrapped. The value
    # of a bar is the second element of its data (the first is its country)
    e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE,
              extraCssText = "white-space: normal;",
              formatter = line_tooltip(fraction_digits, unit_suffix,
                                       value = "value.1")) |>
    e_y_axis(name = axis_name[[1]], max = percent_axis_max(unit),
             axisLabel = list(formatter = axis_number_label)) |>
    add_chart_frame(x_name = "Ország") |>
    # ECharts shrinks the plot so that the labels of the x axis (see below) end
    # within these bounds, which leave room for the credit below them
    e_grid(outerBounds = list(left = 0, right = 0, top = 0, bottom = 25))
  # Every country is labelled, vertically (unless the labels would overlap, as on
  # phones: see add_chart_frame()), the highlighted one in bold
  chart$x$opts$xAxis[[1]]$axisLabel <- modifyList(
    chart$x$opts$xAxis[[1]]$axisLabel,
    c(list(interval = 0, rotate = 90), highlight_axis_labels(highlight))
  )
  chart <- chart |>
    add_country_bars(ids, data, columns, axis_name, year, highlight) |>
    add_chart_description("Oszlopdiagram, az országok csökkenő sorrendben",
                          unit_suffix, fraction_digits) |>
    # The title names the chosen main group, subgroup and year. (CategoryPaths, which
    # the title is made from as in a category_chart(), has the same COFOG categories
    # under the same main groups)
    add_chart_layout(title, ids, year_label = "")
  
  country_block(
    country_filters(ids, data$categories, category, var, var_label, years, year),
    chart, ids, data, unit, var
  )
}

# The data of a country_chart(), a country_lines_chart() or a country_heatmap() (see
# there) of the columns `columns` of RawData: the `categories` of COFOG, each main
# group followed by its subgroups, with their codes (`code`), the codes of their main
# groups (`main`), their labels (`label`, their nameLabel, as the dropdowns show
# them) and their names (`name`); the Hungarian names of the `countries` with data,
# including the groups of countries (see country_names and group_names), whose names
# are also in `groups`; and their `values`, for each category (by label) and column,
# in each year with data for any country (by year), in the order of `countries`, NA
# where a country has none, which the page has in the element with the id
# `values_id` (see country_values_element()). The charts draw from these in the
# browser, as echarts4r would embed every row of the data in the page, with the
# names of the columns, which would make it several megabytes larger. For the same
# reason, the values are rounded to 4 significant digits, which is still more than
# the bars, the lines, the cells, the tooltips and the data files need, as there are
# no ratios of them to compute (see round_for_chart())
country_data <- function(columns) {
  rows <- RawData[type == "actual_cofog" & !is.na(cofog)]
  geo_names <- c(country_names, group_names)
  # A country without a name would be shown as "NA"
  absent <- setdiff(rows$geo, names(geo_names))
  if (length(absent) > 0) stop("No name for the countries ", toString(absent))
  geo_codes <- sort(unique(rows$geo))
  rows[, `:=`(country = match(geo, geo_codes), label = as.character(nameLabel))]
  categories <- unique(rows[, .(code = cofog, main = cofogParent, label, name)])
  # A main group's code comes right before those of its subgroups
  setorder(categories, code)
  values <- lapply(split(rows, by = "label"), \(category) {
    by_year <- split(category, by = "yearchr", sorted = TRUE)
    setNames(lapply(columns, \(column) lapply(by_year, \(year) {
      values <- rep(NA_real_, length(geo_codes))
      values[year$country] <- signif(year[[column]], 4)
      values
    })), columns)
  })
  list(categories = categories, countries = unname(geo_names[geo_codes]),
       groups = unname(group_names), values = values,
       # The same for the same columns, in whatever order
       values_id = paste0("country_values_", paste(sort(columns), collapse = "_")))
}

# The element of the `values` of country_data(), with the id `id` (see
# json_element()): as each copy would add about a megabyte to the page, it only has
# one, which the charts of the same columns share, wherever it is on the page, as
# the charts are only drawn once the page has been read (see defer_chart())
country_values_element <- function(id, values) {
  json_element(id, jsonlite::toJSON(values, na = "null", digits = 16, use_signif = TRUE))
}

# The controls of a country_chart() (see there), with the ids `ids` (see there),
# laid out as in category_filters(): the dropdown of the main groups of the
# `categories` (see country_data()), at first at the one labelled `category`, and
# that of the subgroups of the chosen one, after "Összesen" for the main group as a
# whole, which is chosen whenever the main group changes; the radio buttons of the
# columns `var`, if there are several, labelled `var_label`, which only offer the
# columns relative to the main group (named ...Parent, like propParent) while a
# subgroup is chosen (see below); and unless `years` is NULL (as in a
# country_lines_chart() or a country_heatmap()), the slider of the year, at first at
# `year` among `years`, which the chart takes over once it's drawn (see
# add_country_bars()). With `parts = TRUE`, as in a country_stack_chart(), there's
# no subgroup dropdown: the main group dropdown offers "Összesen" first, for the
# whole made up of the main groups, and the radio buttons only offer the columns
# relative to the main group while a main group is chosen
country_filters <- function(ids, categories, category, var, var_label, years = NULL,
                            year = NULL, parts = FALSE) {
  # The choices of the subgroup dropdown, for each main group (by label)
  groups <- split(categories, by = "main")
  subgroups <- setNames(
    lapply(groups, \(group) c("Összesen", group[code != main, label])),
    vapply(groups, \(group) group[code == main, label], "")
  )
  # The columns relative to the main group, which are 100% for every country for
  # the main group as a whole
  parent_var <- unname(var[endsWith(var, "Parent")])
  # In an element of its own, which the other charts of the countries share (see
  # shared_json())
  shared_subgroups <- shared_json(jsonlite::toJSON(subgroups))
  
  htmltools::div(
    class = "d-flex flex-wrap align-items-end row-gap-2 mb-3",
    # Not Bootstrap's column-gap-3, as in category_filters()
    style = "column-gap: 1rem",
    htmltools::div(
      style = "flex: 1 1 20rem",
      htmltools::tags$label(class = "form-label small mb-1", `for` = ids$main,
                            "Főcsoport:"),
      htmltools::tags$select(
        id = ids$main, class = "form-select form-select-sm",
        # With `parts`, only the main groups made up of subgroups
        lapply(if (parts) c("Összesen", names(subgroups)[lengths(subgroups) > 1]) else
          names(subgroups), \(label) {
            htmltools::tags$option(value = label, selected = if (label == category) NA,
                                   label)
          })
      )
    ),
    if (!parts) htmltools::div(
      style = "flex: 1 1 20rem",
      htmltools::tags$label(class = "form-label small mb-1", `for` = ids$sub,
                            "Csoport:"),
      htmltools::tags$select(id = ids$sub, class = "form-select form-select-sm")
    ),
    if (length(var) > 1) htmltools::div(
      # Full width on phones, where it's alone in its row
      class = "flex-grow-1 flex-md-grow-0",
      # While the buttons offer only one column, as they may at first, for
      # "Összesen" (see kiadas.countryFilters() in js/filters.js)
      hidden = if (length(setdiff(var, parent_var)) < 2) NA,
      radio_buttons(id = ids$var, label = var_label, choices = var, compact = TRUE)
    ),
    if (!is.null(years)) htmltools::div(
      style = "flex: 1 1 20rem",
      htmltools::tags$label(
        class = "form-label small mb-1", `for` = ids$base, "Év: ",
        # The chosen year, which the chart updates, and also gives to screen readers
        # as the value of the slider
        htmltools::tags$span(id = paste0(ids$base, "_value"), `aria-hidden` = "true",
                             year)
      ),
      # Its value is the position of the year among those offered
      htmltools::tags$input(type = "range", class = "form-range", id = ids$base,
                            min = 0, max = length(years) - 1, step = 1,
                            value = match(year, years) - 1, `aria-valuetext` = year)
    ),
    # Fills in the subgroup dropdown, whenever the main group changes, and at once,
    # as the page is read (as in filter_select_dependent()), for the main group
    # chosen then, which the browser may have restored when reloading the page. The
    # change of the subgroup is what the chart follows (see add_country_bars()).
    # Whenever the subgroup changes, the radio buttons (if any) are updated first, as
    # the chart only starts listening once it's drawn: as in add_column_choice(),
    # while the subgroup dropdown is at "Összesen", they don't offer the columns
    # relative to the main group, and if one of those is chosen, the first column
    # offered is checked (and plotted) instead, until a subgroup is chosen again.
    # The buttons are hidden while they offer only one column. With `parts`, the
    # buttons follow the main group dropdown in the same way, at once, as the page
    # is read. After the controls, which it uses
    shared_subgroups$element,
    htmltools::tags$script(htmltools::HTML(sprintf(
      "kiadas.countryFilters({
        subgroups: %s, parentColumns: %s, mainId: %s, hasSub: %s, subId: %s,
        choiceId: %s
      });",
      jsonlite::toJSON(shared_subgroups$id, auto_unbox = TRUE),
      jsonlite::toJSON(parent_var),
      jsonlite::toJSON(ids$main, auto_unbox = TRUE),
      # No subgroup dropdown with `parts`
      jsonlite::toJSON(!parts, auto_unbox = TRUE),
      jsonlite::toJSON(ids$sub, auto_unbox = TRUE),
      # No element has this id if there's only one column
      jsonlite::toJSON(ids$var, auto_unbox = TRUE)
    )))
  )
}

# The bars of a country_chart() (see there), with the ids `ids` (see there), from
# `data` (see country_data()): a bar for each country with a value for the category
# chosen with the dropdowns (the main group itself if the subgroup dropdown is at
# "Összesen"), in the column chosen with the radio buttons (or in the only one of
# `columns`), in the year chosen with the slider, sorted from the largest value to
# the smallest (the ties by name). The y axis is named after the column, in
# `axis_name`, and the series after the category and the year, which thus name the
# values in the tooltip, in the description for screen readers and in the data
# files. The slider offers the years with a value for the category in the column,
# and when the choices change, it keeps its year if that's still offered, else it
# moves to the nearest one that is. It starts from `year` (not from where the
# browser may have restored it when reloading the page, as the years offered may
# differ). The chosen year is also shown beside the slider, and kept in its
# data-year attribute for the title, which is set after this so that it reads the
# year already updated (see add_chart_layout()). The bar of the country named
# `highlight` (if any) is orange, the fourth colour of ECharts' palette, whose first
# colours the bars of the other countries, and those of the groups of countries
# (`groups` in `data`) are grey
add_country_bars <- function(chart, ids, data, columns, axis_name, year, highlight) {
  htmlwidgets::onRender(chart, "kiadas.countryBars",
    data = list(
      mainId = ids$main, subId = ids$sub, columnId = if (length(columns) > 1) ids$var,
      yearId = ids$base, year = year, columns = as.list(columns),
      axisNames = as.list(setNames(axis_name, columns)),
      names = as.list(setNames(data$categories$name, data$categories$label)),
      countries = as.list(data$countries), groups = as.list(data$groups),
      valuesId = data$values_id, highlight = highlight
    ))
}

# The end of the y axis of a country_chart() or a country_lines_chart() of values in
# `unit`: unless it's "%", NULL, for ECharts' own end. Their percentages are shares
# of a whole (see add_country_heatmap()), so with "%", the axis ends at 100 at most
# while no value shown (no bar, or no value of the lines switched on in the legend)
# is above it. ECharts rounds the end of the axis up to a multiple of its step,
# which takes it beyond 100 when large negative values widen its range (as to 120,
# with a step of 30), and then it ends at 100 instead. A value above 100 (a part
# larger than its whole, as when the other parts are negative) is left to ECharts,
# so that it's shown in full. With `stacked = TRUE`, as in a country_stack_chart(),
# the values are the tops of stacked parts, which add up to a whole of 100% a hair
# above 100 from rounding errors, or as much above it as negative parts too small to
# see go below 0, so an overshoot of less than 0.5% of the axis is cut off, as in
# add_part_lines() (see kiadas.percentAxisMax() in js/kiadas.js)
percent_axis_max <- function(unit, stacked = FALSE) {
  if (unit != "%") return(NULL)
  htmlwidgets::JS(sprintf("kiadas.percentAxisMax(%s)",
                          jsonlite::toJSON(stacked, auto_unbox = TRUE)))
}

# The filters and the line chart of how much each country spent on a category of
# COFOG over the years, by Eurostat's accrual-based data (type "actual_cofog") in
# RawData, in the column `var`: a line for each country (or group of countries: see
# group_names), over the years with data for the category, with `axis_name` on the y
# axis and `unit` (unless it's "") after the values in the tooltip. The category is
# chosen with two dropdowns, as in country_chart() (at first Egészségügy as a
# whole). If `var` has several columns, the chart plots the one chosen with radio
# buttons labelled `var_label`, which offer them by their names (the first one at
# first), and `axis_name` may give a different name for each; the columns relative
# to the main group only while a subgroup is chosen, as in country_chart(). The
# legend lists all the countries, but at first only the lines of `shown` (by their
# codes) are on, and the others can be switched on there: `shown` come first in the
# legend, in their order, and the others after them, in alphabetical order. The line
# of the country `highlight` (by its code; none if NULL) stands out in orange, as its
# bar in a country_chart(), and is thicker than the others. With `unit = "%"`, the
# y axis ends at 100 at most, unless a line switched on in the legend has a value
# above it, as in country_chart(). The chart has no title if `title` is NULL, else
# `title` followed by the names of the main group and of the subgroup (unless it's
# "Összesen"), as in category_chart(). As in category_chart(), each call has its own
# element ids (made from `id`), and the chart is only drawn once it comes near the
# viewport, then starting from the current choices of its controls. The helpers of
# country_chart() and of category_chart() make its parts
country_lines_chart <- function(var, axis_name, unit, title = NULL, var_label = NULL,
                                highlight = "HU",
                                shown = c(highlight, "EU27", "EU15", "EU11", "V4"),
                                id = paste0(var[[1]], "_country_lines")) {
  # Without the slider of country_chart()
  ids <- country_ids(id, year = FALSE)
  columns <- unname(var)
  axis_name <- rep_len(axis_name, length(var))
  data <- country_data(columns)
  # As in country_chart()
  unit_suffix <- unit_suffix_for(unit)
  fraction_digits <- 1L
  # The names of the lines, in the order of the legend (and of the tooltip and the
  # data files). Before `highlight` is changed, as the default of `shown` reads it
  ordered <- country_order(shown, data$countries)
  lines <- ordered$all
  others <- setdiff(data$countries, ordered$first)
  highlight <- if (!is.null(highlight)) country_names[[highlight]]
  # At first Egészségügy as a whole, as in country_chart()
  category <- data$categories[code == "GF07", label]
  
  # The lines, without their points, which add_country_lines() adds at once, as it
  # does the years of the x axis (an empty list of them here would make echarts4r's
  # e_x_axis() warn): that of `highlight` thicker than the others, and drawn over them
  # (whose z is ECharts' default for lines, 3). Hovering a line, or its name in the
  # legend, highlights it, as in category_chart()
  chart <- e_chart(elementId = ids$chart)
  chart$x$opts$xAxis <- list(list(type = "category", boundaryGap = TRUE))
  chart$x$opts$series <- lapply(lines, \(name) c(
    list(name = name, type = "line", data = list(), emphasis = list(focus = "series")),
    if (identical(name, highlight)) list(lineStyle = list(width = 3), z = 4)
  ))
  chart$x$opts$legend <- list(
    data = as.list(lines),
    selected = as.list(setNames(rep(FALSE, length(others)), others))
  )
  chart <- chart |>
    # As the tooltip of the lines in add_category_lines(), but the value of a point
    # is the second element of its data (the first is its year), as the data aren't
    # rows of a dataset here, and the title is the year on the axis
    e_tooltip(trigger = "axis", confine = TRUE, extraCssText = "white-space: normal;",
              formatter = line_tooltip(fraction_digits, unit_suffix, value = "value.1",
                                       title = "axisValue")) |>
    e_y_axis(name = axis_name[[1]], max = percent_axis_max(unit),
             axisLabel = list(formatter = axis_number_label)) |>
    add_chart_frame() |>
    add_country_lines(ids, data, lines, columns, axis_name, highlight) |>
    add_chart_description("Vonaldiagram", unit_suffix, fraction_digits) |>
    # The title names the chosen main group and subgroup, as in a country_chart()
    add_chart_layout(title, ids)
  
  country_block(country_filters(ids, data$categories, category, var, var_label),
                chart, ids, data, unit, var)
}

# The lines of a country_lines_chart() (see there), with the ids `ids` (see there),
# from `data` (see country_data()): those of the countries named `lines` (in the
# order of the series), each of the country's values for the category chosen with
# the dropdowns (the main group itself if the subgroup dropdown is at "Összesen"), in
# the column chosen with the radio buttons (or in the only one of `columns`), over
# the years with a value for the category in the column for any country, with a gap
# where the country has none. The y axis is named after the column, in `axis_name`.
# Each line switched on in the legend has a colour of its own: that of the country
# named `highlight` (if any) is orange, as its bar in add_country_bars(), and each of
# the others takes the first colour of chart_palette (without the orange) that no
# other line shown has, and keeps it while it's shown, so that the lines shown differ
# in colour however they're switched on and off (as long as there are colours
# enough: beyond that, the colours are taken again, in turn)
add_country_lines <- function(chart, ids, data, lines, columns, axis_name, highlight) {
  highlight_color <- "#ff994d"
  htmlwidgets::onRender(chart, "kiadas.countryLines",
    data = list(
      mainId = ids$main, subId = ids$sub, columnId = if (length(columns) > 1) ids$var,
      columns = as.list(columns), axisNames = as.list(setNames(axis_name, columns)),
      valuesId = data$values_id, lines = as.list(lines),
      # The position of each line's country among the countries of `data`
      positions = as.list(match(lines, data$countries) - 1L),
      highlight = highlight, highlightColor = highlight_color,
      palette = as.list(setdiff(chart_palette, highlight_color))
    ))
}

# The filters and the heatmap of how much each country spent on a category of COFOG
# over the years, by Eurostat's accrual-based data (type "actual_cofog") in RawData,
# in the column `var`: a row for each country (or group of countries: see
# group_names), labelled by its name, and a column for each year with data for the
# category, with each cell coloured by the country's value in the year (see
# add_country_heatmap()), `axis_name` on the y axis, as it tells what the colours
# show, and `unit` (unless it's "") after the values in the tooltip (with "%", the
# colour scale ends at 100 at most: see add_country_heatmap()). Where a country
# has no value, its cell is left empty (its whole row, if it has none for the
# category). The category is chosen with two dropdowns, as in country_chart() (at
# first Egészségügy as a whole). If `var` has several columns, the chart shows the
# one chosen with radio buttons labelled `var_label`, which offer them by their
# names (the first one at first), and `axis_name` may give a different name for
# each; the columns relative to the main group only while a subgroup is chosen, as
# in country_chart(). The rows of `top` (by their codes) come first, from the top,
# in their order, and the others after them, in alphabetical order, as in the legend
# of a country_lines_chart(), and the name of the country `highlight` (by its code;
# none if NULL) is in bold, as in a country_chart(). The rows are as tall as those
# of a category_chart() on average. The chart has no title if `title` is NULL, else
# `title` followed by the names of the main group and of the subgroup (unless it's
# "Összesen"), as in category_chart(). As in category_chart(), each call has its own
# element ids (made from `id`), and the chart is only drawn once it comes near the
# viewport, then starting from the current choices of its controls. The helpers of
# country_chart() and of category_chart() make its parts
country_heatmap <- function(var, axis_name, unit, title = NULL, var_label = NULL,
                            highlight = "HU",
                            top = c(highlight, "EU27", "EU15", "EU11", "V4"),
                            id = paste0(var[[1]], "_country_heatmap")) {
  # Without the slider of country_chart()
  ids <- country_ids(id, year = FALSE)
  columns <- unname(var)
  axis_name <- rep_len(axis_name, length(var))
  data <- country_data(columns)
  # As in country_chart()
  unit_suffix <- unit_suffix_for(unit)
  fraction_digits <- 1L
  # The names of the rows, from the top. They're also the rows' values on the y axis,
  # by which the data files and the description for screen readers name the rows.
  # Before `highlight` is changed, as the default of `top` reads it
  rows <- country_order(top, data$countries)$all
  highlight <- if (!is.null(highlight)) country_names[[highlight]]
  row_names <- as.list(setNames(rows, rows))
  # At first Egészségügy as a whole, as in country_chart()
  category <- data$categories[code == "GF07", label]
  
  chart <- e_chart(elementId = ids$chart)
  # A row for each cell, which add_country_heatmap() fills in (and the years of the x
  # axis), as add_download_menu() and add_chart_description() read the row and the
  # year of a cell of a heatmap from its row of data, as an object
  chart$x$opts$dataset <- list(list(
    dimensions = list(list(name = "yearchr", type = "ordinal"),
                      list(name = "country", type = "ordinal"),
                      list(name = "value", type = "number")),
    source = list()
  ))
  # The cells line the axes, which would only show beside the empty ones, as in
  # add_category_heatmap()
  chart$x$opts$xAxis <- list(list(type = "category", boundaryGap = TRUE,
                                  axisLine = list(show = FALSE)))
  # Every row is labelled, the highlighted one in bold, as in a country_chart(). The
  # name of the axis starts above the rows, as in add_category_heatmap()
  chart$x$opts$yAxis <- list(list(
    type = "category", data = as.list(rows), inverse = TRUE, name = axis_name[[1]],
    nameLocation = "start", nameTextStyle = list(align = "left"),
    axisLine = list(show = FALSE), axisTick = list(show = FALSE),
    axisLabel = c(list(interval = 0), highlight_axis_labels(highlight))
  ))
  # White lines between the cells, and dark ones around the highlighted cell, as in
  # add_category_heatmap()
  chart$x$opts$series <- list(list(
    id = "heatmap", type = "heatmap",
    encode = list(x = "yearchr", y = "country", value = "value"),
    itemStyle = list(borderColor = "#fff", borderWidth = 1),
    emphasis = list(itemStyle = list(borderColor = "#333", borderWidth = 1.5))
  ))
  chart <- chart |>
    # ECharts shrinks the plot so that the names of the countries end within these
    # bounds, 10px from the left edge, as the title
    e_grid(left = 10, outerBounds = list(left = 10, right = 0, top = 0, bottom = 0)) |>
    # The year, then the country and its value, as in the tooltip of
    # add_category_heatmap(): kept within the chart, with its lines wrapped, all from
    # the data of the cell, which with a dataset is the whole data row as an object
    e_tooltip(trigger = "item", confine = TRUE, extraCssText = "white-space: normal;",
              formatter = line_tooltip(fraction_digits, unit_suffix, value = "value.value",
                                       title = "value.yearchr", label = "value.country")) |>
    add_chart_frame() |>
    add_country_heatmap(ids, data, rows, columns, axis_name, heatmap_sizes, unit) |>
    add_chart_description("Hőtérkép", unit_suffix, fraction_digits, row_names,
                          rows_name = "országok és országcsoportok") |>
    # The title names the chosen main group and subgroup, as in a country_chart()
    add_chart_layout(title, ids, row_height = heatmap_sizes$row,
                     min_plot_height = heatmap_sizes$plot)
  
  country_block(country_filters(ids, data$categories, category, var, var_label),
                chart, ids, data, unit, var, row_names = row_names)
}

# The cells of a country_heatmap() (see there), with the ids `ids` (see there), from
# `data` (see country_data()): in the rows of the countries named `rows` (the
# categories of the y axis), a cell for each year in which the country has a value
# for the category chosen with the dropdowns (the main group itself if the subgroup
# dropdown is at "Összesen"), in the column chosen with the radio buttons (or in the
# only one of `columns`), over the years with a value for the category in the column
# for any country, as in add_country_lines(). The y axis is named after the column,
# in `axis_name`. The cells are coloured by their values, along viridis from light
# (the smallest) to dark (the largest), on a linear scale from the 5th to the 95th
# percentile of the values shown, with the values beyond in the colours at the ends:
# over the whole range, a few extreme values (as Ireland's economic affairs in 2010,
# when it rescued its banks) would leave the others barely told apart. The range is
# widened to the 5 ticks of the colour scale beside the plot (see
# kiadas.colorScale()), spaced evenly by the least of 1, 1.5, 2, 2.5, 3, 4, 5, 6 and
# 8 times a power of 10 that lets them cover it from a multiple of the step. With
# `unit = "%"`, the values are percentages of a whole (of the GDP, of the total
# spending or of the main group's spending), which a category's spending doesn't
# exceed, so the ticks don't go beyond 100: the range is cut off there, and the
# ticks start from a multiple of the step low enough for them to end at 100 at
# most. (A part larger than its whole, as when the other parts are negative, then
# takes the colour at the top end, as the values beyond the range do.) As in
# add_category_heatmap(), the colours run in pieces 1/64 of the scale wide, each in
# the colour of its middle, and the scale is `sizes$scale` px tall
add_country_heatmap <- function(chart, ids, data, rows, columns, axis_name, sizes,
                                unit = "") {
  # The colours of the middles of the pieces, from the bottom of the scale to its top
  colors <- grDevices::hcl.colors(65, "viridis", rev = TRUE)
  # Its pieces are set in js/country.js, before any cell is drawn. Its own legend
  # can't label a gradient between its ends, so it's hidden, as in
  # add_category_heatmap()
  chart$x$opts$visualMap <- list(list(
    type = "piecewise", show = FALSE, seriesIndex = 0, dimension = "value"
  ))
  htmlwidgets::onRender(chart, "kiadas.countryHeatmap",
    data = list(
      mainId = ids$main, subId = ids$sub, columnId = if (length(columns) > 1) ids$var,
      columns = as.list(columns), axisNames = as.list(setNames(axis_name, columns)),
      valuesId = data$values_id, rows = as.list(rows),
      # The position of each row's country among the countries of `data`
      positions = as.list(match(rows, data$countries) - 1L),
      colors = as.list(colors),
      # The largest tick there may be (see above)
      scaleMax = if (unit == "%") 100,
      # The colours of the bar of the scale, from the bottom up
      scale = list(height = sizes$scale,
                   stops = Map(\(offset, color) list(offset = offset, color = color),
                               seq(0, 1, length.out = length(colors)), colors))
    ))
}

# The filters and the stacked column chart of what each country's spending was made
# up of in a year, by Eurostat's accrual-based data (type "actual_cofog") in RawData,
# in the column `var`: a column for each country (or group of countries: see
# group_names) switched on in the legend of the countries, with the parts of the
# whole chosen with a dropdown stacked in it (see add_country_stacks()): the main
# groups of COFOG for "Összesen" (at first), else the subgroups of the chosen main
# group. The year is chosen with a slider, as in country_chart() (at first the last
# year with data for the most of the countries shown at first). If `var` has several
# columns, the chart plots the one chosen with radio buttons labelled `var_label`,
# which offer them by their names (the first one at first), and `axis_name` may give
# a different name for each (on the y axis); the columns relative to the main group
# (named ...Parent, like propParent) only while a main group is chosen. So a column
# adds up to 100% in the column relative to the whole whose parts it shows (prop for
# "Összesen", propParent for a main group), but only to the whole's share in the
# others (as of the GDP). `unit` (unless it's "") follows the values in the tooltip,
# and with `unit = "%"`, the y axis ends at 100 at most, unless the columns reach
# above it (see percent_axis_max()). Below the chart are two legends: that of the
# parts, and that of all the countries, which switches them on and off, at first
# only `shown` (by their codes), as in a country_lines_chart(): `shown` come first,
# in their order, and the others after them, in alphabetical order, and the columns
# follow the same order. The name of the country `highlight` (by its code; none if
# NULL) is in bold, in the legend and below its column, as in a country_chart(). The
# chart has no title if `title` is NULL, else `title` followed by the name of the
# main group (unless it's "Összesen") and the year. As in category_chart(), each call
# has its own element ids (made from `id`), and the chart is only drawn once it comes
# near the viewport, then starting from the current choices of its controls. The
# helpers of country_chart() and of category_chart() make its parts
country_stack_chart <- function(var, axis_name, unit, title = NULL, var_label = NULL,
                                highlight = "HU",
                                shown = c(highlight, "EU27", "EU15", "EU11", "V4"),
                                id = paste0(var[[1]], "_country_stack")) {
  # Without the subgroup dropdown of country_chart()
  ids <- country_ids(id, sub = FALSE)
  columns <- unname(var)
  axis_name <- rep_len(axis_name, length(var))
  data <- country_data(columns)
  # As in country_chart()
  unit_suffix <- unit_suffix_for(unit)
  fraction_digits <- 1L
  # The names of the countries in the order of the legend and of the columns. Before
  # `highlight` is changed, as the default of `shown` reads it
  ordered <- country_order(shown, data$countries)
  shown <- ordered$first
  countries <- ordered$all
  others <- setdiff(data$countries, shown)
  highlight <- if (!is.null(highlight)) country_names[[highlight]]
  # The parts of each whole that the dropdown offers (by their labels), in the order
  # of their codes: "Összesen" first, made up of the main groups, then each main
  # group with subgroups, made up of them
  categories <- data$categories
  mains <- categories[code == main]
  parts <- c(
    list("Összesen" = mains$label),
    setNames(lapply(mains$code, \(m) categories[main == m & code != m, label]),
             mains$label)
  )
  parts <- parts[lengths(parts) > 0]
  
  # At first the last year with data for the most of the countries shown at first,
  # for the main groups, in the first column, as the latest years may only have
  # data for a few (as in country_chart()). The slider offers the years with data
  # for any of them
  positions <- match(shown, data$countries)
  by_part <- lapply(data$values[parts[["Összesen"]]], `[[`, columns[[1]])
  years <- sort(unique(unlist(lapply(by_part, names))))
  counts <- vapply(years, \(year) {
    has_value <- lapply(by_part, \(by_year) {
      values <- by_year[[year]]
      if (is.null(values)) rep(FALSE, length(positions)) else !is.na(values[positions])
    })
    sum(Reduce(`|`, has_value))
  }, 0L)
  year <- max(years[counts == max(counts)])
  years <- years[counts > 0]
  
  # A stacked series for each category (in the order of the codes, so the parts
  # are stacked in that order, from the bottom up), which add_country_stacks() fills
  # in for the parts shown, and one without data for each country, which give the
  # legend of the countries its names (as in add_plan_actual_colors()), in a grey
  # that tells only whether they're switched on, as the columns aren't coloured by
  # country. (Without the categories of the x axis, which add_country_stacks() adds
  # at once, as an empty list of them here would make echarts4r's e_x_axis() warn.)
  # Hovering a part, or its name in the legend, highlights it in every column
  chart <- e_chart(elementId = ids$chart)
  chart$x$opts$xAxis <- list(list(type = "category"))
  chart$x$opts$series <- c(
    lapply(categories$label, \(label) list(
      name = label, type = "bar", stack = "total", data = list(),
      emphasis = list(focus = "series")
    )),
    lapply(countries, \(name) list(name = name, type = "line", data = list(),
                                   color = "#6d6e73"))
  )
  # The legend of the parts, which add_country_stacks() lists, and that of the
  # countries, with a small circle for each, and the name of `highlight` in bold
  chart$x$opts$legend <- list(
    list(data = list()),
    list(
      data = lapply(countries, \(name) c(
        list(name = name),
        if (identical(name, highlight)) list(textStyle = list(fontWeight = "bold"))
      )),
      selected = as.list(setNames(rep(FALSE, length(others)), others)),
      icon = "circle", itemWidth = 10, itemHeight = 10
    )
  )
  chart <- chart |>
    # Formatted by add_country_stacks()
    e_tooltip(trigger = "item", confine = TRUE, extraCssText = "white-space: normal;") |>
    e_y_axis(name = axis_name[[1]], max = percent_axis_max(unit, stacked = TRUE),
             # As in add_part_lines(), negative parts too small to see don't run the
             # axis down to -20
             min = if (unit == "%") htmlwidgets::JS("kiadas.stackMin"),
             axisLabel = list(formatter = axis_number_label)) |>
    add_chart_frame(x_name = "Ország") |>
    # ECharts shrinks the plot so that the labels of the x axis (see below) end
    # within these bounds, which leave room for a legend of one line and the credit
    # below them (add_chart_layout() moves them up by the room for the legends)
    e_grid(outerBounds = list(left = 0, right = 0, top = 0, bottom = 45))
  # Every country is labelled, the highlighted one in bold, as in a country_chart()
  # (vertically if the names don't fit: see add_country_stacks())
  chart$x$opts$xAxis[[1]]$axisLabel <- modifyList(
    chart$x$opts$xAxis[[1]]$axisLabel,
    c(list(interval = 0), highlight_axis_labels(highlight))
  )
  chart <- chart |>
    add_country_stacks(ids, data, parts, columns, axis_name, countries, year,
                       unit_suffix, fraction_digits) |>
    add_chart_description("Halmozott oszlopdiagram", unit_suffix, fraction_digits) |>
    # The title names the chosen main group, as that of a plan_actual_chart(), and
    # the year. (CategoryPaths, which the names are taken from, has the same COFOG
    # categories, as in a country_chart())
    add_chart_layout(title, ids, data.table(main = names(parts)), second = FALSE,
                     year_label = "")
  
  country_block(
    country_filters(ids, categories, "Összesen", var, var_label, years, year, parts = TRUE),
    chart, ids, data, unit, var
  )
}

# The columns of a country_stack_chart() (see there), with the ids `ids` (see there),
# from `data` (see country_data()): a column for each of the countries named
# `countries` (in their order) that is switched on in the legend of the countries
# and has a value for a part in the year chosen with the slider, made up of the
# parts of the whole chosen with the dropdown (by its choices, their labels in
# `parts`), stacked from the bottom up in their order, in the column chosen with the
# radio buttons (or in the only one of `columns`). The y axis is named after the
# column, in `axis_name`. The parts are coloured in the order of the stack, as in
# add_part_legend(), which also lists them in the legend so, from the top of the
# stack down. The slider offers the years with a value for a part in the column for
# a country switched on (or, if none is, for any country), and when the choices
# change, it keeps its year, or moves to the nearest one offered, as in
# add_country_bars() (which also tells how the year is shown, and kept for the
# title), starting from `year`. The names of the countries are written below their
# columns horizontally if the widest fits in a column, else vertically. The tooltip
# shows the hovered part of a column, as that of the stacked areas of a
# category_chart(): the country and the year, then the part, and its value followed
# by `unit_suffix`, with at most `fraction_digits` decimals (or two significant
# digits if that's more precise)
add_country_stacks <- function(chart, ids, data, parts, columns, axis_name, countries,
                               year, unit_suffix, fraction_digits) {
  htmlwidgets::onRender(chart, "kiadas.countryStacks",
    data = list(
      mainId = ids$main, columnId = if (length(columns) > 1) ids$var, yearId = ids$base,
      year = year, columns = as.list(columns),
      axisNames = as.list(setNames(axis_name, columns)),
      parts = lapply(parts, as.list), labels = as.list(data$categories$label),
      countries = as.list(countries),
      # The position of each country among the countries of `data`
      positions = as.list(match(countries, data$countries) - 1L),
      valuesId = data$values_id, palette = as.list(chart_palette),
      unit = unit_suffix, digits = fraction_digits
    ))
}
