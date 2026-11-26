# The functions that draw the charts of kiadas.qmd (category_chart(),
# plan_actual_chart(), plan_actual_heatmap(), country_lines_chart(),
# country_heatmap(), country_chart() and country_stack_chart()), and their
# helpers. They use data.table, echarts4r, RawData, HURawData and CategoryPaths, so
# kiadas.qmd sources this after attaching the packages and defining the data
# (plan_actual_labels below reads HURawData as soon as this is sourced)

# The page's own JavaScript, the `kiadas` object that the charts and their filters
# use (see its parts where they're used), inlined into the head of the page once,
# however many charts attach it. (htmltools wants a directory for it, but it has no
# files)
kiadas_dependency <- htmltools::htmlDependency(
  "kiadas", "1.0", src = c(file = tempdir()), all_files = FALSE,
  head = paste0("<script>", r"-(
                window.kiadas = (function() {
                // Renders the charts `els` that defer() held back: htmlwidgets renders each widget
                // of echarts4r's class that it hasn't yet. While the page is still being read,
                // it does so once it's read (on DOMContentLoaded), as it would otherwise also
                // render the widgets read since that defer() hasn't held back yet, and run the
                // code of the charts before the elements it uses are read
                function render(els) {
                if (!els.length) return;
                els.forEach(function(el) {
                observer.unobserve(el);
                el.classList.replace('kiadas-deferred', 'echarts4r');
                });
                if (document.readyState !== 'loading') HTMLWidgets.staticRender();
                }
                // Each when it comes within a screen's height of the viewport
                var observer = window.IntersectionObserver && new IntersectionObserver(function(entries) {
                render(entries.filter(function(e) { return e.isIntersecting; })
                .map(function(e) { return e.target; }));
                }, { rootMargin: '100% 0px' });
                // And all that are left before printing, as they'd be missing from the printout
                window.addEventListener('beforeprint', function() {
                render(Array.from(document.querySelectorAll('.kiadas-deferred')));
                });
                // The values of the countries read so far, by the id of their element (see
                // countryValues() below)
                var countryData = {};
                
                return {
                // Renders the chart with the id `id` only once it comes within a screen's height
                // of the viewport (or before printing). Until then htmlwidgets skips it, as this
                // takes the class of echarts4r's widgets from it, before htmlwidgets renders the
                // widgets of the page
                defer: function(id) {
                if (!observer) return;
                var el = document.getElementById(id);
                el.classList.replace('echarts4r', 'kiadas-deferred');
                observer.observe(el);
                },
                // The regular expression that echarts4r's crosstalk filter matches the keys of
                // the rows with (see its binding), for the keys that the filters of the
                // crosstalk group `group` leave, or for every key if there's no filter
                keyRegex: function(group) {
                var keys = crosstalk.group(group).var('filter').get();
                return keys ? '^(' + keys.join('|') + ')$' : '^';
                },
                // Calls fn with the keys that the filters of the crosstalk group `group` leave
                // (null if there's no filter): at once, and whenever they change
                onFilter: function(group, fn) {
                new crosstalk.FilterHandle(group).on('change', function(e) { fn(e.value); });
                fn(crosstalk.group(group).var('filter').get());
                },
                // The values of the countries that the charts of the same columns share, from
                // the JSON in the element with the id `id` (see country_values_element()),
                // parsed when a chart first needs them, and then kept for the others
                countryValues: function(id) {
                countryData[id] = countryData[id] || JSON.parse(document.getElementById(id).textContent);
                return countryData[id];
                },
                // The lines of the tooltip of a category's planned and actual spending in a
                // year, in a plan_actual_chart() or a plan_actual_heatmap(): the `marker` of its
                // point or cell, the `name` of the category and the `year`, then the `plan` and
                // the `actual` value, named by `labels`, followed by `unit`, with at most
                // `digits` decimals (or two significant digits if that's more precise, as in
                // the other tooltips), and the deviation from the plan, in the same unit (in
                // percentage points between percentages) and, unless there's no plan to compare
                // with (a plan of 0), relative to the plan
                planActualTooltip: function(marker, name, year, plan, actual, labels, unit, digits) {
                function number(value, signed) {
                return Number(value).toLocaleString('hu-HU', {
                maximumFractionDigits: digits, maximumSignificantDigits: 2,
                roundingPriority: 'morePrecision', signDisplay: signed ? 'exceptZero' : 'auto'
                });
                }
                function bold(text) { return '<b style="white-space: nowrap">' + text + '</b>'; }
                var deviation = number(actual - plan, true) + (unit === '%' ? ' százalékpont' : unit);
                if (plan) {
                deviation += ' (' + (100 * (actual / plan - 1)).toLocaleString('hu-HU', {
                maximumFractionDigits: 1, signDisplay: 'exceptZero'
                }) + '%)';
                }
                return [
                marker + ' ' + name + ', ' + year,
                labels[0] + ': ' + bold(number(plan) + unit),
                labels[1] + ': ' + bold(number(actual) + unit),
                'Eltérés a tervtől: ' + bold(deviation)
                ];
                },
                // The colour scale of the heatmap `chart` (of add_category_heatmap() or
                // add_country_heatmap()): a function that draws it with the ticks it's given
                // (from the bottom up, at their positions as fractions of the height of the
                // bar), in the font and the colour of the labels of the axes, as a bar of the
                // colours scale.stops (from the bottom up), scale.height px tall, with the ticks
                // and their labels on its right, the labels at the ends within its height, so
                // that it lines up with the plot. It's drawn at first beside the plot, 12px from
                // it, from its top (where add_chart_layout() then puts it), and then over
                // itself: as there are always 5 ticks, ECharts updates its elements one by one
                // (it would mix up the old and the new ones, if the group were replaced)
                colorScale: function(chart, scale) {
                var option = chart.getOption(), label = option.yAxis[0].axisLabel;
                var font = { fill: label.color, fontSize: label.fontSize,
                fontFamily: option.textStyle.fontFamily };
                var context = document.createElement('canvas').getContext('2d');
                context.font = font.fontSize + 'px ' + font.fontFamily;
                var drawn = false;
                return function(ticks) {
                var labelWidth = Math.max.apply(null, ticks.map(function(tick) {
                return context.measureText(tick.label).width;
                }));
                var children = [{
                type: 'rect', shape: { width: 12, height: scale.height }, silent: true,
                style: {
                fill: { type: 'linear', x: 0, y: 1, x2: 0, y2: 0, colorStops: scale.stops }
                }
                }];
                ticks.forEach(function(tick, i) {
                var y = Math.round(scale.height * (1 - tick.position));
                children.push({
                // A pixel high, within the height of the bar. (Not a line, whose bounds
                // ECharts would widen, moving the bar below the top of the plot)
                type: 'rect', silent: true, style: { fill: label.color },
                shape: { x: 12, y: Math.min(y, scale.height - 1), width: 4, height: 1 }
                }, {
                type: 'text', x: 19, y: y, silent: true,
                style: Object.assign({
                text: tick.label,
                verticalAlign: i === 0 ? 'bottom' : i === ticks.length - 1 ? 'top' : 'middle'
                }, font)
                });
                });
                var group = { id: 'colorScale', children: children };
                if (!drawn) {
                Object.assign(group, { type: 'group', right: 10, top: option.grid[0].top });
                }
                drawn = true;
                chart.setOption({
                graphic: { elements: [group] },
                grid: { right: 10 + 19 + Math.ceil(labelWidth) + 12 }
                });
                };
                }
                };
                })();
  )-", "</script>")
)

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

# crosstalk has no radio-button filter, so this builds one on crosstalk's JS API,
# with the buttons of radio_buttons() (which describes `inline` and `compact`)
filter_radio <- function(id, label, sharedData, group, selected = NULL, inline = FALSE,
                         compact = FALSE) {
  df <- sharedData$origData()
  values <- if (inherits(group, "formula")) {
    eval(group[[2]], df, environment(group))
  } else group
  lvls <- if (is.factor(values)) levels(droplevels(values)) else sort(unique(values))
  lvls <- as.character(lvls)
  if (is.null(selected)) selected <- lvls[1]
  keys <- as.character(sharedData$key())
  map <- setNames(lapply(lvls, \(l) keys[which(values == l)]), lvls)
  
  # Set up at once, as the page is read, so that the filter is set before the
  # charts are rendered (see defer_chart()), which start from it (see
  # start_filtered()): setting it then redraws nothing
  js <- sprintf(
    "(function() {
      var map = %s;
      var handle = new crosstalk.FilterHandle(%s);
      var inputs = document.getElementById(%s).querySelectorAll('input');
      function update() {
        inputs.forEach(function(el) { if (el.checked) handle.set(map[el.value]); });
      }
      inputs.forEach(function(el) { el.addEventListener('change', update); });
      update();
    })();",
    jsonlite::toJSON(map),
    jsonlite::toJSON(sharedData$groupName(), auto_unbox = TRUE),
    jsonlite::toJSON(id, auto_unbox = TRUE)
  )
  
  htmltools::attachDependencies(
    htmltools::tagList(
      radio_buttons(id, label, lvls, selected, inline = inline, compact = compact),
      htmltools::tags$script(htmltools::HTML(js))
    ),
    crosstalk::crosstalkLibs()
  )
}

# crosstalk's filter_select can't depend on other filters, so this is a
# single-choice dropdown (without an "(All)" option) that only offers the values
# of `group` occurring in the rows that match the current choice of each parent
# filter: the filter_radio or filter_select_dependent elements in `parent_id`,
# with their grouping variables in `parent_group` (a list if there are several).
# Without parents (the default), it offers the values of all rows. Values are
# offered in their order of appearance, and a choice keeps only its rows within
# the parents' choices, so a key may occur in several rows (e.g. once under each
# classification). `compact = TRUE` makes the dropdown and its label smaller,
# without margin below, as in filter_radio
filter_select_dependent <- function(id, label, sharedData, group,
                                    parent_id = character(), parent_group = list(),
                                    selected = NULL, compact = FALSE) {
  df <- sharedData$origData()
  eval_group <- \(g) {
    as.character(if (inherits(g, "formula")) eval(g[[2]], df, environment(g)) else g)
  }
  if (!is.list(parent_group)) parent_group <- list(parent_group)
  values <- eval_group(group)
  keys <- as.character(sharedData$key())
  # Joined the same way as the parents' current choices in the JavaScript below.
  # Without parents, all rows are under the key "all" (not "", which jsonlite
  # would turn into a number)
  parents <- if (length(parent_group)) {
    do.call(paste, c(lapply(parent_group, eval_group), sep = "\u001f"))
  } else rep("all", length(values))
  rows <- split(seq_along(values), parents)
  choices <- lapply(rows, \(i) unique(values[i]))
  map <- lapply(rows, \(i) split(keys[i], values[i]))
  
  # Set up at once, as in filter_radio()
  js <- sprintf(
    "(function() {
      var choices = %s, map = %s, selected = %s;
      var select = document.getElementById(%s);
      var parents = %s.map(function(id) { return document.getElementById(id); });
      var handle = new crosstalk.FilterHandle(%s);

      function parentKey() {
        if (!parents.length) return 'all';
        return parents.map(function(el) {
          return el.tagName === 'SELECT' ? el.value :
            el.querySelector('input:checked').value;
        }).join('\\u001f');
      }

      function update() {
        var offered = choices[parentKey()] || [];
        // Keep the current choice if it's still offered, else fall back to `selected`
        var value = [select.value, selected]
          .find(function(v) { return offered.includes(v); }) || offered[0];
        select.replaceChildren.apply(select, offered.map(function(v) {
          return new Option(v, v, false, v === value);
        }));
        // Also updates the filters that depend on this one
        select.dispatchEvent(new Event('change'));
      }

      select.addEventListener('change', function() {
        handle.set((map[parentKey()] || {})[select.value] || []);
      });
      parents.forEach(function(el) { el.addEventListener('change', update); });
      update();
    })();",
    jsonlite::toJSON(choices),
    jsonlite::toJSON(map),
    jsonlite::toJSON(selected, auto_unbox = TRUE, null = "null"),
    jsonlite::toJSON(id, auto_unbox = TRUE),
    jsonlite::toJSON(parent_id),
    jsonlite::toJSON(sharedData$groupName(), auto_unbox = TRUE)
  )
  
  htmltools::attachDependencies(
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
      htmltools::tags$script(htmltools::HTML(js))
    ),
    crosstalk::crosstalkLibs()
  )
}

# filter_radio with choices that depend on other filters, the way the choices of
# filter_select_dependent do (which describes `parent_id`, `parent_group` and how
# the choice is kept when they change): it only offers the values of `group`
# occurring in the rows that match the parents' current choices, in the order
# filter_radio would offer them, and shows its buttons even if it offers a single
# value. `inline` and `compact` are as in radio_buttons()
filter_radio_dependent <- function(id, label, sharedData, group, parent_id,
                                   parent_group, selected = NULL, inline = FALSE,
                                   compact = FALSE) {
  df <- sharedData$origData()
  eval_group <- \(g) if (inherits(g, "formula")) eval(g[[2]], df, environment(g)) else g
  if (!is.list(parent_group)) parent_group <- list(parent_group)
  values <- eval_group(group)
  lvls <- as.character(
    if (is.factor(values)) levels(droplevels(values)) else sort(unique(values))
  )
  values <- as.character(values)
  keys <- as.character(sharedData$key())
  # Joined the same way as the parents' current choices in the JavaScript below
  parents <- do.call(paste, c(lapply(parent_group, \(g) as.character(eval_group(g))),
                              sep = "\u001f"))
  rows <- split(seq_along(values), parents)
  fieldsets <- lapply(rows, \(i) {
    offered <- intersect(lvls, values[i])
    radio_buttons(id, label, offered,
                  selected = if (isTRUE(selected %in% offered)) selected else offered[[1]],
                  inline = inline, compact = compact)
  })
  map <- lapply(rows, \(i) split(keys[i], values[i]))
  
  # Set up at once, as in filter_radio()
  js <- sprintf(
    "(function() {
      var contents = %s, map = %s, selected = %s;
      var fieldset = document.getElementById(%s);
      var parents = %s.map(function(id) { return document.getElementById(id); });
      var handle = new crosstalk.FilterHandle(%s);

      function parentKey() {
        return parents.map(function(el) {
          return el.tagName === 'SELECT' ? el.value :
            el.querySelector('input:checked').value;
        }).join('\\u001f');
      }

      function value() {
        var checked = fieldset.querySelector('input:checked');
        return checked && checked.value;
      }

      function update() {
        var current = value();
        fieldset.innerHTML = contents[parentKey()] || '';
        var inputs = Array.from(fieldset.querySelectorAll('input'));
        var offered = inputs.map(function(el) { return el.value; });
        // Keep the current choice if it's still offered, else fall back to `selected`
        var choice = [current, selected]
          .find(function(v) { return offered.includes(v); }) || offered[0];
        inputs.forEach(function(el) { el.checked = el.value === choice; });
        // Also updates the filters that depend on this one
        fieldset.dispatchEvent(new Event('change'));
      }

      fieldset.addEventListener('change', function() {
        handle.set((map[parentKey()] || {})[value()] || []);
      });
      parents.forEach(function(el) { el.addEventListener('change', update); });
      update();
    })();",
    # What goes inside the fieldset (the legend and the buttons) for each choice of
    # the parents
    jsonlite::toJSON(lapply(fieldsets, \(f) as.character(htmltools::tagList(f$children))),
                     auto_unbox = TRUE),
    jsonlite::toJSON(map),
    jsonlite::toJSON(selected, auto_unbox = TRUE, null = "null"),
    jsonlite::toJSON(id, auto_unbox = TRUE),
    jsonlite::toJSON(parent_id),
    jsonlite::toJSON(sharedData$groupName(), auto_unbox = TRUE)
  )
  
  htmltools::attachDependencies(
    htmltools::tagList(
      # The buttons for the parents' first combination of values, until the
      # JavaScript reads their current choices
      fieldsets[[1]],
      htmltools::tags$script(htmltools::HTML(js))
    ),
    crosstalk::crosstalkLibs()
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
# y axis, as shown (in one line). The Excel
# file is put together here (as a zip of a few XML files), so that the page needs
# no library for it. As the result is no longer a widget, this comes after
# everything else done to the chart
add_download_menu <- function(chart, unit, id, choice_id = NULL,
                              download_axis_name = FALSE, row_names = NULL,
                              point_columns = NULL) {
  chart <- htmlwidgets::onRender(chart, r"-(
                                 function(el, x, data) {
                                 var chart = echarts.getInstanceByDom(el);
                                 
                                 // The rows of the table of a scatter plot, the header first: a row for each
                                 // point of each scatter series shown (not of the line through the plot), in
                                 // the order of the series and of their data, with the name of its series,
                                 // then its values in data.points.columns
                                 function pointTable(model) {
                                 var rows = [];
                                 model.eachSeries(function(series) {
                                 if (series.subType !== 'scatter') return;
                                 var points = series.getData();
                                 for (var i = 0; i < points.count(); i++) {
                                 var item = points.getRawDataItem(i);
                                 rows.push([series.name].concat(data.points.columns.map(function(column) {
                                 var value = item[column];
                                 // A year as a number, so that Excel takes it for one
                                 return value == null ? undefined : isNaN(value) ? value : Number(value);
                                 })));
                                 }
                                 });
                                 return [['Kategória'].concat(data.points.headers)].concat(rows);
                                 }
                                 
                                 // The rows of the table, the header first, with the x values in their order
                                 // on the axis, and an empty cell (undefined) where a series has no value
                                 function table() {
                                 var model = chart.getModel(), axis = model.getComponent('xAxis');
                                 if (data.points) return pointTable(model);
                                 var header = [axis.get('name')], rows = {}, columnOf = {};
                                 // Skips the series hidden with the legend; those filtered out have no data.
                                 // Series of the same name (in stack mode, the negative and the positive
                                 // parts of a category) share a column, with their values added up. In a
                                 // heatmap, each row has a column, of the values its cells are coloured by
                                 model.eachSeries(function(series) {
                                 var points = series.getData(), heatmap = series.id === 'heatmap';
                                 var y = points.mapDimension(heatmap ? 'value' : 'y');
                                 for (var i = 0; i < points.count(); i++) {
                                 var name = points.getName(i), value = points.get(y, i);
                                 if (isNaN(value)) continue;
                                 var column = heatmap ?
                                 data.rowNames[points.getRawDataItem(i)[points.mapDimension('y')]] : series.name;
                                 var j = columnOf[column] || header.length;
                                 // A year as a number, so that Excel takes it for one
                                 rows[name] = rows[name] || [isNaN(name) ? name : Number(name)];
                                 rows[name][j] = (rows[name][j] || 0) + value;
                                 if (j === header.length) {
                                 columnOf[column] = j;
                                 header.push(column + (data.unit ? ' [' + data.unit + ']' : ''));
                                 }
                                 }
                                 });
                                 var order = axis.get('data');
                                 return [header].concat(Object.keys(rows)
                                 .sort(function(a, b) { return order.indexOf(a) - order.indexOf(b); })
                                 .map(function(name) {
                                 return header.map(function(_, j) { return rows[name][j]; });
                                 }));
                                 }
                                 
                                 // The name of the y axis, in one line, if the files are to start with it
                                 // (read at the click, as it changes with the plotted column)
                                 function caption() {
                                 if (data.downloadAxisName) {
                                 return chart.getModel().getComponent('yAxis').get('name').replace(/\n/g, ' ');
                                 }
                                 }
                                 
                                 // With 15 significant digits, as R writes numbers, which drops the noise of
                                 // floating-point arithmetic (as in 0.30000000000000004)
                                 function number(value) {
                                 return String(Number(value.toPrecision(15)));
                                 }
                                 
                                 // Semicolons between the columns and decimal commas, as Excel expects them
                                 // with Hungarian regional settings. `caption`, if any, is a line before the
                                 // rows
                                 function csv(rows, caption) {
                                 var text = (caption ? [[caption]] : []).concat(rows).map(function(row) {
                                 return row.map(function(cell) {
                                 if (cell === undefined) return '';
                                 if (typeof cell === 'number') return number(cell).replace('.', ',');
                                 return /[";\r\n]/.test(cell) ? '"' + cell.replace(/"/g, '""') + '"' : cell;
                                 }).join(';');
                                 }).join('\r\n');
                                 // The byte order mark tells Excel that the file is in UTF-8
                                 return new Blob(['\ufeff' + text + '\r\n'], { type: 'text/csv;charset=utf-8' });
                                 }
                                 
                                 // `caption`, if any, is a row before the rows, in one cell, which spills over
                                 // the empty ones next to it, so the columns aren't widened for it
                                 function xlsx(rows, caption) {
                                 // The name of the j-th column (from 0): A to Z, then AA, AB...
                                 function column(j) {
                                 return (j >= 26 ? column(Math.floor(j / 26) - 1) : '') +
                                 String.fromCharCode(65 + j % 26);
                                 }
                                 var sheetData = (caption ? [[caption]] : []).concat(rows).map(function(row, i) {
                                 return '<row r="' + (i + 1) + '">' + row.map(function(cell, j) {
                                 var ref = ' r="' + column(j) + (i + 1) + '"';
                                 if (cell === undefined) return '';
                                 if (typeof cell === 'number') return '<c' + ref + '><v>' + number(cell) + '</v></c>';
                                 cell = cell.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
                                 return '<c' + ref + ' t="inlineStr"><is><t>' + cell + '</t></is></c>';
                                 }).join('') + '</row>';
                                 }).join('');
                                 // Each column wide enough for its header, within limits
                                 var cols = rows[0].map(function(name, j) {
                                 return '<col min="' + (j + 1) + '" max="' + (j + 1) + '" width="' +
                                 Math.min(Math.max(name.length + 2, 10), 50) + '" customWidth="1"/>';
                                 }).join('');
                                 var ns = 'http://schemas.openxmlformats.org/';
                                 var type = 'application/vnd.openxmlformats-';
                                 return zip({
                                 '[Content_Types].xml':
                                 '<Types xmlns="' + ns + 'package/2006/content-types">' +
                                 '<Default Extension="rels" ContentType="' + type + 'package.relationships+xml"/>' +
                                 '<Default Extension="xml" ContentType="application/xml"/>' +
                                 '<Override PartName="/xl/workbook.xml" ContentType="' + type +
                                 'officedocument.spreadsheetml.sheet.main+xml"/>' +
                                 '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="' + type +
                                 'officedocument.spreadsheetml.worksheet+xml"/></Types>',
                                 '_rels/.rels':
                                 '<Relationships xmlns="' + ns + 'package/2006/relationships">' +
                                 '<Relationship Id="rId1" Target="xl/workbook.xml" Type="' + ns +
                                 'officeDocument/2006/relationships/officeDocument"/></Relationships>',
                                 'xl/workbook.xml':
                                 '<workbook xmlns="' + ns + 'spreadsheetml/2006/main" xmlns:r="' + ns +
                                 'officeDocument/2006/relationships"><sheets>' +
                                 '<sheet name="Adatok" sheetId="1" r:id="rId1"/></sheets></workbook>',
                                 'xl/_rels/workbook.xml.rels':
                                 '<Relationships xmlns="' + ns + 'package/2006/relationships">' +
                                 '<Relationship Id="rId1" Target="worksheets/sheet1.xml" Type="' + ns +
                                 'officeDocument/2006/relationships/worksheet"/></Relationships>',
                                 'xl/worksheets/sheet1.xml':
                                 '<worksheet xmlns="' + ns + 'spreadsheetml/2006/main"><cols>' + cols +
                                 '</cols><sheetData>' + sheetData + '</sheetData></worksheet>'
                                 }, type + 'officedocument.spreadsheetml.sheet');
                                 }
                                 
                                 // A zip archive of the XML documents `files` (their contents by name), as a
                                 // Blob of the MIME type `type`, with the files stored uncompressed
                                 function zip(files, type) {
                                 var crcTable = Array.from({ length: 256 }, function(_, c) {
                                 for (var k = 0; k < 8; k++) c = c & 1 ? 0xEDB88320 ^ (c >>> 1) : c >>> 1;
                                 return c;
                                 });
                                 // The integers `values`, little-endian, each of the size in bytes in `sizes`
                                 function bytes(values, sizes) {
                                 var view = new DataView(new ArrayBuffer(sizes.reduce(function(a, b) { return a + b; })));
                                 values.reduce(function(at, value, i) {
                                 view[sizes[i] === 2 ? 'setUint16' : 'setUint32'](at, value, true);
                                 return at + sizes[i];
                                 }, 0);
                                 return new Uint8Array(view.buffer);
                                 }
                                 var encoder = new TextEncoder(), local = [], central = [], offset = 0;
                                 Object.keys(files).forEach(function(name) {
                                 var path = encoder.encode(name);
                                 var content = encoder.encode(
                                 '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n' + files[name]
                                 );
                                 var crc = ~content.reduce(function(c, byte) {
                                 return crcTable[(c ^ byte) & 255] ^ (c >>> 8);
                                 }, -1) >>> 0;
                                 // The fields that both headers of the file have, from the version needed
                                 // to extract it (2.0): no flags, no compression, modified on 1980-01-01 at
                                 // 0:00, the CRC-32 and the sizes, the length of the name and of the extra
                                 // field
                                 var common = [20, 0, 0, 0, 33, crc, content.length, content.length, path.length, 0];
                                 var sizes = [2, 2, 2, 2, 2, 4, 4, 4, 2, 2];
                                 local.push(bytes([0x04034b50].concat(common), [4].concat(sizes)), path, content);
                                 // Also with the version that made it and where its local header is
                                 central.push(bytes([0x02014b50, 20].concat(common, [0, 0, 0, 0, offset]),
                                 [4, 2].concat(sizes, [2, 2, 2, 4, 4])), path);
                                 offset += 30 + path.length + content.length;
                                 });
                                 var size = central.reduce(function(total, part) { return total + part.length; }, 0);
                                 var count = Object.keys(files).length;
                                 var end = bytes([0x06054b50, 0, 0, count, count, size, offset, 0],
                                 [4, 2, 2, 2, 2, 4, 4, 2]);
                                 return new Blob(local.concat(central, [end]), { type: type });
                                 }
                                 
                                 // The title of the chart and, if the plotted column is chosen with radio
                                 // buttons, the plotted one (which is checked even while the buttons are
                                 // hidden), without the characters that Windows doesn't allow in file names
                                 function fileName() {
                                 var parts = [];
                                 chart.getModel().eachComponent('title', function(title) {
                                 parts.push(title.get('text'));
                                 });
                                 var choice = data.choiceId && document.getElementById(data.choiceId);
                                 if (choice) {
                                 var input = choice.querySelector('input:checked');
                                 parts.push(choice.querySelector('label[for="' + input.id + '"]').textContent);
                                 }
                                 return (parts.filter(Boolean).join(', ') || 'adatok')
                                 .replace(/\n/g, ' ').replace(/: /g, ' - ').replace(/[\\/:*?"<>|]/g, '-');
                                 }
                                 
                                 // The height of the legend of the chart `c` (of its items)
                                 function legendHeight(c) {
                                 var model = c.getModel().getComponent('legend');
                                 var view = model && c.getViewOfComponentModel(model);
                                 return view ? view.getContentGroup().getBoundingRect().height : 0;
                                 }
                                 
                                 // A copy of the chart as shown, for an image, drawn with ECharts' `renderer`
                                 // ('canvas' or 'svg'), without animation, on the background of the chart or
                                 // else white, as ECharts' own saveAsImage button would. Its legend is shown
                                 // whole, with whole names, as an image can't scroll it or show a name cut short
                                 // on hover, and the copy is taller than the chart by the room that takes
                                 function copyOf(renderer) {
                                 var width = chart.getWidth(), height = chart.getHeight();
                                 // The canvas copy is drawn at the pixel ratio of the images (see bitmap()), not
                                 // the screen's: ECharts would scale a copy with a higher one (as on phones) down
                                 // to theirs, blurring them slightly
                                 var copy = renderer === 'svg' ?
                                 echarts.init(null, null, { renderer: 'svg', ssr: true, width: width, height: height }) :
                                 echarts.init(document.createElement('div'), null,
                                 { width: width, height: height, devicePixelRatio: 2 });
                                 var option = chart.getOption();
                                 option.animation = false;
                                 option.backgroundColor = chart.getModel().get('backgroundColor') || '#fff';
                                 (option.legend || []).forEach(function(legend) {
                                 legend.type = 'plain';
                                 legend.textStyle = Object.assign({}, legend.textStyle, { width: null, overflow: 'none' });
                                 });
                                 copy.setOption(option);
                                 // If a name is too long for a line of the legend (as on phones), the legend
                                 // becomes a column of the names broken into lines as wide as the room beside
                                 // the icon and the 5px gap after it. (Not rows, as ECharts would let the names
                                 // of more lines overlap the row above. Laid out in a chart tall enough not to
                                 // wrap the column into more)
                                 var legendModel = copy.getModel().getComponent('legend');
                                 if (legendModel) {
                                 // The padding of the legend is given as in CSS
                                 var padding = [].concat(legendModel.get('padding'));
                                 var right = padding.length > 1 ? padding[1] : padding[0];
                                 var room = width - right - (padding.length > 3 ? padding[3] : right);
                                 var items = copy.getViewOfComponentModel(legendModel).getContentGroup();
                                 if (items.getBoundingRect().width > room) {
                                 copy.resize({ width: width, height: 10 * height });
                                 copy.setOption({ legend: {
                                 orient: 'vertical',
                                 textStyle: {
                                 width: room - legendModel.get('itemWidth') - 5, overflow: 'break',
                                 // ECharts would set the lines as close as the size of the font, so that the
                                 // accents of a line touch the one above
                                 lineHeight: Math.round(1.4 * (legendModel.get(['textStyle', 'fontSize']) || 12))
                                 }
                                 } });
                                 }
                                 }
                                 var extra = Math.round(legendHeight(copy) - legendHeight(chart));
                                 // The outer bounds of the plot, which keep the labels of the axes above the
                                 // legend (see add_chart_layout()), move up with it
                                 var outer = option.grid[0].outerBounds || {};
                                 copy.setOption({ grid: {
                                 bottom: option.grid[0].bottom + extra,
                                 outerBounds: { bottom: (outer.bottom || 0) + extra }
                                 } });
                                 copy.resize({ width: width, height: height + extra });
                                 return copy;
                                 }
                                 
                                 // The chart as shown, as the data URL of a PNG or a JPG (`type` 'png' or
                                 // 'jpeg'), at twice the resolution
                                 function bitmap(type) {
                                 var copy = copyOf('canvas');
                                 var url = copy.getDataURL({ type: type, pixelRatio: 2 });
                                 copy.dispose();
                                 return url;
                                 }
                                 
                                 // The chart as shown, in an SVG file
                                 function svg() {
                                 var copy = copyOf('svg');
                                 var text = copy.renderToSVGString();
                                 copy.dispose();
                                 return new Blob([text], { type: 'image/svg+xml' });
                                 }
                                 
                                 // `file` is a Blob or, for a PNG or a JPG, a data URL
                                 function save(file, extension) {
                                 var link = document.createElement('a');
                                 link.href = file instanceof Blob ? URL.createObjectURL(file) : file;
                                 link.download = fileName() + '.' + extension;
                                 document.body.appendChild(link);
                                 link.click();
                                 link.remove();
                                 // Once the download has surely started
                                 if (file instanceof Blob) {
                                 setTimeout(function() { URL.revokeObjectURL(link.href); }, 60000);
                                 }
                                 }
                                 
                                 // Each item of the menu saves in the format it names
                                 var menu = document.getElementById(data.menuId);
                                 menu.addEventListener('click', function(event) {
                                 var item = event.target.closest('[data-format]');
                                 if (!item) return;
                                 var format = item.dataset.format;
                                 if (format === 'png' || format === 'jpg') {
                                 save(bitmap(format === 'jpg' ? 'jpeg' : 'png'), format);
                                 } else if (format === 'svg') {
                                 save(svg(), 'svg');
                                 } else {
                                 save((format === 'csv' ? csv : xlsx)(table(), caption()), format);
                                 }
                                 // The menu closes, so keyboard users are taken back to the button
                                 menu.querySelector('[data-bs-toggle]').focus();
                                 });
                                 }
  )-", data = list(unit = unit, menuId = id, choiceId = choice_id,
                   downloadAxisName = download_axis_name, rowNames = row_names,
                   points = if (!is.null(point_columns)) {
                     list(headers = as.list(names(point_columns)),
                          columns = as.list(unname(point_columns)))
                   }))
  
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
# column is plotted). Each call has its own crosstalk group and element ids (made
# from `id`), so its controls only affect its own chart. The chart is only drawn
# once it comes near the viewport, and then starts from the current choices of its
# filters (see defer_chart() and start_filtered()). The helpers below make its
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
  data <- category_data(var, ids$group, stack, relative, heatmap)
  # After the values, in the tooltip and in the description for screen readers
  unit_suffix <- unit_suffix_for(unit)
  # The decimals of the values there: the ratios of relative mode need two
  fraction_digits <- if (relative) 2L else 1L
  row_names <- if (heatmap) heatmap_row_names(data$paths)
  
  chart <- data$shared_data |>
    # Hovering the legend highlights nothing if the chart had more than
    # hoverLayerThreshold (by default 3000) elements when last redrawn, as it has
    # here: the points of all categories, drawn before filtering, are still
    # fading out
    e_chart(yearchr, hoverLayerThreshold = 1e6, elementId = ids$chart)
  chart <- if (heatmap) {
    add_category_heatmap(chart, var, axis_name, row_names, ids$group, unit_suffix,
                         fraction_digits, heatmap_sizes, row_scale)
  } else {
    add_category_lines(chart, var, axis_name, unit_suffix, fraction_digits, stack,
                       relative, data$neg_var)
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
  if (!heatmap) chart <- add_part_lines(chart, data$paths, ids$group, stack, unit)
  chart <- chart |>
    add_base_year_slider(ids, var, base_year) |>
    add_chart_layout(title, ids, data$paths, relative,
                     row_height = if (heatmap) heatmap_sizes$row,
                     min_plot_height = if (heatmap) heatmap_sizes$plot) |>
    start_filtered(ids$group)
  
  htmltools::attachDependencies(
    htmltools::tagList(
      category_filters(ids, data$shared_paths, var, var_label, parent_var, parts,
                       relative),
      add_download_menu(chart, unit, id = ids$download,
                        choice_id = if (length(var) > 1) ids$var,
                        download_axis_name = download_axis_name, row_names = row_names),
      defer_chart(ids$chart)
    ),
    kiadas_dependency
  )
}

# The ids of the elements of the category_chart() with the id `id`, and the name of
# its crosstalk group
category_ids <- function(id) {
  list(
    group = paste0("category_", id),
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

# What follows the values of a chart in its tooltip and in its description for
# screen readers: `unit`, after a space, but not after one if it's a percent sign
# (or if there's no unit)
unit_suffix_for <- function(unit) if (unit %in% c("", "%")) unit else paste0(" ", unit)

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

# The data of a category_chart() (see there) of the columns `var` of HURawData, in
# the crosstalk group `group`: `shared_paths` for the filters, `shared_data` for
# the chart and, showing parts (with `stack` or `relative`), the parts, `paths`,
# and in stack mode the columns of their negative parts, `neg_var`. The filters
# pick the plotted rows by their keys, as the two share the group. The chart only
# gets the columns it plots (and in a heatmap the spending, by which its rows are
# sized), since echarts4r embeds every column in the page, and those rounded (see
# round_for_chart())
category_data <- function(var, group, stack = FALSE, relative = FALSE,
                          heatmap = FALSE) {
  paths <- neg_var <- NULL
  if (stack || relative) {
    # A row of HURawData in both classifications (e.g. Egészségügy) is a part of a
    # different whole in each, so here a key is a row of CategoryPaths, i.e. a row
    # of HURawData in one classification. The main group dropdown picks the whole:
    # a main group, made up of its subgroups, or "Összesen", made up of the main
    # groups. Each type of data only appears in the classification it comes in
    # (the budget's cash-based data in the Hungarian one, Eurostat's accrual-based
    # data in COFOG), as only there do the parts add up to the whole: the
    # categories mapped from the other classification leave gaps
    native <- c(original_plan = "Magyar", actual = "Magyar",
                actual_cofog = "Nemzetközi (COFOG)")
    paths <- CategoryPaths[classification == native[HURawData$type[key]], .(
      classification, main = fifelse(sub == "Összesen", "Összesen", main),
      typeLabel = HURawData$typeLabel[key], nameLabel, code = node, row = key, key = .I
    )]
    # A series for each part in each classification, in the order of the codes
    # (stacked in that order). Named by its number, as echarts4r writes the name
    # into every row
    paths[, series := paste(classification, nameLabel)]
    paths[, series := factor(series, levels = unique(series),
                             labels = seq_along(unique(series)))]
    shared_paths <- crosstalk::SharedData$new(
      # "Összesen" first in the dropdown
      paths[order(main != "Összesen")], key = ~key, group = group
    )
    chart_data <- HURawData[paths$row, c("yearchr", var), with = FALSE]
    # A heatmap has a row for each part instead, at its code (unique across the
    # classifications), which labels the row, and with the spending of the part
    if (heatmap) {
      chart_data[, `:=`(code = paths$code, spending = HURawData$value[paths$row],
                        key = paths$key)]
    } else {
      chart_data[, `:=`(series = paths$series, key = paths$key)]
    }
    # Before the negative parts are split off (below), so that they're rounded too
    round_for_chart(chart_data, c(var, if (heatmap) "spending"))
    # In relative mode, also the line that each row is on: its part, for its type
    # of data, all of whose values are divided by the one in the base year
    if (relative) {
      chart_data[, line := as.integer(interaction(paths$series, paths$typeLabel,
                                                  drop = TRUE))]
    }
    # In the order of the years, as echarts4r puts them on the x axis in the order
    # they first appear in
    setorder(chart_data, yearchr)
    if (stack) {
      # Each column split into its negative part, in a column of its name with
      # "_neg" appended, and its positive part, in the column itself, as the two
      # are stacked separately (see the series below). The negative part is
      # missing except around the years when it's below 0 (with 0 in the years
      # just before and after, so that its area tapers off), as a series of zeros
      # would still draw its line along 0
      neg_var <- paste0(var, "_neg")
      chart_data[, (neg_var) := lapply(.SD, \(x) {
        below <- !is.na(x) & x < 0
        fcase(below, x,
              shift(below, fill = FALSE) | shift(below, type = "lead", fill = FALSE), 0)
      }), by = series, .SDcols = var]
      chart_data[, (var) := lapply(.SD, pmax, 0), .SDcols = var]
    }
    shared_data <- crosstalk::SharedData$new(
      # A series for each part, except in a heatmap
      if (heatmap) as.data.frame(chart_data[, !"key"]) else
        chart_data[, !"key"] |> dplyr::group_by(series),
      key = as.character(chart_data$key), group = group
    )
  } else {
    # A key is the row number in HURawData (the default key of shared_data)
    shared_paths <- crosstalk::SharedData$new(CategoryPaths, key = ~key, group = group)
    shared_data <- crosstalk::SharedData$new(
      round_for_chart(HURawData[, c("typeLabel", "yearchr", var), with = FALSE], var) |>
        dplyr::group_by(typeLabel),
      group = group
    )
  }
  list(shared_paths = shared_paths, shared_data = shared_data, paths = paths,
       neg_var = neg_var)
}

# Rounds the columns `cols` of the chart data `dt` of category_data() (by
# reference, so `dt` must be a copy of HURawData's columns), which echarts4r would
# otherwise embed in the page with 16 significant digits: the amounts in Mrd Ft to
# 4 decimals, the precision of the source data (0.1 million Ft), so they stay
# exact, and the percentages to 6 significant digits rather than to a number of
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
# `ids` (see category_ids()), on the rows of `shared_paths` (see category_data()).
# They share a row on wide screens, but the dropdowns are at least 20rem wide, so
# on narrower screens they wrap onto rows of their own. (A Bootstrap .row would
# overflow, as Quarto sets overflow-x: auto on the cell output)
category_filters <- function(ids, shared_paths, var, var_label, parent_var,
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
      filter_radio(
        id = ids$classification,
        label = "Osztályozás:",
        sharedData = shared_paths,
        group = ~classification,
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
        sharedData = shared_paths,
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
        sharedData = shared_paths,
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
        sharedData = shared_paths,
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
      # "Összesen" (see below)
      hidden = if (length(setdiff(var, parent_var)) < 2) NA,
      radio_buttons(id = ids$var, label = var_label, choices = var, compact = TRUE)
    ),
    if (relative) htmltools::div(
      style = "flex: 1 1 20rem",
      htmltools::tags$label(
        class = "form-label small mb-1", `for` = ids$base, "Bázisév: ",
        # The chosen year, filled in by the chart (see below), which also gives it
        # to screen readers as the value of the slider
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
# followed by `unit_suffix`, with at most `fraction_digits` decimals. In stack
# mode, the columns `neg_var` have the negative parts (see category_data())
add_category_lines <- function(chart, var, axis_name, unit_suffix, fraction_digits,
                               stack = FALSE, relative = FALSE, neg_var = NULL) {
  chart <- if (stack) {
    # Each category as two series, of its negative and of its positive parts, each
    # stacked only on the series of the same sign, so the negative parts go below
    # 0 and the positive ones above. (ECharts' default would stack a value on the
    # values of its sign, which draws a category that is negative in a year as an
    # area diving from its place in the stack to below 0 and back, and stacks a 0
    # on the negative values if no category before it is positive in that year.)
    # The series of the negative parts come first, with a symbol only where the
    # part is below 0, not at the zeros around it. triggerLineEvent: hovering an
    # area, not only its points, tells which one it is, for the tooltip
    chart |>
      e_area_(neg_var[[1]], stack = "total", stackStrategy = "negative",
              emphasis = list(focus = "series"), triggerLineEvent = TRUE,
              symbolSize = htmlwidgets::JS("
                function(value, params) {
                  return value[params.dimensionNames[params.encode.y[0]]] < 0 ? 4 : 0;
                }
              ")) |>
      e_area_(var[[1]], stack = "total", stackStrategy = "positive",
              emphasis = list(focus = "series"), triggerLineEvent = TRUE)
  } else {
    # In relative mode, as above, hovering a line, not only its points, tells which
    # one it is, for the tooltip
    e_line_(chart, var[[1]], emphasis = list(focus = "series"),
            triggerLineEvent = relative)
  }
  chart |>
    # On phones, the tooltip is often wider than the room beside the finger, or
    # even than the chart (with the long names of the categories), and ECharts
    # would then push it partly off the screen. So it's kept within the chart, and
    # its lines wrap if it's still too wide (but not within a value and its unit).
    # The tooltip of the legend, with the names cut short there, does the same
    e_tooltip(trigger = "axis", confine = TRUE, extraCssText = "white-space: normal;",
              formatter = htmlwidgets::JS(sprintf("
                function(params) {
                  // Leaves out the series without a value in the year (NA in R,
                  // null here, as in the plan for the years without a deflator),
                  // which Number() would show as 0, and shows no tooltip if no
                  // series has one
                  params = params.filter(function(p) {
                    return p.value[p.dimensionNames[p.encode.y[0]]] != null;
                  });
                  if (!params.length) return '';
                  return params[0].name + '<br>' +
                    params.map(function(p) {
                      // With crosstalk, p.value is the whole data row as an object,
                      // not [x, y], so this reads the plotted column from it
                      var column = p.dimensionNames[p.encode.y[0]];
                      // The set decimals, or two significant digits if that's more
                      // precise, so that small values aren't rounded to 0
                      var value = Number(p.value[column]).toLocaleString('hu-HU', {
                        maximumFractionDigits: %d, maximumSignificantDigits: 2,
                        roundingPriority: 'morePrecision'
                      });

                      return p.marker + ' ' + p.seriesName +
                        ': <b style=\"white-space: nowrap\">' + value + '%s</b>';
                    }).join('<br>');
                }", fraction_digits, unit_suffix))) |>
    e_y_axis(name = axis_name[[1]],
             axisLabel = list(
               # toLocaleString keeps only 3 decimals by default, too few for the
               # ticks of the smallest categories
               formatter = htmlwidgets::JS("
                function(value) {
                  return value.toLocaleString('hu-HU', { maximumFractionDigits: 20 });
                }
              ")
             ))
}

# The heatmap of a category_chart() or a plan_actual_heatmap() (see there): a row
# for each part, labelled by its code, from the top in the order of the codes, with
# a height that follows its weight, its share of the whole's spending (in the
# column `spending`) in the base year or, with `base_weights = FALSE`, summed over
# the years shown (proportional to the weight's square root with
# `row_scale = "sqrt"`, or to the weight itself with "linear": see relayout()
# below), a column for each year, and the cells coloured by the column var[[1]] (or
# that chosen with add_column_choice()), with `axis_name[[1]]` on the y axis, as it
# tells what the colours show. The colours of a ratio (as relative to the base
# year) run continuously on a log scale, so that halving looks as
# strong as doubling, from red at 1/2 through light grey at 1 (no change) to blue
# at 2, and the ratios beyond take the colours at the ends: over a wider range, most
# ratios would be barely tinted. Light grey, so that no change can be told from the
# cells without a ratio, which are left empty. The colour scale beside the plot is
# `sizes$scale` px tall, labelled at 1/2, 0.8, 1, 1.25 and 2 (symmetric around 1 on
# the log scale, as 0.8 = 1/1.25). The columns in `linear` (as differences) are
# coloured in the same colours on a linear scale symmetric around 0 instead (see
# restyle() below). As the y axis only shows the codes, the legend
# lists the full names of the rows, `row_names` (by code), and so does the tooltip,
# with the year and the value of the hovered cell, followed by `unit_suffix`, with
# at most `fraction_digits` decimals or, with `plan_actual` (the names of the plan
# and the actual spending), with what the tooltip of a plan_actual_chart() shows of
# a point (see kiadas.planActualTooltip()), from the columns `plan` and `actual`,
# with the same unit and decimals; and then with the weight of the row, as
# `weight_label` says. The filters of the crosstalk group `group` leave the rows of
# the parts they pick, and the years from their first to their last. `sizes` also
# gives the average height of a row (`row`) and the least height of the plot
# (`plot`), in px, as add_chart_layout() lays it out
add_category_heatmap <- function(chart, var, axis_name, row_names, group,
                                 unit_suffix, fraction_digits, sizes, row_scale,
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
  # 1/32 wide on the log2 scale (or on a linear one, see restyle() below), too narrow
  # to tell apart, each as its middle (the ones at the ends also taking every value
  # beyond). Its own legend can't label a gradient between its ends, so it's hidden,
  # and the colour scale drawn by hand
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
  
  # Set up as a heatmap, but drawn by hand, as a custom series (see renderItem()
  # below), as the rows of a heatmap are all equally tall
  chart <- e_heatmap_(chart, "code", var[[1]])
  chart$x$opts$series[[1]] <- modifyList(chart$x$opts$series[[1]], list(
    # So that it can be updated by itself (see add_column_choice())
    id = "heatmap", type = "custom", encode = list(value = var[[1]]),
    # Nothing to draw until the rows are laid out
    renderItem = htmlwidgets::JS("function() { return null; }")
  ))
  # The legend lists series without data, one for each name, without icons, so one
  # name in each line, so that it's clear where one ends and the next one starts.
  # Its names can be switched off, as ECharts makes the items of a legend ignore the
  # mouse otherwise, but they're switched back on at once (see below)
  full_names <- unique(unlist(row_names, use.names = FALSE))
  chart$x$opts$series <- c(chart$x$opts$series, lapply(full_names, \(name) {
    list(name = name, type = "line", data = list())
  }))
  chart$x$opts$legend <- list(data = as.list(full_names), orient = "vertical",
                              icon = "none", itemWidth = 0)
  # Its pieces are set by restyle() (below), before any cell is drawn
  chart$x$opts$visualMap <- list(list(
    type = "piecewise", show = FALSE, seriesIndex = 0, dimension = var[[1]]
  ))
  # The rows take up their weights' shares of the y axis, from 0 at the top to 1 at
  # the bottom, labelled with their codes instead of numbers (see relayout()
  # below). Its name starts above the rows: centred on the axis, as in the other
  # modes, it would push the plot to the right on phones (ECharts keeps it within
  # the chart)
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
    htmlwidgets::onRender("
      function(el, x, data) {
        var chart = echarts.getInstanceByDom(el);
        var option = chart.getOption(), years = option.xAxis[0].data;
        // The data as it came, before the slider divides it by the base year (see
        // add_base_year_slider()), and the series
        var source = option.dataset[0].source, series = chart.getModel().getSeriesByIndex(0);
        var format = new Intl.NumberFormat('hu-HU', {
          maximumFractionDigits: data.digits, maximumSignificantDigits: 2,
          roundingPriority: 'morePrecision'
        });
        var percent = new Intl.NumberFormat('hu-HU', {
          maximumFractionDigits: 1, maximumSignificantDigits: 2,
          roundingPriority: 'morePrecision'
        });
        // The colour scale (see above), which this draws with the ticks it's given
        // (see kiadas.colorScale() in kiadas_dependency)
        var scale = data.scale, drawScale = kiadas.colorScale(chart, scale);

        chart.setOption({
          // The year, the full name of the row and the value of the hovered cell, as
          // in the tooltip of the lines or, with data.planActual, as in that of a
          // plan_actual_chart(), and then the weight of the row (see relayout()).
          // (With crosstalk, p.value is the whole data row as an object)
          tooltip: {
            formatter: function(p) {
              function value(dim) { return p.value[p.dimensionNames[p.encode[dim][0]]]; }
              var row = rows[value('y')], name = data.names[value('y')];
              var lines = data.planActual ?
                kiadas.planActualTooltip(p.marker, name, value('x'), p.value.plan,
                                         p.value.actual, data.planActual, data.unit,
                                         data.digits) :
                [value('x'), p.marker + ' ' + name + ': <b style=\"white-space: nowrap\">' +
                   format.format(value('value')) + data.unit + '</b>'];
              lines.push(data.weightLabel + ': ' +
                         (row ? percent.format(100 * row.share) + '%' : ''));
              return lines.join('<br>');
            }
          }
        });

        // The rows of the data that the filters leave, the base year, and the layout
        // of the rows shown: for each code, its weight (see below) and its span on
        // the y axis
        var shown = [], baseYear = null, rows = {};

        // A cell as a rectangle across its year and down its row's span, coloured by
        // its value (left out if it has none, or if its row isn't shown). White lines
        // between the cells, as in a heatmap, except around rows too thin for them,
        // and dark ones around the highlighted cells
        function renderItem(params, api) {
          var points = series.getData();
          var row = rows[points.getRawDataItem(params.dataIndexInside).code];
          if (!row || isNaN(api.value(points.mapDimension('value')))) return null;
          var year = api.value('yearchr');
          var top = api.coord([year, row.span[0]]), bottom = api.coord([year, row.span[1]]);
          var width = api.size([1, 0])[0], height = bottom[1] - top[1];
          return {
            type: 'rect',
            shape: { x: top[0] - width / 2, y: top[1], width: width, height: height },
            style: { fill: api.visual('color'), stroke: height >= 5 ? '#fff' : null,
                     lineWidth: 1 },
            emphasis: { style: { stroke: '#333', lineWidth: 1.5 } }
          };
        }

        // Lays out the rows shown, from the top in the order of their codes, each
        // with a height proportional to its weight, its share of the whole's spending
        // in the base year or, without data.baseWeights, summed over the years shown
        // (by absolute value; the rows without any then are left out), or with
        // data.rowScale 'sqrt', to the weight's square root, but at
        // least 2px, so that it stays visible. Labels them with their codes, from the
        // largest, wherever a label stays 13px clear of the others, and lists their
        // names in the legend
        function relayout() {
          var weights = {};
          shown.forEach(function(r) {
            if (!r.spending) return;
            if (!data.baseWeights) {
              weights[r.code] = (weights[r.code] || 0) + Math.abs(r.spending);
            } else if (r.yearchr === baseYear) {
              weights[r.code] = Math.abs(r.spending);
            }
          });
          var codes = data.codes.filter(function(code) { return weights[code] > 0; });
          // What the heights are proportional to
          var extents = {};
          codes.forEach(function(code) {
            extents[code] = data.rowScale === 'sqrt' ? Math.sqrt(weights[code]) : weights[code];
          });
          // The height of the plot, as add_chart_layout() makes it
          var n = codes.length, plot = Math.max(n * data.sizes.row, data.sizes.plot);
          var min = 2 / plot, thin = {}, rest = 1, sum, newly;
          // The rows that would be thinner get the minimum, and the others share the
          // rest
          do {
            sum = 0;
            codes.forEach(function(code) { if (!thin[code]) sum += extents[code]; });
            newly = codes.filter(function(code) {
              return !thin[code] && extents[code] / sum * rest < min;
            });
            newly.forEach(function(code) { thin[code] = true; });
            rest -= newly.length * min;
          } while (newly.length);
          var total = codes.reduce(function(s, code) { return s + weights[code]; }, 0);
          var top = 0, labels = new Map();
          rows = {};
          codes.forEach(function(code) {
            var height = thin[code] ? min : extents[code] / sum * rest;
            rows[code] = { share: weights[code] / total, span: [top, top + height] };
            top += height;
          });
          codes.slice().sort(function(a, b) { return weights[b] - weights[a]; })
            .forEach(function(code) {
              var middle = (rows[code].span[0] + rows[code].span[1]) / 2;
              var clear = Array.from(labels.keys()).every(function(m) {
                return Math.abs(m - middle) >= 13 / plot;
              });
              if (clear) labels.set(middle, code);
            });
          chart.setOption({
            legend: { data: codes.map(function(code) { return data.names[code]; }) },
            yAxis: { axisLabel: {
              customValues: Array.from(labels.keys()).sort(function(a, b) { return a - b; }),
              formatter: function(value) { return labels.get(value) || ''; }
            } },
            // Redrawn in the new layout
            series: [{ id: 'heatmap', renderItem: renderItem }]
          });
        }

        // Colours the cells by the column `column`, by pieces 1/32 wide (see above),
        // each in the colour of its middle: on the log2 scale of ratios from 1/2 to 2
        // or, for a column in data.linear, on a linear scale from -M to M, where M is
        // the 90th percentile of the absolute values of the cells shown, rounded up
        // to 1, 2 or 5 times a power of 10, so that a few large values don't leave
        // the others barely tinted (the values beyond take the colours at the ends)
        var column = data.columns[0];
        function restyle() {
          var linear = data.linear.indexOf(column) >= 0, range = 1, ticks = scale.ticks;
          if (linear) {
            var values = shown.filter(function(r) { return r[column] != null; })
              .map(function(r) { return Math.abs(r[column]); })
              .sort(function(a, b) { return a - b; });
            var q = values.length ? values[Math.floor(0.9 * (values.length - 1))] : 0;
            if (q > 0) {
              var power = Math.pow(10, Math.floor(Math.log10(q)));
              range = [1, 2, 5, 10].map(function(s) { return s * power; })
                .find(function(s) { return s >= q; });
            }
            ticks = [-1, -0.5, 0, 0.5, 1].map(function(t, i) {
              var text = (range * t).toLocaleString('hu-HU', { maximumFractionDigits: 20 });
              return { position: (t + 1) / 2,
                       label: i === 0 ? '\\u2264 ' + text : i === 4 ? '\\u2265 ' + text : text };
            });
          }
          // The bound between the pieces i and i + 1, halfway between their middles,
          // which run from -1 to 1
          var n = data.colors.length;
          function bound(i) {
            var position = -1 + 2 * (i + 0.5) / (n - 1);
            return linear ? range * position : Math.pow(2, position);
          }
          chart.setOption({ visualMap: {
            dimension: column,
            pieces: data.colors.map(function(color, i) {
              var piece = { color: color };
              if (i > 0) piece.gte = bound(i - 1);
              if (i < n - 1) piece.lt = bound(i);
              return piece;
            })
          } });
          drawScale(ticks);
        }

        // The heatmap shows only the rows of the parts that the filters leave, and
        // the x axis only the years from their first to their last (from the start,
        // and whenever the filters change), coloured for the values shown. With
        // weights from the base year, the rows are laid out once the slider has
        // chosen it, which it first does right after this
        kiadas.onFilter(data.group, function(value) {
          var keys = value && new Set(value.map(String)), hasYear = {};
          shown = source.filter(function(r) { return !keys || keys.has(String(r.XkeyX)); });
          shown.forEach(function(r) { hasYear[r.yearchr] = true; });
          var covered = years.filter(function(year) { return hasYear[year]; });
          chart.setOption({
            xAxis: {
              data: years.slice(years.indexOf(covered[0]),
                                years.indexOf(covered[covered.length - 1]) + 1)
            }
          });
          restyle();
          if (baseYear !== null || !data.baseWeights) relayout();
        });
        // And whenever the base year changes
        el.addEventListener('baseyear', function(e) {
          baseYear = e.detail;
          relayout();
        });
        // And recoloured whenever another column is chosen (see add_column_choice())
        el.addEventListener('column', function(e) {
          column = e.detail;
          restyle();
        });

        // Hovering a name in the legend highlights the cells of its row (rather than
        // the series of that name, which has none)
        var named = new Set(data.codes.map(function(code) { return data.names[code]; }));
        ['highlight', 'downplay'].forEach(function(type) {
          chart.on(type, function(e) {
            // Not for the actions dispatched here, which name no series
            if (!named.has(e.seriesName)) return;
            var points = chart.getModel().getSeriesByIndex(0).getData();
            var row = points.mapDimension('y'), cells = [];
            for (var i = 0; i < points.count(); i++) {
              if (data.names[points.getRawDataItem(i)[row]] === e.seriesName) {
                cells.push(points.getRawIndex(i));
              }
            }
            chart.dispatchAction({ type: type, seriesIndex: 0, dataIndex: cells });
          });
        });

        // A name switched off in the legend is switched back on at once, as its
        // series has no cells to hide
        chart.on('legendselectchanged', function(e) {
          if (named.has(e.name) && !e.selected[e.name]) {
            chart.dispatchAction({ type: 'legendSelect', name: e.name });
          }
        });
      }
    ", data = list(group = group, codes = as.list(names(row_names)), names = row_names,
                   unit = unit_suffix, digits = fraction_digits, scale = scale,
                   sizes = sizes, rowScale = row_scale, colors = as.list(color_at(middles)),
                   columns = as.list(var), linear = as.list(linear),
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
    htmlwidgets::onRender("
      function(el, x, data) {
        var chart = echarts.getInstanceByDom(el);
        // A value of the column `column`, with at most its decimals, and its unit
        var formats = {};
        function format(value, column) {
          var digits = typeof data.digits === 'number' ? data.digits : data.digits[column];
          formats[digits] = formats[digits] || new Intl.NumberFormat('hu-HU', {
            maximumFractionDigits: digits, maximumSignificantDigits: 2,
            roundingPriority: 'morePrecision'
          });
          return formats[digits].format(value) +
            (typeof data.unit === 'string' ? data.unit : data.unit[column]);
        }
        el.setAttribute('role', 'img');

        // The points of a scatter plot, of each scatter series shown (not of the
        // line through the plot), each labelled, with its values on the two axes
        function describePoints(model) {
          var described = [];
          model.eachSeries(function(series) {
            if (series.subType !== 'scatter') return;
            var points = series.getData(), values = [];
            var x = points.mapDimension('x'), y = points.mapDimension('y');
            for (var i = 0; i < points.count(); i++) {
              values.push(points.getRawDataItem(i)[data.points.label] + ': ' +
                          data.points.x + ' ' + format(points.get(x, i), x) + ', ' +
                          data.points.y + ' ' + format(points.get(y, i), y));
            }
            if (values.length) described.push(series.name + ': ' + values.join('; '));
          });
          return described;
        }

        // After every redraw, so whenever the filters, the plotted column, the
        // title or the legend change
        chart.on('finished', function() {
          var model = chart.getModel(), parts = [];
          model.eachComponent('title', function(title) { parts.push(title.get('text')); });
          var xAxis = model.getComponent('xAxis');
          var yName = model.getComponent('yAxis').get('name');
          parts.push(typeof data.type === 'string' ? data.type : data.type[xAxis.get('type')],
                     'Vízszintes tengely: ' + xAxis.get('name'));
          // A heatmap's y axis is named after what its colours show
          if (data.rowNames) {
            parts.push('Függőleges tengely: ' + data.rowsName, 'A cellák színe: ' + yName);
          } else {
            parts.push('Függőleges tengely: ' + yName);
          }
          // Skips the series hidden with the legend; those filtered out have no data.
          // Series of the same name (in stack mode, the negative and the positive
          // parts of a category) are added up. A heatmap's values, those its cells
          // are coloured by, are listed by row, and a scatter plot's by point
          if (data.points) {
            parts.push.apply(parts, describePoints(model));
          } else {
            // With the column of each name
            var sums = new Map(), columns = new Map();
            model.eachSeries(function(series) {
              var points = series.getData(), heatmap = series.id === 'heatmap';
              var y = points.mapDimension(heatmap ? 'value' : 'y');
              for (var i = 0; i < points.count(); i++) {
                var x = points.getName(i), value = points.get(y, i);
                if (isNaN(value)) continue;
                var name = heatmap ?
                  data.rowNames[points.getRawDataItem(i)[points.mapDimension('y')]] :
                  series.name;
                var values = sums.get(name) || new Map();
                values.set(x, (values.get(x) || 0) + value);
                sums.set(name, values);
                columns.set(name, y);
              }
            });
            sums.forEach(function(values, name) {
              parts.push(name + ': ' + Array.from(values, function(entry) {
                return entry[0] + ': ' + format(entry[1], columns.get(name));
              }).join('; '));
            });
          }
          var label = parts.filter(Boolean).join('. ').replace(/\\n/g, ' ') + '.';
          // The chart is also redrawn on hover
          if (label !== el.getAttribute('aria-label')) el.setAttribute('aria-label', label);
        });
      }
    ", data = list(type = type, unit = unit_suffix, digits = fraction_digits,
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
      htmlwidgets::onRender("
        function(el, x, data) {
          var chart = echarts.getInstanceByDom(el);
          var choice = document.getElementById(data.id);
          var main = data.mainId && document.getElementById(data.mainId);
          var inputs = Array.from(choice.querySelectorAll('input'));
          // The column chosen with the buttons, kept while it isn't offered
          var chosen = choice.querySelector('input:checked').value;

          function update() {
            var whole = !!main && (!main.value || main.value === 'Összesen');
            var offered = inputs.filter(function(input) {
              var shown = !whole || data.parentColumns.indexOf(input.value) < 0;
              input.hidden = input.labels[0].hidden = !shown;
              return shown;
            });
            var plotted = offered.find(function(input) {
              return input.value === chosen;
            }) || offered[0];
            plotted.checked = true;
            // The div around the buttons, so that they leave no gap in the row
            choice.parentElement.hidden = offered.length < 2;
            var column = plotted.value;
            var options = { yAxis: { name: data.axisNames[column] } };
            if (data.heatmap) {
              options.series = [{ id: 'heatmap', encode: { value: column } }];
              options.visualMap = { dimension: column };
            } else {
              options.series = chart.getOption().series.map(function(s) {
                // In stack mode, the series of negative parts plot the column
                // with _neg appended
                var negative = /_neg$/.test([].concat(s.encode.y)[0]);
                return { encode: { y: negative ? column + '_neg' : column } };
              });
            }
            chart.setOption(options);
            // For a heatmap, which colours its cells by the column (see
            // add_category_heatmap())
            el.dispatchEvent(new CustomEvent('column', { detail: column }));
          }

          choice.addEventListener('change', function() {
            chosen = choice.querySelector('input:checked').value;
            update();
          });
          if (main) main.addEventListener('change', update);
          update();
        }
      ", data = c(list(id = ids$var, axisNames = as.list(setNames(axis_name, var)),
                       parentColumns = as.list(unname(parent_var)), heatmap = heatmap),
                  if (parts) list(mainId = ids$main)))
  }
  chart
}

# Showing parts (`paths`, see category_data()), a category_chart() (see there)
# names its lines or stacked areas after their categories, colours them, and shows
# only those that the filters of the crosstalk group `group` leave (see
# add_part_legend()), and only the years from their first to their last. With
# `unit = "%"`, the stacked areas are kept within 0 and 100% (see below)
add_part_lines <- function(chart, paths, group, stack = FALSE, unit = "") {
  if (!is.null(paths)) {
    if (stack && unit == "%") {
      # The positive parts of a whole stack up to 100%, but rounding errors can put
      # the top a hair above it, and negative parts too small to see push it up as
      # much as they go below 0. ECharts would then run the axis to 120, or down to
      # -20, so an overshoot of less than 0.5% of the axis is cut off, above 100 and
      # below 0. (When the top stays at or below 100, so does ECharts' own maximum)
      chart <- e_y_axis(
        chart,
        max = htmlwidgets::JS("
          function(value) {
            var range = value.max - Math.min(value.min, 0);
            return value.max > 100 && value.max - 100 < 0.005 * range ? 100 : null;
          }
        "),
        min = htmlwidgets::JS("
          function(value) {
            return value.min < 0 && -value.min < 0.005 * (value.max - value.min) ? 0 : null;
          }
        ")
      )
    }
    
    chart <- chart |>
      add_part_legend(paths, group) |>
      htmlwidgets::onRender("
        function(el, x, data) {
          var chart = echarts.getInstanceByDom(el);
          var option = chart.getOption(), years = option.xAxis[0].data;

          // Only the years from the first to the last that the filters leave (from
          // the start, and whenever the filters change)
          kiadas.onFilter(data.group, function(value) {
            var keys = value && new Set(value.map(String)), hasYear = {};
            option.dataset[0].source.forEach(function(row) {
              if (!keys || keys.has(String(row.XkeyX))) hasYear[row.yearchr] = true;
            });
            var covered = years.filter(function(year) { return hasYear[year]; });
            chart.setOption({
              xAxis: {
                data: years.slice(years.indexOf(covered[0]),
                                  years.indexOf(covered[covered.length - 1]) + 1)
              }
            });
          });

          // The formatter of e_tooltip() only gets the hovered category: in stack
          // mode, its negative part in a year when it's negative, else its
          // positive part
          var format = option.tooltip[0].formatter, hovered = null;
          function column(p) { return p.dimensionNames[p.encode.y[0]]; }
          chart.setOption({
            tooltip: {
              formatter: function(params) {
                params = params.filter(function(p) { return p.seriesName === hovered; });
                var shown = params.filter(function(p) { return p.value[column(p)] < 0; });
                if (!shown.length) {
                  shown = params.filter(function(p) { return !/_neg$/.test(column(p)); });
                }
                return shown.length ? format(shown) : '';
              }
            }
          });

          chart.on('mouseover', 'series', function(p) {
            hovered = p.seriesName;
            // ECharts only emphasizes the hovered series, but in stack mode a
            // category has a series for its negative and one for its positive
            // parts, so this emphasizes both, as hovering its name in the legend
            // does
            chart.dispatchAction({ type: 'highlight', seriesName: p.seriesName });
            // The tooltip was already drawn for this mouse move, so redraw it
            chart.dispatchAction({
              type: 'showTip', x: p.event.offsetX, y: p.event.offsetY
            });
          });

          chart.on('mouseout', 'series', function(p) {
            hovered = null;
            chart.dispatchAction({ type: 'downplay', seriesName: p.seriesName });
          });
        }
      ", data = list(group = group))
  }
  chart
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

# Names the series of a chart of parts (`paths`: see category_data(), or the points
# of a plan_actual_chart(), see plan_actual_data()), one for each part in each
# classification it's in, after their categories, and colours them. As the chart
# has a series for every part of every whole, the legend only lists the series
# that the filters of the crosstalk group `group` leave, coloured as if they were
# the only ones (from the start, and whenever the filters change). Series added
# after this (as the line of a plan_actual_chart()) are left out of the legend, and
# keep their own colours
add_part_legend <- function(chart, paths, group) {
  # echarts4r names each series after its group, but it should show the label
  # of its category (which has a series in each classification it's in)
  ids <- vapply(chart$x$opts$series, \(s) s$name, "")
  labels <- as.character(paths$nameLabel[match(ids, paths$series)])
  chart$x$opts$series <- Map(\(s, label) modifyList(s, list(name = label)),
                             chart$x$opts$series, labels)
  chart$x$opts$legend$data <- as.list(unique(labels))

  chart |>
    e_color(chart_palette) |>
    htmlwidgets::onRender("
      function(el, x, data) {
        var chart = echarts.getInstanceByDom(el);
        var option = chart.getOption(), palette = option.color;
        var names = option.series.map(function(s) { return s.name; });

        kiadas.onFilter(data.group, function(value) {
          var keys = value && new Set(value.map(String)), shown = {};
          option.dataset[0].source.forEach(function(row) {
            if (!keys || keys.has(String(row.XkeyX))) shown[row.series] = true;
          });
          // Each name once, as in stack mode a category has a series for its
          // negative and one for its positive parts
          var legend = [];
          names.forEach(function(name, i) {
            if (shown[data.series[i]] && legend.indexOf(name) < 0) legend.push(name);
          });
          var color = {};
          legend.forEach(function(name, i) {
            color[name] = palette[i % palette.length];
          });
          chart.setOption({
            // Stacked, in the order of the areas from the top of the stack down
            // (though coloured from the bottom up)
            legend: { data: option.series[0].stack ? legend.slice().reverse() : legend },
            // Also the series of the same category in the other classification,
            // as the legend takes the colour of the first series of a name
            series: names.map(function(name) {
              return color[name] ? { color: color[name] } : {};
            })
          });
        });
      }
    ", data = list(group = group, series = as.list(ids)))
}

# In relative mode (with a `base_year`), a category_chart() (see there) divides
# the values of the columns `var` by those in the base year, chosen with its slider
# (see category_ids() for `ids`), at first `base_year`
add_base_year_slider <- function(chart, ids, var, base_year = NULL) {
  if (!is.null(base_year)) {
    # Plots the values of each line divided by its value in the base year (none if
    # that isn't positive), chosen with the slider among the years that the parts
    # shown have data for. When the filters change, the slider keeps its year if
    # they have data for it, else it moves to the nearest year they do. The chosen
    # year is also kept in the data-year attribute of the slider, for the title,
    # which is set after this so that it reads the year already updated, and the
    # chart's element gets a "baseyear" event (with the year as its detail) whenever
    # it changes
    chart <- chart |>
      htmlwidgets::onRender("
        function(el, x, data) {
          var chart = echarts.getInstanceByDom(el);
          var option = chart.getOption();
          // The values from RawData, which those plotted are computed from
          var rows = option.dataset[0].source, years = option.xAxis[0].data;
          var slider = document.getElementById(data.id);
          var output = document.getElementById(data.id + '_value');
          // The years offered, the chosen one, and the one the values are divided by
          var offered = years, year = data.year, plotted = null;

          slider.addEventListener('input', function() {
            year = offered[slider.value];
            output.textContent = year;
            slider.setAttribute('aria-valuetext', year);
            slider.dataset.year = year;
            if (year === plotted) return;
            plotted = year;
            var base = {};
            rows.forEach(function(row) {
              if (row.yearchr === year) base[row.line] = row;
            });
            chart.setOption({
              dataset: {
                id: 'source',
                source: rows.map(function(row) {
                  var ratios = {}, b = base[row.line];
                  data.columns.forEach(function(column) {
                    ratios[column] = b && b[column] > 0 && row[column] != null ?
                      row[column] / b[column] : null;
                  });
                  return Object.assign({}, row, ratios);
                })
              }
            });
            el.dispatchEvent(new CustomEvent('baseyear', { detail: year }));
          });

          // Offers `years`, at the chosen year or, if it isn't among them, at the
          // nearest one
          function offer(years) {
            offered = years;
            var at = 0;
            years.forEach(function(y, i) {
              if (Math.abs(y - year) < Math.abs(years[at] - year)) at = i;
            });
            slider.max = years.length - 1;
            slider.value = at;
            slider.dispatchEvent(new Event('input'));
          }

          // The years that the filters leave (from the start, and whenever they
          // change). If they leave none, the slider keeps the years it offers or, at
          // the start, offers all
          kiadas.onFilter(data.group, function(value) {
            var keys = value && new Set(value.map(String)), hasYear = {};
            rows.forEach(function(row) {
              if (!keys || keys.has(String(row.XkeyX))) hasYear[row.yearchr] = true;
            });
            var covered = years.filter(function(y) { return hasYear[y]; });
            if (covered.length) offer(covered);
            else if (!slider.dataset.year) offer(years);
          });
        }
      ", data = list(id = ids$base, group = ids$group, columns = as.list(unname(var)),
                     year = as.character(base_year)))
  }
  chart
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
# of the colouring of a plan_actual_chart()). Unless `year_label` is NULL, the title
# ends with the year chosen with the slider `ids$base` (kept in its data-year
# attribute), after `year_label`: in relative mode by default, the base year, after
# "bázisév: ". Without a crosstalk group (`ids$group` NULL, as in a country_chart()),
# the chart doesn't follow the filters. A chart with several legends (as a
# country_stack_chart(), with one of the parts and one of the countries) has them
# one above the other at the bottom, the first one at the top
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
  # (see legendLayout() below)
  chart |>
    htmlwidgets::onRender("
      function(el, x, data) {
        var chart = echarts.getInstanceByDom(el);
        var main = document.getElementById(data.mainId);
        // The subgroup dropdown or, showing parts, the radio buttons of the type, if
        // the chart has a second filter
        var second = data.secondId && document.getElementById(data.secondId);
        // The slider of the year that the title ends with, if any (in relative mode,
        // of the base year)
        var base = data.baseId && document.getElementById(data.baseId);
        var download = document.getElementById(data.downloadId);
        // The height of the chart, and the room above and below the plot without
        // the title and with a legend of one line, and how far from the bottom of
        // the chart ECharts keeps the labels of the axes (see update())
        var option = chart.getOption(), height = el.clientHeight;
        var gridTop = option.grid[0].top, gridBottom = option.grid[0].bottom;
        var outerBottom = (option.grid[0].outerBounds || {}).bottom || 0;
        // Where each legend is at first: how far its bottom is from that of the chart
        var legendBottoms = (option.legend || []).map(function(legend) {
          return legend.bottom;
        });
        var context = document.createElement('canvas').getContext('2d');

        // ECharts could wrap the title too, but wouldn't tell into how many
        // lines, which decides how much room the title needs
        function wrap(text, width) {
          return text.split(' ').reduce(function(lines, word) {
            var last = lines[lines.length - 1];
            if (last && context.measureText(last + ' ' + word).width <= width) {
              lines[lines.length - 1] = last + ' ' + word;
            } else {
              lines.push(word);
            }
            return lines;
          }, []);
        }

        // The items of the legend `model`
        function itemsOf(model) {
          var view = chart.getViewOfComponentModel(model);
          return view && view.getContentGroup();
        }

        // The legends with items, in their order (a hidden legend has none, as has a
        // legend with nothing to list)
        function legends() {
          var shown = [];
          chart.getModel().eachComponent('legend', function(model) {
            var items = itemsOf(model);
            if (items && items.childCount()) shown.push(model);
          });
          return shown;
        }

        // The padding of the legend `model`, which is given as in CSS
        function paddingOf(model) {
          var padding = [].concat(model.get('padding'));
          var top = padding[0], right = padding.length > 1 ? padding[1] : top;
          return { top: top, right: right, bottom: padding.length > 2 ? padding[2] : top,
                   left: padding.length > 3 ? padding[3] : right };
        }

        // How much higher the legends are than a legend of one line, which the room
        // below the plot is for (`room`), and, if there are several, where each of
        // them goes: how far its bottom is from that of the chart (`bottoms`, by the
        // index of the legend). ECharts puts each at the bottom, so the last stays
        // where it is at first, and each other one goes right above the next one,
        // 6px from it. (The items of a legend are all of the same height)
        function legendLayout() {
          var shown = legends(), bottoms = {}, top = null;
          if (!shown.length) return { room: 0, bottoms: bottoms };
          for (var i = shown.length - 1; i >= 0; i--) {
            var model = shown[i], padding = paddingOf(model);
            var bottom = top === null ? legendBottoms[model.componentIndex] : top + 6;
            bottoms[model.componentIndex] = bottom;
            top = bottom + padding.bottom + itemsOf(model).getBoundingRect().height +
              padding.top;
          }
          // The top of the first legend's items, and where they'd end in one line
          var first = shown[0], padding = paddingOf(first);
          var oneLine = legendBottoms[first.componentIndex] + padding.bottom +
            itemsOf(first).childAt(0).getBoundingRect().height;
          return { room: Math.round(top - padding.top - oneLine), bottoms: bottoms };
        }

        // In a heatmap, how much taller its rows are, data.rowHeight px each on
        // average, than the plot was at first. The plot is at least
        // data.minPlotHeight tall, though. (The legend names each row or, if the y
        // axis is of categories, as in a country_heatmap(), the axis does)
        function rowsExtra() {
          var model = chart.getModel(), axis = model.getComponent('yAxis');
          var names = axis.get('type') === 'category' ? axis.get('data') :
            model.getComponent('legend').get('data');
          return Math.max(names.length * data.rowHeight, data.minPlotHeight) -
            (height - gridTop - gridBottom);
        }

        // Sets the options `options` of the legend with the index `index` (leaving the
        // others as they are)
        function setLegend(index, options) {
          var legend = [];
          for (var i = 0; i < index; i++) legend.push({});
          legend.push(options);
          chart.setOption({ legend: legend });
        }

        // Cuts the names in each legend short if one of them is too long for a line
        // (see above). Only redone when the names or the width of the chart change
        var fitted = null, cut = {};
        function fitLegend() {
          var shown = legends();
          var key = JSON.stringify([chart.getWidth(), shown.map(function(model) {
            return model.get('data');
          })]);
          if (key === fitted) return;
          fitted = key;
          shown.forEach(function(model) {
            var index = model.componentIndex;
            // Whole names first, to see if they fit
            if (cut[index]) {
              setLegend(index, {
                textStyle: { width: null, overflow: 'none' }, tooltip: { show: false }
              });
              cut[index] = false;
            }
            var padding = paddingOf(model);
            var room = chart.getWidth() - padding.right - padding.left;
            if (itemsOf(model).getBoundingRect().width > room) {
              setLegend(index, {
                // Beside the icon and the 5px gap after it
                textStyle: { width: room - model.get('itemWidth') - 5, overflow: 'truncate' },
                tooltip: { show: true }
              });
              cut[index] = true;
            }
          });
        }

        function update() {
          // ECharts lays out the legend for the width it has, which htmlwidgets
          // may not have updated yet when the window was resized
          if (chart.getWidth() !== el.clientWidth) {
            chart.resize({ width: el.clientWidth, height: el.clientHeight });
          }
          fitLegend();
          var layout = legendLayout(), legendRoom = layout.room, titleRoom = 0;
          var rowsRoom = data.rowHeight ? rowsExtra() : 0;
          // The plot is moved up by the room for the legends, and so are its outer
          // bounds, within which ECharts keeps the labels of the axes by shrinking
          // the plot, so that labels that reach below the room for them (as the
          // vertical names of the countries of a country_stack_chart()) stay clear
          // of the legends
          var options = { grid: { bottom: gridBottom + legendRoom,
                                  outerBounds: { bottom: outerBottom + legendRoom } } };
          if (Object.keys(layout.bottoms).length > 1) {
            options.legend = legendBottoms.map(function(_, i) {
              return i in layout.bottoms ? { bottom: layout.bottoms[i] } : {};
            });
          }
          // For the choices of the two filters, or of the main group dropdown alone
          // (empty if the title then has no end)
          var end = data.title && (second ?
            (data.ends[main.value] || {})[second.tagName === 'SELECT' ?
              second.value : second.querySelector('input:checked').value] :
            data.ends[main.value]);
          // Unless the chart has no title, or the dropdowns aren't filled yet
          if (end != null) {
            var text = data.title + (end ? ': ' + end : '');
            if (base) text += ', ' + data.yearLabel + base.dataset.year;
            // Smaller on phones, where the title takes several lines anyway
            var fontSize = el.clientWidth < 500 ? 14 : 18, lineHeight = fontSize + 6;
            context.font = 'bold ' + fontSize + 'px ' + option.textStyle.fontFamily;
            // Clear of the download button in the top right corner, by as much as
            // the title is from the left edge
            var right = download.getBoundingClientRect().left - el.getBoundingClientRect().left;
            var lines = wrap(text, right - 20);
            titleRoom = lines.length * lineHeight;
            options.title = {
              id: 'category', text: lines.join('\\n'), left: 10, top: 5,
              textStyle: { fontSize: fontSize, fontWeight: 'bold', lineHeight: lineHeight }
            };
            options.grid.top = gridTop + titleRoom;
          }
          // The colour scale of a heatmap starts at the top of the plot
          if (data.rowHeight) {
            options.graphic = { elements: [{ id: 'colorScale', top: gridTop + titleRoom }] };
          }
          // Drawn by resize() below, or else in the next frame, so that the chart is
          // only redrawn once
          chart.setOption(options, { lazyUpdate: true });
          // The chart grows by the room for the two, so that the plot keeps its
          // size (in a heatmap, the size its rows need). (The size is given
          // explicitly, as ECharts would otherwise keep using the one htmlwidgets
          // last gave it when the window was resized.) Only resized if its size
          // changed, as ECharts then forgets what the mouse is over, and wouldn't end
          // the highlighting that hovering it started when the mouse leaves it
          el.style.height = height + titleRoom + legendRoom + rowsRoom + 'px';
          if (chart.getWidth() !== el.clientWidth || chart.getHeight() !== el.clientHeight) {
            chart.resize({ width: el.clientWidth, height: el.clientHeight });
            // ECharts then lays out the legend anew, for the new height (a vertical
            // legend wraps into more columns when the chart is short), so if that
            // changed its height, the chart is laid out once more (but not endlessly)
            if (legendLayout().room !== legendRoom && passes < 2) {
              passes++;
              update();
            }
          }
          passes = 0;
        }
        var passes = 0;

        // Whenever the choices, the filters or the size of the window change, once
        // in the next frame, however many of these happen before it (one choice
        // changes the filters that depend on it one after the other), so that it
        // reads the legend once the filters have changed which items it has
        var scheduled = false;
        function schedule() {
          if (scheduled) return;
          scheduled = true;
          requestAnimationFrame(function() {
            scheduled = false;
            update();
          });
        }
        main.addEventListener('change', schedule);
        if (second) second.addEventListener('change', schedule);
        if (base) base.addEventListener('input', schedule);
        data.watchIds.forEach(function(id) {
          document.getElementById(id).addEventListener('change', schedule);
        });
        if (data.group) new crosstalk.FilterHandle(data.group).on('change', schedule);
        window.addEventListener('resize', schedule);
        // And at once, so that the chart is laid out before it's first shown
        update();
      }
    ", data = list(
      title = title, mainId = ids$main,
      secondId = if (second) { if (parts) ids$type else ids$sub },
      baseId = if (!is.null(year_label)) ids$base, yearLabel = year_label,
      downloadId = ids$download, group = ids$group, rowHeight = row_height,
      minPlotHeight = min_plot_height, watchIds = as.list(watch_ids),
      ends = if (!is.null(title)) {
        if (second) {
          lapply(split(setNames(ends$end, ends$second), ends$main), as.list)
        } else as.list(setNames(ends$end, ends$main))
      }
    ))
}

# echarts4r draws a chart of crosstalk data unfiltered at first, as it only applies
# the filters of its crosstalk group when they change. So this makes a
# category_chart() (see there) with the crosstalk group `group` start from them
# instead: they're set up before the chart is rendered (see filter_radio() and
# defer_chart()), and htmlwidgets evaluates the JS() values just before rendering
# it. (A dataset of grouped data picks its group before it filters by key)
start_filtered <- function(chart, group) {
  reg <- htmlwidgets::JS(sprintf("kiadas.keyRegex(%s)",
                                 jsonlite::toJSON(group, auto_unbox = TRUE)))
  chart$x$opts$dataset <- lapply(chart$x$opts$dataset, \(d) {
    if (isTRUE(startsWith(d$id, "Xtalk"))) {
      if (is.null(names(d$transform))) {
        d$transform[[2]]$config$reg <- reg
      } else {
        d$transform$config$reg <- reg
      }
    }
    d
  })
  chart
}

# Renders the chart with the id `id` (a category_chart()) only once it comes within a
# screen's height of the viewport, or before printing (see kiadas.defer() in
# kiadas_dependency), so that loading the page only draws the charts in view. Comes
# after the chart and its download menu, which the chart's code uses
defer_chart <- function(id) {
  htmltools::tags$script(htmltools::HTML(
    sprintf("kiadas.defer(%s);", jsonlite::toJSON(id, auto_unbox = TRUE))
  ))
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
# As in category_chart(), each call has its own crosstalk group and element ids
# (made from `id`), and the chart is only drawn once it comes near the viewport,
# starting from the choices of its filters. The helpers below and those of
# category_chart() make its parts
plan_actual_chart <- function(axis_name, unit, title = NULL, var = "value",
                              log_scale = TRUE, id = paste0(var, "_plan_actual")) {
  ids <- c(category_ids(id), scale = paste0(id, "_scale_filter"),
           color = paste0(id, "_color_choice"))
  # The choices of the radio buttons of the axes
  scales <- c(linear = "Lineáris", log = "Logaritmikus")
  data <- plan_actual_data(var, ids$group, scales)
  unit_suffix <- unit_suffix_for(unit)
  # The columns that the chart shows, by category
  shared_data <- crosstalk::SharedData$new(
    data$points[, .(series, yearchr, plan, actual)] |> dplyr::group_by(series),
    key = as.character(data$points$key), group = ids$group
  )

  chart <- shared_data |>
    # hoverLayerThreshold as in category_chart(). Not reordered by the x values (the
    # plans), so that the downloaded data and the description list the points of
    # each category by year
    e_chart(plan, hoverLayerThreshold = 1e6, elementId = ids$chart, reorder = FALSE) |>
    e_scatter_("actual", symbol_size = 8, emphasis = list(focus = "series")) |>
    # Before the line is added (see add_plan_actual_axes()), which isn't a category
    add_part_legend(data$points, ids$group) |>
    add_chart_frame(x_name = axis_name[[1]]) |>
    add_plan_actual_axes(axis_name[[2]], plan_actual_labels, unit_suffix, ids,
                         scales[["log"]], log_scale) |>
    add_plan_actual_colors(ids, sort(unique(data$points$yearchr))) |>
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
    add_chart_layout(title, ids, data$choices, second = FALSE, watch_ids = ids$color) |>
    start_filtered(ids$group)

  htmltools::attachDependencies(
    htmltools::tagList(
      plan_actual_filters(
        ids, data$shared_points,
        filter_radio(id = ids$scale, label = "Tengelyek:", sharedData = data$shared_scales,
                     group = ~scale, selected = scales[[if (log_scale) "log" else "linear"]],
                     compact = TRUE),
        # Not a filter: it only changes the colours (see add_plan_actual_colors())
        radio_buttons(id = ids$color, label = "Színezés:",
                      choices = c("Kategória" = "category", "Év" = "year"), compact = TRUE)
      ),
      add_download_menu(
        chart, unit, id = ids$download, choice_id = ids$scale,
        point_columns = setNames(
          c("yearchr", "plan", "actual"),
          c("Év", paste0(plan_actual_labels, if (unit != "") paste0(" [", unit, "]")))
        )
      ),
      defer_chart(ids$chart)
    ),
    kiadas_dependency
  )
}

# The data of a plan_actual_chart() or a plan_actual_heatmap() (see there) of
# the column `var` of HURawData, rounded (see round_for_chart()), in the crosstalk
# group `group`: `points`, a row for each Hungarian category (its `code` and
# `nameLabel`) in each year (`yearchr`) with both a plan (`plan`) and an actual
# value (`actual`) or, with `complete = FALSE`, in each year from the first to the
# last with both for any category (with NA where the category doesn't have both),
# with its main group (`main`, as in category_data(): "Összesen" for the main
# groups), and a series for each category, in the order of the codes, named by its
# number, as in category_data(); `choices`, a row for each choice of the main group
# dropdown (`main`) and each row of `points` that it shows (`key`); and in the
# group, `shared_points` of the choices for the dropdown, which picks the rows of
# `points` by their keys, their row numbers. With `scales`, also `shared_scales`
# for the radio buttons of the axes of a plan_actual_chart(), which offer its
# values: its "linear" one for all the points, its "log" one for those with a
# positive plan and actual value, which log axes can show
plan_actual_data <- function(var, group, scales = NULL, complete = TRUE) {
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
  points <- if (complete) both else {
    years <- range(as.integer(both$yearchr))
    years <- as.character(seq(years[1], years[2]))
    points[unique(both[, .(main, nameLabel, code)])[, .(yearchr = years),
                                                     by = .(main, nameLabel, code)],
           on = .(main, nameLabel, code, yearchr)]
  }
  setorder(points, code, yearchr)
  points[, `:=`(
    key = .I,
    series = factor(code, levels = unique(code), labels = seq_along(unique(code)))
  )]

  # "Összesen" shows the main groups, and a main group its subgroups or, if it has
  # none, itself, whose points thus come under two choices. "Összesen" first in
  # the dropdown, then the main groups in the order of their codes
  parents <- unique(points$main)
  choices <- rbind(
    points[, .(main, code, key)],
    points[main == "Összesen" & !as.character(nameLabel) %chin% parents,
           .(main = as.character(nameLabel), code, key)]
  )[order(main != "Összesen", code), .(main, key)]
  list(
    points = points,
    choices = choices,
    shared_points = crosstalk::SharedData$new(choices, key = ~key, group = group),
    shared_scales = if (!is.null(scales)) crosstalk::SharedData$new(
      # Offered in the order of `scales`
      rbind(
        points[, .(scale = scales[["linear"]], key)],
        points[plan > 0 & actual > 0, .(scale = scales[["log"]], key)]
      )[, scale := factor(scale, levels = unname(scales))],
      key = ~key, group = group
    )
  )
}

# The filters of a plan_actual_chart() or a plan_actual_heatmap() (see there),
# with the ids `ids` (see there), laid out as in category_filters(): the main group
# dropdown, on the rows of `shared_points` (see plan_actual_data()), at "Összesen"
# at first, followed by the radio buttons in `...` (leaving out NULLs)
plan_actual_filters <- function(ids, shared_points, ...) {
  htmltools::div(
    class = "d-flex flex-wrap align-items-end row-gap-2 mb-3",
    # Not Bootstrap's column-gap-3, as in category_filters()
    style = "column-gap: 1rem",
    htmltools::div(
      style = "flex: 1 1 20rem",
      filter_select_dependent(
        id = ids$main,
        label = "Főcsoport:",
        sharedData = shared_points,
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
# to corner: from the smallest to the largest value of the points that the filters
# leave (from the start, and whenever they or the type of the axes change), on log
# axes widened to 1, 2 or 5 times a power of 10, on linear ones from 0 (unless a
# value is negative) to the first tick at or above the largest value, with the
# ticks spaced as ECharts would space them. The tooltip shows the hovered point's
# category and year, its plan and actual value (named by `type_labels`, followed by
# `unit_suffix`), and its deviation from the plan (see kiadas.planActualTooltip())
add_plan_actual_axes <- function(chart, y_name, type_labels, unit_suffix, ids,
                                 log_value, log_scale = TRUE) {
  axis <- list(
    type = if (log_scale) "log" else "value",
    axisLabel = list(
      showMinLabel = TRUE, showMaxLabel = TRUE, hideOverlap = TRUE,
      # As in add_category_lines()
      formatter = htmlwidgets::JS("
        function(value) {
          return value.toLocaleString('hu-HU', { maximumFractionDigits: 20 });
        }
      ")
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
  # (see below)
  chart$x$opts$series <- c(chart$x$opts$series, list(list(
    id = "diagonal", type = "line", data = list(), symbol = "none", silent = TRUE,
    z = 1, lineStyle = list(type = "dashed", color = "#888", width = 1)
  )))

  chart |>
    # Kept within the chart, with its lines wrapped, as the tooltip of the lines
    e_tooltip(trigger = "item", confine = TRUE, extraCssText = "white-space: normal;") |>
    # Room for the name of the x axis, between its labels and the legend
    e_grid(bottom = 100) |>
    htmlwidgets::onRender("
      function(el, x, data) {
        var chart = echarts.getInstanceByDom(el);
        var source = chart.getOption().dataset[0].source;

        // The largest of 1, 2 and 5 times a power of 10 that is at most `value`
        // (with `up`, the smallest that is at least `value`), for a positive `value`:
        // the ends of a log axis
        function round(value, up) {
          var power = Math.pow(10, Math.floor(Math.log10(value)));
          var steps = [1, 2, 5, 10].map(function(step) { return step * power; });
          return up ? steps.find(function(s) { return s >= value; }) :
            steps.filter(function(s) { return s <= value; }).pop();
        }

        // The step between the ticks of a linear axis over `span`, as ECharts would
        // choose it: about a fifth of the span, rounded to 1, 2, 3 or 5 times a power
        // of 10
        function step(span) {
          var exponent = Math.floor(Math.log10(span / 5));
          var f = span / 5 / Math.pow(10, exponent);
          return multiple(f < 1.5 ? 1 : f < 2.5 ? 2 : f < 4 ? 3 : f < 7 ? 5 : 10,
                          Math.pow(10, exponent));
        }

        // `n` times the step `s` (whose first digit is its only one), without the
        // error of floating-point arithmetic (as in 3 * 0.1)
        function multiple(n, s) {
          return +(n * s).toFixed(Math.max(0, -Math.floor(Math.log10(s))));
        }

        // The radio buttons of the axes, and the keys of the points that the
        // filters leave (null if there's no filter)
        var scale = document.getElementById(data.scaleId), keys = null;
        // The options of the axes other than their type and range, as set at first:
        // ECharts replaces an axis whose type changes, dropping the rest
        var axes = ['xAxis', 'yAxis'].map(function(name) {
          var axis = chart.getOption()[name][0];
          return { name: axis.name, nameLocation: axis.nameLocation,
                   nameGap: axis.nameGap, axisLabel: axis.axisLabel };
        });
        function update() {
          var log = scale.querySelector('input:checked').value === data.logValue;
          var lo = Infinity, hi = -Infinity;
          source.forEach(function(row) {
            if (keys && !keys.has(String(row.XkeyX))) return;
            // Log axes only show positive values (which the radio buttons also
            // filter by)
            if (log && !(row.plan > 0 && row.actual > 0)) return;
            lo = Math.min(lo, row.plan, row.actual);
            hi = Math.max(hi, row.plan, row.actual);
          });
          // Unless the filters leave no points
          if (lo > hi) return;
          var min, max, interval = null;
          if (log) {
            min = round(lo, false);
            max = round(hi, true);
            // If all the values are the same 1, 2 or 5 times a power of 10
            if (max <= min) max = 10 * min;
          } else {
            // With the ticks spaced explicitly, so that both ends are ticks
            interval = step(Math.max(hi, 0) - Math.min(lo, 0) || 1);
            min = lo < 0 ? multiple(Math.floor(lo / interval), interval) : 0;
            max = multiple(Math.ceil(hi / interval), interval);
            // If all the values are 0
            if (max <= min) max = multiple(Math.round(min / interval) + 1, interval);
          }
          var range = { type: log ? 'log' : 'value', min: min, max: max, interval: interval };
          chart.setOption({
            xAxis: Object.assign({}, axes[0], range),
            yAxis: Object.assign({}, axes[1], range),
            series: [{ id: 'diagonal', data: [[min, min], [max, max]] }]
          });
        }
        kiadas.onFilter(data.group, function(value) {
          keys = value && new Set(value.map(String));
          update();
        });
        // The radio buttons change the filters too, which updates the chart, but
        // they change the type of the axes even if the points stay the same
        scale.addEventListener('change', update);

        chart.setOption({
          tooltip: {
            formatter: function(p) {
              // With crosstalk, p.value is the whole data row as an object. With at
              // most one decimal, as in the description for screen readers
              function value(dim) { return p.value[p.dimensionNames[p.encode[dim][0]]]; }
              return kiadas.planActualTooltip(p.marker, p.seriesName, p.value.yearchr,
                                              value('x'), value('y'), data.labels,
                                              data.unit, 1).join('<br>');
            }
          }
        });
      }
    ", data = list(group = ids$group, scaleId = ids$scale, logValue = log_value,
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
# description and the data files too, but the axes stay as they are. Categories
# and years switched off stay off while the colouring changes, so that, e.g., the
# points of a few categories can be coloured by year
add_plan_actual_colors <- function(chart, ids, years) {
  colors <- setNames(
    grDevices::colorRampPalette(grDevices::hcl.colors(9, "viridis")[1:8])(length(years)),
    years
  )
  # Series without data, one for each year, which give the legend its years, as
  # in add_category_heatmap(). Highlighting one fades all the points (see below)
  chart$x$opts$series <- c(chart$x$opts$series, lapply(years, \(year) {
    list(name = year, type = "scatter", data = list(), color = colors[[year]],
         emphasis = list(focus = "self"))
  }))
  # The datasets of the series of the categories filter their points by category
  # and by key (see start_filtered()), and then by year, by the years switched on
  # in the legend (at first all: see below). Not with a crosstalk filter, which
  # would also change the range of the axes (see add_plan_actual_axes())
  chart$x$opts$dataset <- lapply(chart$x$opts$dataset, \(d) {
    if (isTRUE(startsWith(d$id, "Xtalk_"))) {
      d$transform <- c(d$transform, list(list(
        type = "filter", config = list(dimension = "yearchr", reg = "^")
      )))
    }
    d
  })
  # ECharts animates a change of the points by pairing those before and after it,
  # by default by their order, so that when some leave (as when a year is switched
  # off), the others would slide into their places. So it pairs them by their keys
  chart$x$opts$series <- lapply(chart$x$opts$series, \(s) {
    if (!is.null(s$datasetId)) s$encode$itemId <- "XkeyX"
    s
  })

  htmlwidgets::onRender(chart, "
    function(el, x, data) {
      var chart = echarts.getInstanceByDom(el);
      var option = chart.getOption(), source = option.dataset[0].source;
      var choice = document.getElementById(data.colorId);
      // The keys of the points that the filters leave (null if there's no filter),
      // the categories that add_part_legend() lists for them, the years switched
      // off in the legend, and the year highlighted (see highlightYear()), if any
      var keys = null, legend = [], hidden = new Set(), highlighted = null;

      function byYear() { return choice.querySelector('input:checked').value === 'year'; }

      // The colour of a point: that of its category's series, or that of its year.
      // (With crosstalk, p.value is the whole data row as an object)
      function color(p) { return byYear() ? data.colors[p.value.yearchr] : p.color; }

      function update() {
        var year = byYear(), shown = {};
        source.forEach(function(row) {
          if (!keys || keys.has(String(row.XkeyX))) shown[row.yearchr] = true;
        });
        chart.setOption({
          legend: {
            data: year ? Object.keys(data.colors).filter(function(y) { return shown[y]; }) :
              legend
          },
          // The series of the categories (the others have no data from the dataset),
          // which only highlight themselves on hover when coloured by category
          // (when coloured by year, the year is highlighted instead: see below)
          series: option.series.map(function(s) {
            if (s.datasetId) {
              return { itemStyle: { color: color }, emphasis: { focus: year ? 'none' : 'series' } };
            }
            // The series of a year switched off isn't highlighted when the legend is
            // hovered, as that would fade all the points
            return data.colors[s.name] ? { emphasis: { disabled: hidden.has(s.name) } } : {};
          })
        });
      }

      kiadas.onFilter(data.group, function(value) {
        keys = value && new Set(value.map(String));
        // As add_part_legend() has just listed them, before this
        legend = chart.getOption().legend[0].data;
        update();
      });
      choice.addEventListener('change', update);

      // Highlights the points of `year` and fades the others (or with `type`
      // 'downplay', ends that): highlighting the year's series fades all the points
      // (see above), so the points of the year are highlighted too, in the same
      // action, as each action first ends the fading of the one before. (Not by
      // recolouring the points, which would redraw the legend under the mouse)
      function highlightYear(type, year) {
        highlighted = type === 'highlight' ? year : null;
        var batch = [{ seriesName: year }];
        // The series shown (not those switched off in the legend)
        chart.getModel().eachSeries(function(series) {
          if (series.subType !== 'scatter') return;
          var points = series.getData(), indices = [];
          for (var i = 0; i < points.count(); i++) {
            if (points.getRawDataItem(i).yearchr === year) indices.push(i);
          }
          if (indices.length) {
            batch.push({ seriesIndex: series.seriesIndex, dataIndexInside: indices,
                         notBlur: true });
          }
        });
        chart.dispatchAction({ type: type, batch: batch });
      }

      // Hovering a year in the legend highlights its series, which has no points
      // (that action's event has no batch, unlike that of highlightYear()), unless
      // the year is switched off
      ['highlight', 'downplay'].forEach(function(type) {
        chart.on(type, function(e) {
          if (!e.batch && byYear() && data.colors[e.seriesName] && !hidden.has(e.seriesName)) {
            highlightYear(type, e.seriesName);
          }
        });
      });
      // And when coloured by year, so does hovering a point, of its year
      [['mouseover', 'highlight'], ['mouseout', 'downplay']].forEach(function(event) {
        chart.on(event[0], function(p) {
          if (byYear() && p.seriesType === 'scatter' && p.value && p.value.yearchr) {
            highlightYear(event[1], p.value.yearchr);
          }
        });
      });
      // And it ends when the mouse leaves the chart, in case ECharts lost track of
      // what the mouse was over (as when the chart is resized)
      chart.getZr().on('globalout', function() {
        if (highlighted) highlightYear('downplay', highlighted);
      });

      // Clicking a year in the legend switches its series off or on, which has no
      // points, so the points of the years switched off are filtered out by the
      // last filter of each dataset (see above)
      chart.on('legendselectchanged', function(e) {
        if (!data.colors[e.name]) return;
        // Ends the highlighting of the year clicked first, as ECharts can't end the
        // fading of the other points once they're filtered
        highlightYear('downplay', e.name);
        hidden = new Set(Object.keys(data.colors).filter(function(y) {
          return e.selected[y] === false;
        }));
        // Any value but those of the years switched off
        var reg = hidden.size ? '^(?!(' + Array.from(hidden).join('|') + ')$)' : '^';
        chart.setOption({ dataset: chart.getOption().dataset.map(function(d) {
          if (!d.transform || !d.transform[2]) return {};
          var transform = d.transform.slice();
          transform[2] = { type: 'filter', config: { dimension: 'yearchr', reg: reg } };
          return { transform: transform };
        }) });
        // For the series of the years switched off
        update();
      });
    }
  ", data = list(group = ids$group, colorId = ids$color, colors = as.list(colors)))
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
# "Összesen"). As in category_chart(), each call has its own crosstalk group and
# element ids (made from `id`), and the chart is only drawn once it comes near the
# viewport
plan_actual_heatmap <- function(axis_name, unit, title = NULL, var = "value",
                                metrics = c("Hányados" = "ratio",
                                            "Különbség" = "difference"),
                                metric_label = "Mérőszám:",
                                row_scale = c("sqrt", "linear"),
                                id = paste0(var, "_plan_actual_heatmap")) {
  row_scale <- match.arg(row_scale)
  ids <- category_ids(id)
  data <- plan_actual_data(var, ids$group, complete = FALSE)
  columns <- unname(metrics)
  # By column, in the description for screen readers: a ratio has no unit, and
  # needs two decimals, as in relative mode in category_chart()
  unit_suffix <- list(ratio = "", difference = unit_suffix_for(unit))
  fraction_digits <- list(ratio = 2L, difference = 1L)
  # The plan's share of all the planned spending of its year (in %), which weights
  # its row
  shares <- CategoryPaths[
    classification == "Magyar" & HURawData$type[key] == "original_plan",
    .(code = node, yearchr = HURawData$yearchr[key], spending = HURawData$prop[key])
  ]
  # A row for each category in each year, in the order of the years, as echarts4r
  # puts them on the x axis in the order they first appear in, rounded as in
  # round_for_chart()
  rows <- shares[data$points, on = .(code, yearchr)][, .(
    yearchr, code, key, plan, actual,
    ratio = signif(fifelse(plan > 0, actual / plan, NA_real_), 6),
    difference = round(actual - plan, 4),
    spending = signif(spending, 6)
  )]
  setorder(rows, yearchr)
  shared_data <- crosstalk::SharedData$new(
    # With the plan and the actual value, for the tooltip
    as.data.frame(rows[, c("yearchr", "code", columns, "plan", "actual", "spending"),
                       with = FALSE]),
    key = as.character(rows$key), group = ids$group
  )
  row_names <- heatmap_row_names(data$points)

  chart <- shared_data |>
    # As in category_chart()
    e_chart(yearchr, hoverLayerThreshold = 1e6, elementId = ids$chart) |>
    # Its tooltip gives the plan and the actual value, with at most one decimal, as
    # that of plan_actual_chart()
    add_category_heatmap(columns, axis_name, row_names, ids$group,
                         unit_suffix_for(unit), 1L, heatmap_sizes, row_scale,
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
                     min_plot_height = heatmap_sizes$plot) |>
    start_filtered(ids$group)

  htmltools::attachDependencies(
    htmltools::tagList(
      plan_actual_filters(
        ids, data$shared_points,
        if (length(metrics) > 1) {
          radio_buttons(id = ids$var, label = metric_label, choices = metrics,
                        compact = TRUE)
        }
      ),
      # Without a unit in the headers, as the data files start with the name of
      # the y axis, which gives the unit of the metric chosen
      add_download_menu(chart, "", id = ids$download,
                        choice_id = if (length(metrics) > 1) ids$var,
                        download_axis_name = TRUE, row_names = row_names),
      defer_chart(ids$chart)
    ),
    kiadas_dependency
  )
}

# The Hungarian names of the countries in RawData, by their codes (geo): the member
# states of the EU
country_names <- c(
  BE = "Belgium", BG = "Bulgária", CZ = "Csehország", DK = "Dánia",
  DE = "Németország", EE = "Észtország", IE = "Írország", EL = "Görögország",
  ES = "Spanyolország", FR = "Franciaország", HR = "Horvátország",
  IT = "Olaszország", CY = "Ciprus", LV = "Lettország", LT = "Litvánia",
  LU = "Luxemburg", HU = "Magyarország", MT = "Málta", NL = "Hollandia",
  AT = "Ausztria", PL = "Lengyelország", PT = "Portugália", RO = "Románia",
  SI = "Szlovénia", SK = "Szlovákia", FI = "Finnország", SE = "Svédország"
)
# And those of the groups of countries (`countries` in data_prepare.R), whose prop
# and propgdp are the averages of their members', weighted by their GDP. A
# country_chart() shows them among the countries, in grey
group_names <- c(EU27 = "EU27", V4 = "V4", EU15 = "EU15", EU11 = "EU11")

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
  # Named as category_ids() names them, as add_chart_layout() reads them. The
  # controls aren't crosstalk filters (see country_data()), so there's no crosstalk
  # group
  ids <- list(main = paste0(id, "_main_group"), sub = paste0(id, "_subgroup"),
              var = paste0(id, "_var_choice"), base = paste0(id, "_year"),
              download = paste0(id, "_download"), chart = paste0(id, "_chart"))
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
    # add_category_lines(): kept within the chart, with its lines wrapped, and
    # with the set decimals, or two significant digits if that's more precise
    e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE,
              extraCssText = "white-space: normal;",
              formatter = htmlwidgets::JS(sprintf("
                function(params) {
                  var p = params[0];
                  var value = Number(p.value[1]).toLocaleString('hu-HU', {
                    maximumFractionDigits: %d, maximumSignificantDigits: 2,
                    roundingPriority: 'morePrecision'
                  });
                  return p.name + '<br>' + p.marker + ' ' + p.seriesName +
                    ': <b style=\"white-space: nowrap\">' + value + '%s</b>';
                }", fraction_digits, unit_suffix))) |>
    e_y_axis(name = axis_name[[1]], max = percent_axis_max(unit),
             axisLabel = list(
               # As in add_category_lines()
               formatter = htmlwidgets::JS("
                function(value) {
                  return value.toLocaleString('hu-HU', { maximumFractionDigits: 20 });
                }
              ")
             )) |>
    add_chart_frame(x_name = "Ország") |>
    # ECharts shrinks the plot so that the labels of the x axis (see below) end
    # within these bounds, which leave room for the credit below them
    e_grid(outerBounds = list(left = 0, right = 0, top = 0, bottom = 25))
  # Every country is labelled, vertically (unless the labels would overlap, as on
  # phones: see add_chart_frame()), the highlighted one in bold
  chart$x$opts$xAxis[[1]]$axisLabel <- modifyList(
    chart$x$opts$xAxis[[1]]$axisLabel,
    list(interval = 0, rotate = 90, rich = list(highlight = list(fontWeight = "bold")),
         formatter = htmlwidgets::JS(sprintf(
           "function(value) { return value === %s ? '{highlight|' + value + '}' : value; }",
           jsonlite::toJSON(highlight, auto_unbox = TRUE, null = "null")
         )))
  )
  chart <- chart |>
    add_country_bars(ids, data, columns, axis_name, year, highlight) |>
    add_chart_description("Oszlopdiagram, az országok csökkenő sorrendben",
                          unit_suffix, fraction_digits) |>
    # The title names the chosen main group, subgroup and year. (CategoryPaths, which
    # the title is made from as in a category_chart(), has the same COFOG categories
    # under the same main groups)
    add_chart_layout(title, ids, year_label = "")
  
  htmltools::attachDependencies(
    htmltools::tagList(
      # Unless the page already has them (see country_values_element())
      country_values_element(data$values_id, data$values),
      country_filters(ids, data$categories, category, var, var_label, years, year),
      # The data files start with the name of the y axis, as the header of the
      # values only names the category and the year, not what they're relative to
      add_download_menu(chart, unit, id = ids$download,
                        choice_id = if (length(var) > 1) ids$var,
                        download_axis_name = TRUE),
      defer_chart(ids$chart)
    ),
    kiadas_dependency
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
# browser, as crosstalk would embed every row of the data in the page, with the
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

# The ids of the elements of the values of the countries (see
# country_values_element()) that the page being knitted already has
country_values_embedded <- new.env()

# The element of the `values` of country_data(), with the id `id`: a script element
# of their JSON, which the browser doesn't parse until a chart first needs them (see
# kiadas.countryValues() in kiadas_dependency). As each copy would add about a
# megabyte to the page, it only has one: the first chart of the columns that's
# knitted embeds them, and for the other charts of the same columns this is NULL, as
# they read them from there (wherever it is on the page, as the charts are only drawn
# once the page has been read: see defer_chart()). Outside knitting, as when a chunk
# is run in RStudio, each chart is shown on a page of its own, so each embeds them
country_values_element <- function(id, values) {
  if (isTRUE(getOption("knitr.in.progress"))) {
    if (!is.null(country_values_embedded[[id]])) return(NULL)
    country_values_embedded[[id]] <- TRUE
  }
  json <- jsonlite::toJSON(values, na = "null", digits = 16, use_signif = TRUE)
  # So that nothing in the data could end the element early, as a "</script>" would
  # ("<\/" reads the same in JSON)
  htmltools::tags$script(type = "application/json", id = id,
                         htmltools::HTML(gsub("</", "<\\/", json, fixed = TRUE)))
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
      # "Összesen" (see below)
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
    htmltools::tags$script(htmltools::HTML(sprintf(
      "(function() {
        var subgroups = %s, parentColumns = %s;
        var main = document.getElementById(%s), sub = %s && document.getElementById(%s);
        // The dropdown that the buttons follow, the last one
        var last = sub || main;
        var choice = document.getElementById(%s);
        var inputs = choice ? Array.from(choice.querySelectorAll('input')) : [];
        // The column chosen with the buttons, kept while it isn't offered
        var chosen = choice && choice.querySelector('input:checked').value;

        function update() {
          sub.replaceChildren.apply(sub, subgroups[main.value].map(function(label) {
            return new Option(label, label);
          }));
          sub.dispatchEvent(new Event('change'));
        }

        function offer() {
          var whole = !last.value || last.value === 'Összesen';
          var offered = inputs.filter(function(input) {
            var shown = !whole || parentColumns.indexOf(input.value) < 0;
            input.hidden = input.labels[0].hidden = !shown;
            return shown;
          });
          (offered.find(function(input) { return input.value === chosen; }) ||
            offered[0]).checked = true;
          // The div around the buttons, so that they leave no gap in the row
          choice.parentElement.hidden = offered.length < 2;
        }

        if (choice) {
          choice.addEventListener('change', function() {
            chosen = choice.querySelector('input:checked').value;
          });
          last.addEventListener('change', offer);
        }
        if (sub) {
          main.addEventListener('change', update);
          update();
        } else if (choice) {
          offer();
        }
      })();",
      jsonlite::toJSON(subgroups),
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
  htmlwidgets::onRender(chart, "
    function(el, x, data) {
      var chart = echarts.getInstanceByDom(el);
      var main = document.getElementById(data.mainId);
      var sub = document.getElementById(data.subId);
      var choice = data.columnId && document.getElementById(data.columnId);
      var slider = document.getElementById(data.yearId);
      var output = document.getElementById(data.yearId + '_value');
      // The chosen year, and the years offered
      var year = data.year, offered = [];

      // The label of the chosen category
      function category() { return sub.value === 'Összesen' ? main.value : sub.value; }
      function column() {
        return choice ? choice.querySelector('input:checked').value : data.columns[0];
      }
      // The values of the chosen category in the chosen column, by year
      function values() {
        return kiadas.countryValues(data.valuesId)[category()][column()];
      }

      slider.addEventListener('input', function() {
        year = offered[slider.value];
        output.textContent = year;
        slider.setAttribute('aria-valuetext', year);
        slider.dataset.year = year;
        var bars = [];
        values()[year].forEach(function(value, i) {
          if (value != null) bars.push({ name: data.countries[i], value: value });
        });
        bars.sort(function(a, b) {
          return b.value - a.value || a.name.localeCompare(b.name, 'hu');
        });
        chart.setOption({
          xAxis: { data: bars.map(function(bar) { return bar.name; }) },
          yAxis: { name: data.axisNames[column()] },
          series: [{
            name: data.names[category()] + ', ' + year,
            data: bars.map(function(bar) {
              return {
                name: bar.name, value: [bar.name, bar.value],
                itemStyle: bar.name === data.highlight ? { color: '#ff994d' } :
                  data.groups.includes(bar.name) ? { color: '#999999' } : {}
              };
            })
          }]
        });
      });

      // Offers the years with a value for the chosen category in the chosen column,
      // at the chosen year or, if it isn't among them, at the nearest one. (As the
      // keys of an object, the years come in ascending order)
      function offer() {
        var byYear = values();
        offered = Object.keys(byYear).filter(function(y) {
          return byYear[y].some(function(value) { return value != null; });
        });
        var at = 0;
        offered.forEach(function(y, i) {
          if (Math.abs(y - year) < Math.abs(offered[at] - year)) at = i;
        });
        slider.max = offered.length - 1;
        slider.value = at;
        slider.dispatchEvent(new Event('input'));
      }
      // The subgroup dropdown also changes whenever the main group does (see
      // country_filters())
      sub.addEventListener('change', offer);
      if (choice) choice.addEventListener('change', offer);
      offer();
    }
  ", data = list(
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
# add_part_lines()
percent_axis_max <- function(unit, stacked = FALSE) {
  if (unit != "%") return(NULL)
  htmlwidgets::JS(sprintf("
    function(value) {
      // ECharts' own end, for the values shown: a value axis includes 0, and its
      // step is a fifth of its range, rounded by ECharts' nice() to 1, 2, 3 or 5
      // times a power of 10
      var lo = Math.min(value.min, 0), hi = Math.max(value.max, 0);
      if (value.max > 100) return %s;
      var step = echarts.number.nice((hi - lo) / 5, true);
      return Math.ceil(Number((hi / step).toPrecision(12))) * step > 100 ? 100 : null;
    }
  ", if (stacked) "value.max - 100 < 0.005 * (hi - lo) ? 100 : null" else "null"))
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
  # Named as in country_chart(), which also has a slider
  ids <- list(main = paste0(id, "_main_group"), sub = paste0(id, "_subgroup"),
              var = paste0(id, "_var_choice"), download = paste0(id, "_download"),
              chart = paste0(id, "_chart"))
  columns <- unname(var)
  axis_name <- rep_len(axis_name, length(var))
  data <- country_data(columns)
  # As in country_chart()
  unit_suffix <- unit_suffix_for(unit)
  fraction_digits <- 1L
  geo_names <- c(country_names, group_names)
  # Before `highlight` is changed, as the default of `shown` reads it
  absent <- setdiff(shown, names(geo_names))
  if (length(absent) > 0) stop("No name for the countries ", toString(absent))
  shown <- intersect(geo_names[shown], data$countries)
  highlight <- if (!is.null(highlight)) country_names[[highlight]]
  others <- setdiff(data$countries, shown)
  # The names of the lines, in the order of the legend (and of the tooltip and the
  # data files), the others sorted by the Hungarian rules (in which É goes with E)
  lines <- c(shown, stringi::stri_sort(others, locale = "hu"))
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
    # crosstalk's rows here
    e_tooltip(trigger = "axis", confine = TRUE, extraCssText = "white-space: normal;",
              formatter = htmlwidgets::JS(sprintf("
                function(params) {
                  // Leaves out the lines without a value in the year, and shows no
                  // tooltip if none has one
                  params = params.filter(function(p) { return p.value[1] != null; });
                  if (!params.length) return '';
                  return params[0].axisValue + '<br>' +
                    params.map(function(p) {
                      var value = Number(p.value[1]).toLocaleString('hu-HU', {
                        maximumFractionDigits: %d, maximumSignificantDigits: 2,
                        roundingPriority: 'morePrecision'
                      });
                      return p.marker + ' ' + p.seriesName +
                        ': <b style=\"white-space: nowrap\">' + value + '%s</b>';
                    }).join('<br>');
                }", fraction_digits, unit_suffix))) |>
    e_y_axis(name = axis_name[[1]], max = percent_axis_max(unit),
             axisLabel = list(
               # As in add_category_lines()
               formatter = htmlwidgets::JS("
                function(value) {
                  return value.toLocaleString('hu-HU', { maximumFractionDigits: 20 });
                }
              ")
             )) |>
    add_chart_frame() |>
    add_country_lines(ids, data, lines, columns, axis_name, highlight) |>
    add_chart_description("Vonaldiagram", unit_suffix, fraction_digits) |>
    # The title names the chosen main group and subgroup, as in a country_chart()
    add_chart_layout(title, ids)

  htmltools::attachDependencies(
    htmltools::tagList(
      # Unless the page already has them (see country_values_element())
      country_values_element(data$values_id, data$values),
      country_filters(ids, data$categories, category, var, var_label),
      # The data files start with the name of the y axis, as the headers of the
      # values only name the countries, not what the values are relative to
      add_download_menu(chart, unit, id = ids$download,
                        choice_id = if (length(var) > 1) ids$var,
                        download_axis_name = TRUE),
      defer_chart(ids$chart)
    ),
    kiadas_dependency
  )
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
  htmlwidgets::onRender(chart, "
    function(el, x, data) {
      var chart = echarts.getInstanceByDom(el);
      var main = document.getElementById(data.mainId);
      var sub = document.getElementById(data.subId);
      var choice = data.columnId && document.getElementById(data.columnId);

      // As in add_country_bars()
      function category() { return sub.value === 'Összesen' ? main.value : sub.value; }
      function column() {
        return choice ? choice.querySelector('input:checked').value : data.columns[0];
      }

      // Draws the lines of the chosen category in the chosen column, over the years
      // with a value for any country (as the keys of an object, the years come in
      // ascending order), with null where a country has none, which ECharts leaves a
      // gap for
      function update() {
        var byYear = kiadas.countryValues(data.valuesId)[category()][column()];
        var years = Object.keys(byYear).filter(function(y) {
          return byYear[y].some(function(value) { return value != null; });
        });
        chart.setOption({
          xAxis: { data: years },
          yAxis: { name: data.axisNames[column()] },
          series: data.positions.map(function(i) {
            return { data: years.map(function(y) { return [y, byYear[y][i]]; }) };
          })
        });
      }
      // The subgroup dropdown also changes whenever the main group does (see
      // country_filters())
      sub.addEventListener('change', update);
      if (choice) choice.addEventListener('change', update);
      update();

      // The colours of the lines shown, by name (see above): given out at once, and
      // whenever a line is switched on or off in the legend. `selected` is ECharts'
      // map of the names switched on (true) and off (false) in the legend, in which
      // a name that's missing is on
      var colors = {};
      function recolor(selected) {
        var on = data.lines.filter(function(name) { return selected[name] !== false; });
        Object.keys(colors).forEach(function(name) {
          if (on.indexOf(name) < 0) delete colors[name];
        });
        on.forEach(function(name) {
          if (colors[name]) return;
          var used = Object.values(colors);
          colors[name] = name === data.highlight ? data.highlightColor :
            data.palette.find(function(color) { return used.indexOf(color) < 0; }) ||
            data.palette[used.length % data.palette.length];
        });
        chart.setOption({
          series: data.lines.map(function(name) {
            return colors[name] ? { color: colors[name] } : {};
          })
        });
      }
      recolor(chart.getOption().legend[0].selected || {});
      chart.on('legendselectchanged', function(e) { recolor(e.selected); });
    }
  ", data = list(
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
  # Named as in country_chart()
  ids <- list(main = paste0(id, "_main_group"), sub = paste0(id, "_subgroup"),
              var = paste0(id, "_var_choice"), download = paste0(id, "_download"),
              chart = paste0(id, "_chart"))
  columns <- unname(var)
  axis_name <- rep_len(axis_name, length(var))
  data <- country_data(columns)
  # As in country_chart()
  unit_suffix <- unit_suffix_for(unit)
  fraction_digits <- 1L
  geo_names <- c(country_names, group_names)
  # Before `highlight` is changed, as the default of `top` reads it
  absent <- setdiff(top, names(geo_names))
  if (length(absent) > 0) stop("No name for the countries ", toString(absent))
  top <- intersect(geo_names[top], data$countries)
  highlight <- if (!is.null(highlight)) country_names[[highlight]]
  # The names of the rows, from the top, the others sorted as in
  # country_lines_chart(). They're also the rows' values on the y axis, by which the
  # data files and the description for screen readers name the rows
  rows <- c(top, stringi::stri_sort(setdiff(data$countries, top), locale = "hu"))
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
    axisLabel = list(
      interval = 0, rich = list(highlight = list(fontWeight = "bold")),
      formatter = htmlwidgets::JS(sprintf(
        "function(value) { return value === %s ? '{highlight|' + value + '}' : value; }",
        jsonlite::toJSON(highlight, auto_unbox = TRUE, null = "null")
      ))
    )
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
    # add_category_heatmap(): kept within the chart, with its lines wrapped, and with
    # the set decimals, or two significant digits if that's more precise. (With a
    # dataset, p.value is the whole data row as an object)
    e_tooltip(trigger = "item", confine = TRUE, extraCssText = "white-space: normal;",
              formatter = htmlwidgets::JS(sprintf("
                function(p) {
                  var value = Number(p.value.value).toLocaleString('hu-HU', {
                    maximumFractionDigits: %d, maximumSignificantDigits: 2,
                    roundingPriority: 'morePrecision'
                  });
                  return p.value.yearchr + '<br>' + p.marker + ' ' + p.value.country +
                    ': <b style=\"white-space: nowrap\">' + value + '%s</b>';
                }", fraction_digits, unit_suffix))) |>
    add_chart_frame() |>
    add_country_heatmap(ids, data, rows, columns, axis_name, heatmap_sizes, unit) |>
    add_chart_description("Hőtérkép", unit_suffix, fraction_digits, row_names,
                          rows_name = "országok és országcsoportok") |>
    # The title names the chosen main group and subgroup, as in a country_chart()
    add_chart_layout(title, ids, row_height = heatmap_sizes$row,
                     min_plot_height = heatmap_sizes$plot)

  htmltools::attachDependencies(
    htmltools::tagList(
      # Unless the page already has them (see country_values_element())
      country_values_element(data$values_id, data$values),
      country_filters(ids, data$categories, category, var, var_label),
      # The data files start with the name of the y axis, as the headers of the
      # values only name the countries, not what the values are relative to
      add_download_menu(chart, unit, id = ids$download,
                        choice_id = if (length(var) > 1) ids$var,
                        download_axis_name = TRUE, row_names = row_names),
      defer_chart(ids$chart)
    ),
    kiadas_dependency
  )
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
  # Its pieces are set below, before any cell is drawn. Its own legend can't label a
  # gradient between its ends, so it's hidden, as in add_category_heatmap()
  chart$x$opts$visualMap <- list(list(
    type = "piecewise", show = FALSE, seriesIndex = 0, dimension = "value"
  ))
  htmlwidgets::onRender(chart, "
    function(el, x, data) {
      var chart = echarts.getInstanceByDom(el);
      var main = document.getElementById(data.mainId);
      var sub = document.getElementById(data.subId);
      var choice = data.columnId && document.getElementById(data.columnId);
      // The colour scale (see above), which this draws with the ticks it's given (see
      // kiadas.colorScale() in kiadas_dependency)
      var drawScale = kiadas.colorScale(chart, data.scale);

      // As in add_country_bars()
      function category() { return sub.value === 'Összesen' ? main.value : sub.value; }
      function column() {
        return choice ? choice.querySelector('input:checked').value : data.columns[0];
      }

      // `value` without the error of floating-point arithmetic (as in 3 * 0.1)
      function round(value) { return Number(value.toPrecision(12)); }

      // The least of 1, 1.5, 2, 2.5, 3, 4, 5, 6 and 8 times a power of 10 that is at
      // least `value` (a positive number)
      function niceStep(value) {
        var power = Math.pow(10, Math.floor(Math.log10(value)));
        return [1, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10]
          .map(function(m) { return round(m * power); })
          .find(function(step) { return step >= value; });
      }

      // The quantile `p` of the sorted `values`, interpolated between the two values
      // around it, as R's quantile() does by default
      function quantile(values, p) {
        var h = (values.length - 1) * p, i = Math.floor(h);
        return i + 1 < values.length ? values[i] + (h - i) * (values[i + 1] - values[i]) :
          values[i];
      }

      // The colouring of the cells of the values `values` (see above): the pieces of
      // the visualMap, and the ticks of the colour scale
      function coloring(values) {
        values = values.slice().sort(function(a, b) { return a - b; });
        var lo = quantile(values, 0.05), hi = quantile(values, 0.95);
        // The largest tick there may be, if any (see above), which cuts off the range
        var max = data.scaleMax;
        if (max != null) {
          lo = Math.min(lo, max);
          hi = Math.min(hi, max);
        }
        var step, start;
        // The ticks by the step `s`, from its multiple at or below `lo`, and low
        // enough for the last of them to be at or below `max`
        function spread(s) {
          step = s;
          var from = max == null ? lo : Math.min(lo, max - 4 * step);
          start = round(Math.floor(round(from / step)) * step);
        }
        // If all the values are the same, a step of a quarter of their size, or of 1
        // if they're all 0
        spread(niceStep((hi - lo) / 4 || Math.abs(hi) / 4 || 1));
        // A larger step, while the ticks would end below `hi`. If even a step of
        // `max` doesn't do, the ticks may go beyond it, as larger steps would end
        // them at 0 at most. (With a `max` of 100, a step of 25 does for any values
        // within 0 and 100, so only values far below 0 could need that)
        while (round(start + 4 * step) < hi) {
          if (max != null && step >= max) max = null;
          spread(niceStep(1.01 * step));
        }
        // The bound between the pieces i and i + 1, halfway between their middles,
        // which run from `start` to the last tick
        var n = data.colors.length;
        function bound(i) { return start + 4 * step * (i + 0.5) / (n - 1); }
        return {
          pieces: data.colors.map(function(color, i) {
            var piece = { color: color };
            if (i > 0) piece.gte = bound(i - 1);
            if (i < n - 1) piece.lt = bound(i);
            return piece;
          }),
          ticks: [0, 1, 2, 3, 4].map(function(i) {
            var text = round(start + i * step).toLocaleString('hu-HU', {
              maximumFractionDigits: 20
            });
            return { position: i / 4,
                     label: i === 0 ? '\\u2264 ' + text : i === 4 ? '\\u2265 ' + text : text };
          })
        };
      }

      // Draws the cells of the chosen category in the chosen column, over the years
      // with a value for any country (as the keys of an object, the years come in
      // ascending order), row by row, so that the data files and the description
      // list the rows in their order (see add_download_menu())
      function update() {
        var byYear = kiadas.countryValues(data.valuesId)[category()][column()];
        var years = Object.keys(byYear).filter(function(y) {
          return byYear[y].some(function(value) { return value != null; });
        });
        var cells = [];
        data.rows.forEach(function(name, r) {
          years.forEach(function(year) {
            var value = byYear[year][data.positions[r]];
            if (value != null) cells.push({ yearchr: year, country: name, value: value });
          });
        });
        var colors = coloring(cells.map(function(cell) { return cell.value; }));
        chart.setOption({
          dataset: { source: cells },
          xAxis: { data: years },
          yAxis: { name: data.axisNames[column()] },
          visualMap: { pieces: colors.pieces }
        });
        drawScale(colors.ticks);
      }
      // The subgroup dropdown also changes whenever the main group does (see
      // country_filters())
      sub.addEventListener('change', update);
      if (choice) choice.addEventListener('change', update);
      update();
    }
  ", data = list(
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
  # Named as in country_chart(), which also has a slider, but there's no subgroup
  # dropdown
  ids <- list(main = paste0(id, "_main_group"), var = paste0(id, "_var_choice"),
              base = paste0(id, "_year"), download = paste0(id, "_download"),
              chart = paste0(id, "_chart"))
  columns <- unname(var)
  axis_name <- rep_len(axis_name, length(var))
  data <- country_data(columns)
  # As in country_chart()
  unit_suffix <- unit_suffix_for(unit)
  fraction_digits <- 1L
  geo_names <- c(country_names, group_names)
  # Before `highlight` is changed, as the default of `shown` reads it
  absent <- setdiff(shown, names(geo_names))
  if (length(absent) > 0) stop("No name for the countries ", toString(absent))
  shown <- intersect(geo_names[shown], data$countries)
  highlight <- if (!is.null(highlight)) country_names[[highlight]]
  others <- setdiff(data$countries, shown)
  # The names of the countries in the order of the legend and of the columns, the
  # others sorted as in country_lines_chart()
  countries <- c(shown, stringi::stri_sort(others, locale = "hu"))
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
             min = if (unit == "%") htmlwidgets::JS("
               function(value) {
                 return value.min < 0 && -value.min < 0.005 * (value.max - value.min) ?
                   0 : null;
               }
             "),
             axisLabel = list(
               # As in add_category_lines()
               formatter = htmlwidgets::JS("
                function(value) {
                  return value.toLocaleString('hu-HU', { maximumFractionDigits: 20 });
                }
              ")
             )) |>
    add_chart_frame(x_name = "Ország") |>
    # ECharts shrinks the plot so that the labels of the x axis (see below) end
    # within these bounds, which leave room for a legend of one line and the credit
    # below them (add_chart_layout() moves them up by the room for the legends)
    e_grid(outerBounds = list(left = 0, right = 0, top = 0, bottom = 45))
  # Every country is labelled, the highlighted one in bold, as in a country_chart()
  # (vertically if the names don't fit: see add_country_stacks())
  chart$x$opts$xAxis[[1]]$axisLabel <- modifyList(
    chart$x$opts$xAxis[[1]]$axisLabel,
    list(interval = 0, rich = list(highlight = list(fontWeight = "bold")),
         formatter = htmlwidgets::JS(sprintf(
           "function(value) { return value === %s ? '{highlight|' + value + '}' : value; }",
           jsonlite::toJSON(highlight, auto_unbox = TRUE, null = "null")
         )))
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

  htmltools::attachDependencies(
    htmltools::tagList(
      # Unless the page already has them (see country_values_element())
      country_values_element(data$values_id, data$values),
      country_filters(ids, categories, "Összesen", var, var_label, years, year,
                      parts = TRUE),
      # The data files start with the name of the y axis, as the headers of the
      # values only name the parts, not what they're relative to
      add_download_menu(chart, unit, id = ids$download,
                        choice_id = if (length(var) > 1) ids$var,
                        download_axis_name = TRUE),
      defer_chart(ids$chart)
    ),
    kiadas_dependency
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
  htmlwidgets::onRender(chart, "
    function(el, x, data) {
      var chart = echarts.getInstanceByDom(el);
      var main = document.getElementById(data.mainId);
      var choice = data.columnId && document.getElementById(data.columnId);
      var slider = document.getElementById(data.yearId);
      var output = document.getElementById(data.yearId + '_value');
      var option = chart.getOption();
      // The chosen year, the years offered, the names of the countries with a
      // column, and whether each country is switched on in the legend (by name)
      var year = data.year, offered = [], drawn = [], on = {};
      var selected = option.legend[1].selected || {};
      data.countries.forEach(function(name) { on[name] = selected[name] !== false; });

      // The labels of the parts of the chosen whole, from the bottom of the stack up
      function parts() { return data.parts[main.value] || []; }
      function column() {
        return choice ? choice.querySelector('input:checked').value : data.columns[0];
      }
      // The values of each of the parts in the chosen column, by year
      function values() {
        var all = kiadas.countryValues(data.valuesId);
        return parts().map(function(label) { return all[label][column()]; });
      }

      // 90 (vertical) if the widest of the names `names` (in bold, as that of the
      // highlighted country is) wouldn't fit in a column with 8px to spare, else 0.
      // The plot takes up 75% of the width of the chart, between the default
      // margins of the grid (15% on the left, 10% on the right)
      var context = document.createElement('canvas').getContext('2d');
      context.font = 'bold 12px ' + option.textStyle.fontFamily;
      function rotation(names) {
        var widest = Math.max.apply(null, names.map(function(name) {
          return context.measureText(name).width;
        }).concat(0));
        return widest + 8 > 0.75 * el.clientWidth / names.length ? 90 : 0;
      }

      // Draws the columns of the chosen year (see above)
      function draw() {
        var labels = parts(), byPart = values(), at = {}, shown = [];
        labels.forEach(function(label, k) { at[label] = k; });
        data.countries.forEach(function(name, j) {
          var i = data.positions[j];
          var has = byPart.some(function(byYear) {
            return byYear[year] && byYear[year][i] != null;
          });
          if (on[name] && has) shown.push({ name: name, position: i });
        });
        drawn = shown.map(function(country) { return country.name; });
        chart.setOption({
          xAxis: { data: drawn, axisLabel: { rotate: rotation(drawn) } },
          yAxis: { name: data.axisNames[column()] },
          legend: [{ data: labels.slice().reverse() }],
          // Those of the parts (the series of the countries come after them)
          series: data.labels.map(function(label) {
            var k = at[label];
            if (k === undefined) return { data: [] };
            var byYear = byPart[k][year] || [];
            return {
              color: data.palette[k % data.palette.length],
              data: shown.map(function(country) {
                var value = byYear[country.position];
                return { name: country.name,
                         value: [country.name, value == null ? null : value] };
              })
            };
          })
        });
      }

      slider.addEventListener('input', function() {
        year = offered[slider.value];
        output.textContent = year;
        slider.setAttribute('aria-valuetext', year);
        slider.dataset.year = year;
        draw();
      });

      // Offers the years with a value for a part of the chosen whole in the chosen
      // column for a country switched on (or, if none is, for any country), at the
      // chosen year or, if it isn't among them, at the nearest one. (As the keys of
      // an object, the years come in ascending order)
      function offer() {
        var byPart = values();
        function covered(positions) {
          var has = {};
          byPart.forEach(function(byYear) {
            Object.keys(byYear).forEach(function(y) {
              if (positions.some(function(i) { return byYear[y][i] != null; })) {
                has[y] = true;
              }
            });
          });
          return Object.keys(has);
        }
        offered = covered(data.positions.filter(function(_, j) {
          return on[data.countries[j]];
        }));
        if (!offered.length) offered = covered(data.positions);
        var at = 0;
        offered.forEach(function(y, i) {
          if (Math.abs(y - year) < Math.abs(offered[at] - year)) at = i;
        });
        slider.max = offered.length - 1;
        slider.value = at;
        slider.dispatchEvent(new Event('input'));
      }
      // The radio buttons follow the dropdown (see country_filters())
      main.addEventListener('change', offer);
      if (choice) choice.addEventListener('change', offer);
      // Whenever a country is switched on or off in the legend, by a click or by an
      // action of any kind, each of which has an event of its own (the parts' names
      // switch their parts on and off by themselves)
      ['legendselectchanged', 'legendselected', 'legendunselected', 'legendselectall',
       'legendinverseselect'].forEach(function(type) {
        chart.on(type, function(e) {
          var changed = false;
          data.countries.forEach(function(name) {
            var now = e.selected[name] !== false;
            if (now !== on[name]) {
              on[name] = now;
              changed = true;
            }
          });
          if (changed) offer();
        });
      });
      // The names of the countries fit in a new width differently
      window.addEventListener('resize', function() {
        chart.setOption({ xAxis: { axisLabel: { rotate: rotation(drawn) } } });
      });
      offer();

      var format = new Intl.NumberFormat('hu-HU', {
        maximumFractionDigits: data.digits, maximumSignificantDigits: 2,
        roundingPriority: 'morePrecision'
      });
      chart.setOption({
        tooltip: {
          formatter: function(p) {
            if (p.value == null || p.value[1] == null) return '';
            return p.name + ', ' + year + '<br>' + p.marker + ' ' + p.seriesName +
              ': <b style=\"white-space: nowrap\">' + format.format(p.value[1]) +
              data.unit + '</b>';
          }
        }
      });
    }
  ", data = list(
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
