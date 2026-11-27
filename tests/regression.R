# Regression test of the page kiadas.qmd: renders it as it is in a git commit
# (`baseline`, by default the last one) and as it is in the working tree, each into
# a folder of its own (leaving docs/ alone), and compares the two. First their HTML:
# the data of each chart (but for its options whose values are JavaScript code),
# the data of its render hooks, and the page around the charts (but for its
# scripts). This is for information only, as refactoring may well change the code.
# Then, decisively, how they work, in a headless Chrome or Edge (via chromote): the
# charts drawn, what their tooltips show, the state of every chart (its options,
# what it shows, the image it draws, its description for screen readers, and the
# extent and the labels of its axes) and of every control after each step of a
# series of interactions (every dropdown, radio button and slider in turn, then the
# first and the last item of each legend, then hovering one), and the files of every
# download menu, before and after the interactions. With `strict = FALSE`, the
# options may differ, as long as all the rest is the same. See tests/README.md. Run
# it from the folder of the project:
#
#   source("tests/regression.R"); regression_test()
#
# or, as a command, which fails unless the two work the same (with --loose, as with
# `strict = FALSE`):
#
#   Rscript tests/regression.R [baseline] [--loose]

regression_test <- local({

  # The files of the commit `ref` in the folder `dir`, without what the page doesn't
  # need: the rendered site and the big data files. (Of the commit's tree, as git
  # would add a header of the commit's id, which untar() warns about)
  export_commit <- function(ref, dir) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    tar <- file.path(dir, "project.tar")
    processx::run("git", c("archive", "--format=tar", "-o", tar, paste0(ref, "^{tree}"), "--", ".",
                           ":(exclude)docs",
                           ":(exclude)koltsegvetes-funkcionalis-kiadas.csv",
                           ":(exclude)koltsegvetes-funkcionalis-kiadas.xlsx",
                           ":(exclude)koltsegvetes-funkcionalis-kiadas.zip"))
    utils::untar(tar, exdir = dir, tar = "internal")
    unlink(tar)
    dir
  }

  # Renders `page` of the project in the folder `project` into the folder `out`, and
  # returns the path of the HTML file
  render_page <- function(project, page, out) {
    quarto <- if (requireNamespace("quarto", quietly = TRUE)) quarto::quarto_path()
    if (is.null(quarto) || !nzchar(quarto)) quarto <- Sys.which("quarto")
    if (!nzchar(quarto)) stop("Quarto isn't found")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    processx::run(quarto, c("render", page, "--output-dir", normalizePath(out, winslash = "/")),
                  wd = project)
    file.path(out, sub("\\.qmd$", ".html", basename(page)))
  }

  # The HTML files `base` and `new`, compared: for each chart (by the id of its
  # element), whether it's in both, whether its data are the same but for its
  # options whose values are JavaScript code (those htmlwidgets evaluates), how many
  # of those changed, whether the data of its render hooks are the same, and how
  # many of their code changed; and whether the page around the charts is the same
  # but for its scripts (if not, the first line where it differs, and the two lines
  # from a little before where they part)
  compare_html <- function(base, new) {
    read <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    base <- read(base); new <- read(new)
    re <- '<script type="application/json" data-for="([^"]*)">(.*?)</script>'
    widgets <- function(html) {
      m <- regmatches(html, gregexpr(re, html, perl = TRUE))[[1]]
      setNames(lapply(sub(re, "\\2", m, perl = TRUE), jsonlite::fromJSON, simplifyVector = FALSE),
               sub(re, "\\1", m, perl = TRUE))
    }
    # The value at a path of htmlwidgets' evals (as "opts.yAxis.0.max"), or NULL
    key <- function(k) if (grepl("^[0-9]+$", k)) as.integer(k) + 1L else k
    get_at <- function(x, keys) {
      for (k in keys) {
        k <- key(k)
        if (!is.list(x) || (is.integer(k) && k > length(x)) || (is.character(k) && is.null(x[[k]]))) {
          return(NULL)
        }
        x <- x[[k]]
      }
      x
    }
    set_at <- function(x, keys, value) {
      k <- key(keys[1])
      if (length(keys) == 1) x[[k]] <- value else x[[k]] <- set_at(x[[k]], keys[-1], value)
      x
    }
    wb <- widgets(base); wn <- widgets(new)
    rows <- lapply(union(names(wb), names(wn)), function(id) {
      b <- wb[[id]]; n <- wn[[id]]
      if (is.null(b) || is.null(n)) {
        return(data.frame(widget = id, in_both = FALSE, data_same = NA, js_changed = NA,
                          hooks_data_same = NA, hooks_changed = NA))
      }
      xb <- b$x; xn <- n$x; changed <- 0L
      for (p in union(unlist(b$evals), unlist(n$evals))) {
        keys <- strsplit(p, ".", fixed = TRUE)[[1]]
        vb <- get_at(xb, keys); vn <- get_at(xn, keys)
        if (!identical(vb, vn)) changed <- changed + 1L
        if (!is.null(vb)) xb <- set_at(xb, keys, "JS")
        if (!is.null(vn)) xn <- set_at(xn, keys, "JS")
      }
      hb <- b$jsHooks$render; hn <- n$jsHooks$render
      data.frame(
        widget = id, in_both = TRUE,
        data_same = identical(xb, xn) && setequal(unlist(b$evals), unlist(n$evals)),
        js_changed = changed,
        hooks_data_same = identical(lapply(hb, `[[`, "data"), lapply(hn, `[[`, "data")),
        hooks_changed = if (length(hb) == length(hn)) {
          sum(!mapply(function(x, y) identical(x$code, y$code), hb, hn))
        } else NA
      )
    })
    strip <- function(html) {
      html <- gsub(re, "", html, perl = TRUE)
      html <- gsub('<script src="site_libs/kiadas-[^"]*"></script>\\s*', "", html, perl = TRUE)
      strsplit(gsub("(?s)<script>(?:(?!</script>).)*</script>\\s*", "", html, perl = TRUE), "\n")[[1]]
    }
    rb <- strip(base); rn <- strip(new)
    difference <- if (!identical(rb, rn)) {
      n <- min(length(rb), length(rn))
      line <- which(rb[seq_len(n)] != rn[seq_len(n)])[1]
      if (is.na(line)) line <- n + 1L
      lb <- strsplit(if (line <= length(rb)) rb[line] else "", "")[[1]]
      ln <- strsplit(if (line <= length(rn)) rn[line] else "", "")[[1]]
      m <- min(length(lb), length(ln))
      at <- which(lb[seq_len(m)] != ln[seq_len(m)])[1]
      if (is.na(at)) at <- m + 1L
      around <- function(chars) paste(chars[max(1, at - 40):min(length(chars), at + 60)], collapse = "")
      list(line = line, baseline = around(lb), working = around(ln))
    }
    list(widgets = do.call(rbind, rows), rest_same = identical(rb, rn), rest_difference = difference)
  }

  # Run before any script of the page: collects its errors and warnings, and keeps
  # the files it saves (the links clicked to download them) instead of saving them
  js_inject <- r"-(
    window.__errors = [];
    window.addEventListener('error', function(e) {
      var t = e.target;
      window.__errors.push(t && t !== window && (t.src || t.href) ?
        'resource: ' + (t.src || t.href).replace(/^.*?\/(site_libs\/)/, '$1') :
        'error: ' + e.message + ' @' + e.filename + ':' + e.lineno);
    }, true);
    window.addEventListener('unhandledrejection', function(e) {
      window.__errors.push('rejection: ' + e.reason);
    });
    ['error', 'warn'].forEach(function(level) {
      var f = console[level];
      console[level] = function() {
        window.__errors.push('console.' + level + ': ' + Array.from(arguments).join(' '));
        return f.apply(console, arguments);
      };
    });
    window.__blobs = {};
    var create = URL.createObjectURL;
    URL.createObjectURL = function(b) {
      var u = create.call(URL, b);
      window.__blobs[u] = b;
      return u;
    };
    window.__saved = [];
    var click = HTMLElement.prototype.click;
    HTMLAnchorElement.prototype.click = function() {
      if (this.download) {
        window.__saved.push({ name: this.download, href: this.href });
        return;
      }
      return click.call(this);
    };
  )-"

  # The functions of the test in the page
  js_helpers <- r"-(
    const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
    // A 32-bit FNV-1a hash of a string or of bytes, in hexadecimal
    function fnv(s) {
      let h = 0x811c9dc5;
      for (let i = 0; i < s.length; i++) {
        h ^= typeof s === 'string' ? s.charCodeAt(i) : s[i];
        h = Math.imul(h, 0x01000193) >>> 0;
      }
      return h.toString(16);
    }
    const widgets = () => Array.from(document.querySelectorAll('.html-widget'));
    const pending = () => document.querySelectorAll('.kiadas-deferred').length +
      widgets().filter(el => !echarts.getInstanceByDom(el)).length;

    // Draws every chart, as before printing, and returns their ids
    async function renderAll() {
      window.dispatchEvent(new Event('beforeprint'));
      for (let i = 0; i < 60 && pending(); i++) await sleep(500);
      await sleep(3000);
      return widgets().map(el => el.id);
    }

    // The axes of a chart: their extent, and the labels their formatters make of
    // their ticks
    function axesOf(chart) {
      const out = [];
      ['xAxis', 'yAxis'].forEach(type => {
        chart.getModel().eachComponent(type, model => {
          const axis = model.axis;
          if (!axis) { out.push(type + ':none'); return; }
          try {
            const fmt = model.get(['axisLabel', 'formatter']);
            const ticks = axis.scale.getTicks().map(t => t.value);
            const labels = typeof fmt === 'function' ? ticks.map((v, i) =>
              fmt(axis.type === 'category' ? axis.scale.getLabel({ value: v }) : v, i)) : [];
            out.push(type + ':' + axis.scale.getExtent().join('/') + ':' + labels.join('|'));
          } catch (e) {
            out.push(type + ':ERR ' + e.message);
          }
        });
      });
      return out.join(';');
    }

    // What a chart shows, read from ECharts' model rather than from its options, so
    // that it doesn't depend on how the options are organised (as into datasets): of
    // each series shown (not switched off in the legend), its name, type and colours,
    // and of each of its items, its name, its values on the axes (or that its colour
    // shows), from its row of data if that's an object (from a dataset), else as
    // ECharts parsed them, and its own colours; the items of each legend, and whether
    // they're switched on; the titles; the pieces of each visualMap; and the size of
    // the chart
    function contentOf(chart) {
      const model = chart.getModel();
      const series = [], legends = [], titles = [], maps = [];
      model.eachSeries(s => {
        const data = s.getData();
        const dims = ['x', 'y', 'value'].map(d => data.mapDimension(d)).filter(Boolean);
        const style = data.getVisual('style') || {};
        const items = [];
        for (let i = 0; i < data.count(); i++) {
          const raw = data.getRawDataItem(i);
          const values = dims.map(d => {
            if (raw && raw.constructor === Object && d in raw) return raw[d];
            const info = data.getDimensionInfo(d), v = data.get(d, i);
            return info && info.ordinalMeta ? info.ordinalMeta.categories[v] : v;
          });
          const own = data.getItemVisual(i, 'style') || {};
          items.push([data.getName(i)].concat(values, [own.fill, own.stroke]));
        }
        series.push({ name: s.name, type: s.subType, fill: style.fill, stroke: style.stroke,
                      items: items });
      });
      model.eachComponent('legend', legend => {
        legends.push(legend.getData().map(item => {
          const name = item.get('name');
          return [name, legend.isSelected(name)];
        }));
      });
      model.eachComponent('title', title => titles.push(title.get('text')));
      model.eachComponent('visualMap', map => {
        maps.push([map.get('dimension'), map.get('pieces')]);
      });
      return JSON.stringify({ series: series, legends: legends, titles: titles, maps: maps,
                              size: [chart.getWidth(), chart.getHeight()] });
    }

    // The state of the page: for each chart, its options, what it shows (see
    // contentOf()), the image it draws, its description for screen readers and its
    // axes; the state of every control; and the years shown beside the sliders
    function snapshot() {
      const charts = widgets().map(el => {
        const c = echarts.getInstanceByDom(el);
        return c ? [fnv(JSON.stringify(c.getOption())), fnv(contentOf(c)),
                    fnv(c.getDataURL({ type: 'png', pixelRatio: 1 })),
                    fnv(el.getAttribute('aria-label') || ''), fnv(axesOf(c))].join(':') :
          'none:none:none:none:none';
      });
      const controls = Array.from(document.querySelectorAll('select, input')).map(e => {
        const opts = e.tagName === 'SELECT' ? Array.from(e.options).map(o => o.value).join('|') : '';
        return e.id + '=' + (e.type === 'radio' ? e.checked : e.value) +
          (e.hidden || e.closest('[hidden]') ? ':h' : '') + '[' + fnv(opts) + ']' +
          (e.dataset.year ? '@' + e.dataset.year : '');
      }).join(';');
      const shown = Array.from(document.querySelectorAll('[id$="_value"]')).map(s => s.textContent);
      return charts.join(',') + '#' + fnv(controls) + '#' + shown.join('|');
    }

    // Clicks every item of every download menu
    function clickDownloads() {
      document.querySelectorAll('.dropdown[id$="_download"]').forEach(menu => {
        ['png', 'jpg', 'svg', 'xlsx', 'csv'].forEach(f => {
          menu.querySelector('[data-format="' + f + '"]').click();
        });
      });
    }

    // What each chart's tooltip formatter returns for the first, the middle and the
    // last point of its first two series with data (null if it has no formatter).
    // Then the tooltip and the axis pointer are hidden, as when the mouse leaves the
    // chart (hiding the tooltip alone would leave the axis pointer shown, and the
    // points under it highlighted, which no mouse leaves)
    async function captureTooltips() {
      const all = [];
      for (const el of widgets()) {
        const chart = echarts.getInstanceByDom(el);
        const original = ((chart.getOption().tooltip || [])[0] || {}).formatter;
        if (typeof original !== 'function') { all.push(null); continue; }
        const out = [];
        chart.setOption({ tooltip: { formatter: function() {
          const r = original.apply(this, arguments);
          out.push(r);
          return r;
        } } });
        const targets = [];
        chart.getModel().eachSeries(s => {
          const n = s.getData().count();
          if (n && targets.length < 6) {
            targets.push([s.seriesIndex, 0], [s.seriesIndex, Math.floor(n / 2)],
                         [s.seriesIndex, n - 1]);
          }
        });
        for (const t of targets) {
          chart.dispatchAction({ type: 'showTip', seriesIndex: t[0], dataIndex: t[1] });
          await sleep(100);
        }
        chart.dispatchAction({ type: 'hideTip' });
        chart.dispatchAction({ type: 'updateAxisPointer', currTrigger: 'leave' });
        await sleep(100);
        chart.setOption({ tooltip: { formatter: original } });
        all.push(out);
      }
      return all;
    }

    // Changes every control in turn (a dropdown to its last choice, a group of radio
    // buttons to its last button shown, a slider to its start), then switches the
    // first and the last item of each chart's first legend with several, then
    // highlights the second item of each chart's first legend with more than two, as
    // hovering it would, and returns the state of the page after each step, `wait` ms
    // after it (as the description for screen readers is only updated once the
    // animation is over)
    async function interact(wait) {
      const steps = [{ step: 'initial', state: snapshot() }];
      const controls = document.querySelectorAll('select.form-select, fieldset, input[type=range]');
      for (const c of controls) {
        let done = false;
        if (c.tagName === 'SELECT' && c.options.length > 1 &&
            c.value !== c.options[c.options.length - 1].value) {
          c.value = c.options[c.options.length - 1].value;
          c.dispatchEvent(new Event('change'));
          done = true;
        } else if (c.tagName === 'FIELDSET') {
          const radios = Array.from(c.querySelectorAll('input[type=radio]'))
            .filter(r => !r.checked && !r.hidden && !r.closest('[hidden]'));
          if (radios.length) { radios[radios.length - 1].click(); done = true; }
        } else if (c.type === 'range' && Number(c.max) > 0 && c.value !== '0') {
          c.value = 0;
          c.dispatchEvent(new Event('input', { bubbles: true }));
          done = true;
        }
        if (!done) continue;
        await sleep(wait);
        steps.push({ step: c.tagName.toLowerCase() + '#' + c.id, state: snapshot() });
      }
      for (const el of widgets()) {
        const chart = echarts.getInstanceByDom(el);
        const legend = (chart.getOption().legend || []).find(l => (l.data || []).length > 1);
        if (!legend) continue;
        const names = legend.data.map(d => typeof d === 'string' ? d : d.name);
        for (const name of [names[0], names[names.length - 1]]) {
          chart.dispatchAction({ type: 'legendToggleSelect', name: name });
          await sleep(wait);
          steps.push({ step: el.id + ' legend: ' + name, state: snapshot() });
        }
      }
      for (const el of widgets()) {
        const chart = echarts.getInstanceByDom(el);
        const legend = (chart.getOption().legend || []).find(l => (l.data || []).length > 2);
        if (!legend) continue;
        const name = typeof legend.data[1] === 'string' ? legend.data[1] : legend.data[1].name;
        chart.dispatchAction({ type: 'highlight', seriesName: name });
        await sleep(wait);
        steps.push({ step: el.id + ' highlight: ' + name, state: snapshot() });
        chart.dispatchAction({ type: 'downplay', seriesName: name });
        await sleep(wait);
      }
      return steps;
    }

    // The files saved so far, each by its name, type, size and hash, and the errors
    async function collectFiles() {
      const files = [];
      for (const s of window.__saved) {
        const file = { name: s.name };
        if (s.href.startsWith('blob:')) {
          const blob = window.__blobs[s.href];
          file.type = blob.type;
          file.size = blob.size;
          file.hash = /\.(csv|svg)$/.test(s.name) ? fnv(await blob.text()) :
            fnv(new Uint8Array(await blob.arrayBuffer()));
        } else {
          file.type = s.href.slice(5, s.href.indexOf(';'));
          file.size = s.href.length;
          file.hash = fnv(s.href);
        }
        files.push(file);
      }
      return { files: files, errors: window.__errors };
    }
  )-"

  # Opens the HTML file `html` in a new tab of the browser `chrome`, of the size
  # `width` x `height`, draws its charts, and records what the test compares
  browser_run <- function(html, chrome, wait, width = 1280, height = 1000) {
    b <- chromote::ChromoteSession$new(parent = chrome, width = width, height = height)
    on.exit(b$close(), add = TRUE)
    b$Page$addScriptToEvaluateOnNewDocument(source = js_inject)
    loaded <- b$Page$loadEventFired(wait_ = FALSE)
    url <- normalizePath(html, winslash = "/")
    b$Page$navigate(paste0("file://", if (!startsWith(url, "/")) "/", url), wait_ = FALSE)
    b$wait_for(loaded)
    Sys.sleep(1)
    evaluate <- function(js, simplify = TRUE) {
      res <- b$Runtime$evaluate(paste0("(async () => {", js_helpers, "\n", js, "\n})()"),
                                awaitPromise = TRUE, returnByValue = TRUE, timeout_ = 900)
      if (!is.null(res$exceptionDetails)) {
        stop("JavaScript error: ", res$exceptionDetails$exception$description)
      }
      jsonlite::fromJSON(res$result$value, simplifyVector = simplify)
    }
    ids <- evaluate("return JSON.stringify(await renderAll());")
    tips_before <- evaluate("return JSON.stringify(await captureTooltips());", simplify = FALSE)
    evaluate("clickDownloads(); return 'null';")
    steps <- evaluate(sprintf("return JSON.stringify(await interact(%d));", as.integer(wait)))
    tips_after <- evaluate("return JSON.stringify(await captureTooltips());", simplify = FALSE)
    evaluate("clickDownloads(); await sleep(2000); return 'null';")
    files <- evaluate("return JSON.stringify(await collectFiles());")
    list(ids = ids, tips_before = tips_before, tips_after = tips_after, steps = steps,
         files = files$files, errors = files$errors)
  }

  # The parts of the state of a chart in a snapshot (see snapshot() in js_helpers)
  chart_parts <- c("options", "content", "image", "description", "axes")

  # The states `states` (from browser_run()) without the options of the charts, which
  # refactoring may organise differently without changing what the charts show
  without_options <- function(states) {
    vapply(states, function(state) {
      p <- strsplit(state, "#", fixed = TRUE)[[1]]
      charts <- strsplit(p[1], ",", fixed = TRUE)[[1]]
      p[1] <- paste(sub("^[^:]*", "-", charts), collapse = ",")
      paste(p, collapse = "#")
    }, "", USE.NAMES = FALSE)
  }

  # The steps after which the states `base` and `new` differ (from browser_run()), and
  # what differs then: which parts of which charts (by the ids of their elements in
  # `ids`), the controls, or the years shown. A data frame of the step and what
  # differs, separated by commas; if the steps themselves differ, only the first one
  differences <- function(base, new, ids) {
    n <- min(nrow(base), nrow(new))
    i <- which(base$step[seq_len(n)] != new$step[seq_len(n)])[1]
    if (!is.na(i)) {
      return(data.frame(step = base$step[i], what = paste("the step itself:", new$step[i])))
    }
    parts <- function(state) {
      p <- strsplit(state, "#", fixed = TRUE)[[1]]
      charts <- strsplit(p[1], ",", fixed = TRUE)[[1]]
      by_chart <- unlist(lapply(seq_along(charts), function(k) {
        setNames(strsplit(charts[k], ":", fixed = TRUE)[[1]], paste(ids[k], chart_parts))
      }))
      c(by_chart, controls = p[2], `years shown` = if (length(p) > 2) p[3] else "")
    }
    rows <- lapply(which(base$state[seq_len(n)] != new$state[seq_len(n)]), function(i) {
      a <- parts(base$state[i]); b <- parts(new$state[i])
      keys <- union(names(a), names(b))
      what <- keys[is.na(a[keys]) | is.na(b[keys]) | a[keys] != b[keys]]
      data.frame(step = base$step[i], what = paste(what, collapse = ", "))
    })
    if (nrow(base) != nrow(new)) {
      rows <- c(rows, list(data.frame(step = "(the end)", what = "the number of steps")))
    }
    do.call(rbind, c(list(data.frame(step = character(), what = character())), rows))
  }

  # The results of browser_run() for the two versions, compared. Whether the options of
  # the charts are the same after each step (`options`) only counts with `strict`
  compare_runs <- function(base, new, strict) {
    fb <- base$files; fn <- new$files
    same_files <- NROW(fb) == NROW(fn) && NROW(fb) > 0 &&
      all(fb$name == fn$name & fb$type == fn$type & fb$size == fn$size & fb$hash == fn$hash)
    content <- function(steps) data.frame(step = steps$step, state = without_options(steps$state))
    checks <- c(charts = identical(base$ids, new$ids),
                tooltips_before = identical(base$tips_before, new$tips_before),
                tooltips_after = identical(base$tips_after, new$tips_after),
                interactions = identical(content(base$steps), content(new$steps)),
                options = identical(base$steps, new$steps),
                downloads = same_files,
                errors = identical(base$errors, new$errors))
    decisive <- if (strict) names(checks) else setdiff(names(checks), "options")
    # The charts (by the ids of their elements) whose tooltips differ
    tips <- function(b, n) {
      if (length(b) != length(n)) return("all, as their number differs")
      new$ids[!mapply(identical, b, n)]
    }
    list(pass = all(checks[decisive]), checks = checks, strict = strict,
         charts = length(new$ids), steps = nrow(new$steps),
         distinct_states = length(unique(new$steps$state)), files = NROW(fn),
         differences = if (!checks[["interactions"]]) {
           differences(content(base$steps), content(new$steps), new$ids)
         } else if (!checks[["options"]]) differences(base$steps, new$steps, new$ids),
         tooltips = list(before = tips(base$tips_before, new$tips_before),
                         after = tips(base$tips_after, new$tips_after)),
         # The files that differ (by name), if there are as many of them
         files_different = if (NROW(fb) == NROW(fn)) {
           unique(fn$name[!(fb$name == fn$name & fb$type == fn$type & fb$size == fn$size &
                              fb$hash == fn$hash)])
         },
         errors = list(baseline = base$errors, working = new$errors))
  }

  print_summary <- function(r, baseline, kept) {
    w <- r$static$widgets
    cat("\nThe working tree against ", baseline, "\n\n", sep = "")
    cat("HTML (for information, as refactoring may change the code):\n")
    cat(sprintf("  charts in both: %d of %d, their data the same in %d\n",
                sum(w$in_both), nrow(w), sum(w$data_same, na.rm = TRUE)))
    cat(sprintf("  options whose JavaScript changed: %d, render hooks whose code changed: %d\n",
                sum(w$js_changed, na.rm = TRUE), sum(w$hooks_changed, na.rm = TRUE)))
    cat(sprintf("  the data of the render hooks the same in %d charts\n",
                sum(w$hooks_data_same, na.rm = TRUE)))
    d <- r$static$rest_difference
    cat("  the page around the charts (but for its scripts):",
        if (r$static$rest_same) "the same\n" else
          paste0("different, first in line ", d$line, ":\n    ", baseline, ":      ...",
                 d$baseline, "...\n    working tree: ...", d$working, "...\n"))
    cat("\nIn a browser:\n")
    labels <- c(charts = "the charts drawn", tooltips_before = "the tooltips",
                tooltips_after = "the tooltips after the interactions",
                interactions = "the state after each interaction",
                options = "the options of the charts after each",
                downloads = "the downloaded files", errors = "the errors in the browser")
    for (k in names(r$checks)) {
      cat(sprintf("  %-38s %s%s\n", labels[[k]], if (r$checks[[k]]) "the same" else "DIFFERENT",
                  if (k == "options" && !r$strict) " (for information)" else ""))
    }
    cat(sprintf("  (%d charts, %d steps with %d distinct states, %d files downloaded)\n",
                r$charts, r$steps, r$distinct_states, r$files))
    d <- r$differences
    if (NROW(d)) {
      cat(sprintf("  %d steps differ, the first after %s: %s\n", nrow(d), d$step[1], d$what[1]))
    }
    for (k in c("before", "after")) {
      if (length(r$tooltips[[k]])) {
        cat("  the tooltips ", k, " the interactions differ in: ",
            paste(r$tooltips[[k]], collapse = ", "), "\n", sep = "")
      }
    }
    if (length(r$files_different)) {
      cat("  the files that differ:", paste(r$files_different, collapse = "; "), "\n")
    }
    if (length(r$errors$working)) {
      cat("  errors in the working tree's page:", paste(r$errors$working, collapse = "; "), "\n")
    }
    cat("\n", if (r$pass) "PASS: the working tree works as " else "FAIL: the working tree doesn't work as ",
        baseline, " does\n", sep = "")
    if (!is.null(kept)) cat("The rendered pages are in ", kept, "\n", sep = "")
  }

  # The test: see the top of this file. `page` is the page compared, `wait` the time
  # waited after each interaction (in ms), and `out_dir` the folder of the files of
  # the test, deleted at the end unless `keep` is TRUE. With `strict = FALSE`, the
  # options of the charts may differ, as long as the charts show the same (as when
  # their data are organised differently): the options are then compared for
  # information only. Returns the results invisibly: `pass`, whether the two work the
  # same in the browser, `checks`, the comparisons that tell it, and the details
  # (`differences`, the steps after which the two differ, and what differs then;
  # `tooltips`, the charts whose tooltips differ; `files_different`; `static`, the
  # comparison of the HTML; and `runs`, what was recorded in each browser)
  function(baseline = "HEAD", page = "kiadas.qmd", wait = 1500,
           out_dir = tempfile("kiadas-regression-"), keep = FALSE, strict = TRUE) {
    if (!file.exists("_quarto.yml") || !file.exists(page)) {
      stop("Run this from the folder of the project, with _quarto.yml and ", page, " in it")
    }
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_dir <- normalizePath(out_dir, winslash = "/")
    if (!keep) on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

    message("Rendering ", page, " as in ", baseline, "...")
    base_html <- render_page(export_commit(baseline, file.path(out_dir, "baseline")), page,
                             file.path(out_dir, "site_baseline"))
    message("Rendering ", page, " as in the working tree...")
    new_html <- render_page(getwd(), page, file.path(out_dir, "site_working"))
    message("Comparing their HTML...")
    static <- compare_html(base_html, new_html)

    message("Comparing how they work in a browser (a few minutes)...")
    chrome <- chromote::Chromote$new()
    on.exit(chrome$close(), add = TRUE, after = FALSE)
    base <- browser_run(base_html, chrome, wait)
    new <- browser_run(new_html, chrome, wait)

    result <- c(compare_runs(base, new, strict),
                list(static = static, runs = list(baseline = base, working = new)))
    print_summary(result, baseline, if (keep) out_dir)
    invisible(result)
  }
})

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  result <- regression_test(if (length(args) && args[[1]] != "--loose") args[[1]] else "HEAD",
                            strict = !"--loose" %in% args)
  quit(status = if (result$pass) 0L else 1L)
}
