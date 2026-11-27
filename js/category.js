// The parts of the charts of the Hungarian categories, of category_chart() and
// plan_actual_heatmap() in charts.R: the data of category_chart() (categoryData),
// their heatmap (categoryHeatmap), and the lines and the legend of the charts of
// the parts of a whole (partLines, partLegend).
// Each is the render hook of the function of charts.R that it's named after
// (add_category_data() and so on), which describes what it does: htmlwidgets
// calls it with the chart's element `el`, its data `x`, and the hook's `data`

// The render hook of add_category_data()
kiadas.categoryData = function(el, x, data) {
  var chart = echarts.getInstanceByDom(el);
  // The rows of HURawData, the values of the columns that the chart reads, and the
  // parts, if it shows parts (see category_data() in charts.R)
  var hu = kiadas.json(data.rowsId), values = {};
  Object.keys(data.valuesIds).forEach(function(column) {
    values[column] = kiadas.json(data.valuesIds[column]);
  });
  var parts = data.partsId ? kiadas.json(data.partsId) : null;
  function control(id) { return id ? document.getElementById(id) : null; }
  var classification = control(data.classificationId), type = control(data.typeId);
  var main = control(data.mainId), sub = control(data.subId);
  // In relative mode, the slider of the base year, and the year shown beside it
  var slider = control(data.baseId), output = control(data.baseId && data.baseId + '_value');
  var years = x.opts.xAxis[0].data;
  // The rows of each slot, and the name of the part in it (see filter()), the
  // choices of the controls they're for, the years that the slider offers, and the
  // base year
  var groups = [], names = [], drawn = null, offered = years, year = data.year;

  // The value of the checked radio button of `fieldset`
  function checked(fieldset) {
    var input = fieldset.querySelector('input:checked');
    return input ? input.value : null;
  }

  // The rows of the category labelled `category` of the type of data `typeLabel`
  // (none if it has none), in the order of the years, each an object of its year
  // (yearchr) and its values in data.columns (null if missing), in a heatmap also
  // of the code of its part, `code`, and its spending (from the column value). In
  // stack mode, each column is split into its negative part, in a column of its
  // name with _neg appended, and its positive part, in the column itself
  function rowsOf(category, typeLabel, code) {
    var rows = ((hu.groups[category] || {})[typeLabel] || []).map(function(i) {
      var row = { yearchr: hu.yearchr[i] };
      if (data.heatmap) row.code = code;
      data.columns.forEach(function(column) { row[column] = values[column][i]; });
      if (data.heatmap) row.spending = values.value[i];
      return row;
    });
    if (data.stack) {
      data.columns.forEach(function(column) {
        var below = rows.map(function(row) { return row[column] != null && row[column] < 0; });
        rows.forEach(function(row, i) {
          row[column + '_neg'] = below[i] ? row[column] : below[i - 1] || below[i + 1] ? 0 : null;
          if (row[column] != null) row[column] = Math.max(row[column], 0);
        });
      });
    }
    return rows;
  }

  // The current choices of the controls that choose the rows: of the
  // classification, the type of data and the whole showing parts, else of the
  // category (the main group itself if the subgroup dropdown is at "Összesen")
  function choices() {
    return parts ? [checked(classification), checked(type), main.value] :
      [sub.value === 'Összesen' ? main.value : sub.value];
  }

  // The rows of the parts of the whole chosen (of the type chosen) that have any,
  // each in a slot of its own (see add_category_data()), from the first, in their
  // order, and the names of the parts in the slots (null for an empty slot) or,
  // without parts, the rows of each type of data of the category chosen, and the
  // names of the types
  function filter(chosen) {
    if (!parts) {
      groups = data.types.map(function(typeLabel) { return rowsOf(chosen[0], typeLabel); });
      names = data.types;
      return;
    }
    groups = [];
    names = [];
    parts.category.forEach(function(category, k) {
      if (parts.classification[k] !== chosen[0] || parts.main[k] !== chosen[2]) return;
      var rows = rowsOf(category, chosen[1], parts.code[k]);
      if (!rows.length) return;
      groups.push(rows);
      names.push(category);
    });
    while (groups.length < data.slots) {
      groups.push([]);
      names.push(null);
    }
  }

  // Takes the year at the slider as the base year, and shows it beside the slider,
  // to screen readers, and in its data-year attribute, for the title
  function choose() {
    year = offered[slider.value];
    output.textContent = year;
    slider.setAttribute('aria-valuetext', year);
    slider.dataset.year = year;
  }

  // Offers the years of the rows shown on the slider, at the base year or, if it
  // isn't among them, at the nearest one. If they have none, the slider keeps the
  // years it offers or, at first, offers all
  function offer() {
    var hasYear = {};
    groups.forEach(function(rows) {
      rows.forEach(function(row) { hasYear[row.yearchr] = true; });
    });
    var covered = years.filter(function(y) { return hasYear[y]; });
    if (!covered.length) {
      if (slider.dataset.year) return;
      covered = years;
    }
    offered = covered;
    var at = 0;
    offered.forEach(function(y, i) {
      if (Math.abs(y - year) < Math.abs(offered[at] - year)) at = i;
    });
    slider.max = offered.length - 1;
    slider.value = at;
    choose();
  }

  // The rows `rows` of a part, each divided by the part's value in the base year
  // (null if that isn't positive), in each column of data.columns
  function divide(rows) {
    var base = rows.find(function(row) { return row.yearchr === year; });
    return rows.map(function(row) {
      var ratios = {};
      data.columns.forEach(function(column) {
        ratios[column] = base && base[column] > 0 && row[column] != null ?
          row[column] / base[column] : null;
      });
      return Object.assign({}, row, ratios);
    });
  }

  // Draws the rows shown: each group as a dataset (in relative mode divided by the
  // base year) or, in a heatmap, all of them as one, in the order of the years
  // (each year's rows in the order of the parts). The first time with all the
  // options of the chart, which echarts4r hasn't drawn (see add_category_data()),
  // and later lazily, as the other parts of the chart change it too (see
  // kiadas.onFilter())
  function draw(first) {
    var sets = slider ? groups.map(divide) : groups;
    if (data.heatmap) {
      sets = [[].concat.apply([], sets).sort(function(a, b) {
        return a.yearchr < b.yearchr ? -1 : a.yearchr > b.yearchr ? 1 : 0;
      })];
    }
    var dataset = sets.map(function(rows, k) {
      return first ? Object.assign({}, x.opts.dataset[k], { source: rows }) : { source: rows };
    });
    // Showing parts as lines or stacked areas, the series of each slot are named
    // after its part, and those of the empty slots after none (a space, as ECharts
    // would make up a name for a series without one), by their ids (see
    // add_category_lines())
    var series = parts && !data.heatmap && x.opts.series.map(function(s) {
      var name = { id: s.id, name: names[s.datasetIndex] || ' ' };
      return first ? Object.assign({}, s, name) : name;
    });
    var option = series ? { dataset: dataset, series: series } : { dataset: dataset };
    if (first) {
      chart.setOption(Object.assign({}, x.opts, option));
    } else {
      chart.setOption(option, { lazyUpdate: true });
    }
    kiadas.showView(el, { groups: groups, names: names, rows: [].concat.apply([], groups),
                          baseYear: slider ? year : null });
  }

  // Whenever the controls change, unless the rows they choose stay the same (as
  // when a change is passed on to the controls that depend on it, one after the
  // other), first hiding the tooltip (see kiadas.hideTip())
  function update(first) {
    var chosen = choices(), key = JSON.stringify(chosen);
    if (key === drawn) return;
    drawn = key;
    if (!first) kiadas.hideTip(chart);
    filter(chosen);
    if (slider) offer();
    draw(first);
  }
  update(true);
  [classification, type, main, sub].forEach(function(el) {
    if (el) el.addEventListener('change', function() { update(false); });
  });
  // And whenever the base year changes
  if (slider) {
    slider.addEventListener('input', function() {
      var before = year;
      choose();
      if (year === before) return;
      kiadas.hideTip(chart);
      draw(false);
    });
  }
};

// The render hook of add_category_heatmap()
kiadas.categoryHeatmap = function(el, x, data) {
  var chart = echarts.getInstanceByDom(el);
  var option = chart.getOption(), years = option.xAxis[0].data;
  var series = chart.getModel().getSeriesByIndex(0);
  var format = new Intl.NumberFormat('hu-HU', {
    maximumFractionDigits: data.digits, maximumSignificantDigits: 2,
    roundingPriority: 'morePrecision'
  });
  var percent = new Intl.NumberFormat('hu-HU', {
    maximumFractionDigits: 1, maximumSignificantDigits: 2,
    roundingPriority: 'morePrecision'
  });
  // The colour scale (see add_category_heatmap()), which this draws with the ticks
  // it's given (see kiadas.colorScale() in js/kiadas.js)
  var scale = data.scale, drawScale = kiadas.colorScale(chart, scale);

  chart.setOption({
    // The year, the full name of the row and the value of the hovered cell, as
    // in the tooltip of the lines or, with data.planActual, as in that of a
    // plan_actual_chart(), and then the weight of the row (see relayout()).
    // (From a dataset, p.value is the whole row of data as an object)
    tooltip: {
      formatter: function(p) {
        function value(dim) { return p.value[p.dimensionNames[p.encode[dim][0]]]; }
        var row = rows[value('y')], name = data.names[value('y')];
        var lines = data.planActual ?
          kiadas.planActualTooltip(p.marker, name, value('x'), p.value.plan,
                                   p.value.actual, data.planActual, data.unit,
                                   data.digits) :
          [value('x'), p.marker + ' ' + name + ': <b style="white-space: nowrap">' +
             format.format(value('value')) + data.unit + '</b>'];
        lines.push(data.weightLabel + ': ' +
                   (row ? percent.format(100 * row.share) + '%' : ''));
        return lines.join('<br>');
      }
    }
  });

  // The rows of the data shown (as they are, not divided by the base year), the
  // base year, and the layout of the rows shown: for each code, its weight (see
  // below) and its span on the y axis
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
  // names in the legend, as the names of its series without data (see
  // add_category_heatmap()). With `lazy`, with ECharts' lazyUpdate (see
  // kiadas.onFilter())
  function relayout(lazy) {
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
    var legend = codes.map(function(code) { return data.names[code]; });
    // The series without data that give the legend its names, each named after a
    // row shown, in their order, and the others after none, by their ids (as
    // ECharts would merge a renamed series into the one with its name already)
    var legendSeries = [];
    for (var i = 0; i < data.slots; i++) {
      legendSeries.push({ id: 'legend' + i, name: legend[i] || ' ' });
    }
    chart.setOption({
      legend: { data: legend },
      yAxis: { axisLabel: {
        customValues: Array.from(labels.keys()).sort(function(a, b) { return a - b; }),
        formatter: function(value) { return labels.get(value) || ''; }
      } },
      // Redrawn in the new layout
      series: [{ id: 'heatmap', renderItem: renderItem }].concat(legendSeries)
    }, { lazyUpdate: !!lazy });
  }

  // Colours the cells by the column `column`, by pieces 1/32 wide (see
  // add_category_heatmap()), each in the colour of its middle: on the log2 scale
  // of ratios from 1/2 to 2
  // or, for a column in data.linear, on a linear scale from -M to M, where M is
  // the 90th percentile of the absolute values of the cells shown, rounded up
  // to 1, 2 or 5 times a power of 10, so that a few large values don't leave
  // the others barely tinted (the values beyond take the colours at the ends).
  // With `lazy`, with ECharts' lazyUpdate (see kiadas.onFilter())
  var column = data.columns[0];
  function restyle(lazy) {
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
                 label: i === 0 ? '\u2264 ' + text : i === 4 ? '\u2265 ' + text : text };
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
    } }, { lazyUpdate: !!lazy });
    drawScale(ticks, lazy);
  }

  // The heatmap shows only the rows of the parts shown, and the x axis only the
  // years from their first to their last (from the start, and whenever they or
  // the base year change), coloured for the values shown, with the rows laid out
  // for the weights in the base year (or summed over the years shown)
  kiadas.onFilter(el, function(view, lazy) {
    var hasYear = {};
    shown = view.rows;
    baseYear = view.baseYear;
    shown.forEach(function(r) { hasYear[r.yearchr] = true; });
    var covered = years.filter(function(year) { return hasYear[year]; });
    chart.setOption({
      xAxis: {
        data: years.slice(years.indexOf(covered[0]),
                          years.indexOf(covered[covered.length - 1]) + 1)
      }
    }, { lazyUpdate: lazy });
    restyle(lazy);
    if (baseYear !== null || !data.baseWeights) relayout(lazy);
  });
  // And recoloured whenever another column is chosen (see add_column_choice())
  el.addEventListener('column', function(e) {
    column = e.detail.column;
    restyle(e.detail.lazy);
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
};

// The render hook of add_part_lines()
kiadas.partLines = function(el, x, data) {
  var chart = echarts.getInstanceByDom(el);
  var option = chart.getOption(), years = option.xAxis[0].data;

  // Only the years from the first to the last of the rows shown (from the start,
  // and whenever they change)
  kiadas.onFilter(el, function(view, lazy) {
    var hasYear = {};
    view.rows.forEach(function(row) { hasYear[row.yearchr] = true; });
    var covered = years.filter(function(year) { return hasYear[year]; });
    chart.setOption({
      xAxis: {
        data: years.slice(years.indexOf(covered[0]),
                          years.indexOf(covered[covered.length - 1]) + 1)
      }
    }, { lazyUpdate: lazy });
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
};

// The render hook of add_part_legend()
kiadas.partLegend = function(el, x, data) {
  var chart = echarts.getInstanceByDom(el);
  var option = chart.getOption(), palette = option.color;

  kiadas.onFilter(el, function(view, lazy) {
    // The name of each series: that of the part whose dataset it plots (in a slot,
    // the part that its data hook put there), else its own
    var names = option.series.map(function(s, i) {
      var k = data.datasets[i];
      return k != null ? view.names[k] : s.name;
    });
    // The series whose dataset has rows, each name once, as in stack mode a
    // part has a series for its negative and one for its positive parts
    var legend = [];
    names.forEach(function(name, i) {
      var k = data.datasets[i];
      if (k != null && view.groups[k].length && legend.indexOf(name) < 0) legend.push(name);
    });
    var color = {};
    legend.forEach(function(name, i) {
      color[name] = palette[i % palette.length];
    });
    chart.setOption({
      // Stacked, in the order of the areas from the top of the stack down
      // (though coloured from the bottom up)
      legend: { data: option.series[0].stack ? legend.slice().reverse() : legend },
      // By name, so in stack mode both series of a part
      series: names.map(function(name) {
        return color[name] ? { color: color[name] } : {};
      })
    }, { lazyUpdate: lazy });
  });
};
