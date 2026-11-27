// The parts of the charts of the planned against the actual spending, of
// plan_actual_chart() and plan_actual_heatmap() in charts.R: their data
// (planActualData), and the axes (planActualAxes) and the colouring
// (planActualColors) of the scatter plot.
// Each is the render hook of the function of charts.R that it's named after
// (add_plan_actual_data() and so on), which describes what it does: htmlwidgets
// calls it with the chart's element `el`, its data `x`, and the hook's `data`

// The render hook of add_plan_actual_data()
kiadas.planActualData = function(el, x, data) {
  var chart = echarts.getInstanceByDom(el);
  // The values of each category, by year (see plan_actual_data() in charts.R)
  var values = kiadas.json(data.valuesId);
  var main = document.getElementById(data.mainId);
  var scale = data.scaleId ? document.getElementById(data.scaleId) : null;
  // The rows of each slot, and the name of the category in it (see filter()), the
  // choices of the controls they're for, and the years switched off in the legend
  // (see planActualColors())
  var groups = [], names = [], drawn = null, hidden = new Set();

  // The current choices of the main group dropdown and of the radio buttons of the
  // axes (if any)
  function choices() {
    return [main.value, scale && scale.querySelector('input:checked').value];
  }

  // The rows of each category that the main group dropdown shows, each an object
  // of its year (yearchr) and its values: of a scatter plot, those with both a plan
  // and an actual value (on log axes, with positive ones), with a key that tells
  // the points apart; of a heatmap, all of them, with the category's code. Each
  // category with rows in a slot of its own (see add_plan_actual_data()), from the
  // first, in their order, with the names of the categories in the slots (null for
  // an empty slot)
  function filter(chosen) {
    var log = chosen[1] === data.logValue;
    groups = [];
    names = [];
    (data.choices[chosen[0]] || []).forEach(function(k) {
      var code = values.codes[k], rows = [];
      values.years.forEach(function(year, j) {
        var plan = values.plan[k][j], actual = values.actual[k][j];
        if (!data.points) {
          rows.push({ yearchr: year, code: code, ratio: values.ratio[k][j],
                      difference: values.difference[k][j], plan: plan, actual: actual,
                      spending: values.spending[k][j] });
        } else if (plan != null && actual != null && (!log || plan > 0 && actual > 0)) {
          rows.push({ yearchr: year, plan: plan, actual: actual,
                      key: k * values.years.length + j });
        }
      });
      if (!rows.length) return;
      groups.push(rows);
      names.push(values.labels[k]);
    });
    while (groups.length < data.slots) {
      groups.push([]);
      names.push(null);
    }
  }

  // Draws the rows shown: of a scatter plot, those of each slot as a dataset,
  // without the years switched off in the legend, with the series of the slot named
  // after its category, and those of the empty slots after none, by their ids (see
  // kiadas.categoryData()); of a heatmap, all of them as one, in the order of the
  // years (each year's rows in the order of the categories). The first time with all
  // the options of the chart, which echarts4r hasn't drawn (see
  // add_plan_actual_data()), and later lazily, as the other parts of the chart
  // change it too (see kiadas.onFilter())
  function draw(first) {
    var sets = data.points ?
      groups.map(function(rows) {
        return rows.filter(function(row) { return !hidden.has(row.yearchr); });
      }) :
      [[].concat.apply([], groups).sort(function(a, b) {
        return a.yearchr < b.yearchr ? -1 : a.yearchr > b.yearchr ? 1 : 0;
      })];
    var option = {
      dataset: sets.map(function(rows, k) {
        return first ? Object.assign({}, x.opts.dataset[k], { source: rows }) : { source: rows };
      })
    };
    if (data.points) {
      // The first time all the series, then only those of the slots
      option.series = x.opts.series.map(function(s) {
        if (s.datasetIndex == null) return first ? s : null;
        var name = { id: s.id, name: names[s.datasetIndex] || ' ' };
        return first ? Object.assign({}, s, name) : name;
      }).filter(Boolean);
    }
    if (first) {
      chart.setOption(Object.assign({}, x.opts, option));
    } else {
      chart.setOption(option, { lazyUpdate: true });
    }
  }

  // Whenever the controls change, unless the rows they choose stay the same, first
  // hiding the tooltip (see kiadas.hideTip())
  function update(first) {
    var chosen = choices(), key = JSON.stringify(chosen);
    if (key === drawn) return;
    drawn = key;
    if (!first) kiadas.hideTip(chart);
    filter(chosen);
    draw(first);
    kiadas.showView(el, { groups: groups, names: names,
                          rows: [].concat.apply([], groups), baseYear: null });
  }
  update(true);
  main.addEventListener('change', function() { update(false); });
  if (scale) scale.addEventListener('change', function() { update(false); });
  // And whenever years are switched off or on in the legend, which leaves the rest
  // of the chart as it is
  el.addEventListener('hiddenyears', function(e) {
    hidden = e.detail;
    draw(false);
  });
};

// The render hook of add_plan_actual_axes()
kiadas.planActualAxes = function(el, x, data) {
  var chart = echarts.getInstanceByDom(el);

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

  // The radio buttons of the axes, and the points shown (see
  // kiadas.planActualData()), with those of the years switched off in the legend
  var scale = document.getElementById(data.scaleId), rows = [];
  // The options of the axes other than their type and range, as set at first:
  // ECharts replaces an axis whose type changes, dropping the rest
  var axes = ['xAxis', 'yAxis'].map(function(name) {
    var axis = chart.getOption()[name][0];
    return { name: axis.name, nameLocation: axis.nameLocation,
             nameGap: axis.nameGap, axisLabel: axis.axisLabel };
  });
  // With `lazy`, with ECharts' lazyUpdate (see kiadas.onFilter())
  function update(lazy) {
    var log = scale.querySelector('input:checked').value === data.logValue;
    var lo = Infinity, hi = -Infinity;
    rows.forEach(function(row) {
      // Log axes only show positive values (which are the only ones shown then)
      if (log && !(row.plan > 0 && row.actual > 0)) return;
      lo = Math.min(lo, row.plan, row.actual);
      hi = Math.max(hi, row.plan, row.actual);
    });
    // Unless no points are shown
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
    }, { lazyUpdate: lazy });
  }
  // Whenever the points shown change, as when the type of the axes does
  kiadas.onFilter(el, function(view, lazy) {
    rows = view.rows;
    update(lazy);
  });

  chart.setOption({
    tooltip: {
      formatter: function(p) {
        // From a dataset, p.value is the whole row of data as an object. With at
        // most one decimal, as in the description for screen readers
        function value(dim) { return p.value[p.dimensionNames[p.encode[dim][0]]]; }
        return kiadas.planActualTooltip(p.marker, p.seriesName, p.value.yearchr,
                                        value('x'), value('y'), data.labels,
                                        data.unit, 1).join('<br>');
      }
    }
  });
};

// The render hook of add_plan_actual_colors()
kiadas.planActualColors = function(el, x, data) {
  var chart = echarts.getInstanceByDom(el);
  var option = chart.getOption();
  var choice = document.getElementById(data.colorId);
  // The years of the points shown (see kiadas.planActualData()), the categories
  // that add_part_legend() lists for them, the years switched off in the legend,
  // and the year highlighted (see highlightYear()), if any
  var years = {}, legend = [], hidden = new Set(), highlighted = null;

  function byYear() { return choice.querySelector('input:checked').value === 'year'; }

  // The colour of a point: that of its category's series, or that of its year.
  // (From a dataset, p.value is the whole row of data as an object)
  function color(p) { return byYear() ? data.colors[p.value.yearchr] : p.color; }

  // With `lazy`, with ECharts' lazyUpdate (see kiadas.onFilter())
  function update(lazy) {
    var year = byYear();
    chart.setOption({
      legend: {
        data: year ? Object.keys(data.colors).filter(function(y) { return years[y]; }) :
          legend
      },
      // The series of the categories (the others have no dataset), which only
      // highlight themselves on hover when coloured by category (when coloured by
      // year, the year is highlighted instead: see below)
      series: option.series.map(function(s) {
        if (s.datasetIndex != null) {
          return { itemStyle: { color: color }, emphasis: { focus: year ? 'none' : 'series' } };
        }
        // The series of a year switched off isn't highlighted when the legend is
        // hovered, as that would fade all the points
        return data.colors[s.name] ? { emphasis: { disabled: hidden.has(s.name) } } : {};
      })
    }, { lazyUpdate: !!lazy });
  }

  kiadas.onFilter(el, function(view, lazy) {
    years = {};
    view.rows.forEach(function(row) { years[row.yearchr] = true; });
    // As add_part_legend() has just listed them, before this
    legend = chart.getOption().legend[0].data;
    update(lazy);
  });
  // The tooltip would show the colour before (see kiadas.hideTip())
  choice.addEventListener('change', function() {
    kiadas.hideTip(chart);
    update(false);
  });

  // Highlights the points of `year` and fades the others (or with `type`
  // 'downplay', ends that): highlighting the year's series fades all the points
  // (see add_plan_actual_colors()), so the points of the year are highlighted
  // too, in the same action, as each action first ends the fading of the one
  // before. (Not by
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
  // points, so the points of the years switched off are left out of the data
  // (see kiadas.planActualData())
  chart.on('legendselectchanged', function(e) {
    if (!data.colors[e.name]) return;
    // Ends the highlighting of the year clicked first, as ECharts can't end the
    // fading of the other points once they're left out
    highlightYear('downplay', e.name);
    hidden = new Set(Object.keys(data.colors).filter(function(y) {
      return e.selected[y] === false;
    }));
    el.dispatchEvent(new CustomEvent('hiddenyears', { detail: hidden }));
    // For the series of the years switched off
    update();
  });
};
