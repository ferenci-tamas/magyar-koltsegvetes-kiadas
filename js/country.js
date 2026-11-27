// The parts of the charts of the countries, of country_chart(),
// country_lines_chart(), country_heatmap() and country_stack_chart() in charts.R:
// their bars, lines, cells and columns (countryBars, countryLines,
// countryHeatmap, countryStacks).
// Each is the render hook of the function of charts.R that it's named after
// (add_country_bars() and so on), which describes what it does: htmlwidgets
// calls it with the chart's element `el`, its data `x`, and the hook's `data`

// The render hook of add_country_bars()
kiadas.countryBars = function(el, x, data) {
  var chart = echarts.getInstanceByDom(el);
  var main = document.getElementById(data.mainId);
  var sub = document.getElementById(data.subId);
  var choice = data.columnId && document.getElementById(data.columnId);
  var slider = document.getElementById(data.yearId);
  var output = document.getElementById(data.yearId + '_value');
  // The chosen year, the years offered, and whether the bars have been drawn at
  // first (after which a new year means that the controls changed it)
  var year = data.year, offered = [], ready = false;

  // The label of the chosen category
  function category() { return sub.value === 'Összesen' ? main.value : sub.value; }
  function column() {
    return choice ? choice.querySelector('input:checked').value : data.columns[0];
  }
  // The values of the chosen category in the chosen column, by year
  function values() {
    return kiadas.json(data.valuesId)[category()][column()];
  }

  // After hiding the tooltip (see kiadas.hideTip())
  slider.addEventListener('input', function() {
    if (ready) kiadas.hideTip(chart);
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
  ready = true;
};

// The render hook of add_country_lines()
kiadas.countryLines = function(el, x, data) {
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
    var byYear = kiadas.json(data.valuesId)[category()][column()];
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
  // country_filters()). After hiding the tooltip (see kiadas.hideTip())
  function change() {
    kiadas.hideTip(chart);
    update();
  }
  sub.addEventListener('change', change);
  if (choice) choice.addEventListener('change', change);
  update();

  // The colours of the lines shown, by name (see add_country_lines()): given out
  // at once, and whenever a line is switched on or off in the legend. `selected`
  // is ECharts'
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
};

// The render hook of add_country_heatmap()
kiadas.countryHeatmap = function(el, x, data) {
  var chart = echarts.getInstanceByDom(el);
  var main = document.getElementById(data.mainId);
  var sub = document.getElementById(data.subId);
  var choice = data.columnId && document.getElementById(data.columnId);
  // The colour scale (see add_country_heatmap()), which this draws with the ticks
  // it's given (see kiadas.colorScale() in js/kiadas.js)
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

  // The colouring of the cells of the values `values` (see add_country_heatmap()):
  // the pieces of the visualMap, and the ticks of the colour scale
  function coloring(values) {
    values = values.slice().sort(function(a, b) { return a - b; });
    var lo = quantile(values, 0.05), hi = quantile(values, 0.95);
    // The largest tick there may be, if any (see add_country_heatmap()), which cuts
    // off the range
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
                 label: i === 0 ? '\u2264 ' + text : i === 4 ? '\u2265 ' + text : text };
      })
    };
  }

  // Draws the cells of the chosen category in the chosen column, over the years
  // with a value for any country (as the keys of an object, the years come in
  // ascending order), row by row, so that the data files and the description
  // list the rows in their order (see add_download_menu())
  function update() {
    var byYear = kiadas.json(data.valuesId)[category()][column()];
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
  // country_filters()). After hiding the tooltip (see kiadas.hideTip())
  function change() {
    kiadas.hideTip(chart);
    update();
  }
  sub.addEventListener('change', change);
  if (choice) choice.addEventListener('change', change);
  update();
};

// The render hook of add_country_stacks()
kiadas.countryStacks = function(el, x, data) {
  var chart = echarts.getInstanceByDom(el);
  var main = document.getElementById(data.mainId);
  var choice = data.columnId && document.getElementById(data.columnId);
  var slider = document.getElementById(data.yearId);
  var output = document.getElementById(data.yearId + '_value');
  var option = chart.getOption();
  // The chosen year, the years offered, the names of the countries with a
  // column, whether each country is switched on in the legend (by name), and
  // whether the columns have been drawn at first (after which a new year means
  // that the controls or the legend changed it)
  var year = data.year, offered = [], drawn = [], on = {}, ready = false;
  var selected = option.legend[1].selected || {};
  data.countries.forEach(function(name) { on[name] = selected[name] !== false; });

  // The labels of the parts of the chosen whole, from the bottom of the stack up
  function parts() { return data.parts[main.value] || []; }
  function column() {
    return choice ? choice.querySelector('input:checked').value : data.columns[0];
  }
  // The values of each of the parts in the chosen column, by year
  function values() {
    var all = kiadas.json(data.valuesId);
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

  // Draws the columns of the chosen year (see add_country_stacks())
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

  // After hiding the tooltip (see kiadas.hideTip())
  slider.addEventListener('input', function() {
    if (ready) kiadas.hideTip(chart);
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
  ready = true;

  var format = new Intl.NumberFormat('hu-HU', {
    maximumFractionDigits: data.digits, maximumSignificantDigits: 2,
    roundingPriority: 'morePrecision'
  });
  chart.setOption({
    tooltip: {
      formatter: function(p) {
        if (p.value == null || p.value[1] == null) return '';
        return p.name + ', ' + year + '<br>' + p.marker + ' ' + p.seriesName +
          ': <b style="white-space: nowrap">' + format.format(p.value[1]) +
          data.unit + '</b>';
      }
    }
  });
};
