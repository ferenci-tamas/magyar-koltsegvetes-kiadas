// The parts that the charts of charts.R share: their description for screen
// readers (chartDescription), the choice of the column they plot (columnChoice),
// and their layout, with their title and legends (chartLayout).
// Each is the render hook of the function of charts.R that it's named after
// (add_chart_layout() and so on), which describes what it does: htmlwidgets
// calls it with the chart's element `el`, its data `x`, and the hook's `data`

// The render hook of add_chart_description()
kiadas.chartDescription = function(el, x, data) {
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
    var label = parts.filter(Boolean).join('. ').replace(/\n/g, ' ') + '.';
    // The chart is also redrawn on hover
    if (label !== el.getAttribute('aria-label')) el.setAttribute('aria-label', label);
  });
};

// The render hook of add_column_choice()
kiadas.columnChoice = function(el, x, data) {
  var chart = echarts.getInstanceByDom(el);
  var choice = document.getElementById(data.id);
  var main = data.mainId && document.getElementById(data.mainId);
  var inputs = Array.from(choice.querySelectorAll('input'));
  // The column chosen with the buttons, kept while it isn't offered
  var chosen = choice.querySelector('input:checked').value;

  // At first, and then on each change (`changed`), after hiding the tooltip (see
  // kiadas.hideTip()), with ECharts' lazyUpdate, as the main group dropdown also
  // changes the data (see kiadas.onFilter())
  function update(changed) {
    if (changed) kiadas.hideTip(chart);
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
    chart.setOption(options, { lazyUpdate: changed });
    // For a heatmap, which colours its cells by the column (see
    // add_category_heatmap())
    el.dispatchEvent(new CustomEvent('column', { detail: { column: column, lazy: changed } }));
  }

  choice.addEventListener('change', function() {
    chosen = choice.querySelector('input:checked').value;
    update(true);
  });
  if (main) main.addEventListener('change', function() { update(true); });
  update(false);
};

// The render hook of add_chart_layout()
kiadas.chartLayout = function(el, x, data) {
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
  // (see add_chart_layout()). Only redone when the names or the width of the
  // chart change
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
        id: 'category', text: lines.join('\n'), left: 10, top: 5,
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

  // Whenever the choices, the rows shown (see kiadas.showView()) or the size of
  // the window change, once in the next frame, however many of these happen
  // before it (one choice changes the controls that depend on it one after the
  // other), so that it reads the legend once the rows shown have changed which
  // items it has
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
  el.addEventListener('filter', schedule);
  window.addEventListener('resize', schedule);
  // And at once, so that the chart is laid out before it's first shown
  update();
};
