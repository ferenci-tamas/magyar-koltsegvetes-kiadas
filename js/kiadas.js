// The page's own JavaScript: the `kiadas` object that the charts of kiadas.qmd
// and their filters use (see charts.R, which makes them). This file makes it, with
// what the charts share; the other files of js/ add the code of their parts to it
// (see kiadas_dependency in charts.R, which loads them in the head of the page, in
// order, so that they're all there before the charts are rendered, and before the
// scripts of the filters run)
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
  // The data read so far from the elements of JSON of the page, by their ids (see
  // json() below)
  var parsed = {};
  // What the controls of each chart of the Hungarian categories leave of its data, by
  // the chart's element (see showView() below)
  var views = new WeakMap();

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
    // Tells the parts of the chart `el` what its controls leave of its data: `view`,
    // with the rows of each of its groups (each part, type of data or category, in
    // the order of its datasets), `groups`, all of them in that order, `rows`, and in
    // relative mode the base year, `baseYear` (else null). The chart's data hook,
    // kiadas.categoryData() or kiadas.planActualData(), calls this whenever they
    // change, once it has drawn them. The rows are those of the data, not divided by
    // the base year, nor without the years switched off in a legend
    showView: function(el, view) {
      views.set(el, view);
      el.dispatchEvent(new CustomEvent('filter', { detail: view }));
    },
    // Calls fn with what the controls of the chart `el` leave of its data (see
    // showView()): at once, if the chart has been drawn, and whenever it changes.
    // Then fn also gets `true`, as several parts of the chart change it at once, so
    // they set its options with ECharts' lazyUpdate, which draws it only once, in the
    // next frame (still before the browser shows it), and not after each of them
    onFilter: function(el, fn) {
      if (views.has(el)) fn(views.get(el), false);
      el.addEventListener('filter', function(e) { fn(e.detail, true); });
    },
    // Hides the tooltip and the axis pointer of `chart`, and ends any highlighting
    // of its points, as the mouse leaving the chart does. Called whenever its
    // controls change what it shows, before it changes: on touch screens, nothing
    // leaves the chart, so the tooltip of the point last tapped would stay, showing
    // what was there before, and so would the highlighting (which ECharts even keeps
    // on the points it reuses for the new data, if the axis pointer is shown)
    hideTip: function(chart) {
      chart.dispatchAction({ type: 'hideTip' });
      chart.dispatchAction({ type: 'updateAxisPointer', currTrigger: 'leave' });
      chart.dispatchAction({ type: 'downplay' });
    },
    // The size of the symbols of the series of the negative parts of a stack (see
    // add_category_lines() in charts.R): 4 where the part is below 0, else 0 (as at
    // the zeros around it). `value` is the point's row of data
    negativeSymbolSize: function(value, params) {
      return value[params.dimensionNames[params.encode.y[0]]] < 0 ? 4 : 0;
    },
    // The data in the element of JSON with the id `id` (see json_element() in
    // charts.R), which several charts or filters may share: parsed when first
    // needed, and then kept for the others
    json: function(id) {
      parsed[id] = parsed[id] || JSON.parse(document.getElementById(id).textContent);
      return parsed[id];
    },
    // The labels of a value axis, in Hungarian, with all their decimals:
    // toLocaleString keeps only 3 by default, too few for the ticks of the smallest
    // categories
    axisNumber: function(value) {
      return value.toLocaleString('hu-HU', { maximumFractionDigits: 20 });
    },
    // The formatter of the labels of an axis of categories that sets the label of
    // the category `name` (none if it's null) in the rich text style `highlight` of
    // the labels (see highlight_axis_label() in charts.R), and the others as they are
    highlightLabel: function(name) {
      return function(value) { return value === name ? '{highlight|' + value + '}' : value; };
    },
    // The end of the y axis of stacked parts of a whole of 100% (see add_part_lines()
    // in charts.R): 100 if the top is above it by less than 0.5% of the axis (from
    // rounding errors, or from negative parts too small to see), else ECharts' own
    // (null)
    stackMax: function(value) {
      var range = value.max - Math.min(value.min, 0);
      return value.max > 100 && value.max - 100 < 0.005 * range ? 100 : null;
    },
    // The start of the y axis of stacked parts (see add_part_lines() in charts.R): 0
    // if the bottom is below it by less than 0.5% of the axis (from negative parts
    // too small to see), else ECharts' own (null)
    stackMin: function(value) {
      return value.min < 0 && -value.min < 0.005 * (value.max - value.min) ? 0 : null;
    },
    // The end of the y axis of percentages of a whole (see percent_axis_max() in
    // charts.R): 100 if ECharts' own end would be beyond it while no value is, else
    // ECharts' own (null). A value above 100 is left to ECharts, unless the values
    // are `stacked`, when the end is as stackMax() makes it
    percentAxisMax: function(stacked) {
      return function(value) {
        // ECharts' own end, for the values shown: a value axis includes 0, and its
        // step is a fifth of its range, rounded by ECharts' nice() to 1, 2, 3 or 5
        // times a power of 10
        var lo = Math.min(value.min, 0), hi = Math.max(value.max, 0);
        if (value.max > 100) return stacked ? kiadas.stackMax(value) : null;
        var step = echarts.number.nice((hi - lo) / 5, true);
        return Math.ceil(Number((hi / step).toPrecision(12))) * step > 100 ? 100 : null;
      };
    },
    // The formatter of a tooltip of values (for ECharts' tooltip.formatter), made
    // from `cfg` (see line_tooltip() in charts.R): a title, then a line for each
    // point hovered (or for the item hovered) with a value, with its marker, its
    // name and, in bold, its value, with at most cfg.digits decimals (or two
    // significant digits if that's more precise, so that small values aren't
    // rounded to 0), followed by cfg.unit. The points without a value (null here,
    // NA in R, as in the plan for the years without a deflator) are left out, as
    // Number() would show them as 0, and there's no tooltip if none has one. The
    // value, the title (of the first point) and the names are the properties of a
    // point at the paths cfg.value, cfg.title and cfg.label (as 'value.1', the
    // second element of its data, or 'value.country', a field of its data row). By
    // default, a point's name is its 'name' (as for the title) and its 'seriesName',
    // and its value is that of the column its series plots: from a dataset, the
    // data of a point is its whole row as an object, not [x, y]
    lineTooltip: function(cfg) {
      var format = new Intl.NumberFormat('hu-HU', {
        maximumFractionDigits: cfg.digits, maximumSignificantDigits: 2,
        roundingPriority: 'morePrecision'
      });
      function at(p, path) {
        return path.split('.').reduce(function(o, key) { return o[key]; }, p);
      }
      function value(p) {
        return cfg.value ? at(p, cfg.value) : p.value[p.dimensionNames[p.encode.y[0]]];
      }
      return function(params) {
        params = [].concat(params).filter(function(p) { return value(p) != null; });
        if (!params.length) return '';
        return at(params[0], cfg.title || 'name') + '<br>' + params.map(function(p) {
          return p.marker + ' ' + at(p, cfg.label || 'seriesName') +
            ': <b style="white-space: nowrap">' + format.format(Number(value(p))) +
            cfg.unit + '</b>';
        }).join('<br>');
      };
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
    // (it would mix up the old and the new ones, if the group were replaced). With
    // `lazy`, it's drawn with ECharts' lazyUpdate (see onFilter())
    colorScale: function(chart, scale) {
      var option = chart.getOption(), label = option.yAxis[0].axisLabel;
      var font = { fill: label.color, fontSize: label.fontSize,
        fontFamily: option.textStyle.fontFamily };
      var context = document.createElement('canvas').getContext('2d');
      context.font = font.fontSize + 'px ' + font.fontFamily;
      var drawn = false;
      return function(ticks, lazy) {
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
        }, { lazyUpdate: !!lazy });
      };
    }
  };
})();
