// The download menu of the charts: the render hook of add_download_menu() in
// charts.R, which describes what it saves. htmlwidgets calls it with the chart's
// element `el`, its data `x`, and the hook's `data`
kiadas.downloadMenu = function(el, x, data) {
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
};
