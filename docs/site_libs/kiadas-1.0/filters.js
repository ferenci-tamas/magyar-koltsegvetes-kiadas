// The controls of charts.R whose choices depend on other controls: the dropdowns and
// the radio buttons of filter_select_dependent() and filter_radio_dependent()
// (filterSelectDependent, filterRadioDependent, both with parentKey), and the
// controls of the charts of the countries, of country_filters() (countryFilters).
// Each is called by the
// script of the R function it's named after, as the page is read, with the values
// of the control in `cfg`; the R function describes what it does. The large ones,
// which several controls may share (the choices offered, and the subgroups), are
// the ids of elements of JSON, which kiadas.json() reads. The charts read the
// choices of their controls themselves, and follow their changes

// The current choices of the controls `parents` (radio buttons or dropdowns), joined
// as filter_select_dependent() and filter_radio_dependent() join the values of
// their parents' groups, or 'all' if there are none
kiadas.parentKey = function(parents) {
  if (!parents.length) return 'all';
  return parents.map(function(el) {
    return el.tagName === 'SELECT' ? el.value : el.querySelector('input:checked').value;
  }).join('\u001f');
};

// The script of filter_select_dependent()
kiadas.filterSelectDependent = function(cfg) {
  var choices = kiadas.json(cfg.choices), selected = cfg.selected;
  var select = document.getElementById(cfg.id);
  var parents = cfg.parents.map(function(id) { return document.getElementById(id); });

  function update() {
    var offered = choices[kiadas.parentKey(parents)] || [];
    // Keep the current choice if it's still offered, else fall back to `selected`
    var value = [select.value, selected]
      .find(function(v) { return offered.includes(v); }) || offered[0];
    select.replaceChildren.apply(select, offered.map(function(v) {
      return new Option(v, v, false, v === value);
    }));
    // So that the controls and the charts that depend on this one follow it
    select.dispatchEvent(new Event('change'));
  }

  parents.forEach(function(el) { el.addEventListener('change', update); });
  update();
};

// The script of filter_radio_dependent()
kiadas.filterRadioDependent = function(cfg) {
  var contents = cfg.contents, selected = cfg.selected;
  var fieldset = document.getElementById(cfg.id);
  var parents = cfg.parents.map(function(id) { return document.getElementById(id); });

  function value() {
    var checked = fieldset.querySelector('input:checked');
    return checked && checked.value;
  }

  function update() {
    var current = value();
    fieldset.innerHTML = contents[kiadas.parentKey(parents)] || '';
    var inputs = Array.from(fieldset.querySelectorAll('input'));
    var offered = inputs.map(function(el) { return el.value; });
    // Keep the current choice if it's still offered, else fall back to `selected`
    var choice = [current, selected]
      .find(function(v) { return offered.includes(v); }) || offered[0];
    inputs.forEach(function(el) { el.checked = el.value === choice; });
    // So that the controls and the charts that depend on this one follow it
    fieldset.dispatchEvent(new Event('change'));
  }

  parents.forEach(function(el) { el.addEventListener('change', update); });
  update();
};

// The script of country_filters()
kiadas.countryFilters = function(cfg) {
  var subgroups = kiadas.json(cfg.subgroups), parentColumns = cfg.parentColumns;
  var main = document.getElementById(cfg.mainId), sub = cfg.hasSub && document.getElementById(cfg.subId);
  // The dropdown that the buttons follow, the last one
  var last = sub || main;
  var choice = document.getElementById(cfg.choiceId);
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
};
