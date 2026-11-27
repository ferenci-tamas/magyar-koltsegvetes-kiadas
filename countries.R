# The countries of the international comparisons, the member states of the EU, and
# the groups of them whose averages the charts also show, from countries.csv: a row
# for each country (in the EU's protocol order), with its code in Eurostat's data
# (geo), its Hungarian name (name), and in each column after that, whether it's a
# member of the group of that code (TRUE or FALSE). data_prepare.R, kiadas.qmd (for
# charts.R) and fuggelek.qmd all source this, so the countries and their groups are
# only defined there
CountryTable <- fread("countries.csv", encoding = "UTF-8")

# The codes of the members of each group of countries, by the group's code, both in
# the order of the table. (data_prepare.R downloads the data of the group EU27)
country_groups <- local({
  groups <- setdiff(names(CountryTable), c("geo", "name"))
  stopifnot(!anyDuplicated(CountryTable$geo), !anyNA(CountryTable),
            all(vapply(groups, \(g) is.logical(CountryTable[[g]]), NA)))
  sapply(groups, \(g) CountryTable$geo[CountryTable[[g]]], simplify = FALSE)
})
