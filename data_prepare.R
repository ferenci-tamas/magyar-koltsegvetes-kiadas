library(data.table)

source("aggregates.R")

BridgeData <- fread("BridgeData.csv")
CofogName <- fread("cofog-kod-nev.csv")

geos <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", 
          "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", 
          "PT", "RO", "SI", "SK", "FI", "SE", "IS", "NO", "CH", "UK", "ME",
          "MK", "AL", "RS", "TR", "XK", "LI", "BA", "MD", "UA")

################################################################

HunData <- readRDS(url("https://github.com/ferenci-tamas/magyar-koltsegvetes-adatbazis/raw/refs/heads/main/magyar-koltsegvetes-funkcionalis-kiadasok.rds"))
saveRDS(HunData, "HunData.rds")
HunData <- rbind(
  HunData,
  data.table(doc_year = 2016, doc_type = "Zarszamadas", row = 7, level = "subgroup",
             huncode = "F01.e", hunname = "Műszaki fejlesztés",
             column = c("2015. évi teljesítés", "2016. évi eredeti előirányzat",
                        "2016. évi törvényi módosított", "2016. évi teljesítés"),
             year = c(2015, 2016, 2016, 2016),
             type = c("actual", "original_plan", "modified_plan", "actual"),
             value = 0),
  data.table(doc_year = 2027, doc_type = "Tervezet", row = 7, level = "subgroup",
             huncode = "F01.e", hunname = "Műszaki fejlesztés",
             column = "2027. évi előirányzat", year = 2027,
             type = "original_plan", value = 0)
)

HunName <- unique(HunData[level %in% c("main_group", "subgroup"), .(hunMagyar = hunname, huncode)])

HunData <- rbind(
  HunData[doc_type == "Zarszamadas" & type == "original_plan" & year == doc_year & level %in% c("main_group", "subgroup"), .(year, type, level, huncode, value)],
  HunData[year == 2026 & type == "original_plan" & year == doc_year & level %in% c("main_group", "subgroup"), .(year, type, level, huncode, value)],
  HunData[year == 2027 & type == "original_plan" & year == doc_year & level %in% c("main_group", "subgroup"), .(year, type, level, huncode, value)],
  HunData[doc_type == "Zarszamadas" & type == "actual" & year == doc_year & level %in% c("main_group", "subgroup"), .(year, type, level, huncode, value)],
  HunData[doc_type == "Zarszamadas" & type == "actual" & year == 2002 & level %in% c("main_group", "subgroup"), .(year, type, level, huncode, value)]
)
# HunData[is.na(value)]
HunData[is.na(value) & huncode == "F01.e", value := 0]

HunData <- merge(HunData, BridgeData, by = "huncode", sort = FALSE, all.x = TRUE)
HunData[, geo := "HU"]

HunData <- rbind(HunData, aggregate_rows(HunData, from = "huncode"))

HunData <- merge(HunData, HunData[!is.na(huncode) & level == "main_group", .(total = sum(value)), .(year, type)], by = c("year", "type"), sort = FALSE)
HunData[, huncodeParent := substring(huncode, 1, 3)]
HunData[, cofogParent := substring(cofog, 1, 4)]

HunData <- merge(HunData, HunData[!is.na(huncode) & level == "main_group", .(totalParent = value, year, type, huncodeParent)],
                 by = c("year", "type", "huncodeParent"), sort = FALSE, all.x = TRUE)

################################################################

ESData <- data.table(eurostat::get_eurostat(
  "gov_10a_exp", filters = list(freq = "A", unit = "MIO_NAC", sector = "S13", na_item = "TE", geo = geos), cache = FALSE)
)
saveRDS(ESData, "ESData.rds")
ESData <- ESData[freq == "A" & unit == "MIO_NAC" & sector == "S13" & na_item == "TE" & cofog99 != "TOTAL" & geo %in% geos & !is.na(values),
                 .(cofog = cofog99, year = year(time), type = "actual_cofog", value = values, geo)]

ESData[, level := ifelse(nchar(cofog) == 4, "main_group", "subgroup")]
ESData <- merge(ESData, BridgeData, by = "cofog", sort = FALSE, all.x = TRUE)

ESData <- rbind(ESData, aggregate_rows(ESData, from = "cofog"))

ESData <- merge(ESData, ESData[!is.na(cofog) & level == "main_group", .(total = sum(value)), .(geo, year, type)],
                by = c("geo", "year", "type"), sort = FALSE)
ESData[, cofogParent := substring(cofog, 1, 4)]
ESData[, huncodeParent := substring(huncode, 1, 3)]

ESData <- merge(ESData, ESData[!is.na(cofog) & level == "main_group", .(totalParent = value, geo, year, type, cofogParent)],
                by = c("geo", "year", "type", "cofogParent"), sort = FALSE, all.x = TRUE)

################################################################

RawData <- rbind(HunData, ESData)

RawData <- merge(RawData, CofogName, by = "cofog", sort = FALSE, all.x = TRUE)
RawData <- merge(RawData, HunName, by = "huncode", sort = FALSE, all.x = TRUE)

RawData[, name := ifelse(!is.na(huncode), hunMagyar, cofogMagyar)]
RawData[, hunMagyar := NULL]
RawData[, cofogMagyar := NULL]

################################################################

InflData <- data.table(eurostat::get_eurostat("prc_hicp_aind", filters = list(freq = "A", unit = "RCH_A_AVG", coicop = "CP00", geo = geos), cache = FALSE))
saveRDS(InflData, "InflData.rds")
InflData <- InflData[freq == "A" & unit == "RCH_A_AVG" & coicop == "CP00" & geo %in% geos & !is.na(values),
                     .(geo, year = year(time), inflation = 1 + values / 100)]
stopifnot(nrow(InflData[, .(unique(diff(year))), .(geo)][V1 != 1]) == 0)
InflData <- rbind(InflData[, .(year = min(year) - 1, inflation = 1), .(geo)], InflData)
if(nrow(InflData[geo == "HU" & year == 2026]) == 0) InflData <- rbind(InflData, data.table(geo = "HU", year = 2026, inflation = 1.017))
if(nrow(InflData[geo == "HU" & year == 2027]) == 0) InflData <- rbind(InflData, data.table(geo = "HU", year = 2027, inflation = 1.030))
InflData <- InflData[order(geo, year)]
InflData[, inflation := cumprod(inflation), .(geo)]
InflData[, inflation := inflation[match(2025, year)] / inflation, .(geo)]

NAMA10Data <- data.table(eurostat::get_eurostat("nama_10_gdp", filters = list(freq = "A", na_item = c("B1GQ", "P3_S13"), geo = geos), cache = FALSE))
saveRDS(NAMA10Data, "NAMA10Data.rds")

DeflatorData <- dcast(NAMA10Data[freq == "A" & unit == "PD_PCH_PRE_NAC" & na_item %in% c("B1GQ", "P3_S13") & geo %in% geos & !is.na(values)], freq + unit + geo + time ~ na_item, value.var = "values")
DeflatorData <- DeflatorData[, .(geo, year = year(time), deflatorGDP = 1 + B1GQ / 100, deflatorP3S13 = 1 + P3_S13/100)]
stopifnot(nrow(DeflatorData[, .(unique(diff(year))), .(geo)][V1 != 1]) == 0)
DeflatorData <- rbind(DeflatorData[, .(year = min(year) - 1, deflatorGDP = 1, deflatorP3S13 = 1), .(geo)], DeflatorData)
if(nrow(DeflatorData[geo == "HU" & year == 2026]) == 0) DeflatorData <- rbind(DeflatorData, data.table(geo = "HU", year = 2026, deflatorGDP = 1.040, deflatorP3S13 = NA))
if(nrow(DeflatorData[geo == "HU" & year == 2027]) == 0) DeflatorData <- rbind(DeflatorData, data.table(geo = "HU", year = 2027, deflatorGDP = 1.034, deflatorP3S13 = NA))
DeflatorData <- DeflatorData[order(geo, year)]
DeflatorData[, deflatorGDP := cumprod(deflatorGDP), .(geo)]
DeflatorData[, deflatorP3S13 := cumprod(deflatorP3S13), .(geo)]
DeflatorData[, deflatorGDP := deflatorGDP[match(2025, year)] / deflatorGDP, .(geo)]
DeflatorData[, deflatorP3S13 := deflatorP3S13[match(2025, year)] / deflatorP3S13, .(geo)]

GDPData <- NAMA10Data[freq == "A" & unit == "CP_MNAC" & na_item == "B1GQ" & geo %in% geos & !is.na(values),
                      .(geo, year = year(time), gdp = values)]
if(nrow(GDPData[geo == "HU" & year == 2026]) == 0) GDPData <- rbind(GDPData, data.table(geo = "HU", year = 2026, gdp = 92303000))
if(nrow(GDPData[geo == "HU" & year == 2027]) == 0) GDPData <- rbind(GDPData, data.table(geo = "HU", year = 2027, gdp = 97716000))

RawData <- merge(RawData, InflData, by = c("geo", "year"), sort = FALSE, all.x = TRUE)
RawData <- merge(RawData, DeflatorData, by = c("geo", "year"), sort = FALSE, all.x = TRUE)
RawData <- merge(RawData, GDPData, by = c("geo", "year"), sort = FALSE, all.x = TRUE)

################################################################

RawData <- merge(
  RawData,
  # The label of each type of data, a factor so that the charts list the types
  # in this order
  data.table(type = c("original_plan", "actual", "actual_cofog"),
             typeLabel = c("Tervezet (pénzforgalmi)", "Tényleges (pénzforgalmi)",
                           "Tényleges (eredményszemléletű)"))[
                             , typeLabel := factor(typeLabel, levels = typeLabel)],
  by = "type", sort = FALSE)

RawData[, nameLabel := paste0(name, " (Magyar: ", code_label(huncode, cofog),
                              ", COFOG: ", code_label(cofog, huncode), ")")]
RawData[, nameLabel := factor(nameLabel,
                              levels = unique(RawData[order(huncode, cofog)]$nameLabel))]

RawData[, prop := value / total * 100]
RawData[, propParent := value / totalParent * 100]
RawData[, value := value / 1000]
RawData[, propgdp := value / (gdp / 1000) * 100]
RawData[, valuedeflGDP := value * deflatorGDP]
RawData[, valuedeflP3S13 := value * deflatorP3S13]
RawData[, valueinfl := value * inflation]

RawData[, yearchr := as.character(year)]
RawData <- RawData[order(year)]

################################################################

saveRDS(RawData, "RawData.rds")