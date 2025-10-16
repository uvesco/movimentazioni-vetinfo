converti_coord <- function(df,
													 utm_datum = c("ED50","WGS84"),
													 preferenza = c("ll","gb","utm")) {
	stopifnot(requireNamespace("sf", quietly = TRUE))
	stopifnot(requireNamespace("dplyr", quietly = TRUE))
	stopifnot(requireNamespace("stringr", quietly = TRUE))
	
	utm_datum   <- match.arg(utm_datum)
	preferenza  <- match.arg(preferenza, several.ok = TRUE)
	
	library(sf); library(dplyr); library(stringr)
	
	# Copia con id riga
	base <- df %>%
		mutate(rowid = dplyr::row_number())
	
	# --- 1) Originali ll (si assume WGS84 già in gradi) -------------------------
	orig_wgs <- base %>%
		transmute(
			rowid,
			lon_wgs84_orig = suppressWarnings(as.numeric(longitudine)),
			lat_wgs84_orig = suppressWarnings(as.numeric(latitudine))
		)
	
	# --- 2) Gauss–Boaga (Monte Mario) ------------------------------------------
	# fuso_gb: "O" -> EPSG:3003, "E" -> EPSG:3004
	gb_base <- base %>%
		filter(!is.na(nord_gb), !is.na(est_gb), !is.na(fuso_gb)) %>%
		mutate(
			fuso_gb_norm = str_to_upper(str_sub(str_trim(fuso_gb), 1, 1)),
			epsg_gb = dplyr::case_when(
				fuso_gb_norm == "O" ~ 3003L,
				fuso_gb_norm == "E" ~ 3004L,
				TRUE ~ NA_integer_
			)
		) %>%
		filter(!is.na(epsg_gb))
	
	gb_wgs_out <- tibble(rowid = integer(), lon_wgs84_from_gb = numeric(), lat_wgs84_from_gb = numeric())
	if (nrow(gb_base) > 0) {
		gb_split <- split(gb_base, gb_base$epsg_gb)
		gb_list  <- lapply(names(gb_split), function(epsg) {
			sfobj <- st_as_sf(gb_split[[epsg]], coords = c("est_gb", "nord_gb"),
												crs = as.integer(epsg), remove = FALSE)
			st_transform(sfobj, 4326) %>%
				mutate(
					lon_wgs84_from_gb = st_coordinates(.)[,1],
					lat_wgs84_from_gb = st_coordinates(.)[,2]
				) %>%
				st_drop_geometry() %>%
				select(rowid, lon_wgs84_from_gb, lat_wgs84_from_gb)
		})
		gb_wgs_out <- dplyr::bind_rows(gb_list)
	}
	
	# --- 3) UTM (ED50 di default; opz. WGS84) -----------------------------------
	# fuso_utm: "O" -> zona 32N ; "E" -> zona 33N
	crs32 <- if (utm_datum == "ED50") 23032L else 32632L
	crs33 <- if (utm_datum == "ED50") 23033L else 32633L
	
	utm_base <- base %>%
		filter(!is.na(x_utm), !is.na(y_utm), !is.na(fuso_utm)) %>%
		mutate(
			fuso_utm_norm = str_to_upper(str_sub(str_trim(fuso_utm), 1, 1)),
			epsg_utm = dplyr::case_when(
				fuso_utm_norm == "O" ~ crs32,  # 32 = Ovest
				fuso_utm_norm == "E" ~ crs33,  # 33 = Est
				TRUE ~ NA_integer_
			)
		) %>%
		filter(!is.na(epsg_utm))
	
	utm_wgs_out <- tibble(rowid = integer(), lon_wgs84_from_utm = numeric(), lat_wgs84_from_utm = numeric())
	if (nrow(utm_base) > 0) {
		utm_split <- split(utm_base, utm_base$epsg_utm)
		utm_list  <- lapply(names(utm_split), function(epsg) {
			sfobj <- st_as_sf(utm_split[[epsg]], coords = c("x_utm", "y_utm"),
												crs = as.integer(epsg), remove = FALSE)
			st_transform(sfobj, 4326) %>%
				mutate(
					lon_wgs84_from_utm = st_coordinates(.)[,1],
					lat_wgs84_from_utm = st_coordinates(.)[,2]
				) %>%
				st_drop_geometry() %>%
				select(rowid, lon_wgs84_from_utm, lat_wgs84_from_utm)
		})
		utm_wgs_out <- dplyr::bind_rows(utm_list)
	}
	
	# --- 4) Unione e scelta finale ----------------------------------------------
	out <- base %>%
		left_join(orig_wgs,  by = "rowid") %>%
		left_join(gb_wgs_out, by = "rowid") %>%
		left_join(utm_wgs_out, by = "rowid")
	
	# priorità selezione (di default: ll > gb > utm)
	pick_lon <- c("lon_wgs84_orig", "lon_wgs84_from_gb", "lon_wgs84_from_utm")
	pick_lat <- c("lat_wgs84_orig", "lat_wgs84_from_gb", "lat_wgs84_from_utm")
	# riordina secondo 'preferenza'
	ord <- match(preferenza, c("ll","gb","utm"))
	pick_lon <- pick_lon[ord]; pick_lat <- pick_lat[ord]
	
	out <- out %>%
		mutate(
			lon = dplyr::coalesce(!!!rlang::syms(pick_lon)),
			lat = dplyr::coalesce(!!!rlang::syms(pick_lat)),
			sorgente_coord = dplyr::case_when(
				!is.na(!!rlang::sym(pick_lat[1])) ~ preferenza[1],
				length(preferenza) >= 2 & !is.na(!!rlang::sym(pick_lat[2])) ~ preferenza[2],
				length(preferenza) >= 3 & !is.na(!!rlang::sym(pick_lat[3])) ~ preferenza[3],
				TRUE ~ NA_character_
			)
		)
	
	# --- 5) Avvisi diagnostici ---------------------------------------------------
	n_non <- sum(is.na(out$lat) | is.na(out$lon))
	if (n_non > 0) message("Righe senza WGS84 finale: ", n_non)
	if (utm_datum == "ED50") {
		message("UTM trattato come ED50 (EPSG 23032/23033). Se serve WGS84, usa utm_datum='WGS84'.")
	}
	
	out
}
