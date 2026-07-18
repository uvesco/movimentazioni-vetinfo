# =============================================================================
# utils_eml.R
# Costruzione di file .eml (MIME) con corpo HTML e allegati opzionali.
#
# Il file .eml generato include l'header "X-Unsent: 1": Outlook lo apre come
# bozza modificabile pronta all'invio; in Thunderbird usare "Modifica come
# nuovo messaggio". Tutte le parti sono codificate base64 (righe da 76 char,
# terminatori CRLF come da RFC 2045).
# =============================================================================

# Codifica base64 con a-capo CRLF ogni 76 caratteri.
# `what` puo' essere un percorso file o un vettore raw.
.eml_base64 <- function(what) {
	base64enc::base64encode(what, linewidth = 76L, newline = "\r\n")
}

# Oggetto con encoded-word RFC 2047 (sicuro per accenti/UTF-8)
.eml_subject <- function(subject) {
	paste0("=?UTF-8?B?", base64enc::base64encode(charToRaw(enc2utf8(subject))), "?=")
}

# Costruisce il contenuto di un file .eml.
# - subject: oggetto (testo semplice, viene codificato RFC 2047)
# - html_body: corpo HTML completo (stringa)
# - attachments: lista di liste con campi `path` (file esistente), `name`
#   (nome file mostrato) e opzionale `mime` (default: xlsx)
# Ritorna una stringa con terminatori CRLF, da scrivere con writeBin.
.build_eml <- function(subject, html_body, attachments = list()) {
	mime_xlsx <- "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

	headers <- c(
		paste0("Subject: ", .eml_subject(subject)),
		paste0("Date: ", format(Sys.time(), "%a, %d %b %Y %H:%M:%S %z", tz = "GMT")),
		"X-Unsent: 1",
		"MIME-Version: 1.0"
	)

	body_part <- c(
		"Content-Type: text/html; charset=UTF-8",
		"Content-Transfer-Encoding: base64",
		"",
		.eml_base64(charToRaw(enc2utf8(html_body)))
	)

	if (length(attachments) == 0) {
		righe <- c(headers, body_part)
	} else {
		boundary <- paste0("=_eml_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
			paste(sample(c(0:9, letters), 12, replace = TRUE), collapse = ""))
		righe <- c(
			headers,
			paste0("Content-Type: multipart/mixed; boundary=\"", boundary, "\""),
			"",
			paste0("--", boundary),
			body_part
		)
		for (att in attachments) {
			mime <- if (is.null(att$mime)) mime_xlsx else att$mime
			righe <- c(
				righe,
				paste0("--", boundary),
				paste0("Content-Type: ", mime, "; name=\"", att$name, "\""),
				paste0("Content-Disposition: attachment; filename=\"", att$name, "\""),
				"Content-Transfer-Encoding: base64",
				"",
				.eml_base64(att$path)
			)
		}
		righe <- c(righe, paste0("--", boundary, "--"))
	}

	# Le stringhe base64 contengono gia' CRLF interni: normalizza per non
	# produrre CRCRLF quando si uniscono le righe.
	testo <- paste(righe, collapse = "\r\n")
	gsub("\r\r\n", "\r\n", testo, fixed = TRUE)
}

# Scrive il .eml su file preservando i CRLF (nessuna traduzione di piattaforma)
.write_eml <- function(testo, file) {
	writeBin(charToRaw(testo), file)
}
