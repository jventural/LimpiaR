# Una función por paso de limpieza. Todas reciben la base, muestran en la
# consola qué hicieron y devuelven la base (para guardarla con <-).

.acentos <- "\u00e1\u00e9\u00ed\u00f3\u00fa\u00c1\u00c9\u00cd\u00d3\u00da\u00f1\u00d1\u00fc"
.sin_acentos <- "aeiouAEIOUnNu"
.simple <- function(x) tolower(trimws(chartr(.acentos, .sin_acentos, as.character(x))))

#' Leer la base
#'
#' Lee un Excel (.xlsx, .xls) o un CSV y dice cuántas filas y columnas tiene.
#'
#' @param archivo Ruta del archivo.
#' @return Un data.frame con la base, tal como llegó.
#' @examples
#' f <- system.file("extdata", "base_cruda.csv", package = "LimpiaR")
#' base <- leer(f)
#' @export
leer <- function(archivo) {
  if (!file.exists(archivo)) stop("No encuentro el archivo: ", archivo, call. = FALSE)
  ext <- tolower(tools::file_ext(archivo))
  base <- if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(archivo, sheet = 1))
  } else {
    utils::read.csv(archivo, encoding = "UTF-8", check.names = FALSE, stringsAsFactors = FALSE)
  }
  cat("La base tiene", nrow(base), "filas y", ncol(base), "columnas.\n")
  invisible(base)
}

#' Revisar la base antes de tocarla
#'
#' Para cada columna muestra cómo se escribieron sus valores (texto), de cuánto
#' a cuánto van (números) y cuántas celdas vacías tiene. No cambia nada.
#'
#' @param base La base.
#' @param columnas Nombres o números de las columnas a revisar. Si se omite, todas.
#' @return La misma base, sin cambios (invisible).
#' @examples
#' base <- leer(system.file("extdata", "base_cruda.csv", package = "LimpiaR"))
#' revisar(base, c("grupo", "sexo", "edad"))
#' revisar(base, 1:6)
#' @export
revisar <- function(base, columnas = seq_along(base)) {
  pos <- .posiciones(base, columnas)
  etiqueta <- paste0("[", pos, "] ", .corto(names(base)[pos], 28))
  ancho <- max(nchar(etiqueta))
  for (k in seq_along(pos)) {
    x <- base[[pos[k]]]
    vacios <- sum(is.na(x) | x %in% "")
    if (is.numeric(x)) {
      que <- if (all(is.na(x))) "todo vac\u00edo" else paste("n\u00fameros de", min(x, na.rm = TRUE), "a", max(x, na.rm = TRUE))
    } else {
      u <- sort(unique(x[!is.na(x) & x != ""]))
      que <- if (length(u) <= 6) paste0(length(u), " formas: ", paste(u, collapse = " | "))
             else paste0(length(u), " formas, por ejemplo: ", paste(utils::head(u, 4), collapse = " | "))
    }
    cat(formatC(etiqueta[k], width = -ancho), "  ", .corto(que, 70), if (vacios) paste0("  [", vacios, " vac\u00edos]"), "\n", sep = "")
  }
  invisible(base)
}

#' Renombrar columnas con nombres cortos
#'
#' Cambia los enunciados largos del formulario por nombres cortos, sin tildes ni
#' espacios. Una columna: `edad = 4` (o `edad = "Edad (años)"`). Varias columnas
#' de una escala: `SCS = 9:34`; si cada enunciado empieza con su número
#' ("7. Es difícil..."), se usa ese número (EG7), y si no, se numeran 1, 2, 3...
#' El enunciado completo se guarda en un diccionario que [guardar()] escribe
#' en otra hoja.
#'
#' @param base La base.
#' @param ... `nombre_nuevo = columna` (número o nombre actual), o
#'   `PREFIJO = posiciones` para una escala.
#' @return La base con los nombres nuevos.
#' @examples
#' base <- leer(system.file("extdata", "base_formulario.csv", package = "LimpiaR"))
#' base <- renombrar(base, edad = 4, sexo = 5, EG = 48:63)
#' @export
renombrar <- function(base, ...) {
  reglas <- list(...)
  if (!length(reglas) || is.null(names(reglas)) || any(names(reglas) == ""))
    stop("Escribe cada cambio as\u00ed: nombre_nuevo = columna, por ejemplo edad = 4.", call. = FALSE)
  dicc <- attr(base, "diccionario")
  nuevos <- names(base)
  for (nuevo in names(reglas)) {
    pos <- .posiciones(base, reglas[[nuevo]])
    viejos <- names(base)[pos]
    if (length(pos) == 1) {
      nombres <- nuevo
      cat(formatC(nuevo, width = -16), "\u2190 ", .corto(viejos, 50), "\n", sep = "")
    } else {
      num <- suppressWarnings(as.integer(sub("^\\s*([0-9]+).*$", "\\1", viejos)))
      usa_num <- !anyNA(num) && !anyDuplicated(num)
      nombres <- paste0(nuevo, if (usa_num) num else seq_along(pos))
      cat(formatC(nuevo, width = -16), length(pos), " columnas \u2192 ", nombres[1], " ... ", nombres[length(nombres)],
          if (usa_num) " (n\u00famero tomado del enunciado)", "\n", sep = "")
      if (usa_num && any(!seq_len(max(num)) %in% num))
        cat("                 ojo, no hay \u00edtem: ", paste(setdiff(seq_len(max(num)), num), collapse = ", "), "\n", sep = "")
    }
    nuevos[pos] <- nombres
    dicc <- rbind(dicc, data.frame(nombre = nombres, enunciado = viejos))
  }
  names(base) <- nuevos
  attr(base, "diccionario") <- dicc
  invisible(base)
}

#' Pasar texto a número
#'
#' Saca el número que hay dentro del texto: "19 años" pasa a 19, "18.0" a 18,
#' "15,7" a 15.7 y "3 - Cerca de la mitad de las veces" a 3. Lo que no tiene
#' ningún número queda vacío y se avisa.
#'
#' @param base La base.
#' @param columnas Nombre de la columna, nombres de varias o el inicio del
#'   nombre de los ítems (por ejemplo, "SCS").
#' @return La base con esas columnas en número.
#' @examples
#' base <- leer(system.file("extdata", "base_formulario.csv", package = "LimpiaR"))
#' base <- renombrar(base, edad = 4)
#' base <- numero(base, "edad")
#' @export
numero <- function(base, columnas) {
  cols <- .columnas(base, columnas)
  con_texto <- 0; sin_num <- character(0)
  for (v in cols) {
    x <- base[[v]]
    if (is.numeric(x)) next
    x <- trimws(as.character(x)); x[x %in% ""] <- NA
    n <- suppressWarnings(as.numeric(sub(",", ".", sub("^[^0-9-]*(-?[0-9]+([.,][0-9]+)?).*$", "\\1", x))))
    n[!grepl("[0-9]", x)] <- NA
    con_texto <- con_texto + sum(!is.na(x) & !grepl("^-?[0-9]+$", x))
    sin_num <- c(sin_num, x[!is.na(x) & is.na(n)])
    base[[v]] <- n
  }
  cat(if (length(cols) == 1) cols else paste(length(cols), "columnas"), ": ", con_texto,
      " valores ten\u00edan texto o decimales y pasaron a n\u00famero.\n", sep = "")
  if (length(sin_num)) cat("Ojo, sin n\u00famero (quedan vac\u00edos):", paste0("\"", unique(sin_num), "\"", collapse = ", "), "\n")
  invisible(base)
}

#' Recodificar respuestas en texto
#'
#' Cambia cada etiqueta por su número, según el instrumento. Compara sin
#' importar mayúsculas, tildes ni espacios. Ejemplo:
#' `recodificar(base, "EG", "En desacuerdo" = 2, "De acuerdo" = 4)`.
#'
#' @param base La base.
#' @param items Inicio del nombre de los ítems (por ejemplo, "EG") o sus nombres.
#' @param ... `"Etiqueta" = número`, una por opción de respuesta.
#' @return La base con los ítems en número. Una etiqueta sin regla queda vacía
#'   y se avisa.
#' @examples
#' base <- leer(system.file("extdata", "base_formulario.csv", package = "LimpiaR"))
#' base <- renombrar(base, BP = 35:47)
#' base <- recodificar(base, "BP", "Estoy en desacuerdo" = 1,
#'   "Ni de acuerdo ni en desacuerdo" = 2, "Estoy de acuerdo" = 3)
#' @export
recodificar <- function(base, items, ...) {
  reglas <- unlist(list(...))
  if (!length(reglas) || is.null(names(reglas))) stop("Escribe cada regla as\u00ed: \"De acuerdo\" = 4.", call. = FALSE)
  cols <- .columnas(base, items)
  clave <- .simple(names(reglas))
  sin <- character(0)
  if (all(vapply(base[cols], is.numeric, logical(1)))) {
    cat(items[1], ": ya estaba en n\u00fameros; no se cambi\u00f3 nada.\n", sep = "")
    return(invisible(base))
  }
  for (v in cols) {
    x <- base[[v]]
    if (is.numeric(x)) next
    n <- unname(reglas[match(.simple(x), clave)])
    sin <- c(sin, x[!is.na(x) & x != "" & is.na(n)])
    base[[v]] <- n
  }
  cat(items[1], ": ", length(cols), " \u00edtems; ", length(reglas), " etiquetas \u2192 ",
      paste(range(reglas), collapse = " a "), "\n", sep = "")
  if (length(sin)) cat("Ojo, sin regla (quedan vac\u00edos):", paste0("\"", unique(sin), "\"", collapse = ", "), "\n")
  else cat("Todas las respuestas tuvieron su n\u00famero.\n")
  invisible(base)
}

#' Invertir ítems redactados al revés
#'
#' Para los ítems inversos: en una escala de 1 a 5, el 1 pasa a 5, el 2 a 4...
#' Si se corre dos veces, no los vuelve a invertir.
#'
#' @param base La base.
#' @param items Nombres de los ítems inversos.
#' @param min,max Valores mínimo y máximo de la escala.
#' @return La base con esos ítems invertidos.
#' @examples
#' base <- data.frame(EG5 = c(1, 2, 5))
#' invertir(base, "EG5", min = 1, max = 5)
#' @export
invertir <- function(base, items, min, max) {
  cols <- .columnas(base, items)
  hechos <- attr(base, "invertidos")
  ya <- intersect(cols, hechos)
  if (length(ya)) cat("Ya estaban invertidos (no se tocan otra vez):", paste(ya, collapse = ", "), "\n")
  cols <- setdiff(cols, hechos)
  for (v in cols) base[[v]] <- min + max - base[[v]]
  if (length(cols)) cat("Invertidos (", min, " \u2194 ", max, "): ", paste(cols, collapse = ", "), "\n", sep = "")
  attr(base, "invertidos") <- c(hechos, cols)
  invisible(base)
}

#' Control de calidad
#'
#' Marca, sin borrar, tres cosas: ítems vacíos por persona, respuesta plana
#' (la misma opción en toda una escala de 6 ítems o más y 4 opciones o más;
#' con menos, responder todo igual es esperable) y envíos repetidos
#' (el mismo código dos veces: se marca el segundo). Agrega las columnas
#' `vacios`, `plana` y `repetido` para decidir con un criterio.
#'
#' @param base La base.
#' @param escalas Inicio del nombre de los ítems de cada escala, por ejemplo
#'   `c("SCS", "BP", "EG")`.
#' @param id Columna con el código de cada persona (para los repetidos).
#' @return La base con las columnas `vacios`, `plana` y `repetido`.
#' @examples
#' base <- data.frame(codigo = c("a", "b", "a"), X1 = c(1, 2, 1), X2 = c(1, NA, 1))
#' calidad(base, "X", id = "codigo")
#' @export
calidad <- function(base, escalas, id = NULL) {
  todas <- unlist(lapply(escalas, function(e) .columnas(base, e)))
  base$vacios <- rowSums(is.na(base[todas]))
  base$plana <- FALSE
  for (e in escalas) {
    cols <- .columnas(base, e)
    opciones <- length(unique(stats::na.omit(unlist(base[cols]))))
    if (length(cols) >= 6 && opciones >= 4)
      base$plana <- base$plana | apply(base[cols], 1, function(f) length(unique(stats::na.omit(f))) == 1)
  }
  base$repetido <- if (is.null(id)) FALSE else duplicated(base[[id]]) & !is.na(base[[id]])
  cat("\u00cdtems vac\u00edos:     ", sum(base$vacios > 0), " persona(s) con alg\u00fan \u00edtem sin responder\n", sep = "")
  cat("Respuesta plana:  ", sum(base$plana), " persona(s), marcadas en la columna plana (no se borran)\n", sep = "")
  cat("Env\u00edo repetido:   ", sum(base$repetido), if (is.null(id)) " (sin columna de c\u00f3digo)" else " (columna repetido)", "\n", sep = "")
  invisible(base)
}

#' Unificar las formas de escribir una categoría
#'
#' Junta en una sola categoría todas las respuestas que EMPIEZAN con el texto
#' indicado, sin importar mayúsculas, tildes ni espacios.
#' Ejemplo: `Experimental = "exp"` junta "experimental", "Experimental" y "Exp.".
#'
#' @param base La base.
#' @param columna Nombre de la columna.
#' @param ... Una regla por categoría: `Categoria = "inicio"` o
#'   `Categoria = c("inicio1", "inicio2")`.
#' @return La base con la columna unificada. Lo que no coincide con ninguna
#'   regla queda vacío y se avisa.
#' @examples
#' base <- leer(system.file("extdata", "base_cruda.csv", package = "LimpiaR"))
#' base <- unificar(base, "sexo", Femenino = c("fem", "mujer"), Masculino = c("mas", "hombre"))
#' @export
unificar <- function(base, columna, ...) {
  reglas <- list(...)
  if (!length(reglas) || is.null(names(reglas)) || any(names(reglas) == ""))
    stop("Escribe cada regla con su nombre, por ejemplo: Control = \"con\".", call. = FALSE)
  if (!columna %in% names(base)) stop("La base no tiene la columna ", columna, call. = FALSE)
  x <- base[[columna]]
  limpio <- .simple(x)
  nuevo <- rep(NA_character_, length(x))
  for (categoria in names(reglas)) {
    for (inicio in .simple(reglas[[categoria]])) nuevo[is.na(nuevo) & startsWith(limpio, inicio)] <- categoria
  }
  antes <- length(unique(x[!is.na(x)]))
  base[[columna]] <- nuevo
  cat(columna, ": ", antes, " formas de escribir \u2192 ", length(reglas), " categor\u00edas\n", sep = "")
  print(table(nuevo, dnn = NULL))
  sin <- unique(x[is.na(nuevo) & !is.na(x) & x != ""])
  if (length(sin)) cat("Ojo, quedaron vac\u00edos (no coinciden con ninguna regla):", paste0("\"", sin, "\"", collapse = ", "), "\n")
  invisible(base)
}

#' Validar que los valores sean posibles
#'
#' Revisa que los ítems estén dentro de su rango (por ejemplo, de 0 a 3).
#' Un valor imposible se deja vacío y se muestra en qué fila estaba.
#'
#' @param base La base.
#' @param items Inicio del nombre de los ítems (por ejemplo, "PHQ" para
#'   PHQ1, PHQ2...) o los nombres exactos de las columnas.
#' @param min,max Valores mínimo y máximo posibles.
#' @return La base con los valores imposibles vacíos.
#' @examples
#' base <- leer(system.file("extdata", "base_cruda.csv", package = "LimpiaR"))
#' base <- validar(base, "PHQ", min = 0, max = 3)
#' @export
validar <- function(base, items, min, max) {
  cols <- .columnas(base, items)
  n <- 0
  for (v in cols) {
    malo <- which(base[[v]] < min | base[[v]] > max)
    if (length(malo)) {
      cat(v, ": fila ", paste(malo, collapse = ", "), " ten\u00eda ", paste(base[[v]][malo], collapse = ", "),
          " (fuera de ", min, " a ", max, ") \u2192 queda vac\u00edo\n", sep = "")
      base[[v]][malo] <- NA
      n <- n + length(malo)
    }
  }
  if (n == 0) cat("Todos los valores de", if (length(cols) == 1) cols else paste(length(cols), "\u00edtems"), "est\u00e1n entre", min, "y", max, "\n")
  invisible(base)
}

#' Sumar el puntaje de una escala
#'
#' Crea una columna con la suma de los ítems. Si a una persona le falta algún
#' ítem, su puntaje queda vacío (no se inventa).
#'
#' @param base La base.
#' @param items Inicio del nombre de los ítems (por ejemplo, "PHQ").
#' @param nombre Nombre de la columna nueva. Por defecto, el mismo de `items`.
#' @return La base con la columna del puntaje.
#' @examples
#' base <- leer(system.file("extdata", "base_cruda.csv", package = "LimpiaR"))
#' base <- puntaje(base, "PHQ")
#' @export
puntaje <- function(base, items, nombre = items[1]) {
  cols <- .columnas(base, items)
  base[[nombre]] <- rowSums(base[cols])
  cat("Columna ", nombre, ": suma de ", length(cols), " \u00edtems; ", sum(is.na(base[[nombre]])),
      " persona(s) sin puntaje por \u00edtems vac\u00edos.\n", sep = "")
  invisible(base)
}

#' Excluir con los criterios del protocolo, en orden
#'
#' Cada criterio se escribe como `"Motivo" = condición para salir`. Se aplican
#' en el orden escrito y cada persona sale por el PRIMER criterio que cumple,
#' así nadie se cuenta dos veces. Muestra el flujo de participantes.
#'
#' @param base La base.
#' @param ... Los criterios: `"Motivo" = condición`, usando los nombres de las
#'   columnas tal cual.
#' @return La base solo con quienes quedan. El flujo se guarda dentro y se ve
#'   con [flujo()].
#' @examples
#' base <- leer(system.file("extdata", "base_cruda.csv", package = "LimpiaR"))
#' final <- excluir(base, "Menor de 30" = edad < 30, "Mayor de 59" = edad > 59)
#' @export
excluir <- function(base, ...) {
  criterios <- eval(substitute(alist(...)))
  if (!length(criterios) || is.null(names(criterios)) || any(names(criterios) == ""))
    stop("Escribe cada criterio con su motivo, por ejemplo: \"Menor de edad\" = edad < 18.", call. = FALSE)
  quedan <- rep(TRUE, nrow(base))
  fl <- data.frame(Etapa = "Recibidos", n = nrow(base))
  for (motivo in names(criterios)) {
    sale <- quedan & (eval(criterios[[motivo]], base, parent.frame()) %in% TRUE)
    quedan <- quedan & !sale
    fl <- rbind(fl, data.frame(Etapa = motivo, n = -sum(sale)))
  }
  fl <- rbind(fl, data.frame(Etapa = "Quedan", n = sum(quedan)))
  final <- base[quedan, , drop = FALSE]
  rownames(final) <- NULL
  attr(final, "flujo") <- fl
  print(fl, row.names = FALSE)
  invisible(final)
}

#' Ver el flujo de participantes
#'
#' Dibuja el flujo que dejó [excluir()]: cuántos llegaron, cuántos salieron por
#' cada motivo y cuántos quedan.
#'
#' @param base La base que devolvió [excluir()].
#' @return La tabla del flujo (invisible).
#' @examples
#' base <- leer(system.file("extdata", "base_cruda.csv", package = "LimpiaR"))
#' final <- excluir(base, "Menor de 30" = edad < 30, "Mayor de 59" = edad > 59)
#' flujo(final)
#' @export
flujo <- function(base) {
  fl <- attr(base, "flujo")
  if (is.null(fl)) stop("Esta base no pas\u00f3 por excluir(): no tiene flujo.", call. = FALSE)
  k <- nrow(fl)
  col <- c("#8C8C8C", rep("#F6E2E2", k - 2), "#BB1715")
  viejo <- graphics::par(mar = c(2, max(nchar(fl$Etapa)) * 0.5 + 1, 2, 2))
  on.exit(graphics::par(viejo))
  y <- graphics::barplot(rev(abs(fl$n)), names.arg = rev(fl$Etapa), horiz = TRUE, las = 1, border = NA,
                         col = rev(col), xlim = c(0, fl$n[1] * 1.15), main = "Flujo de participantes")
  graphics::text(rev(abs(fl$n)), y, labels = rev(ifelse(seq_len(k) %in% c(1, k), fl$n, paste0("\u2212", abs(fl$n)))), pos = 4)
  invisible(fl)
}

#' Guardar la base limpia
#'
#' Guarda en Excel (.xlsx, con el flujo y el diccionario en otras hojas si
#' existen) o en CSV.
#'
#' @param base La base limpia.
#' @param archivo Nombre del archivo de salida.
#' @return La ruta del archivo (invisible).
#' @examples
#' base <- leer(system.file("extdata", "base_cruda.csv", package = "LimpiaR"))
#' guardar(base, file.path(tempdir(), "base_limpia.csv"))
#' @export
guardar <- function(base, archivo) {
  if (tolower(tools::file_ext(archivo)) == "xlsx") {
    hojas <- list(datos = as.data.frame(base))
    if (!is.null(attr(base, "flujo"))) hojas$flujo <- attr(base, "flujo")
    if (!is.null(attr(base, "diccionario"))) hojas$diccionario <- attr(base, "diccionario")
    writexl::write_xlsx(hojas, archivo)
  } else {
    utils::write.csv(base, archivo, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  }
  cat("Guardado:", archivo, "(", nrow(base), "filas )\n")
  invisible(archivo)
}

.columnas <- function(base, items) {
  # Cada elemento es el inicio del nombre de una escala (SCS -> SCS1, SCS2...)
  # o el nombre exacto de una columna (edad). Primero se busca como escala.
  unlist(lapply(items, function(it) {
    cols <- grep(paste0("^", it, "[0-9]+$"), names(base), value = TRUE)
    if (length(cols)) return(cols[order(as.numeric(sub(it, "", cols, fixed = TRUE)))])
    if (it %in% names(base)) return(it)
    stop("La base no tiene la columna \"", it, "\" ni columnas que empiecen con \"", it,
         "\" seguidas de un n\u00famero.", call. = FALSE)
  }))
}

.posiciones <- function(base, columnas) {
  if (is.numeric(columnas)) {
    malas <- columnas[columnas < 1 | columnas > ncol(base)]
    if (length(malas)) stop("La base tiene ", ncol(base), " columnas; no existe la ", malas[1], ".", call. = FALSE)
    return(as.integer(columnas))
  }
  pos <- match(columnas, names(base))
  if (anyNA(pos)) stop("La base no tiene la(s) columna(s): ", paste(columnas[is.na(pos)], collapse = ", "), call. = FALSE)
  pos
}

.corto <- function(x, n) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "\u2026"), x)
