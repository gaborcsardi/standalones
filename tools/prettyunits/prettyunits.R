
prettyunits <- local({

  pretty_bytes <- function(bytes, style = c("default", "nopad", "6")) {

    style <- switch(
      match.arg(style),
      "default" = pretty_bytes_default,
      "nopad" = pretty_bytes_nopad,
      "6" = pretty_bytes_6
    )

    style(bytes)
  }

  compute_bytes <- function(bytes, smallest_unit = "B") {
    units0 <- c("B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")

    stopifnot(
      is.numeric(bytes),
      is.character(smallest_unit),
      length(smallest_unit) == 1,
      !is.na(smallest_unit),
      smallest_unit %in% units0
    )

    limits <- c(1000, 999950 * 1000 ^ (seq_len(length(units0) - 2) - 1))
    low <- match(smallest_unit, units0)
    units <- units0[low:length(units0)]
    limits <- limits[low:length(limits)]

    neg <- bytes < 0 & !is.na(bytes)
    bytes <- abs(bytes)

    mat <- matrix(
      rep(bytes, each = length(limits)),
      nrow = length(limits),
      ncol = length(bytes)
    )
    mat2 <- matrix(mat < limits, nrow  = length(limits), ncol = length(bytes))
    exponent <- length(limits) - colSums(mat2) + low - 1L
    res <- bytes / 1000 ^ exponent
    unit <- units[exponent - low + 2L]

    ## Zero bytes
    res[bytes == 0] <- 0
    unit[bytes == 0] <- units[1]

    ## NA and NaN bytes
    res[is.na(bytes)] <- NA_real_
    res[is.nan(bytes)] <- NaN
    unit[is.na(bytes)] <- units0[low]     # Includes NaN as well

    data.frame(
      stringsAsFactors = FALSE,
      amount = res,
      unit = unit,
      negative = neg
    )
  }

  pretty_bytes_default <- function(bytes) {
    szs <- compute_bytes(bytes)
    amt <- szs$amount

    ## String. For fractions we always show two fraction digits
    res <- character(length(amt))
    int <- is.na(amt) | amt == as.integer(amt)
    res[int] <- format(
      ifelse(szs$negative[int], -1, 1) * amt[int],
      scientific = FALSE
    )
    res[!int] <- sprintf("%.2f", ifelse(szs$negative[!int], -1, 1) * amt[!int])

    format(paste(res, szs$unit), justify = "right")
  }

  pretty_bytes_nopad <- function(bytes) {
    sub("^\\s+", "", pretty_bytes_default(bytes))
  }

  pretty_bytes_6 <- function(bytes) {
    szs <- compute_bytes(bytes, smallest_unit = "kB")
    amt <- szs$amount

    na   <- is.na(amt)
    nan  <- is.nan(amt)
    neg  <- !na & !nan & szs$negative
    l10  <- !na & !nan & !neg & amt < 10
    l100 <- !na & !nan & !neg & amt >= 10 & amt < 100
    b100 <- !na & !nan & !neg & amt >= 100

    szs$unit[neg] <- "kB"

    famt <- character(length(amt))
    famt[na] <- " NA"
    famt[nan] <- "NaN"
    famt[neg] <- "< 0"
    famt[l10] <- sprintf("%.1f", amt[l10])
    famt[l100] <- sprintf(" %.0f", amt[l100])
    famt[b100] <- sprintf("%.0f", amt[b100])

    paste0(famt, " ", szs$unit)
  }

  e <- expression

  vague_dt_default <- list(
    list(c = e(seconds < 10), s = "moments ago"),
    list(c = e(seconds < 45), s = "less than a minute ago"),
    list(c = e(seconds < 90), s = "about a minute ago"),
    list(c = e(minutes < 45), s = e("%d minutes ago" %s% round(minutes))),
    list(c = e(minutes < 90), s = "about an hour ago"),
    list(c = e(hours < 24),   s = e("%d hours ago" %s% round(hours))),
    list(c = e(hours < 42),   s = "a day ago"),
    list(c = e(days < 30),    s = e("%d days ago" %s% round(days))),
    list(c = e(days < 45),    s = "about a month ago"),
    list(c = e(days < 335),   s = e("%d months ago" %s% round(days / 30))),
    list(c = e(years < 1.5),  s = "about a year ago"),
    list(c = TRUE,            s = e("%d years ago" %s% round(years)))
  )

  vague_dt_short <- list(
    list(c = e(seconds < 50), s = "<1 min"),
    list(c = e(minutes < 50), s = e("%d min" %s% round(minutes))),
    list(c = e(hours < 1.5),  s = "1 hour"),
    list(c = e(hours < 18),   s = e("%d hours" %s% round(hours))),
    list(c = e(hours < 42),   s = "1 day"),
    list(c = e(days < 30),    s = e("%d day" %s% round(days))),
    list(c = e(days < 45),    s = "1 mon"),
    list(c = e(days < 335),   s = e("%d mon" %s% round(days / 30))),
    list(c = e(years < 1.5),  s = "1 year"),
    list(c = TRUE,            s = e("%d years" %s% round(years)))
  )

  vague_dt_terse <- list(
    list(c = e(seconds < 50), s = e("%2ds" %s% round(seconds))),
    list(c = e(minutes < 50), s = e("%2dm" %s% round(minutes))),
    list(c = e(hours < 18),   s = e("%2dh" %s% round(hours))),
    list(c = e(days < 30),    s = e("%2dd" %s% round(days))),
    list(c = e(days < 335),   s = e("%2dM" %s% round(days / 30))),
    list(c = TRUE,            s = e("%2dy" %s% round(years)))
  )

  vague_dt_formats <- list(
    "default" = vague_dt_default,
    "short" = vague_dt_short,
    "terse" = vague_dt_terse
  )

  time_ago <- function(date, format = c("default", "short", "terse")) {

    date <- as.POSIXct(date)

    if (length(date) > 1) return(sapply(date, time_ago, format = format))

    seconds <- difftime(Sys.time(), date, units = "secs")

    vague_dt(seconds, format = format)
  }

  vague_dt <- function(dt, format = c("default", "short", "terse")) {

    assert_diff_time(dt)

    units(dt) <- "secs"
    seconds <- as.vector(dt)

    ## Simplest to quit here for empty input
    if (!length(seconds)) return(character())

    pieces <- list(
      minutes = seconds / 60,
      hours = seconds / 60 / 60,
      days = seconds / 60 / 60 / 24,
      years = seconds / 60 / 60 / 24 / 365.25
    )

    format <- match.arg(format)

    for (p in vague_dt_formats[[format]]) {
      if (eval(p$c, pieces)) return(eval(p$s, pieces))
    }
  }

  parse_ms <- function(ms) {
    stopifnot(is.numeric(ms))

    data.frame(
      days = floor(ms / 86400000),
      hours = floor((ms / 3600000) %% 24),
      minutes = floor((ms / 60000) %% 60),
      seconds = round((ms / 1000) %% 60, 1)
    )
  }

  first_positive <- function(x) which(x > 0)[1]

  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

  pretty_ms <- function(ms, compact = FALSE) {

    stopifnot(is.numeric(ms))

    parsed <- t(parse_ms(ms))

    if (compact) {
      units <- c("d", "h", "m", "s")
      parsed2 <- parsed
      parsed2[] <- paste0(parsed, units)
      idx <- cbind(
        apply(parsed, 2, first_positive),
        seq_len(length(ms))
      )
      tmp <- paste0("~", parsed2[idx])

      # handle NAs
      tmp[is.na(parsed2[idx])] <- NA_character_
      tmp

    } else {

      ## Exact for small ones
      exact            <- paste0(ceiling(ms), "ms")
      exact[is.na(ms)] <- NA_character_

      ## Approximate for others, in seconds
      merge_pieces <- function(pieces) {
        ## handle NAs
        if (all(is.na(pieces))) {
          return(NA_character_)
        }

        ## handle non-NAs
        (
          (if (pieces[1]) pieces[1] %+% "d " else "") %+%
            (if (pieces[2]) pieces[2] %+% "h " else "") %+%
            (if (pieces[3]) pieces[3] %+% "m " else "") %+%
            (if (pieces[4]) pieces[4] %+% "s " else "")
        )
      }
      approx <- trim(apply(parsed, 2, merge_pieces))

      ifelse(ms < 1000, exact, approx)
    }
  }

  pretty_sec <- function(sec, compact = FALSE) {
    pretty_ms(sec * 1000, compact = compact)
  }

  pretty_dt <- function(dt, compact = FALSE) {

    assert_diff_time(dt)

    units(dt) <- "secs"

    pretty_sec(as.vector(dt), compact = compact)
  }

  `%s%` <- function(lhs, rhs) {
    assert_string(lhs)
    do.call(
      sprintf,
      c(list(lhs), as.list(rhs))
    )
  }

  `%+%` <- function(lhs, rhs) {
    paste0(lhs, rhs)
  }

  assert_diff_time <- function(x) {
    stopifnot(inherits(x, "difftime"))
  }

  assert_string <- function(x) {
    stopifnot(is.character(x), length(x) == 1L)
  }

  structure(
    list(
      .internal     = environment(),
      compute_bytes = compute_bytes,
      pretty_bytes  = pretty_bytes,
      pretty_dt     = pretty_dt,
      pretty_ms     = pretty_ms,
      pretty_sec    = pretty_sec,
      time_ago      = time_ago,
      vague_dt      = vague_dt
    ),
    class = c("standalone_prettyunits", "standalone")
  )
})
