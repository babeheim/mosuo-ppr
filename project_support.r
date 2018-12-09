
library(rethinking)
library(tictoc)

set.seed(1868)

scaffold <- FALSE
save_temp <- FALSE

n.sims <- 30
enable_sampling <- TRUE
n_iter <- 4000

write_log <- function(title, path, start_time) {
  tic.log(format = TRUE)
  msg_log <- unlist(tic.log())
  msg_log <- paste0("- ", msg_log)
  if (!exists("start_time")) start_time <- NA
  header <- c(
    title,
    paste("start_time:", start_time),
    paste("finish_time:", Sys.time()),
    "events:")
  msg_log <- c(header, msg_log)
  writeLines(msg_log, path)
  print("tictoc log written to file")
}

hex <- function(x, y, col, ...) {
  for (i in 1:length(x)) {
    polygon(
      c(
        x[i],
        x[i] - sqrt(3) / 2,
        x[i] - sqrt(3) / 2,
        x[i], x[i] + sqrt(3) / 2,
        x[i] + sqrt(3) / 2
      ),
      c(
        y[i] - 1,
        y[i] - 1 / 2,
        y[i] + 1 / 2,
        y[i] + 1,
        y[i] + 1 / 2,
        y[i] - 1 / 2
      ),
    col = col[i], ...)
  }
}

extract_rethinking_table <- function(model) {
  if (.hasSlot(precis(model), ".S3Class")) {
    output <- as.data.frame(precis(model))
    colnames(output) <- c("Mean", "SE", "lower",
      "upper", "n_eff", "Rhat")
  } else {
    output <- precis(model)@output
  }
  return(output)
}

task_timer <- function(task = "", start = start.time, stop = stop.time) {
  time.diff <- round(as.numeric(difftime(stop, start, units = "mins")), 1)
  output <- paste(Sys.time(), ": ", task, " (", time.diff, " mins)\n", sep = "")
  print(output)
  return(output)
}

gradient_maker <- function(start = NA, stop = NA,
  cols = c("darkorange", "white", "darkcyan"), vis = FALSE, n = 1000) {
  if (is.na(start) | is.na(stop)) stop("need to specify start and
    stop points on a numerical scale")
  colfunc <- colorRampPalette(cols)
  color.list <- colfunc(n)
  color.locations <- seq(start, stop, length = n)
  names(color.locations) <- color.list
  if (vis == TRUE) plot(color.locations, rep(1, n),
    col = color.list, pch = "|", ylim = c(0.9, 1.1), cex = 5)
  return(color.locations)
}

col_alpha <- function (acol, alpha = 0.2) {
  acol <- col2rgb(acol)
  acol.red <- acol["red", ] / 255
  acol.green <- acol["green", ] / 255
  acol.blue <- acol["blue", ] / 255
  acol <- mapply(function(red, green, blue, alphas) rgb(red, green,
    blue, alphas), acol.red, acol.green, acol.blue, alpha)
  return(as.character(acol))
}

pid_maker <- function(n, reserved = "") {
  my.let <- LETTERS
  my.num <- 0:9
  output <- replicate(n, paste(sample(c(my.let, my.num), 4,
    replace = TRUE), collapse = ""))
  while (any(duplicated(output) | output %in% reserved)) {
    output <- output[-which(output %in% reserved | duplicated(output))]
    remaining <- n - length(output)
    output <- c(output, replicate(remaining,
      paste(sample(c(my.let, my.num), 4, replace = TRUE), collapse = "")))
  }
  return(output)
}

dir_init <- function(path, verbose = FALSE) {
  if (substr(path, 1, 2) != "./") stop("path argument must be formatted
    with './' at beginning")
  contents <- dir(path)
  if (verbose) {
    if (length(contents) == 0) print(paste("folder ",
      path, " created.", sep = ""))
    if (length(contents) > 0) print(paste("folder ",
      path, " wiped of ", length(contents), " files/folders.", sep = ""))
  }
  if (dir.exists(path)) unlink(path, recursive = TRUE)
  dir.create(path)
}

texttab <- function(input.matrix, alignment = NA,
  hlines = NA, caption = "", scale = NA) {
  output <- character(nrow(input.matrix))
  for (i in 1:nrow(input.matrix)) {
    add.amps <- paste(input.matrix[i, ], collapse = " & ")
    output[i] <- paste(add.amps, "\\\\", sep = " ")
  }
  if (all(!is.na(hlines))) {
    for (i in 1:length(hlines)) output <- append(output,
      "\\hline", hlines[i] + (i - 1))
  }
  return(output)
}


logistic <- function(x) exp(x) / (1 + exp(x))


# this should specify the input mortality schedules as arguments

grim_reaper <- function(data) {

  dead.names <- character(0)
  alive.rows <- which(is.na(data$yod))

  if (length(alive.rows)>  0) {

    baseline <- rep(0, length(alive.rows))
    alive.age.cats <- as.character(data$age.cats[alive.rows])
    alive.male <- data$male[alive.rows]

    tar <- which(alive.male == 1)
    baseline[tar] <- age.specific.mortality.males[alive.age.cats[tar]] / 1000
    tar <- which(alive.male == 0)
    baseline[tar] <- age.specific.mortality.females[alive.age.cats[tar]] / 1000

    alpha = log(baseline / (1 - baseline))

    adjustment.factor <- (-2)

    annual.pr.death <- (1 - (1 / (1 + exp(alpha + adjustment.factor))))
    died <- rbinom(length(alive.rows), 1, annual.pr.death)
    if (any(died == 1)) dead.names <- data$pid[alive.rows[which(died == 1)]]

  }

  return(dead.names)

}

# this function should include the parityX.post objects as inputs

baby_maker <- function(data) {

  preggo.names <- character(0)

  data$linear.predictor <- NA

  fertile.rows.parity0 <- which(is.na(data$yod) & data$male == 0 &
    data$age.cat >= 15 & data$age.cat < 45 & data$n.kids == 0)

  if (length(fertile.rows.parity0) > 0) {

    tar <- sample(1:length(parity0.post$a_mu), 1)

    data$linear.predictor[fertile.rows.parity0] <- parity0.post$a_mu[tar] +
    parity0.post$b2[tar] * data$patrilineal[fertile.rows.parity0] +
    parity0.post$b4[tar] * data$age15[fertile.rows.parity0] +
    parity0.post$b15[tar] * data$age25[fertile.rows.parity0] +
    parity0.post$b16[tar] * data$age30[fertile.rows.parity0] +
    parity0.post$b17[tar] * data$age35[fertile.rows.parity0] +
    parity0.post$b18[tar] * data$age40[fertile.rows.parity0]

  }

  fertile.rows.parity1 <- which(is.na(data$yod) & data$male == 0 &
    data$age.cat >= 15 & data$age.cat < 45 & data$n.kids == 1)

  if (length(fertile.rows.parity1) > 0) {

    tar <- sample(1:length(parity1.post$a_mu), 1)

    data$linear.predictor[fertile.rows.parity1] <- parity1.post$a_mu[tar] +
    parity1.post$b1[tar] * data$son[fertile.rows.parity1] +
    parity1.post$b2[tar] * data$patrilineal[fertile.rows.parity1] +
    parity1.post$b3[tar] * data$son_patrilineal[fertile.rows.parity1] +
    parity1.post$b4[tar] * data$age15[fertile.rows.parity1] +
    parity1.post$b15[tar] * data$age25[fertile.rows.parity1] +
    parity1.post$b16[tar] * data$age30[fertile.rows.parity1] +
    parity1.post$b17[tar] * data$age35[fertile.rows.parity1] +
    parity1.post$b18[tar] * data$age40[fertile.rows.parity1]

  }

  fertile.rows.parity2 <- which(is.na(data$yod) &
    data$male == 0 & data$age.cat >= 15 & data$age.cat < 45 & data$n.kids == 2)

  if (length(fertile.rows.parity2) > 0) {

    tar <- sample(1:length(parity2.post$a_mu), 1)

    data$linear.predictor[fertile.rows.parity2] <- parity2.post$a_mu[tar] +
    parity2.post$b1[tar] * data$pcode10[fertile.rows.parity2] +
    parity2.post$b2[tar] * data$pcode11[fertile.rows.parity2] +
    parity2.post$b3[tar] * data$patrilineal[fertile.rows.parity2] +
    parity2.post$b4[tar] * data$pat_10[fertile.rows.parity2] +
    parity2.post$b5[tar] * data$pat_11[fertile.rows.parity2] +
    parity2.post$b6[tar] * data$age15[fertile.rows.parity2] +
    parity2.post$b7[tar] * data$age20[fertile.rows.parity2] +
    parity2.post$b17[tar] * data$age30[fertile.rows.parity2] +
    parity2.post$b18[tar] * data$age35[fertile.rows.parity2] +
    parity2.post$b19[tar] * data$age40[fertile.rows.parity2]

  }

  fertile.rows.parity3 <- which(is.na(data$yod) & data$male == 0 &
    data$age.cat >= 15 & data$age.cat < 45 & data$n.kids == 3)

  if (length(fertile.rows.parity3) > 0) {

    tar <- sample(1:length(parity3.post$a_mu), 1)

    data$linear.predictor[fertile.rows.parity3] <- parity3.post$a_mu[tar] +
    parity3.post$b1[tar] * data$pcode100[fertile.rows.parity3] +
    parity3.post$b2[tar] * data$pcode110[fertile.rows.parity3] +
    parity3.post$b3[tar] * data$pcode111[fertile.rows.parity3] +
    parity3.post$b4[tar] * data$patrilineal[fertile.rows.parity3] +
    parity3.post$b5[tar] * data$pat_100[fertile.rows.parity3] +
    parity3.post$b6[tar] * data$pat_110[fertile.rows.parity3] +
    parity3.post$b7[tar] * data$pat_111[fertile.rows.parity3] +
    parity3.post$b9[tar] * data$age20[fertile.rows.parity3] +
    parity3.post$b18[tar] * data$age30[fertile.rows.parity3] +
    parity3.post$b19[tar] * data$age35[fertile.rows.parity3] +
    parity3.post$b20[tar] * data$age40[fertile.rows.parity3]

  }

  fertile.rows.parity4 <- which(is.na(data$yod) & data$male == 0 &
    data$age.cat >= 15 & data$age.cat < 45 & data$n.kids >= 4)

  if (length(fertile.rows.parity4) > 0) {

    tar <- sample(1:length(parity4.post$a_mu), 1)

    data$linear.predictor[fertile.rows.parity4] <- parity4.post$a_mu[tar] +
    parity4.post$b1[tar] * data$pcode1000[fertile.rows.parity4] +
    parity4.post$b2[tar] * data$pcode1100[fertile.rows.parity4] +
    parity4.post$b3[tar] * data$pcode1110[fertile.rows.parity4] +
    parity4.post$b4[tar] * data$pcode1111[fertile.rows.parity4] +
    parity4.post$b5[tar] * data$patrilineal[fertile.rows.parity4] +
    parity4.post$b6[tar] * data$pat_1000[fertile.rows.parity4] +
    parity4.post$b7[tar] * data$pat_1100[fertile.rows.parity4] +
    parity4.post$b8[tar] * data$pat_1110[fertile.rows.parity4] +
    parity4.post$b9[tar] * data$pat_1111[fertile.rows.parity4] +
    parity4.post$b15[tar] * data$age25[fertile.rows.parity4] +
    parity4.post$b16[tar] * data$age30[fertile.rows.parity4] +
    parity4.post$b17[tar] * data$age35[fertile.rows.parity4]

  }

  fertile.rows <- which(is.na(data$yod) & data$male == 0 &
    data$age.cat >= 15 & data$age.cat < 45)

  if (length(fertile.rows) > 0) {

    annual.pr.birth <- logistic(data$linear.predictor[fertile.rows])
    preggo <- rbinom(length(fertile.rows), 1, annual.pr.birth)
    if (any(preggo == 1)){
      preggo.names <- data$pid[fertile.rows[which(preggo == 1)]]
    }
  }

  return(preggo.names)
}
