perform_analysis <- function(x, mon, count, Date, num_cores = 7) {
  p <- ncol(x)

  w.frame <- replicate(13, matrix(data = NA, nrow = max(mon) - 6, ncol = p))
  stat.frame <- matrix(data = NA, nrow = max(mon) - 6, ncol = 13)

  w <- w.frame
  turn_over <- stat.frame
  w0.tmp <- matrix(data = 1/p, nrow = p, ncol = 13)
  r <- matrix(NA, sum(count[7:length(count)]), 13)

  tic("total")

  # Main analysis loop
  for (i in (7:max(mon))) {
    w.tmp <- matrix(data = NA, nrow = p, ncol = 13)
    x_tmp <- x[which((mon < i) & (mon >= i - 6)), ]
    n_tmp <- nrow(x_tmp)
    group <- buh.clust(x_tmp)

    # Perform calculations for different methods
    w.tmp[,1] <- po.cols(rep(0, n_tmp), x_tmp)                                                 # MV
    w.tmp[,2] <- po.JM(x_tmp)                                                                 # JM
    w.tmp[,3] <- po.avg(rep(0, n_tmp), x_tmp, method = "LASSO")                               # TRP_min
    w.tmp[,4] <- po.avg(rep(0, n_tmp), x_tmp, method = "RIDGE")                               # APP-Ridge
    w.tmp[,5] <- po.grossExp(rep(0, n_tmp), x_tmp, method = "NOSHORT")                        # Fan.et.al_JM
    w.tmp[,6] <- po.grossExp(rep(0, n_tmp), x_tmp, method = "EQUAL")                           # FZY
    w.tmp[,7] <- rep(1/p, p)                                                                   # EW
    w.tmp[,9] <- po.bhu(rep(0, n_tmp), x_tmp, group, 100)                                      # TRP_clu
    w.tmp[,10] <- po.TZT(x_tmp, gamma = 3)                                                     # TZ
    w.tmp[,11] <- po.SW(x_tmp, b = round(p^0.7), sample = 1000)                                # SW
    w.tmp[,12] <- po.SW(x_tmp, b = round(length(group)), sample = 1000)                        # SW_clu
    w.tmp[,13] <- po.SW.lasso(rep(0, n_tmp), x_tmp, b = round(length(group)), sample = 1000)   # SW_shrink

    # Store weights and turnover
    for (j in 1:13) {
      w[i - 6,,j] <- w.tmp[,j]
      turn_over[i - 6,j] <- norm(as.matrix(w.tmp[,j]) - w0.tmp[,j])
    }

    w0.tmp <- w.tmp

    # Calculate returns
    r_tmp <- (1 + (x[which(mon == i), ] / 100))
    for (j in 1:13) {
      for (k in 1:nrow(r_tmp)) {
        idx <- which(mon == i)[k] - 124
        if (idx > 0 && idx <= nrow(r)) {
          r[idx,j] <- r_tmp[k,] %*% w0.tmp[,j]
          w0.tmp[,j] <- (r_tmp[k,] * w0.tmp[,j]) / sum(r_tmp[k,] * w0.tmp[,j])
        }
      }
      w0.tmp[,j] <- w0.tmp[,j] / sum(w0.tmp[,j])
    }

    print(i - 6)
  }

  toc()

  # Performance Metrics
  colnames(turn_over) <- c("MV","JM","TRP_min","APP-Ridge","Fan.et.al_JM","FZY","EW","LW","TRP_clu","TZ","SW","SW_clu","SW_shrink")
  turnover_mean <- colMeans(turn_over) * 100 # TO

  sharpe_ratio <- sqrt(252) * (apply(r - 1, 2, mean) / apply(r - 1, 2, sd)) # SR
  volatility <- sqrt(252) * apply(r - 1, 2, sd) * 100 # VO

  v <- apply((1 + (r - 1)), 2, cumprod)
  v1 <- as.matrix(apply(v, 2, max))
  v1 <- matrix(rep(t(v1), dim(v)[1]), ncol = dim(v)[2], byrow = TRUE)
  mdd <- (v1 - v) / v1 * 100
  max_drawdown <- apply(mdd, 2, max)

  ## VW Calculation
  r_vw <- matrix(NA, sum(count[7:length(count)]), 1)
  w_vw <- matrix(data = NA, nrow = max(mon) - 6, ncol = p)

  vw0.tmp <- matrix(data = 1/p, nrow = p, ncol = 1)
  vw.to <- matrix(data = NA, nrow = max(mon) - 6, ncol = 1)

  for (i in 7:max(mon)) {
    x_tmp <- x[which((mon < i) & (mon >= i - 6)), ]
    n_tmp <- dim(x_tmp)[1]

    r_tmp <- (1 + (x[which(mon == i), ] / 100))

    for (k in 1:dim(r_tmp)[1]) {
      idx <- which(mon == i)[k] - 124
      if (idx > 0 && idx <= nrow(r_vw)) {
        r_vw[idx] <- r_tmp[k,] %*% vw0.tmp
        vw.tmp <- (r_tmp[k,] * vw0.tmp) / sum(r_tmp[k,] * vw0.tmp)
      }
    }
    w_vw[i - 6, ] <- vw.tmp
    vw.to[i - 6, ] <- norm(as.matrix(vw.tmp) - vw0.tmp)
    vw0.tmp <- vw.tmp
  }

  vw_to_mean <- colMeans(vw.to) * 100 # TO
  vw_sharpe_ratio <- sqrt(252) * (apply(r_vw - 1, 2, mean) / apply(r_vw - 1, 2, sd)) # SR
  vw_volatility <- sqrt(252) * apply(r_vw - 1, 2, sd) * 100 # VO

  vw_v <- apply((1 + (r_vw - 1)), 2, cumprod)
  vw_v1 <- as.matrix(apply(vw_v, 2, max))
  vw_v1 <- matrix(rep(t(vw_v1), dim(vw_v)[1]), ncol = dim(vw_v)[2], byrow = TRUE)
  vw_mdd <- (vw_v1 - vw_v) / vw_v1 * 100
  vw_max_drawdown <- apply(vw_mdd, 2, max)

  ## Visualization
  # Combine all the returns for visualization
  r.final <- cbind(r, r_vw) # Combine all the returns of different methods
  r.final <- r.final[, colSums(is.na(r.final)) == 0] # Remove empty columns

  # Cumulative returns
  cum.r <- apply(r.final - 1, 2, cumsum)
  df <- data.frame(time = Date[-seq(1:sum(count[1:6]))], return = cum.r)
  colnames(df) <- c("time", "MV", "JM", "FZY", "EW", "REN", "TZ", "VW")

  df <- melt(df, id.vars = "time")
  colnames(df) <- c("time", "Method", "value")

  p1.ff25 <- ggplot(df, aes(x = time, y = value, color = Method)) + theme_bw() + geom_line() +
    ggtitle("Cumulative Returns") +
    scale_color_manual(values = c('red','cyan','magenta','grey47','black','lightblue',"green","orange","brown")) +
    labs(y = "Cumulative Return", x = "Time") +
    theme(legend.justification = c(0, 1), legend.position = c(0, 1)) + theme(legend.position = "bottom")

  # Cumulative turnover
  to.final <- cbind(turn_over, vw.to) # Combine all the TOs of different methods
  to.final <- to.final[, colSums(is.na(to.final)) == 0] # Remove empty columns

  index <- which(month(Date)[-1] != month(Date)[-length(month(Date))])

  df2 <- data.frame(time = Date[index[6:length(index)]], turnover = apply(abs(to.final), 2, cumsum))
  colnames(df2) <- c("time", "MV", "JM", "FZY", "EW", "REN", "TZ", "VW")

  df2 <- melt(df2, id.vars = "time")
  colnames(df2) <- c("time", "Method", "value")

  p2.ff25 <- ggplot(df2, aes(x = time, y = value, color = Method)) + theme_bw() + geom_line() +
    ggtitle("Cumulative Turnover") +
    scale_color_manual(values = c('red','cyan','magenta','grey47','black','lightblue',"green","orange","brown")) +
    labs(y = "Cumulative Turnover", x = "Time") +
    theme(legend.justification = c(0, 1), legend.position = c(0, 1)) + theme(legend.position = "bottom")

  return(list(
    cumulative_return_plot = p1.ff25,
    cumulative_turnover_plot = p2.ff25,
    turnover_mean = turnover_mean,
    sharpe_ratio = sharpe_ratio,
    volatility = volatility,
    max_drawdown = max_drawdown,
    vw_to_mean = vw_to_mean,
    vw_sharpe_ratio = vw_sharpe_ratio,
    vw_volatility = vw_volatility,
    vw_max_drawdown = vw_max_drawdown
  ))
}
