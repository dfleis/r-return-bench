############
# Compares the speed of different ways for returning a value of a user-defined functions
############
setwd("~/projects/r_return_bench/")
library(microbenchmark)


###### define functions ######
f1 <- function(z) z
f2 <- function(z) (z)
f3 <- function(z) return (z)

nsims <- 1e5
x <- 1

###### perform benchmarks ######
# try all orderings to see if microbenchmark is exchangable (order invariant)
mb1 <- summary(microbenchmark(f1(x), f2(x), f3(x), times = nsims, unit = "us"))
mb2 <- summary(microbenchmark(f1(x), f3(x), f2(x), times = nsims, unit = "us"))
mb3 <- summary(microbenchmark(f2(x), f1(x), f3(x), times = nsims, unit = "us"))
mb4 <- summary(microbenchmark(f2(x), f3(x), f1(x), times = nsims, unit = "us"))
mb5 <- summary(microbenchmark(f3(x), f2(x), f1(x), times = nsims, unit = "us"))
mb6 <- summary(microbenchmark(f3(x), f1(x), f2(x), times = nsims, unit = "us"))

###### restructure data ######
lmb <- list(mb1, mb2, mb3, mb4, mb5, mb6)
# sort rows for plotting purposes
lmb_sort <- lapply(lmb, FUN = function(mb) mb[order(as.character(mb$expr)),])

###### plot ######
ymin <- min(sapply(lmb, FUN = function(mb) min(mb$mean - mb$lq)))
ymax <- max(sapply(lmb, FUN = function(mb) max(mb$mean + mb$uq)))
mycols <- rainbow(length(lmb_sort))
mypchs <- 11 + 1:length(lmb_sort)

# means
pdf("./img/return_benchmarks_mean.pdf", height = 5, width = 6)
plot(NA, xlim = c(1, nrow(lmb_sort[[1]])), ylim = c(ymin, ymax),
     xaxt = "n", xlab = "",
     ylab = substitute(paste("Time (", mu, "s)")),
     sub = paste0("N = ", nsims),
     main = "Mean Computation Times")
axis(1, at = 1:nrow(lmb_sort[[1]]), labels = paste0("f", 1:nrow(lmb_sort[[1]])))
for (idx in 1:length(lmb_sort)) {
  smb_i <- lmb_sort[[idx]]
  points(smb_i$mean, col = mycols[idx], pch = mypchs[idx], type = 'b')
  # error bars
  segments(x0 = 1:nrow(smb_i), y0 = smb_i$mean - smb_i$lq, 
           x1 = 1:nrow(smb_i), y1 = smb_i$mean + smb_i$uq,
           col = mycols[idx])
  segments(x0 = 1:nrow(smb_i) - 0.05, y0 = smb_i$mean - smb_i$lq, 
           x1 = 1:nrow(smb_i) + 0.05, y1 = smb_i$mean - smb_i$lq,
           col = mycols[idx])
  segments(x0 = 1:nrow(smb_i) - 0.05, y0 = smb_i$mean + smb_i$uq, 
           x1 = 1:nrow(smb_i) + 0.05, y1 = smb_i$mean + smb_i$uq,
           col = mycols[idx])
}
legend(x = 1.1, y = ymax + 0.025, legend = c("123", "132", "213", "231", "321", "312"), 
       bty = 'n', col = mycols, pch = mypchs, title = "Ordering", cex = 0.75)
legend(x = 1.35, y = ymax + 0.025, legend = c("f1: x", "f2: (z)", "f3: return(z)"), 
       bty = 'n', title = "Function", cex = 0.75)
dev.off()

# median
pdf("./img/return_benchmarks_med.pdf", height = 5, width = 6)
plot(NA, xlim = c(1, nrow(lmb_sort[[1]])), ylim = c(ymin, ymax),
     xaxt = "n", xlab = "",
     ylab = substitute(paste("Time (", mu, "s)")),
     sub = paste0("N = ", nsims),
     main = "Median Computation Times")
axis(1, at = 1:nrow(lmb_sort[[1]]), labels = paste0("f", 1:nrow(lmb_sort[[1]])))
for (idx in 1:length(lmb_sort)) {
  smb_i <- lmb_sort[[idx]]
  points(smb_i$median, col = mycols[idx], pch = mypchs[idx], type = 'b')
  # error bars
  segments(x0 = 1:nrow(smb_i), y0 = smb_i$median - smb_i$lq, 
           x1 = 1:nrow(smb_i), y1 = smb_i$median + smb_i$uq,
           col = mycols[idx])
  segments(x0 = 1:nrow(smb_i) - 0.05, y0 = smb_i$median - smb_i$lq, 
           x1 = 1:nrow(smb_i) + 0.05, y1 = smb_i$median - smb_i$lq,
           col = mycols[idx])
  segments(x0 = 1:nrow(smb_i) - 0.05, y0 = smb_i$median + smb_i$uq, 
           x1 = 1:nrow(smb_i) + 0.05, y1 = smb_i$median + smb_i$uq,
           col = mycols[idx])
}
legend(x = 1.1, y = ymax + 0.025, legend = c("123", "132", "213", "231", "321", "312"), 
       bty = 'n', col = mycols, pch = mypchs, title = "Ordering", cex = 0.75)
legend(x = 1.35, y = ymax + 0.025, legend = c("f1: x", "f2: (z)", "f3: return(z)"), 
       bty = 'n', title = "Function", cex = 0.75)
dev.off()








