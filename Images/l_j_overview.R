par(mfrow=c(4, 2))

mmax <- 1/sqrt(2*1/16)

k <- 1/2
p <- 1/sqrt(2*k)

l1 <- rep(-p, times=50)
r1 <- rep(p, times=50)

plot(
  seq(from=0, to=k, along.with=l1), 
  l1, 
  type='l', 
  xlim=c(0, 1),
  ylim=c(-mmax, mmax),
  main="Function l_1",
  xlab="u",
  ylab="l_j(u)",
  xaxt = "n"
)
axis(1, at = c(0, 1/8, 1/4, 3/8, 2/4, 5/8, 3/4, 7/8, 4/4))
points(0.5, 0, pch=16)
lines(seq(from=1-k, to=1, along.with=r1), r1, type='l')

k <- 1/4
p <- 1/sqrt(2*k)

l1 <- rep(-p, times=50)
r1 <- rep(p, times=50)

plot(
  seq(from=0, to=k, along.with=l1), 
  l1, 
  type='l', 
  xlim=c(0, 1),
  ylim=c(-mmax, mmax),
  main="Function l_2",
  xlab="u",
  ylab="l_j(u)",
  xaxt = "n"
)
axis(1, at = c(0, 1/8, 1/4, 3/8, 2/4, 5/8, 3/4, 7/8, 4/4))
lines(c(k, 1-k), c(0, 0))
lines(seq(from=1-k, to=1, along.with=r1), r1, type='l')

k <- 1/8
p <- 1/sqrt(2*k)

l1 <- rep(-p, times=50)
r1 <- rep(p, times=50)

plot(
  seq(from=0, to=k, along.with=l1), 
  l1, 
  type='l', 
  xlim=c(0, 1),
  ylim=c(-mmax, mmax),
  main="Function l_3",
  xlab="u",
  ylab="l_j(u)",
  xaxt = "n"
)
axis(1, at = c(0, 1/8, 1/4, 3/8, 2/4, 5/8, 3/4, 7/8, 4/4))
lines(c(k, 1-k), c(0, 0))
lines(seq(from=1-k, to=1, along.with=r1), r1, type='l')

k <- 3/8
p <- 1/sqrt(2*k)

l1 <- rep(-p, times=50)
r1 <- rep(p, times=50)

plot(
  seq(from=0, to=k, along.with=l1), 
  l1, 
  type='l', 
  xlim=c(0, 1),
  ylim=c(-mmax, mmax),
  main="Function l_4",
  xlab="u",
  ylab="l_j(u)",
  xaxt = "n"
)
axis(1, at = c(0, 1/8, 1/4, 3/8, 2/4, 5/8, 3/4, 7/8, 4/4))
lines(c(k, 1-k), c(0, 0))
lines(seq(from=1-k, to=1, along.with=r1), r1, type='l')

k <- 1/16
p <- 1/sqrt(2*k)

l1 <- rep(-p, times=50)
r1 <- rep(p, times=50)

plot(
  seq(from=0, to=k, along.with=l1), 
  l1, 
  type='l', 
  xlim=c(0, 1),
  ylim=c(-mmax, mmax),
  main="Function l_5",
  xlab="u",
  ylab="l_j(u)",
  xaxt = "n"
)
axis(1, at = c(0, 1/8, 1/4, 3/8, 2/4, 5/8, 3/4, 7/8, 4/4))
lines(c(k, 1-k), c(0, 0))
lines(seq(from=1-k, to=1, along.with=r1), r1, type='l')

k <- 3/16
p <- 1/sqrt(2*k)

l1 <- rep(-p, times=50)
r1 <- rep(p, times=50)

plot(
  seq(from=0, to=k, along.with=l1), 
  l1, 
  type='l', 
  xlim=c(0, 1),
  ylim=c(-mmax, mmax),
  main="Function l_6",
  xlab="u",
  ylab="l_j(u)",
  xaxt = "n"
)
axis(1, at = c(0, 1/8, 1/4, 3/8, 2/4, 5/8, 3/4, 7/8, 4/4))
lines(c(k, 1-k), c(0, 0))
lines(seq(from=1-k, to=1, along.with=r1), r1, type='l')


k <- 5/16
p <- 1/sqrt(2*k)

l1 <- rep(-p, times=50)
r1 <- rep(p, times=50)

plot(
  seq(from=0, to=k, along.with=l1), 
  l1, 
  type='l', 
  xlim=c(0, 1),
  ylim=c(-mmax, mmax),
  main="Function l_7",
  xlab="u",
  ylab="l_j(u)",
  xaxt = "n"
)
axis(1, at = c(0, 1/8, 1/4, 3/8, 2/4, 5/8, 3/4, 7/8, 4/4))
lines(c(k, 1-k), c(0, 0))
lines(seq(from=1-k, to=1, along.with=r1), r1, type='l')

k <- 7/16
p <- 1/sqrt(2*k)

l1 <- rep(-p, times=50)
r1 <- rep(p, times=50)

plot(
  seq(from=0, to=k, along.with=l1), 
  l1, 
  type='l', 
  xlim=c(0, 1),
  ylim=c(-mmax, mmax),
  main="Function l_8",
  xlab="u",
  ylab="l_j(u)",
  xaxt = "n"
)
axis(1, at = c(0, 1/8, 1/4, 3/8, 2/4, 5/8, 3/4, 7/8, 4/4))
lines(c(k, 1-k), c(0, 0))
lines(seq(from=1-k, to=1, along.with=r1), r1, type='l')
