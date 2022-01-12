###D‘Esopo-Lefkowitzモデルを応用した理論期待得点と理論得点確率の導出

##単打時2塁走者生還率(単打時にpの確率で生還・p-1の確率で三塁へ進塁)の設定
p = 1

##仮の打者データ(各打撃結果をもたらす確率)を乱数により作成
BattingProbability <- data.frame(matrix(0, 9, 6))
rownames(BattingProbability) = c("一番打者", "二番打者", "三番打者", "四番打者", "五番打者", "六番打者", "七番打者", "八番打者", "九番打者")
colnames(BattingProbability) = c("単打", "二塁打", "三塁打", "本塁打", "四球", "アウト")
for(i in 1:9){
  BattingProbability[i, 1] <- rnorm(1, mean = 0.176138782, sd = 0.05)
  BattingProbability[i, 2] <- rnorm(1, mean = 0.024904215, sd = 0.01)
  BattingProbability[i, 3] <- rnorm(1, mean = 0.005108557, sd = 0.0025)
  BattingProbability[i, 4] <- rnorm(1, mean = 0.004469987, sd = 0.002)
  BattingProbability[i, 5] <- rnorm(1, mean = 0.09706258, sd = 0.03)
  for(j in 1:5){
    if(BattingProbability[i, j] < 0){
      BattingProbability[i, j] <- 0
    }
  }
  BattingProbability[i, 6] <- 1 - sum(BattingProbability[i, 1:5])
}

##得点期待値の計算
#定数項の列ベクトルbを設定
b = numeric(216)
for(i in 1:9){
  for(j in 1:3){
    b[((i - 1) * 24) + ((j - 1) * 8) + 1] <- BattingProbability[i, 4]
    b[((i - 1) * 24) + ((j - 1) * 8) + 2] <- BattingProbability[i, 3] + 2 * BattingProbability[i, 4]
    b[((i - 1) * 24) + ((j - 1) * 8) + 3] <- p * BattingProbability[i, 1] + BattingProbability[i, 2] + BattingProbability[i, 3] + 2 * BattingProbability[i, 4]
    b[((i - 1) * 24) + ((j - 1) * 8) + 4] <- BattingProbability[i, 1] + BattingProbability[i, 2] + BattingProbability[i, 3] + 2 * BattingProbability[i, 4]
    b[((i - 1) * 24) + ((j - 1) * 8) + 5] <- p * BattingProbability[i, 1] + BattingProbability[i, 2] + 2 * BattingProbability[i, 3] + 3 * BattingProbability[i, 4]
    b[((i - 1) * 24) + ((j - 1) * 8) + 6] <- BattingProbability[i, 1] + BattingProbability[i, 2] + 2 * BattingProbability[i, 3] + 3 * BattingProbability[i, 4]
    b[((i - 1) * 24) + ((j - 1) * 8) + 7] <- (p + 1) * BattingProbability[i, 1] + 2 * BattingProbability[i, 2] + 2 * BattingProbability[i, 3] + 3 * BattingProbability[i, 4]
    b[((i - 1) * 24) + ((j - 1) * 8) + 8] <- (p + 1) * BattingProbability[i, 1] + 2 * BattingProbability[i, 2] + 3 * BattingProbability[i, 3] + 4 * BattingProbability[i, 4] + BattingProbability[i, 5]
  }
}
#係数行列Aの設定
A = matrix(0, 216, 216)
for(i in 1:9){
  A[((i - 1) * 24) + 1, (i - 1) * 24 + 1] = 1
  A[((i - 1) * 24) + 1, ((i - 1) * 24 + 25) %% 216 + 1] <- -BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 1, ((i - 1) * 24 + 26) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 1, ((i - 1) * 24 + 27) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 1, ((i - 1) * 24 + 24) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 1, ((i - 1) * 24 + 32) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 2, (i - 1) * 24 + 2] = 1
  A[((i - 1) * 24) + 2, ((i - 1) * 24 + 28) %% 216 + 1] <- -BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 2, ((i - 1) * 24 + 30) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 2, ((i - 1) * 24 + 27) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 2, ((i - 1) * 24 + 24) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 2, ((i - 1) * 24 + 33) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 3, (i - 1) * 24 + 3] = 1
  A[((i - 1) * 24) + 3, ((i - 1) * 24 + 29) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1]
  A[((i - 1) * 24) + 3, ((i - 1) * 24 + 25) %% 216 + 1] <- - p * BattingProbability[i, 1]
  A[((i - 1) * 24) + 3, ((i - 1) * 24 + 26) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 3, ((i - 1) * 24 + 27) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 3, ((i - 1) * 24 + 24) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 3, ((i - 1) * 24 + 28) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 3, ((i - 1) * 24 + 34) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 4, (i - 1) * 24 + 4] = 1
  A[((i - 1) * 24) + 4, ((i - 1) * 24 + 25) %% 216 + 1] <- -BattingProbability[i, 1]
  A[((i - 1) * 24) + 4, ((i - 1) * 24 + 26) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 4, ((i - 1) * 24 + 27) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 4, ((i - 1) * 24 + 24) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 4, ((i - 1) * 24 + 29) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 4, ((i - 1) * 24 + 35) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 5, (i - 1) * 24 + 5] = 1
  A[((i - 1) * 24) + 5, ((i - 1) * 24 + 31) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 5, ((i - 1) * 24 + 28) %% 216 + 1] <- - p * BattingProbability[i, 1]
  A[((i - 1) * 24) + 5, ((i - 1) * 24 + 30) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 5, ((i - 1) * 24 + 27) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 5, ((i - 1) * 24 + 24) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 5, ((i - 1) * 24 + 36) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 6, (i - 1) * 24 + 6] = 1
  A[((i - 1) * 24) + 6, ((i - 1) * 24 + 28) %% 216 + 1] <- -BattingProbability[i, 1]
  A[((i - 1) * 24) + 6, ((i - 1) * 24 + 30) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 6, ((i - 1) * 24 + 27) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 6, ((i - 1) * 24 + 24) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 6, ((i - 1) * 24 + 31) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 6, ((i - 1) * 24 + 37) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 7, (i - 1) * 24 + 7] = 1
  A[((i - 1) * 24) + 7, ((i - 1) * 24 + 29) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1]
  A[((i - 1) * 24) + 7, ((i - 1) * 24 + 25) %% 216 + 1] <- - p * BattingProbability[i, 1]
  A[((i - 1) * 24) + 7, ((i - 1) * 24 + 26) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 7, ((i - 1) * 24 + 27) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 7, ((i - 1) * 24 + 24) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 7, ((i - 1) * 24 + 31) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 7, ((i - 1) * 24 + 38) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 8, (i - 1) * 24 + 8] = 1
  A[((i - 1) * 24) + 8, ((i - 1) * 24 + 31) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 8, ((i - 1) * 24 + 28) %% 216 + 1] <- - p * BattingProbability[i, 1]
  A[((i - 1) * 24) + 8, ((i - 1) * 24 + 30) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 8, ((i - 1) * 24 + 27) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 8, ((i - 1) * 24 + 24) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 8, ((i - 1) * 24 + 39) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 9, (i - 1) * 24 + 9] = 1
  A[((i - 1) * 24) + 9, ((i - 1) * 24 + 33) %% 216 + 1] <- -BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 9, ((i - 1) * 24 + 34) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 9, ((i - 1) * 24 + 35) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 9, ((i - 1) * 24 + 32) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 9, ((i - 1) * 24 + 40) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 10, (i - 1) * 24 + 10] = 1
  A[((i - 1) * 24) + 10, ((i - 1) * 24 + 36) %% 216 + 1] <- -BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 10, ((i - 1) * 24 + 38) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 10, ((i - 1) * 24 + 35) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 10, ((i - 1) * 24 + 32) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 10, ((i - 1) * 24 + 41) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 11, (i - 1) * 24 + 11] = 1
  A[((i - 1) * 24) + 11, ((i - 1) * 24 + 37) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1]
  A[((i - 1) * 24) + 11, ((i - 1) * 24 + 33) %% 216 + 1] <- - p * BattingProbability[i, 1]
  A[((i - 1) * 24) + 11, ((i - 1) * 24 + 34) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 11, ((i - 1) * 24 + 35) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 11, ((i - 1) * 24 + 32) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 11, ((i - 1) * 24 + 36) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 11, ((i - 1) * 24 + 42) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 12, (i - 1) * 24 + 12] = 1
  A[((i - 1) * 24) + 12, ((i - 1) * 24 + 33) %% 216 + 1] <- -BattingProbability[i, 1]
  A[((i - 1) * 24) + 12, ((i - 1) * 24 + 34) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 12, ((i - 1) * 24 + 35) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 12, ((i - 1) * 24 + 32) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 12, ((i - 1) * 24 + 37) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 12, ((i - 1) * 24 + 43) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 13, (i - 1) * 24 + 13] = 1
  A[((i - 1) * 24) + 13, ((i - 1) * 24 + 39) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 13, ((i - 1) * 24 + 36) %% 216 + 1] <- - p * BattingProbability[i, 1]
  A[((i - 1) * 24) + 13, ((i - 1) * 24 + 38) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 13, ((i - 1) * 24 + 35) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 13, ((i - 1) * 24 + 32) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 13, ((i - 1) * 24 + 44) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 14, (i - 1) * 24 + 14] = 1
  A[((i - 1) * 24) + 14, ((i - 1) * 24 + 36) %% 216 + 1] <- -BattingProbability[i, 1]
  A[((i - 1) * 24) + 14, ((i - 1) * 24 + 38) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 14, ((i - 1) * 24 + 35) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 14, ((i - 1) * 24 + 32) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 14, ((i - 1) * 24 + 39) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 14, ((i - 1) * 24 + 45) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 15, (i - 1) * 24 + 15] = 1
  A[((i - 1) * 24) + 15, ((i - 1) * 24 + 37) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1]
  A[((i - 1) * 24) + 15, ((i - 1) * 24 + 33) %% 216 + 1] <- - p * BattingProbability[i, 1]
  A[((i - 1) * 24) + 15, ((i - 1) * 24 + 34) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 15, ((i - 1) * 24 + 35) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 15, ((i - 1) * 24 + 32) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 15, ((i - 1) * 24 + 39) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 15, ((i - 1) * 24 + 46) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 16, (i - 1) * 24 + 16] = 1
  A[((i - 1) * 24) + 16, ((i - 1) * 24 + 39) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 16, ((i - 1) * 24 + 36) %% 216 + 1] <- - p * BattingProbability[i, 1]
  A[((i - 1) * 24) + 16, ((i - 1) * 24 + 38) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 16, ((i - 1) * 24 + 35) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 16, ((i - 1) * 24 + 32) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 16, ((i - 1) * 24 + 47) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 17, (i - 1) * 24 + 17] = 1
  A[((i - 1) * 24) + 17, ((i - 1) * 24 + 41) %% 216 + 1] <- -BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 17, ((i - 1) * 24 + 42) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 17, ((i - 1) * 24 + 43) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 17, ((i - 1) * 24 + 40) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 18, (i - 1) * 24 + 18] = 1
  A[((i - 1) * 24) + 18, ((i - 1) * 24 + 44) %% 216 + 1] <- -BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 18, ((i - 1) * 24 + 46) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 18, ((i - 1) * 24 + 43) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 18, ((i - 1) * 24 + 40) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 19, (i - 1) * 24 + 19] = 1
  A[((i - 1) * 24) + 19, ((i - 1) * 24 + 45) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1]
  A[((i - 1) * 24) + 19, ((i - 1) * 24 + 41) %% 216 + 1] <- - p * BattingProbability[i, 1]
  A[((i - 1) * 24) + 19, ((i - 1) * 24 + 42) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 19, ((i - 1) * 24 + 43) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 19, ((i - 1) * 24 + 40) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 19, ((i - 1) * 24 + 44) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 20, (i - 1) * 24 + 20] = 1
  A[((i - 1) * 24) + 20, ((i - 1) * 24 + 41) %% 216 + 1] <- -BattingProbability[i, 1]
  A[((i - 1) * 24) + 20, ((i - 1) * 24 + 42) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 20, ((i - 1) * 24 + 43) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 20, ((i - 1) * 24 + 40) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 20, ((i - 1) * 24 + 45) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 21, (i - 1) * 24 + 21] = 1
  A[((i - 1) * 24) + 21, ((i - 1) * 24 + 47) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 21, ((i - 1) * 24 + 44) %% 216 + 1] <- - p * BattingProbability[i, 1]
  A[((i - 1) * 24) + 21, ((i - 1) * 24 + 46) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 21, ((i - 1) * 24 + 43) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 21, ((i - 1) * 24 + 40) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 22, (i - 1) * 24 + 22] = 1
  A[((i - 1) * 24) + 22, ((i - 1) * 24 + 44) %% 216 + 1] <- -BattingProbability[i, 1]
  A[((i - 1) * 24) + 22, ((i - 1) * 24 + 46) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 22, ((i - 1) * 24 + 43) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 22, ((i - 1) * 24 + 40) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 22, ((i - 1) * 24 + 47) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 23, (i - 1) * 24 + 23] = 1
  A[((i - 1) * 24) + 23, ((i - 1) * 24 + 45) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1]
  A[((i - 1) * 24) + 23, ((i - 1) * 24 + 41) %% 216 + 1] <- - p * BattingProbability[i, 1]
  A[((i - 1) * 24) + 23, ((i - 1) * 24 + 42) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 23, ((i - 1) * 24 + 43) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 23, ((i - 1) * 24 + 40) %% 216 + 1] <- -BattingProbability[i, 4]
  A[((i - 1) * 24) + 23, ((i - 1) * 24 + 47) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 24, (i - 1) * 24 + 24] = 1
  A[((i - 1) * 24) + 24, ((i - 1) * 24 + 47) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 24, ((i - 1) * 24 + 44) %% 216 + 1] <- - p * BattingProbability[i, 1]
  A[((i - 1) * 24) + 24, ((i - 1) * 24 + 46) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 24, ((i - 1) * 24 + 43) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 24, ((i - 1) * 24 + 40) %% 216 + 1] <- -BattingProbability[i, 4]
}
#方程式Ax=bを解く
Expectation <- data.frame(t(matrix(solve(A) %*% b, 24, 9)))
rownames(Expectation) = c("一番打者", "二番打者", "三番打者", "四番打者", "五番打者", "六番打者", "七番打者", "八番打者", "九番打者")
colnames(Expectation) = c("無死走者なし", "無死1塁", "無死2塁", "無死3塁", "無死1・2塁", "無死1・3塁", "無死2・3塁", "無死満塁", 
                          "一死走者なし", "一死1塁", "一死2塁", "一死3塁", "一死1・2塁", "一死1・3塁", "一死2・3塁", "一死満塁", 
                          "二死走者なし", "二死1塁", "二死2塁", "二死3塁", "二死1・2塁", "二死1・3塁", "二死2・3塁", "二死満塁")

##得点確率の計算
#定数項の列ベクトルbを設定
for(k in 1:9){
  for(l in 1:3){
    b[((k - 1) * 24) + ((l - 1) * 8) + 1] <- BattingResult[k, 4]
    b[((k - 1) * 24) + ((l - 1) * 8) + 2] <- BattingResult[k, 3] + BattingResult[k, 4]
    b[((k - 1) * 24) + ((l - 1) * 8) + 3] <- p * BattingResult[k, 1] + BattingResult[k, 2] + BattingResult[k, 3] + BattingResult[k, 4]
    b[((k - 1) * 24) + ((l - 1) * 8) + 4] <- BattingResult[k, 1] + BattingResult[k, 2] + BattingResult[k, 3] + BattingResult[k, 4]
    b[((k - 1) * 24) + ((l - 1) * 8) + 5] <- p * BattingResult[k, 1] + BattingResult[k, 2] + BattingResult[k, 3] + BattingResult[k, 4]
    b[((k - 1) * 24) + ((l - 1) * 8) + 6] <- BattingResult[k, 1] + BattingResult[k, 2] + BattingResult[k, 3] + BattingResult[k, 4]
    b[((k - 1) * 24) + ((l - 1) * 8) + 7] <- BattingResult[k, 1] + BattingResult[k, 2] + BattingResult[k, 3] + BattingResult[k, 4]
    b[((k - 1) * 24) + ((l - 1) * 8) + 8] <- BattingResult[k, 1] + BattingResult[k, 2] + BattingResult[k, 3] + BattingResult[k, 4] + BattingResult[k, 5]
  }
}
#係数行列Aの設定
A = matrix(0, 216, 216)
for(i in 1:9){
  A[((i - 1) * 24) + 1, (i - 1) * 24 + 1] = 1
  A[((i - 1) * 24) + 1, ((i - 1) * 24 + 25) %% 216 + 1] <- -BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 1, ((i - 1) * 24 + 26) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 1, ((i - 1) * 24 + 27) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 1, ((i - 1) * 24 + 32) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 2, (i - 1) * 24 + 2] = 1
  A[((i - 1) * 24) + 2, ((i - 1) * 24 + 28) %% 216 + 1] <- -BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 2, ((i - 1) * 24 + 30) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 2, ((i - 1) * 24 + 33) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 3, (i - 1) * 24 + 3] = 1
  A[((i - 1) * 24) + 3, ((i - 1) * 24 + 29) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1]
  A[((i - 1) * 24) + 3, ((i - 1) * 24 + 28) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 3, ((i - 1) * 24 + 34) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 4, (i - 1) * 24 + 4] = 1
  A[((i - 1) * 24) + 4, ((i - 1) * 24 + 29) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 4, ((i - 1) * 24 + 35) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 5, (i - 1) * 24 + 5] = 1
  A[((i - 1) * 24) + 5, ((i - 1) * 24 + 31) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 5, ((i - 1) * 24 + 36) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 6, (i - 1) * 24 + 6] = 1
  A[((i - 1) * 24) + 6, ((i - 1) * 24 + 31) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 6, ((i - 1) * 24 + 37) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 7, (i - 1) * 24 + 7] = 1
  A[((i - 1) * 24) + 7, ((i - 1) * 24 + 31) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 7, ((i - 1) * 24 + 38) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 8, (i - 1) * 24 + 8] = 1
  A[((i - 1) * 24) + 8, ((i - 1) * 24 + 39) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 9, (i - 1) * 24 + 9] = 1
  A[((i - 1) * 24) + 9, ((i - 1) * 24 + 33) %% 216 + 1] <- -BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 9, ((i - 1) * 24 + 34) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 9, ((i - 1) * 24 + 35) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 9, ((i - 1) * 24 + 40) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 10, (i - 1) * 24 + 10] = 1
  A[((i - 1) * 24) + 10, ((i - 1) * 24 + 36) %% 216 + 1] <- -BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 10, ((i - 1) * 24 + 38) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 10, ((i - 1) * 24 + 41) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 11, (i - 1) * 24 + 11] = 1
  A[((i - 1) * 24) + 11, ((i - 1) * 24 + 37) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1]
  A[((i - 1) * 24) + 11, ((i - 1) * 24 + 36) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 11, ((i - 1) * 24 + 42) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 12, (i - 1) * 24 + 12] = 1
  A[((i - 1) * 24) + 12, ((i - 1) * 24 + 37) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 12, ((i - 1) * 24 + 43) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 13, (i - 1) * 24 + 13] = 1
  A[((i - 1) * 24) + 13, ((i - 1) * 24 + 39) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 13, ((i - 1) * 24 + 44) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 14, (i - 1) * 24 + 14] = 1
  A[((i - 1) * 24) + 14, ((i - 1) * 24 + 39) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 14, ((i - 1) * 24 + 45) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 15, (i - 1) * 24 + 15] = 1
  A[((i - 1) * 24) + 15, ((i - 1) * 24 + 39) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 15, ((i - 1) * 24 + 46) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 16, (i - 1) * 24 + 16] = 1
  A[((i - 1) * 24) + 16, ((i - 1) * 24 + 47) %% 216 + 1] <- -BattingProbability[i, 6]
  A[((i - 1) * 24) + 17, (i - 1) * 24 + 17] = 1
  A[((i - 1) * 24) + 17, ((i - 1) * 24 + 41) %% 216 + 1] <- -BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 17, ((i - 1) * 24 + 42) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 17, ((i - 1) * 24 + 43) %% 216 + 1] <- -BattingProbability[i, 3]
  A[((i - 1) * 24) + 18, (i - 1) * 24 + 18] = 1
  A[((i - 1) * 24) + 18, ((i - 1) * 24 + 44) %% 216 + 1] <- -BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 18, ((i - 1) * 24 + 46) %% 216 + 1] <- -BattingProbability[i, 2]
  A[((i - 1) * 24) + 19, (i - 1) * 24 + 19] = 1
  A[((i - 1) * 24) + 19, ((i - 1) * 24 + 45) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1]
  A[((i - 1) * 24) + 19, ((i - 1) * 24 + 44) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 20, (i - 1) * 24 + 20] = 1
  A[((i - 1) * 24) + 20, ((i - 1) * 24 + 45) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 21, (i - 1) * 24 + 21] = 1
  A[((i - 1) * 24) + 21, ((i - 1) * 24 + 47) %% 216 + 1] <- - (1 - p) * BattingProbability[i, 1] - BattingProbability[i, 5]
  A[((i - 1) * 24) + 22, (i - 1) * 24 + 22] = 1
  A[((i - 1) * 24) + 22, ((i - 1) * 24 + 47) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 23, (i - 1) * 24 + 23] = 1
  A[((i - 1) * 24) + 23, ((i - 1) * 24 + 47) %% 216 + 1] <- -BattingProbability[i, 5]
  A[((i - 1) * 24) + 24, (i - 1) * 24 + 24] = 1
}
#方程式Ax=bを解く
Probability <- data.frame(t(matrix(solve(A) %*% b, 24, 9)))
rownames(Probability) = c("一番打者", "二番打者", "三番打者", "四番打者", "五番打者", "六番打者", "七番打者", "八番打者", "九番打者")
colnames(Probability) = c("無死走者なし", "無死1塁", "無死2塁", "無死3塁", "無死1・2塁", "無死1・3塁", "無死2・3塁", "無死満塁", 
                          "一死走者なし", "一死1塁", "一死2塁", "一死3塁", "一死1・2塁", "一死1・3塁", "一死2・3塁", "一死満塁", 
                          "二死走者なし", "二死1塁", "二死2塁", "二死3塁", "二死1・2塁", "二死1・3塁", "二死2・3塁", "二死満塁")

