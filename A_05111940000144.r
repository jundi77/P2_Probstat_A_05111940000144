library(BSDA)

no_1_x = c(78, 75, 67, 77, 70, 72, 78, 74, 77)
no_1_y = c(100, 95, 70, 90, 90, 90, 89, 90, 100)
no_1_selisih = abs(no_1_x - no_1_y)
no_1_mean = mean(no_1_selisih)
no_1_s_numerator = sum((no_1_selisih - no_1_mean) ^ 2)
no_1_s = no_1_stdev_numerator / length(no_1_selisih)
no_1_stdev = sqrt(no_1_s)
# H0: rata2 selisih = no_1_mean
# H1: rata2 selisih != no_1_mean
no_1_ans_b_c = t.test(no_1_selisih, alternative = 'two.sided', conf.level = 0.95, mu = no_1_mean)

no_2_xbar = 20000
no_2_n = 100
no_2_miu = 23500
no_2_stdev = 3900
zsum.test(conf.level=0.95, n.x=no_2_n, mean.x=no_2_miu, sigma.x=no_2_stdev, mu=no_2_xbar, alternative="greater")
# a. Tidak.
# b. Berdasarkan output yang dihasilkan, diperoleh nilai z 8.9744 dengan p-value < 2.2e-16.
# c. Dapat disimpulkan bahwa terdapat cukup bukti di mana rata-rata secara signifikan adalah lebih besar dari 20000.


### No. 3 Pooled Variance T
# H0: no_3_miu_bg != no_3_miu_ba
# H1: no_3_miu_bg = no_3_miu_ba
no_3_n_bg = 19
no_3_n_ba = 27
no_3_mean_bg = 3.64
no_3_mean_ba = 2.79
no_3_std_bg = 1.67
no_3_std_ba = 1.32
no_3_s_bg = no_3_std_bg ^ 2
no_3_s_ba = no_3_std_ba ^ 2
no_3_conf_level = 0.95
no_3_sample_bg = rt(no_3_n_bg, no_3_n_bg - 1)
no_3_sample_ba = rt(no_3_n_ba, no_3_n_ba - 1)
tsum.test(
  mean.x = no_3_mean_bg,
  mean.y = no_3_mean_ba,
  s.x = no_3_s_bg,
  s.y = no_3_s_ba,
  n.x = no_3_n_bg,
  n.y = no_3_n_ba,
  alternative = 'two.sided',
  mu = 0, var.equal = TRUE,
  conf.level = 0.95
)

setwd("~/Documents/P2_Probstat_A_05111940000144")
no_4_dataset = read.table(file = 'onewayanova.tsv', sep = '\t', header = TRUE)
no_5_dataset = read.csv(file = 'GTL.csv')