# Praktikum 2 Probstat

Berikut adalah hasil pengerjaan praktikum 2.

[Gambar pengerjaan nomor 1]
Untuk nomor 1, dimasukkan dahulu variabel-variabel yang didapat dari penjelasan, kemudian dilakukan penghitungan menggunakan t.test two.sided, menghasilkan output sebagai berikut:

```
	One Sample t-test

data:  no_1_selisih
t = 0, df = 8, p-value = 1
alternative hypothesis: true mean is not equal to 16.22222
95 percent confidence interval:
 11.33381 21.11064
sample estimates:
mean of x 
 16.22222 
```

[Gambar pengerjaan nomor 2]
Untuk nomor 2, dimasukkan dahulu variabel-variabel yang didapat dari penjelasan, kemudian dilakukan penghitungan menggunakan zsum.test greater, menghasilkan output sebagai berikut:

```
	One-sample z-Test

data:  Summarized x
z = 8.9744, p-value < 2.2e-16
alternative hypothesis: true mean is greater than 20000
95 percent confidence interval:
 22858.51       NA
sample estimates:
mean of x 
    23500 
```

[Gambar pengerjaan nomor 3]
Untuk nomor 3, dimasukkan dahulu variabel-variabel yang didapat dari penjelasan, kemudian dilakukan pembuatan sampel random menggunakan fungsi rt, lalu 
dilakukan penghitungan menggunakan tsum.test two.sided, menghasilkan output sebagai berikut:

```
	Standard Two-Sample t-Test

data:  Summarized x and y
t = 1.2725, df = 44, p-value = 0.2099
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.4961981  2.1961981
sample estimates:
mean of x mean of y 
     3.64      2.79 
```

[Gambar masukkan dataset nomor 4 dan 5]
Telah dilakukan import dataset nomor 4 dan 5. Nomor 4 menggunakan read.table dengan delimiter tab, nomor 5 menggunakan read.csv.