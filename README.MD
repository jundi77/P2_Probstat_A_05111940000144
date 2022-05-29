# Praktikum 2 Probstat

Berikut adalah hasil pengerjaan praktikum 2.

![image](https://user-images.githubusercontent.com/40772378/170873854-66a9a284-5cf3-4c77-8854-443becff3a2e.png)

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

![image](https://user-images.githubusercontent.com/40772378/170873867-7ac0bf55-e95e-4c76-9ab2-26f325c39906.png)

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

![image](https://user-images.githubusercontent.com/40772378/170873898-324b81c9-82d5-405c-93ee-e95de83d40d1.png)

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

![image](https://user-images.githubusercontent.com/40772378/170873912-56b62905-e4fd-4809-af80-3878eb43db17.png)

Telah dilakukan import dataset nomor 4 dan 5. Nomor 4 menggunakan read.table dengan delimiter tab, nomor 5 menggunakan read.csv.
