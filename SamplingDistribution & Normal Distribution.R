### KEMBALI LAGI ###

# Kita akan masuk kepada konsep sampling distribution dengan konsep koin

# ============================================================
# Sampling Distribution of the Mean, Normal Distribution dan Z-Score — Experimental via Simulation
# Butir 6 — kembali sebelum lanjut ke hypothesis testing (butir 13)
# Butir 7 - sikat ke normal distribution dan z-score
# ============================================================

# --- 1. Populasi yang JELAS tidak normal (eksponensial, skewed kanan) ---
set.seed(42)
pop_rate <- 0.5
pop_mean <- 1 / pop_rate          # E[X] untuk eksponensial = 1/rate
pop_sd   <- 1 / pop_rate          # SD untuk eksponensial = 1/rate

# Cek bentuk populasi asli — HARUS skewed, bukan lonceng
population_sample <- rexp(100000, rate = pop_rate)
hist(population_sample, breaks = 50, main = "Distribusi Populasi (Eksponensial)",
     xlab = "X", col = "grey80")

# --- 2. Fungsi manual: ambil satu sampel, hitung mean-nya ---
one_sample_mean <- function(n, rate) {
  sampel <- rexp(n, rate = rate)   # tarik n observasi dari populasi
  return(mean(sampel))             # hitung mean sampel itu
}

# --- 3. Bangun distribusi sampling SECARA MANUAL pakai for loop ---
simulate_sampling_distribution <- function(n, n_reps, rate) {
  hasil <- numeric(n_reps)          # wadah kosong buat nampung tiap mean
  for (i in 1:n_reps) {
    hasil[i] <- one_sample_mean(n, rate)
  }
  return(hasil)
}

n_reps <- 10000

# --- 4. Jalankan untuk beberapa ukuran sampel ---
dist_n5   <- simulate_sampling_distribution(n = 5,   n_reps = n_reps, rate = pop_rate)
dist_n30  <- simulate_sampling_distribution(n = 30,  n_reps = n_reps, rate = pop_rate)
dist_n100 <- simulate_sampling_distribution(n = 100, n_reps = n_reps, rate = pop_rate)

# --- 5. Bandingkan empirical mean & SD vs prediksi teori CLT ---
cat("n=5   -> empirical mean:", mean(dist_n5),
    "| empirical SE:", sd(dist_n5),
    "| teori SE (sigma/sqrt(n)):", pop_sd / sqrt(5), "\n")
cat("n=30  -> empirical mean:", mean(dist_n30),
    "| empirical SE:", sd(dist_n30),
    "| teori SE (sigma/sqrt(n)):", pop_sd / sqrt(30), "\n")
cat("n=100 -> empirical mean:", mean(dist_n100),
    "| empirical SE:", sd(dist_n100),
    "| teori SE (sigma/sqrt(n)):", pop_sd / sqrt(100), "\n")

# --- 6. Visualisasi: histogram distribusi sampling + kurva normal teori ---
par(mfrow = c(1, 3))

plot_sampling_dist <- function(dist, n, rate) {
  hist(dist, breaks = 40, freq = FALSE, col = "skyblue",
       main = paste("n =", n), xlab = "Sample Mean")
  teori_se <- (1 / rate) / sqrt(n)
  curve(dnorm(x, mean = 1/rate, sd = teori_se), add = TRUE,
        col = "red", lwd = 2)
}

plot_sampling_dist(dist_n5,   5,   pop_rate)
plot_sampling_dist(dist_n30,  30,  pop_rate)
plot_sampling_dist(dist_n100, 100, pop_rate)

par(mfrow = c(1, 1))

### MASUK KE KONSEP NORMAL DISTRIBUTION BOSKUH ###

pnorm(0) # Hasilnya 0.5 karena berdiri pada sisi 0 kurva normal (jadi sisi kanan dan kiri adalah 0.5)

pnorm(1)
pnorm(-1)
pnorm(1.96)
pnorm(-1.96)

pnorm(1.96) - pnorm(-1.96)

# Again, all are code instruction, purpose for understanding simulation