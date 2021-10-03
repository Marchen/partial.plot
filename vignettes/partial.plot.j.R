## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  install.packages(
#      c("model.adapter", "partial.plot"), type = "source",
#      repos = c(
#          "http://florivory.net/R/repos", options()$repos
#      )
#  )

## ---- echo = FALSE, message = FALSE-------------------------------------------
library(partial.plot)

## -----------------------------------------------------------------------------
# データの読み込み。
data(iris)

# 予測モデルの作成。
model <- glm(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
)

# ライブラリの読み込み。
library(partial.plot)

# 萼片の長さと花弁の長さとの関係を作図。
info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)

# 凡例を追加。
pp.legend(info, "topleft")

# 三次元プロット。
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), col = terrain.colors,
    theta = 20
)


## -----------------------------------------------------------------------------
# データの読み込み。
data(iris)

# データの構造を見てみる。
str(iris)

## -----------------------------------------------------------------------------
# データの可視化。
par(mfrow = c(2, 2))
plot(
    Petal.Length ~ Sepal.Length, data = iris,
    pch = as.numeric(Species) + 14, col = as.numeric(Species) + 1
)
plot(
    Petal.Length ~ Sepal.Width, data = iris,
    pch = as.numeric(Species) + 14, col = as.numeric(Species) + 1
)
plot(
    Petal.Length ~ Petal.Width, data = iris,
    pch = as.numeric(Species) + 14, col = as.numeric(Species) + 1
)

## -----------------------------------------------------------------------------
# 予測モデルの作成。
model <- glm(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
)

# 結果を確認します。
summary(model)

## -----------------------------------------------------------------------------
# ライブラリの読み込み。
library(partial.plot)

# 萼片の長さと花弁の長さとの関係を作図。
partial.plot(model, c("Sepal.Length", "Species"), pch = 16)

## -----------------------------------------------------------------------------
# 花弁の幅と花弁の長さとの関係を作図する。
partial.plot(model, c("Petal.Width", "Species"), pch = 16)

## ---- eval = FALSE------------------------------------------------------------
#  # 萼片の長さと花弁の長さとの関係を作図。
#  info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)

## -----------------------------------------------------------------------------
# 凡例を追加。
info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
# CO2データを読み込む
data(CO2)

# データフレームに変換
class(CO2) <- "data.frame"

# データの傾向を見てみる。
plot(
    uptake ~ conc, data = CO2,
    col = c("blue3", "red3")[as.numeric(CO2$Type)],
    pch = c(16, 17)[as.numeric(CO2$Treatment)],
    ylim = c(0, 50)
)
legend(
    "bottomright",
    legend = c(
        "Mississippi - Chilled", "Mississippi - Non-Chilled",
        "Quebec - Chilled", "Quebec - Non-Chilled"
    ),
    col = rep(c("blue3", "red3"), each = 2),
    pch = c(16, 17, 16, 17)
)


## -----------------------------------------------------------------------------
model <- glm(uptake ~ conc + I(conc^2) + Type * Treatment, data = CO2)
summary(model)

## -----------------------------------------------------------------------------
info <- partial.plot(
    model, c("conc", "Treatment", "Type"), data = CO2
)
pp.legend(info, "bottomright")

## -----------------------------------------------------------------------------
# データの読み込み。
data(iris)

# 予測モデルの作成。
model <- glm(
    Petal.Length ~ Sepal.Length + Petal.Width, data = iris
)

# 予測結果を見る。
summary(model)

## -----------------------------------------------------------------------------
partial.plot(
    model, c("Sepal.Length", "Petal.Width"), col = terrain.colors, theta = 20
)

## -----------------------------------------------------------------------------
partial.plot(
    model, c("Sepal.Length", "Petal.Width"), fun.3d = image,
    col = terrain.colors
)
partial.plot(
    model, c("Sepal.Length", "Petal.Width"), fun.3d = contour, col = "black"
)

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  # rglパッケージをインストール。
#  install.packages("rgl")
#  
#  # 三次元プロット
#  partial.plot(
#      model, c("Sepal.Length", "Petal.Width"), fun.3d = persp3d,
#      col = terrain.colors
#  )

## ---- message = FALSE, echo = FALSE-------------------------------------------
# 予測モデルの作成。
model <- glm(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
)

## -----------------------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), draw.residual = FALSE
)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), draw.relationship = FALSE,
    pch = 16
)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), draw.interval = FALSE,
    pch = 16
)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, extrapolate = TRUE
)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
# グラフの色を変える。
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, col = rainbow
)
# 凡例の色も自動調節される。
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
# 色ベクトルを用意。
col <- c(
    setosa = "darkgreen", versicolor = "blue", virginica = "orange2"
)
# 作図
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, col = col
)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16:18, col = rainbow
)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), lty = c("solid", "dashed", "dotted"),
    col = "black"
)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), lwd = c(1, 4, 8), col = "black"
)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
model <- glm(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
)
plot(Petal.Length ~ Sepal.Length, data = iris, pch = 16)
partial.plot(model, c("Sepal.Length", "Species"), add = TRUE)

## -----------------------------------------------------------------------------
model <- glm(uptake ~ conc + I(conc^2) + Type * Treatment, data = CO2)
info <- partial.plot(
    model, c("conc", "Treatment", "Type"), data = CO2, sep = "/"
)
pp.legend(info, "bottomright")

## -----------------------------------------------------------------------------
par(mfrow = c(2, 2))
model <- glm(uptake ~ conc + I(conc^2) + Type * Treatment, data = CO2)
# 95%（デフォルト）
info <- partial.plot(
    model, c("conc", "Treatment", "Type"), data = CO2,
    main = "interval.levels = 0.95"
)
# 80%
info <- partial.plot(
    model, c("conc", "Treatment", "Type"), data = CO2, interval.levels = 0.8,
    main = "interval.levels = 0.8"
)
# 70%
info <- partial.plot(
    model, c("conc", "Treatment", "Type"), data = CO2, interval.levels = 0.7,
    main = "interval.levels = 0.7"
)
# 60%
info <- partial.plot(
    model, c("conc", "Treatment", "Type"), data = CO2, interval.levels = 0.6,
    main = "interval.levels = 0.6"
)
par(mfrow = c(1, 1))


## -----------------------------------------------------------------------------
model <- glm(
    Petal.Length ~ (Sepal.Length + Petal.Width), data = iris,
    family = Gamma(log)
)
summary(model)

## -----------------------------------------------------------------------------
partial.plot(
    model, c("Sepal.Length", "Petal.Width"), type = "response",
    col = terrain.colors, main = "response"
)
partial.plot(
    model, c("Sepal.Length", "Petal.Width"), type = "link",
    col = terrain.colors, main = "link"
)

## -----------------------------------------------------------------------------
library(randomForest)
model <- randomForest(Species ~ ., data = iris)
partial.plot(
    model, "Petal.Length", positive.class = "setosa", type = "prob",
    col = "red", resolution = 20, draw.residual = FALSE
)
partial.plot(
    model, "Petal.Length", positive.class = "versicolor", add = TRUE,
    type = "prob", col = "blue", resolution = 20, draw.residual = FALSE
)
partial.plot(
    model, "Petal.Length", positive.class = "virginica", add = TRUE,
    type = "prob", col = "green", resolution = 20, draw.residual = FALSE
)

## -----------------------------------------------------------------------------
model <- glm(Petal.Length ~ ., data = iris)
info <- partial.plot(
    model, c("Sepal.Length", "Species"), draw.hist = TRUE
)

## -----------------------------------------------------------------------------
# モデル作成
model <- glm(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
)
# シンボルのサイズ、軸ラベルも変えてみる。
info <- partial.plot(
    model, c("Sepal.Length", "Species"), cex = 1.5,
    xlab = "Sepal length (mm)", ylab = "Petal Length"
)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
par(mfrow = c(2, 2))

# 最初のpartial.plotを実行。
info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
pp.legend(info, "topleft", cex = 0.5)

# 結果を再利用して、シンボルと関係式描画の設定を変える。
info <- partial.plot(info, pch = 6, draw.relationship = FALSE)
pp.legend(info, "topleft", cex = 0.5)

# 結果を再利用して、シンボル、色、信頼区間描画の設定を変える。
info <- partial.plot(info, col = rainbow, draw.interval = FALSE)
pp.legend(info, "topleft", cex = 0.5)

# 結果を再利用して、シンボル、色、偏残差描画の設定を変える。
info <- partial.plot(info, col = heat.colors, draw.residual = FALSE)
pp.legend(info, "topleft", cex = 0.5)

par(mfrow = c(1, 1))

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  data$column1 <- c(scale(data$column1))

## ---- eval = FALSE------------------------------------------------------------
#  # 例
#  model <- glmer.nb(y ~ log(x) + (1 | random), data = dat)
#  partial.plot(model, "x", data = dat)

## -----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#   データとモデルの準備
#-----------------------------------------------------------------------------

# MASSパッケージから車の保険請求データを準備。
utils::data(Insurance, package="MASS")

# 年齢をカテゴリーから数値データに変換。
Insurance$Age.numeric <- c(25, 27, 32.5, 35)[as.numeric(Insurance$Age)]

# 保険請求数を地区・車の種類・年齢でモデル化。
# offset項は契約者数。
model <- glm(
    Claims ~ District + Group + Age.numeric + offset(log(Holders)),
    data = Insurance, family = poisson
)

#-----------------------------------------------------------------------------
#   作図１：失敗例
#-----------------------------------------------------------------------------

# 平均の契約者数を使ってpartial.plotを描画。
# そのまま描画すると偏残差と予測値が対応しない。
pp <- partial.plot(model, c("Age.numeric", "Group"))
pp.legend(pp, "topright")

#-----------------------------------------------------------------------------
#   対策１：偏残差を描画しない
#   この場合は平均契約者数あたりの請求数が描画される。
#-----------------------------------------------------------------------------
pp <- partial.plot(model, c("Age.numeric", "Group"), draw.residual = FALSE)
pp.legend(pp, "topright")

#-----------------------------------------------------------------------------
#   対策２：offset項あたりの応答変数を用いる
#   この場合は契約者数あたりの請求数を先に計算し、
#   契約者数を１に固定したデータを用いる。
#-----------------------------------------------------------------------------

# 描画用にデータのコピーを作成。
Insurance2 <- Insurance

# 応答変数を契約者あたりの請求数に変換。
Insurance2$Claims <- Insurance2$Claims / Insurance2$Holders

# 契約者数を１に固定。
Insurance2$Holders <- 1

# 契約者数あたりの請求数に対してモデルの予測値を描画。
pp <- partial.plot(model, c("Age.numeric", "District"), data = Insurance2)
pp.legend(pp, "topright")


## -----------------------------------------------------------------------------
library(partial.plot)
plot(
    Petal.Length ~ Sepal.Length, col = color.ramp(Species),
    data = iris, pch = 16
)

## -----------------------------------------------------------------------------
plot(
    Petal.Length ~ Sepal.Length, col = color.ramp(Species, rainbow),
    data = iris, pch = 16
)

## -----------------------------------------------------------------------------
col <- c(
    setosa = "darkgreen", versicolor = "blue", virginica = "orange2"
)
plot(
    Petal.Length ~ Sepal.Length, col = color.ramp(Species, col),
    data = iris, pch = 16
)

## -----------------------------------------------------------------------------
plot(Petal.Length~Sepal.Length, data = iris, pch = switch.par(Species, 16:18))

## -----------------------------------------------------------------------------
barplot(1:10, col = gg.colors(10))

