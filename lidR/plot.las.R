LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
lidar = readLAS(LASfile)

plot(lidar)

# Outliers of intensity breaks the color range. Use the trim parameter.
plot(lidar, color = "Intensity", colorPalette = heat.colors(50))
plot(lidar, color = "Intensity", colorPalette = heat.colors(50), trim = 0.99)
