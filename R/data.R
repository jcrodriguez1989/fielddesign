#' Data of Grain Yield of Rice
#'
#' @description
#' This dataset contains yield data from a uniformity trial with rice
#' (Oryza sativa) of the cultivar Palmar 18. The trial was conducted at
#' Hacienda Mojica, located in the cantón of Bagaces, Guanacaste, Costa Rica.
#' The experiment was carried out on a 20m x 20m (400m²) plot, which was
#' divided into 400 basic experimental units of 1m² each. The yield of
#' each unit is recorded in grams.
#'
#' @usage
#' data("arroz")
#'
#' @format
#' A matrix with 20 rows and 20 columns, where each cell represents the rice
#' yield in grams of a 1m² subplot.
#'
#' @details
#' \itemize{
#'   \item \strong{Crop:} Rice (Oryza sativa)
#'   \item \strong{Cultivar:} Palmar 18
#'   \item \strong{Planting Method:} Direct seeding with rows separated by 17.6cm.
#'   \item \strong{Total Area:} 20m x 20m
#'   \item \strong{Basic Unit:} 1m x 1m
#'   \item \strong{Data:} Yield in grams per basic unit.
#'   \item \strong{Dimensions:} The matrix has 20 rows (length, Y-axis) and 20 columns (width,
#'     X-axis).
#' }
#'
#' @keywords datasets
#'
#' @source
#' \doi{10.22458/urj.v11i3.2653}
#'
#' @references
#' Vargas-Rojas, J. C., & Navarro-Flores, J. R. (2019). Tamaño y forma de unidad
#' experimental para ensayos de rendimiento de arroz (Oryza sativa), en
#' Guanacaste, Costa Rica. UNED Research Journal, 11(3).
#' \doi{10.22458/urj.v11i3.2653}
#'
#' @examples
#' library("fielddesign")
#' data("arroz")
#' # View the first 6 rows and 6 columns.
#' arroz[1:6, 1:6]
#'
"arroz"

#' Data of Grain Yield of Common Bean
#'
#' @description
#' This dataset contains yield data from a uniformity trial with common bean
#' (Phaseolus vulgaris L.) of the variety Nambí. The trial was conducted at the
#' Finca Experimental de Santa Cruz, owned by the University of Costa Rica,
#' located in the cantón of Santa Cruz, Guanacaste, Costa Rica. The experiment
#' was carried out on a 20m x 10m (200m²) plot, which was divided into 200
#' basic experimental units, each corresponding to a one-meter row. The yield
#' of each unit is recorded in grams.
#'
#' @usage
#' data("frijol")
#'
#' @format
#' A matrix with 10 rows and 20 columns, where each cell represents the
#' common bean yield in grams of a 1m² subplot.
#'
#' @details
#' \itemize{
#'   \item \strong{Crop:} Common bean (Phaseolus vulgaris L.)
#'   \item \strong{Variety:} Nambí
#'   \item \strong{Location:} Finca Experimental de Santa Cruz, Guanacaste, Costa Rica
#'   \item \strong{Planting Density:} 150,000 plants/ha
#'   \item \strong{Total Area:} 20m x 10m
#'   \item \strong{Basic Unit:} 1m x 1m (one linear meter of a row)
#'   \item \strong{Data:} Yield in grams per basic unit.
#'   \item \strong{Dimensions:} The matrix has 10 rows (length, Y-axis) and 20 columns (width,
#'     X-axis).
#' }
#'
#' @keywords datasets
#'
#' @source
#' Data from an unpublished study. For more details, see the references.
#'
#' @references
#' Vargas-Rojas, J. C., Hernández-González, G., & Ríos Gómez, E. (in preparation).
#' Tamaño y forma de la unidad experimental para ensayos de rendimiento con la
#' variedad de frijol Nambí (Phaseolus vulgaris L.), Santa Cruz, Guanacaste.
#'
#' @examples
#' library("fielddesign")
#' data("frijol")
#' # View the first 6 rows and 6 columns.
#' frijol[1:6, 1:6]
#'
"frijol"

#' Data of Grain Yield of Corn
#'
#' @description
#' This dataset contains the yield data from a uniformity trial with corn
#' (Zea mays L.) of the hybrid HS5G. The trial was conducted at the Finca
#' Experimental de Santa Cruz, owned by the University of Costa Rica, located
#' in the cantón of Santa Cruz, Guanacaste, Costa Rica. The experiment was
#' carried out on a 20m x 20m (400m²) plot, which was divided into 400 basic
#' experimental units, each corresponding to a one-meter row. The yield of
#' each unit is recorded in grams.
#'
#' @usage
#' data("maiz")
#'
#' @format
#' A matrix with 20 rows and 20 columns, where each cell represents the corn
#' yield in grams of a 1m² subplot.
#'
#' @details
#' \itemize{
#'   \item \strong{Crop:} Corn (Zea mays L.)
#'   \item \strong{Hybrid:} HS5G
#'   \item \strong{Planting Density:} 40,000 plants/ha (1m between rows, 0.25m between plants)
#'   \item \strong{Total Area:} 20m x 20m
#'   \item \strong{Basic Unit:} 1m x 1m (one linear meter of a row)
#'   \item \strong{Data:} Yield in grams per basic unit.
#'   \item \strong{Dimensions:} The matrix has 20 rows (length, Y-axis) and 20 columns (width,
#'     X-axis).
#' }
#'
#' @keywords datasets
#'
#' @source
#' \doi{10.15517/isucr.v21i43.41972}
#'
#' @references
#' Vargas-Rojas, J. C., & Navarro-Flores, J. R. (2020). Determinación del tamaño y la forma de
#' unidad experimental, con el método de regresión múltiple, para ensayos de rendimiento de maíz
#' (Zea mays), Guanacaste, Costa Rica. InterSedes, 21(43), 1-10.
#' \doi{10.15517/isucr.v21i43.41972}
#'
#' @examples
#' library("fielddesign")
#' data("maiz")
#' # View the first 6 rows and 6 columns.
#' maiz[1:6, 1:6]
#'
"maiz"

#' Data of Grain Yield of Cowpea Bean
#'
#' @description
#' This dataset contains yield data from a uniformity trial with cowpea bean
#' (Vigna unguiculata). The trial was conducted at the Finca Experimental de
#' Santa Cruz, owned by the University of Costa Rica, located in the cantón
#' of Santa Cruz, Guanacaste, Costa Rica. The experiment was carried out on a
#' 20m x 10m (200m²) plot, which was divided into 200 basic experimental
#' units of 1m² each. The yield of each unit is recorded in grams.
#'
#' @usage
#' data("vigna")
#'
#' @format
#' A matrix with 10 rows and 20 columns, where each cell represents the
#' cowpea bean yield in grams of a 1m² subplot.
#'
#' @details
#' \itemize{
#'   \item \strong{Crop:} Cowpea bean (Vigna unguiculata)
#'   \item \strong{Location:} Finca Experimental de Santa Cruz, Guanacaste, Costa Rica
#'   \item \strong{Planting Density:} 100,000 plants/ha
#'   \item \strong{Total Area:} 20m x 10m
#'   \item \strong{Basic Unit:} 1m x 1m
#'   \item \strong{Data:} Yield in grams per basic unit.
#'   \item \strong{Dimensions:} The matrix has 10 rows (length, Y-axis) and 20 columns (width,
#'     X-axis).
#' }
#'
#' @keywords datasets
#'
#' @source
#' Data from an unpublished study. For more details, see the references.
#'
#' @references
#' Hernández-González, G., Vargas-Rojas, J. C., & Ríos Gómez, E. (in preparation).
#' Tamaño y forma de la unidad experimental para ensayos de rendimiento de
#' frijol caupí (Vigna unguiculata), Santa Cruz, Guanacaste.
#'
#' @examples
#' library("fielddesign")
#' data("vigna")
#' # View the first 6 rows and 6 columns.
#' vigna[1:6, 1:6]
#'
"vigna"
