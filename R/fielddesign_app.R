#' Launch the Field Design Shiny Gadget
#'
#' This function launches a Shiny gadget for interactive analysis of experimental unit size and
#' spatial variation in yield data.
#'
#' @param x An optional argument for initial yield data. Can be:
#'   - `NULL` (default): The app will start with no data loaded, and the user can upload a file.
#'   - A character string: Path to a file containing yield data. The file is read as a table and
#'     converted to a matrix.
#'   - A matrix or data frame with the data.
#'
#' @return No return value. Called for its side effects to launch the Field Design Shiny Gadget.
#'
#' @examples
#' if (interactive()) {
#'   fielddesign_app()
#'   # Specify the input data.
#'   fielddesign_app(matrix(rnorm(130, 500, 60), nrow = 10))
#' }
#'
#' @importFrom shiny runGadget
#'
#' @export
#'
fielddesign_app <- function(x = NULL) {
  runGadget(fielddesign_ui(), fielddesign_server(x))
  invisible()
}

# nocov start

#' Field Design Shiny UI
#'
#' @importFrom bslib accordion accordion_panel nav_panel navset_card_underline page_sidebar sidebar
#' @importFrom plotly plotlyOutput
#' @importFrom shiny actionButton br checkboxInput column fileInput fluidRow h6 numericInput
#' @importFrom shiny selectInput tableOutput textOutput uiOutput
#' @importFrom waiter useWaiter
#'
#' @keywords internal
#'
fielddesign_ui <- function() {
  page_sidebar(
    title = "Experimental Unit Size Analysis",
    sidebar = sidebar(
      fileInput("input_file", "Load yield data file"),
      accordion(
        open = FALSE,
        accordion_panel(
          "Exhaustive",
          checkboxInput("exh_combine_orientations", "Combine Orientations", TRUE)
          # TODO: max_area.
        ),
        accordion_panel(
          "Tiling",
          selectInput("til_exclusion", "Exclusion", c("smith", "only_full"))
        ),
        accordion_panel(
          "Optimal Plot Size",
          "Penalty for Deviations",
          fluidRow(
            column(6, numericInput("tau_height", "Tau Height", 1, min = 0, step = 0.5)),
            column(6, numericInput("tau_width", "Tau Width", 1, min = 0, step = 0.5))
          )
        )
      ),
      actionButton("run_analysis", "Run Analysis"),
    ),
    useWaiter(),
    accordion(
      accordion_panel(
        "Input Data",
        h6(textOutput("input_data_dims")), br(),
        navset_card_underline(
          nav_panel("Plot", plotlyOutput("input_data_plot")),
          nav_panel("Table", tableOutput("input_data_table"))
        )
      )
    ),
    uiOutput("spatial_variation_panel_ui"),
    uiOutput("optimal_eu_size_panel_ui")
  )
}

#' Field Design Shiny Server
#'
#' @importFrom bslib accordion accordion_panel nav_panel navset_card_underline
#' @importFrom DT datatable DTOutput formatRound renderDT
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom shiny br column fluidRow h5 h6 hr htmlOutput observeEvent plotOutput reactive
#' @importFrom shiny reactiveVal renderPlot renderPrint renderTable renderText renderUI req
#' @importFrom shiny tableOutput verbatimTextOutput
#' @importFrom stats AIC anova cor median setNames
#' @importFrom utils read.table
#' @importFrom waiter waiter_hide waiter_show
#'
#' @keywords internal
#'
fielddesign_server <- function(x) {
  if (is.character(x)) {
    if (file.exists(x)) {
      x <- as.matrix(read.table(x))
    } else {
      stop("Yield data file not found")
    }
  } else if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  function(input, output, session) {
    r_input_data <- reactiveVal(x)
    r_sv_exh <- reactiveVal()
    r_sv_til <- reactiveVal()
    r_ops_exh_int <- reactiveVal()
    r_ops_exh_no_int <- reactiveVal()
    r_ops_til_int <- reactiveVal()
    r_ops_til_no_int <- reactiveVal()
    r_tau <- reactive(c(input$tau_height, input$tau_width))
    observeEvent(input$input_file, {
      r_sv_exh(NULL)
      r_sv_til(NULL)
      r_ops_exh_int(NULL)
      r_ops_exh_no_int(NULL)
      r_ops_til_int(NULL)
      r_ops_til_no_int(NULL)
      r_input_data(as.matrix(read.table(input$input_file$datapath)))
    })
    observeEvent(input$run_analysis, {
      waiter_show()
      on.exit(waiter_hide())
      r_sv_exh(spatial_variation_exhaustive(
        r_input_data(), input$exh_combine_orientations,
        return_sums = TRUE
      ))
      r_sv_til(spatial_variation_tiling(
        r_input_data(), input$til_exclusion,
        return_sums = TRUE
      ))
    })
    ### Exhaustive.
    observeEvent(r_sv_exh(), {
      x <- r_sv_exh()$res
      suppressWarnings(r_ops_exh_int(fit_optimal_plot_size(x, nrow(x), ncol(x), tau = r_tau())))
      suppressWarnings(r_ops_exh_no_int(fit_optimal_plot_size(
        x, nrow(x), ncol(x),
        include_interaction = FALSE, tau = r_tau()
      )))
    })
    ### Tiling.
    observeEvent(r_sv_til(), {
      x <- r_sv_til()$res
      suppressWarnings(r_ops_til_int(fit_optimal_plot_size(x, nrow(x), ncol(x), tau = r_tau())))
      suppressWarnings(r_ops_til_no_int(fit_optimal_plot_size(
        x, nrow(x), ncol(x),
        include_interaction = FALSE, tau = r_tau()
      )))
    })
    ### Outputs.
    # Input Data.
    output$input_data_dims <- renderText({
      if (is.null(r_input_data())) {
        "Load the yield input data"
      } else {
        paste("Matrix dimensions: ", nrow(r_input_data()), "x", ncol(r_input_data()))
      }
    })
    output$input_data_plot <- renderPlotly({
      req(r_input_data())
      ggplotly(plot_yield_contour(r_input_data()))
    })
    output$input_data_table <- renderTable(
      {
        req(r_input_data())
        r_input_data()
      },
      rownames = FALSE,
      colnames = FALSE
    )
    # Spatial Variation.
    output$spatial_variation_panel_ui <- renderUI({
      if (is.null(r_sv_exh()) || is.null(r_sv_til())) {
        NULL
      } else {
        accordion(
          accordion_panel(
            "Spatial Variation",
            htmlOutput("sv_comparison"), br(),
            navset_card_underline(
              nav_panel("Exhaustive", DTOutput("sv_exh_table")),
              nav_panel("Tiling", DTOutput("sv_til_table"))
            )
          )
        )
      }
    })
    output$sv_comparison <- renderText({
      req(r_sv_exh(), r_sv_til())
      exh_comparison <- r_sv_exh()$res[, c("Size", "Width", "Length", "CV")]
      names(exh_comparison)[[4]] <- "CV_exh"
      til_comparison <- r_sv_til()$res[, c("Width", "Length", "CV")]
      names(til_comparison)[[3]] <- "CV_til"
      sv_comparison <- merge(exh_comparison, til_comparison, by = c("Width", "Length"))
      output_text <- paste0("<h5>CV Comparison (Exhaustive vs Tiling)</h4>")
      output_text <- paste0(
        output_text, "<strong>Common n:</strong> ", nrow(sv_comparison), "<br>"
      )
      if (nrow(sv_comparison) > 0) {
        output_text <- paste0(
          output_text, "<strong>Correlation:</strong> ",
          sprintf("%.4f", cor(sv_comparison$CV_exh, sv_comparison$CV_til, use = "complete.obs")),
          "<br>"
        )
        output_text <- paste0(
          output_text, "<strong>Mean Bias (exh - til):</strong> ",
          sprintf("%.4f", mean(sv_comparison$CV_exh - sv_comparison$CV_til, na.rm = TRUE)), "<br>"
        )
        output_text <- paste0(
          output_text, "<strong>Median Bias (exh - til):</strong> ",
          sprintf("%.4f", median(sv_comparison$CV_exh - sv_comparison$CV_til, na.rm = TRUE)), "<br>"
        )
      }
      output_text
    })
    output$sv_exh_table <- renderDT({
      req(r_sv_exh())
      formatRound(datatable(r_sv_exh()$res), "CV", 2)
    })
    output$sv_til_table <- renderDT({
      req(r_sv_til())
      formatRound(datatable(r_sv_til()$res), c("Vx", "CV"), 2)
    })
    # Optimal EU Size.
    output$optimal_eu_size_panel_ui <- renderUI({
      if (is.null(r_ops_exh_int()) || is.null(r_ops_exh_no_int())) {
        NULL
      } else {
        accordion(
          accordion_panel(
            "Optimal EU Size",
            h5("Optimization Results (Exhaustive)"),
            tableOutput("ops_exh_table"),
            hr(),
            h6("Model Comparison (ANOVA)"),
            verbatimTextOutput("ops_exh_anova"),
            h6("Model Comparison (AIC)"),
            verbatimTextOutput("ops_exh_aic"),
            fluidRow(
              column(6, plotOutput("ops_exh_int_plot")),
              column(6, plotOutput("ops_exh_no_int_plot"))
            ),
            h5("Optimization Results (Tiling)"),
            tableOutput("ops_til_table"),
            hr(),
            h6("Model Comparison (ANOVA)"),
            verbatimTextOutput("ops_til_anova"),
            h6("Model Comparison (AIC)"),
            verbatimTextOutput("ops_til_aic"),
            fluidRow(
              column(6, plotOutput("ops_til_int_plot")),
              column(6, plotOutput("ops_til_no_int_plot"))
            )
          )
        )
      }
    })
    output$ops_exh_table <- renderTable({
      req(r_ops_exh_int(), r_ops_exh_no_int())
      setNames(data.frame(
        c("With Interaction", "Without Interaction"),
        c(r_ops_exh_int()$h_star, r_ops_exh_no_int()$h_star),
        c(r_ops_exh_int()$w_star, r_ops_exh_no_int()$w_star),
        c(ceiling(r_ops_exh_int()$h_star), ceiling(r_ops_exh_no_int()$h_star)),
        c(ceiling(r_ops_exh_int()$w_star), ceiling(r_ops_exh_no_int()$w_star))
      ), c("Model", "L*", "W*", "L (rounded)", "W (rounded)"))
    })
    output$ops_exh_anova <- renderPrint({
      req(r_ops_exh_int()$fit, r_ops_exh_no_int()$fit)
      anova(r_ops_exh_no_int()$fit, r_ops_exh_int()$fit)
    })
    output$ops_exh_aic <- renderPrint({
      req(r_ops_exh_int()$fit, r_ops_exh_no_int()$fit)
      aic_res <- AIC(r_ops_exh_no_int()$fit, r_ops_exh_int()$fit)
      rownames(aic_res) <- c("Without Interaction", "With Interaction")
      aic_res
    })
    output$ops_exh_int_plot <- renderPlot({
      req(r_ops_exh_int())
      plot_cv_contour(
        r_ops_exh_int()$fit,
        nr = nrow(r_input_data()), nc = ncol(r_input_data()),
        title = "With Interaction",
        mark = data.frame(Length = r_ops_exh_int()$h_opt, Width = r_ops_exh_int()$w_opt),
        mark_col = "red",
        mark_lab = paste0("Optimal (L=", r_ops_exh_int()$h_opt, ", W=", r_ops_exh_int()$w_opt, ")")
      )
    })
    output$ops_exh_no_int_plot <- renderPlot({
      req(r_ops_exh_no_int())
      plot_cv_contour(
        r_ops_exh_no_int()$fit,
        nr = nrow(r_input_data()), nc = ncol(r_input_data()),
        title = "Without Interaction",
        mark = data.frame(Length = r_ops_exh_no_int()$h_opt, Width = r_ops_exh_no_int()$w_opt),
        mark_col = "red",
        mark_lab = paste0(
          "Optimal (L=", r_ops_exh_no_int()$h_opt, ", W=", r_ops_exh_no_int()$w_opt, ")"
        )
      )
    })
    output$ops_til_table <- renderTable({
      req(r_ops_til_int(), r_ops_til_no_int())
      setNames(data.frame(
        c("With Interaction", "Without Interaction"),
        c(r_ops_til_int()$h_star, r_ops_til_no_int()$h_star),
        c(r_ops_til_int()$w_star, r_ops_til_no_int()$w_star),
        c(ceiling(r_ops_til_int()$h_star), ceiling(r_ops_til_no_int()$h_star)),
        c(ceiling(r_ops_til_int()$w_star), ceiling(r_ops_til_no_int()$w_star))
      ), c("Model", "L*", "W*", "L (rounded)", "W (rounded)"))
    })
    output$ops_til_anova <- renderPrint({
      req(r_ops_til_int()$fit, r_ops_til_no_int()$fit)
      anova(r_ops_til_no_int()$fit, r_ops_til_int()$fit)
    })
    output$ops_til_aic <- renderPrint({
      req(r_ops_til_int()$fit, r_ops_til_no_int()$fit)
      aic_res <- AIC(r_ops_til_no_int()$fit, r_ops_til_int()$fit)
      rownames(aic_res) <- c("Without Interaction", "With Interaction")
      aic_res
    })
    output$ops_til_int_plot <- renderPlot({
      req(r_ops_til_int())
      plot_cv_contour(
        r_ops_til_int()$fit,
        nr = nrow(r_input_data()), nc = ncol(r_input_data()),
        title = "With Interaction",
        mark = data.frame(Length = r_ops_til_int()$h_opt, Width = r_ops_til_int()$w_opt),
        mark_col = "red",
        mark_lab = paste0("Optimal (L=", r_ops_til_int()$h_opt, ", W=", r_ops_til_int()$w_opt, ")")
      )
    })
    output$ops_til_no_int_plot <- renderPlot({
      req(r_ops_til_no_int())
      plot_cv_contour(
        r_ops_til_no_int()$fit,
        nr = nrow(r_input_data()), nc = ncol(r_input_data()),
        title = "Without Interaction",
        mark = data.frame(Length = r_ops_til_no_int()$h_opt, Width = r_ops_til_no_int()$w_opt),
        mark_col = "red",
        mark_lab = paste0(
          "Optimal (L=", r_ops_til_no_int()$h_opt, ", W=", r_ops_til_no_int()$w_opt, ")"
        )
      )
    })
  }
}

# nocov end
