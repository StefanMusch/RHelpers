
# introduction ------------------------------------------------------------
# the script describes basic usages of drake
create_plot <- function(data) {
  ggplot(data, aes(x = Petal.Width, fill = Species)) +
    geom_histogram()
}

# example_plan || that's how one can prepare the example plan
example_plan <- drake_plan(
  raw_data = iris,
  data = raw_data %>%
    mutate(Species = forcats::fct_inorder(Species)) %>%
    select(-X__1),
  hist = create_plot(data),
  fit = lm(Sepal.Width ~ Petal.Width + Species, data),
  report = rmarkdown::render(
    knitr_in(system.file('markdown_templates/PDF/pdf_template.rmd', package = 'g6tr')),
    output_file = file_out("report.pdf"),
    quiet = TRUE
  )
)


# or divide into chunks ---------------------------------------------------
# the plan can be also divided into smaller chunks to make it more visible what's going on
simulate <- function(n){
  # Pick a random set of cars to bootstrap from the mtcars data.
  data <- random_rows(data = mtcars, n = n)
  
  # x is the car's weight, and y is the fuel efficiency.
  data.frame(
    x = data$wt,
    y = data$mpg
  )
}

# function's doesn't need to be commented - they need to be documented but that's other story
regression <- function(d){
  lm(y ~ + x, data = d)
}

regression2 <- function(d){
  lm(y ~ + I(x^2), data = d)
}

# start planning in chunks ------------------------------------------------
# in this way you can follow from bottom to top the full plan specification

# my_datasets || which datasets to use
my_datasets <- drake_plan(
  small = simulate(48),
  large = simulate(64)
)
# methods || which methods to be used in the analysis
methods <- drake_plan(
  regression1 = regression(dataset__),
  regression2 = regression2(dataset__)
)
# my_analyses || which methods are applied to which datasets
my_analyses <- plan_analyses(methods, datasets = my_datasets)

# summary_types || what kind of summaries are of interest
summary_types <- drake_plan(
  summ = suppressWarnings(summary(analysis__$residuals)), # Summarize the RESIDUALS of the model fit. # nolint
  coef = suppressWarnings(summary(analysis__))$coefficients # Coefficinents with p-values # nolint
)

# results || what kind of results are of the interest
results <- plan_summaries(
  summary_types,
  my_analyses,
  my_datasets,
  gather = NULL
)

# finalization ------------------------------------------------------------
# create the final report and bind it

# report || final drake plan
report <- drake_plan(
  report = knit(system.file('markdown_templates/PDF/pdf_template.rmd', package = 'g6tr'), file_out("report.pdf"), quiet = TRUE)
)
# gradient_plan || bind plans
gradient_plan <- rbind(report, my_datasets, my_analyses, results)