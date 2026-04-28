# OpenLand - available commands

# Show this help
help:
    @just --list --unsorted

# Run deps, doc, check, and test
all: deps doc check test

# Install package dependencies
deps:
    Rscript -e "if (!require('pak', quietly = TRUE)) install.packages('pak', repos=c('https://r-lib.github.io/p/pak/stable', 'https://cran.r-project.org/')); pak::local_install_deps(dependencies = TRUE)"

# Install development dependencies
deps-dev:
    Rscript -e "if (!require('pak', quietly = TRUE)) install.packages('pak', repos=c('https://r-lib.github.io/p/pak/stable', 'https://cran.r-project.org/')); pak::local_install_dev_deps()"

# Install optional dependencies
deps-optional:
    Rscript -e "if (!require('pak', quietly = TRUE)) install.packages('pak', repos = c('https://r-lib.github.io/p/pak/stable', 'https://cran.r-project.org'))"
    Rscript -e "pak::pkg_install(c('urlchecker','withr','checkhelper','spelling','usethis','rhub','lubridate'), ask = FALSE)"

# Run code linting
lint:
    Rscript -e "if (!require('lintr', quietly = TRUE)) install.packages('lintr', repos='https://cran.r-project.org/'); lintr::lint_package()"

# Format code with styler
format:
    Rscript -e "if (!require('styler', quietly = TRUE)) install.packages('styler', repos='https://cran.r-project.org/'); styler::style_pkg()"

# Load package for development
load:
    Rscript -e "devtools::load_all()"

# Generate documentation
doc:
    Rscript -e "if (!require('devtools', quietly = TRUE)) install.packages('devtools', repos='https://cran.r-project.org/')"
    Rscript -e "devtools::document()"

# Run tests
test:
    Rscript -e "devtools::test(reporter = 'summary')"

# Run R CMD check
check:
    Rscript -e "devtools::check(args = c('--no-manual', '--as-cran'))"

# Run R CMD check like on CRAN
check-cran:
    Rscript -e "withr::with_options(list(repos = c(CRAN = 'https://cloud.r-project.org/')), {callr::default_repos(); rcmdcheck::rcmdcheck(args = c('--no-manual', '--as-cran'))})"

# Generate test coverage report
coverage:
    Rscript -e "if (!require('covr', quietly = TRUE)) install.packages('covr', repos='https://cran.r-project.org/'); covr::package_coverage()"

# Build package
build:
    Rscript -e "devtools::build()"

# Install package
install:
    Rscript -e "devtools::install()"

# Build vignettes
vignettes:
    Rscript -e "if (!require('pkgdown', quietly = TRUE)) install.packages('pkgdown', repos='https://cran.r-project.org/')"
    Rscript -e "pkgdown::build_article(name = 'openland_vignette')"

# Clean build artifacts
clean:
    Rscript -e "devtools::clean_dll(); unlink(Sys.glob('*.tar.gz')); unlink(Sys.glob('man/*.Rd~'))"

# Run CI pipeline (deps, lint, check, test, coverage)
ci: deps lint check test coverage

# Prepare package for release/CRAN (clean, deps, doc, build, check-cran, test)
release: clean deps doc build check-cran test
    @echo "Package ready for release"

# Render README.Rmd to markdown
knit-readme:
    Rscript -e "rmarkdown::render('README.Rmd', output_format = 'github_document')"

# Build pkgdown site
pkgdown-build:
    Rscript -e "if (!require('pkgdown', quietly = TRUE)) install.packages('pkgdown', repos='https://cran.r-project.org/')"
    Rscript -e "pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)"

# Check pkgdown
pkgdown-check:
    Rscript -e "pkgdown::check_pkgdown()"
