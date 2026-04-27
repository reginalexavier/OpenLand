# Download and load a remote .rda dataset (graceful)

Internal helper to download a remote \`.rda\` file (e.g., from Zenodo),
load it into an environment, and return a single \`TRUE/FALSE\`
indicating whether the dataset is available.

## Usage

``` r
.openland_try_download_and_load_rda(
  url,
  object = NULL,
  envir = parent.frame(),
  timeout = 10,
  cache = TRUE,
  cache_dir = NULL,
  quiet = TRUE
)
```

## Arguments

- url:

  character. Remote \`.rda\` URL.

- object:

  character or NULL. If provided, requires that this object name exists
  inside the \`.rda\` file; otherwise returns \`FALSE\`.

- envir:

  environment. Environment where the objects from the \`.rda\` file will
  be loaded.

- timeout:

  integer. Download timeout (seconds) set via \`options(timeout)\`.

- cache:

  logical. Whether to cache the downloaded file.

- cache_dir:

  character or NULL. Cache directory. When \`NULL\`, uses
  \`tools::R_user_dir("OpenLand", which = "cache")\` when available.

- quiet:

  logical. Passed to \`utils::download.file()\`.

## Value

logical. \`TRUE\` if the \`.rda\` was successfully downloaded (or found
in cache) and loaded into \`envir\`; \`FALSE\` otherwise.

## Details

The function is defensive by design: it should not error if the remote
host is unavailable, if the download fails, or if the \`.rda\` cannot be
loaded. It can optionally cache the downloaded file in a persistent user
cache directory.
