# movecost

QGIS plugin for the calculation of slope-dependent accumulated cost surfaces,
least-cost paths, least-cost corridors and least-cost networks related to
human movement across the landscape. Wraps the
[`movecost` R package](https://cran.r-project.org/package=movecost) by
Gianmarco Alberti as a set of QGIS Processing R algorithms with a Python UI.

## Algorithms

Each algorithm is exposed both directly and as a `_by_polygon` variant that
restricts the analysis to an area of interest:

- `movecost` — accumulated cost surface, isolines, least-cost paths from one origin to one or more destinations
- `movealloc` — cost allocation polygons (Voronoi-style territories) around multiple origin points
- `movebound` — least-cost boundary at a specified cost threshold
- `movecomp` — comparison of multiple cost functions on the same origin/destination pair
- `movecorr` — least-cost corridor between two points
- `movenetw` — least-cost network across multiple origins
- `moverank` — rank multiple destinations by cost from a single origin

## 4.0.0 — movecost R package 3.0.0 (mc_* API)

Version 4.0.0 ports all 14 Processing scripts to the redesigned movecost R
package 3.0.0, which removed the legacy `movecost()` / `movebound()` /
`movecorr()` / `movealloc()` / `movecomp()` / `movenetw()` / `moverank()`
functions. Each script now builds the cost surface once with `mc_surface()`
and reuses it for every analysis (`mc_accum()`, `mc_paths()`,
`mc_corridor()`, `mc_boundary()`, `mc_alloc()`, `mc_network()`,
`mc_rank()`, `mc_comp()`), on the terra + sf + igraph stack with ggplot2
plots. The parameter interface of every algorithm is unchanged; two new
optional parameters are exposed: `Corridor_Method` (reach/through, Movecorr)
and `Penalty` (Moverank, whose LCPN is no longer capped at 6). The
`IrregularDTM` option is kept for compatibility but is handled
automatically by movecost ≥ 3.0.

## Topo_Dist parameter (since v3.1.0, requires `movecost` R package ≥ 2.2)

All algorithms now expose the `Topo_Dist` parameter (default `FALSE`).

When `TRUE`, the accumulated cost is calculated using the **topographic
(surface) distance** instead of the planar distance:

```
surface_distance = planar_distance × sqrt(1 + slope^2)
```

A 30 % slope adds ~4.4 % to surface distance; a 50 % slope adds ~11.8 %.

**When to enable it**: time-based hiking functions (Tobler, etc.) where the
extra surface distance directly affects walking time.

**When to leave it disabled**: metabolic-energy cost functions (Pandolf,
ICMP/ICFP variants, etc.) — these were typically derived from planar
measurements, so enabling `Topo_Dist` may double-count slope effects.

## Requirements

- QGIS ≥ 3.0 (Qt5 and Qt6 compatible)
- [Processing R Provider](https://north-road.github.io/qgis-processing-r/) plugin
- R installation with the `movecost` package ≥ 3.0.0 (auto-installed on
  first run by each script), plus `terra`, `sf`, `igraph`, `ggplot2`
  (`elevatr` for the "by polygon" variants)

## Author

- Enzo Cocca (plugin author and maintainer) — `enzo.ccc@gmail.com`
- Gianmarco Alberti (`movecost` R package author)

## License

See `LICENSE`.
