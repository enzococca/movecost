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
- R installation with the `movecost` package ≥ 2.2 (auto-installed on first
  run by each script)

## Author

- Enzo Cocca (plugin author and maintainer) — `enzo.ccc@gmail.com`
- Gianmarco Alberti (`movecost` R package author)

## License

See `LICENSE`.
