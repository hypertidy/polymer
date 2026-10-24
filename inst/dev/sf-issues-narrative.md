# Two sf requests as polymer workflows

Status: confirmed 2026-10-25 and written up as the vignette
`vignettes/identity-nary.Rmd`. Running `sf-issues-prep.R` as written fails its
first check: `soil` and `field` are longitude and latitude, so `st_area()`
measures geodesic edges while polymer splits edges as straight lines (feature
10 differs by about 1e-6). With both layers projected first (EPSG:3035), every
check holds to about 1e-12. The vignette does the attribute bookkeeping from
`pool$layers` rather than `sf::st_drop_geometry()`, needs no 'dplyr', and
converts to 'sf' once at the end.

## r-spatial/sf#1959: Esri Identity

**Request.** Update the polygons of layer A with the boundaries and attributes
of overlapping polygons in layer B. The reporter's workaround is
`st_intersection(a, b)` bound to `st_difference(a, st_union(b))`, and notes
that the geometry of both layers is broken up more than once.

**Formulation with polymer.** Triangulate the edges of A and B once
(`polymer(a, b)`), then dissolve with `fuse()`. Each piece carries `origins`,
the (layer, feature) pairs covering it. Identity keeps the pieces with at least
one layer-1 origin and attaches attributes from the layer-1 and layer-2
features listed there; pieces with no layer-2 origin get `NA` for B's
attributes, and pieces covered only by B are dropped.

**Points to settle.**

- A piece covered by several features of one layer (overlapping features in A
  or in B) has no single attribute row. The helper expands such a piece into
  one row per (A feature, B feature) pair, so the geometry repeats. Esri
  Identity documents no equivalent, so this behaviour needs a decision.
- Identity returns polygons only. polymer returns MULTIPOLYGON pieces, one per
  distinct set of covering features, so a feature split into disconnected parts
  by B comes back as parts of one piece only when both parts share the same
  coverage set.
- Expected checks: area by A feature is preserved, and the pieces do not
  overlap.

## r-spatial/sf#2242: n-ary intersection of one layer

**Request.** `st_intersection()` on a single polygon layer returns the
overlap partition, but the reporter finds it slow on larger inputs and posts a
loop (`st_stack()`) that accumulates pieces pairwise, which stops on duplicate
geometries.

**Formulation with polymer.** `fuse(polymer(x))` gives the same kind of
partition: one piece per distinct covering set, with `n` (overlap count) and
`origins` (input row numbers). Duplicated geometries are two features covering
the same triangles, giving `n = 2` there; no special handling should be
needed.

**Differences from sf to keep in view.**

- `st_intersection(x)` returns one row per (piece, origin) combination with
  `n.overlaps` and `origins`; polymer returns one MULTIPOLYGON per distinct
  covering set. Comparisons should be by area per overlap count, not row count.
- Crossings are computed in double precision, and vertices are shared on exact
  coordinate equality. The pieces form a partition by construction (the mesh),
  but the position of a crossing carries the usual rounding error. This is
  different from pairwise clipping, which can leave small overlaps or gaps
  between pieces; it is not exact arithmetic (see issue 7 and the exact
  triangulation work in laridae).

**Checks in the code.** Overlap-weighted piece area equals summed input area;
plain piece area equals the union area; per-count areas agree with sf; timing
at 50, 100 and 200 features (sf's call is wrapped in `try()` because the
GEOS failure in issue 7 may occur).

## Packaging

If the results hold, a single vignette could cover both: "Identity and n-ary
overlay with polymer". Both helpers are short enough to document rather than
export, unless a function such as `polymer_identity()` proves widely useful.
Vignette code would need sf in Suggests (already present) and, for the
timing comparison, should be pre-computed rather than evaluated on CRAN.
