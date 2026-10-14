# polymer2 Design Checkpoint

**Date**: 2025-02-08
**Status**: Prototype working on synthetic data, real-world LIST data partially tested
**Companion files**: `polymer2_prototype.R`, `polymer2_test.R`, `polymer2_list_test.R`

## What is polymer2?

A rewrite of [hypertidy/polymer](https://github.com/hypertidy/polymer) — general polygon overlay via constrained triangulation. Build one triangle mesh from N polygon layers, classify each triangle by layer/object membership, answer all overlay questions from the membership table without reconstructing geometry. Inspired by Eonfusion's data fusion engine and [anglr#16](https://github.com/hypertidy/anglr/issues/16).

## Core insight

Triangles are area buckets. Quality doesn't matter — skinny triangles are fine since we only use centroids (for classification) and areas (for queries). All overlay relationships fall out of the membership table: which triangles belong to which objects in which layers.

## Dependencies

| Package | Role | Notes |
|---------|------|-------|
| **wk** | Input marshalling | Any WKB/WKT source works (sf, gdalraster, geoarrow) |
| **wkpool** | Vertex pool + segment extraction | `establish_topology()`, hole detection |
| **geos** | Point-in-polygon classification | `geos_strtree` for spatial index, `geos_contains` for PIP |
| **RTriangle** | Constrained triangulation | CC BY-NC-SA license; `decido` viable for pure overlay |


## Architecture

### Data structures (plain data frames)

```
vertices:   vid, x, y
triangles:  tid, v1, v2, v3, area
membership: tid, layer_id, object_id
signatures: tid, signature_id, signature_key
attributes: list of data frames (one per layer)
```

Triangle areas: `0.5 * abs(cross_product)`, computed once, used as weights everywhere.

### Build pipeline

1. **Extract edges** per layer via `wkpool::establish_topology()`
2. **Combine layers**: stack vertex pools with index offsets, concatenate segments
3. **Triangulate**: `RTriangle::triangulate(pslg(...), Q = TRUE)`
4. **Classify**: centroid of each triangle → `geos_contains()` against each layer's polygons
5. **Signatures**: group triangles by identical membership sets (equivalence classes)

### Query API (no geometry reconstruction)

All queries operate on membership + triangle areas:

- `pm_crosstab(pool, rows_layer, rows_col, cols_layer, cols_col)` — area-weighted cross-tabulation
- `pm_coverage(pool, of_layer, by_layer)` — fraction of each object covered by another layer
- `pm_overlap_area(pool, layer_a, layer_b)` — intersection areas
- `pm_predicates(pool, layer_a, layer_b)` — spatial predicates from triangle counts
- `pm_weighted_summary(pool, values_layer, value_col, by_layer, fun)` — area-weighted attribute transfer
- `pm_where(pool, \(L) L(1) & L(2) & !L(3))` — set algebra → triangle IDs
- `pm_where_area(pool, \(L) L(1) & L(2))` — set algebra → total area

The `\(L)` function interface avoids NSE. `L(i)` returns a logical vector built via `%in%` on the membership table.

## What works

### Synthetic test (A/B/C datasets from original polymer)

```
polymer mesh:
  Layers:    3
  Triangles: 37
  Signatures: 9

pm_where_area(pool, \(L) L(1) & L(2))           # 0.035 sq units
pm_where_area(pool, \(L) L(1) & L(2) & L(3))    # 0.008 sq units
pm_where_area(pool, \(L) L(1) & !L(2) & !L(3))  # 0.816 sq units
```

Set algebra is additive and complete — all answers from the same triangle areas.

### RTriangle handles crossing segments

Confirmed: RTriangle **does** insert vertices at segment-segment crossings (noding). Two overlapping squares with 4 crossing points → Triangle split segments and triangulated correctly.

## Real-world testing: LIST Tasmania WFS

### What works

**gdalraster WFS access** with `GDALVector`:

```r
wfs_base <- paste0(
  "WFS:https://services.thelist.tas.gov.au/arcgis/services/Public/OpenDataWFS/MapServer/WFSServer",
  "?service=WFS&version=2.0.0&request=GetFeature&srsName=EPSG:28355"
)
v <- new(GDALVector, url)
v$convertToLinear <- TRUE   # fixes MULTISURFACE/CURVEPOLYGON pretence
v$fetch(-1)
```

Key discoveries:
- Layer names need `Public_OpenDataWFS:` prefix (e.g. `Public_OpenDataWFS:LIST_CADASTRAL_PARCELS`)
- TASVEG layer is `Public_OpenDataWFS:TASVEG_4.0` (not `TASVEG_40`)
- `ogr_ds_layer_names()` on GetCapabilities URL lists available layers
- `convertToLinear = TRUE` on `GDALVector` handles ESRI's MULTISURFACE geometry
- Geometry arrives as WKB in `SHAPE` column — directly `wk::wkb()`-ready
- Rich attribute data comes through (464 parcels for a 600m×600m Sandy Bay bbox)

Sandy Bay TASVEG communities:
- (FUR) Urban areas
- (FAG) Agricultural land
- (DPU) *Eucalyptus pulchella* forest and woodland
- (DAS) *Eucalyptus amygdalina* forest and woodland on sandstone
- (DTO) *Eucalyptus tenuiramis* forest and woodland on sediments

### What crashed

**`polymer(lga_geom)`** — single LGA polygon:
```
Error in array(x, c(length(x), 1L), ...) : 'data' must be of a vector type, was 'NULL'
```

This is a polymer bug, not an RTriangle bug. The issue is in extracting data from the `wkpool` object.

**`polymer(parcels_geom, tasveg_geom, lga_geom)`** — three layers combined:
```
Internal error in segmentintersection(): Topological inconsistency after splitting a segment.
```

RTriangle crash. Parcels-only and TASVEG-only work fine individually. The cross-layer combination has problematic segments — likely **collinear overlapping segments** from shared boundaries between layers, not simple crossings (which RTriangle handles).

## Open issues

### 1. wkpool accessor API

`wkpool` is a `vctrs_rcrd` object. Polymer prototype assumed `$segments` / `$pool` accessors which don't exist.

**Correct access pattern**:
```r
topo <- wkpool::establish_topology(geom)

# Segments
vctrs::field(topo, ".vx0")      # start vertex indices
vctrs::field(topo, ".vx1")      # end vertex indices
vctrs::field(topo, ".feature")  # source feature ID

# Vertex pool
attr(topo, "pool")              # data.frame: .vx, x, y
```

The `array(NULL)` crash in `polymer(lga_geom)` is because the prototype doesn't use these accessors.

### 2. Collinear/overlapping segments

RTriangle handles crossing segments (confirmed with test) but cannot handle **collinear overlapping segments** — two segments from different layers sharing the same line. This is common in real data where layer boundaries coincide (e.g. LGA boundary running along a cadastral parcel edge).

**Potential solutions**:
- `geos_node()` on the combined edge set (loses provenance, needs re-association)
- Segment deduplication before PSLG construction
- Tolerance-based vertex snapping in wkpool's `merge_coincident()`

### 3. Vertex deduplication

Cross-layer shared boundaries will have near-coincident but not identical coordinates. RTriangle requires unique vertices. Need tolerance-based snapping — a `wkpool` responsibility (`merge_coincident()` or similar).

Different from noding: dedup handles same-location different-coordinate vertices; noding handles crossing edges.

### 4. Hole handling

`wkpool::hole_points()` provides one point per interior ring for RTriangle's H parameter. geos containment test correctly excludes centroids in holes. Not yet tested on real data with holes.

## Related work

### TU Delft (Ledoux, Arroyo Ohori)

Uses constrained triangulation for:
- **prepair**: single polygon repair (now in CGAL 6.0)
- **pprepair**: planar partition repair (gaps/overlaps)
- **Horizontal conflation**: cross-border dataset alignment without snapping

Key difference from polymer2: TU Delft targets repair problems where each triangle gets **one** label. polymer2 embraces **multi-label** triangles as the output — preserving full provenance is the point.

Their work uses **CGAL** constrained triangulation which handles intersecting constraints internally (splits them). This is why they don't hit the segment-intersection issues that RTriangle has.

### Eonfusion (Myriax, defunct)

Commercial 4D data fusion software from Hobart. polymer's original inspiration.
"Fusion operator" reconciled data at different spatial/temporal scales, merged
attributes, identified topological relationships. 

## Design principles

1. **Observe don't correct** — wkpool represents input exactly; snapping is a separate explicit step
2. **Triangles are buckets** — quality doesn't matter for area accounting
3. **Provenance preserved** — membership table maintains full layer/object lineage
4. **Geometry-free queries** — 90% of overlay questions answered without polygon reconstruction
5. **wk as universal input** — no sf dependency, works with any geometry source
6. **Tabular operations** — all queries are merge/aggregate on data frames with triangle areas as weights

## Next steps

1. **Fix wkpool accessor** in polymer prototype (`vctrs::field()` + `attr()`)
2. **Segment dedup/noding** for cross-layer combinations
3. **Test on LIST data** — parcels × TASVEG is the real target
4. **Benchmark** geos classification at scale (464 parcels × thousands of triangles)
5. **Package structure** built on wkpool foundation
6. **Consider CGAL alternative** — avoids segment intersection issues entirely, but heavier dependency
