# polymer: Overlay Any Number of Polygon Layers with a Triangle Mesh

Overlay polygon layers by building one constrained Delaunay
triangulation of every input edge from every layer, with crossings
inserted as new vertices, and recording which input features cover each
triangle. The mesh and its index answer overlay questions for any number
of layers at once: where features overlap, how many times, and which
features are involved. Overlay pieces are dissolved from the mesh, so
they form an exact partition with shared vertices, unlike pairwise
polygon clipping. Input is anything the 'wk' package reads, the vertex
pool comes from 'wkpool' and the triangulation from 'cdtr'.

## See also

Useful links:

- <https://github.com/hypertidy/polymer>

- <https://hypertidy.github.io/polymer/>

- Report bugs at <https://github.com/hypertidy/polymer/issues>

## Author

**Maintainer**: Michael Sumner <mdsumner@gmail.com>
([ORCID](https://orcid.org/0000-0002-2471-7511)) \[copyright holder\]

Authors:

- Michael Sumner <mdsumner@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-2471-7511)) \[copyright holder\]

Other contributors:

- Gabriel Becker (Provided the package name.) \[contributor\]
