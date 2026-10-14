## Submission

This is a first submission of polymer.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Dependencies

polymer imports 'cdtr', which is itself a new submission to CRAN. polymer
will only be submitted once 'cdtr' is on CRAN, and the `Remotes` field in
DESCRIPTION is removed before submission.

## Method references

There is no published reference for the method as a whole. It combines a
constrained Delaunay triangulation of all input edges (provided by 'cdtr')
with point-in-polygon labelling of the triangles; both are standard
computational geometry.
