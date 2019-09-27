accelerate-quicksort
====================


Quicksort is a recursive sorting algorithm. It chooses a pivot, which is one of the
elements of the array and splits the array into three segments, namely the elements
smaller than the pivot, the pivot itself and the elements greater than the pivot.
If the pivot occurs multiple times, then it may be placed in any of those segments.
It is usually included in the smaller-than or greater-than segment, such that the
segment of the pivot contains only one element. The algorithm is then recursively
invoked on the smaller-than and greater-than segments.

For a parallel implementation of quicksort, we need to make use of two forms of
parallelism. First, in the beginning of the algorithm we compute a partition of a
large array, which can be done in parallel. Second, at the end of the algorithm we
have many small arrays which need to be sorted, in parallel.

This implementation makes use of both forms of parallelism. It denotes the segments
of the array with flags, where True denotes the beginning of a segment. This array
has one element more than the number of values in the input; this prevents some edge
cases. In the beginning we only have one segment, which represent by an array where
only the first and the last elements are True.

Example
-------

Consider the following example:

```
values: 5 8 2 7 3 1 6
flags:  T F F F F F F T
```

The first value of a segment is chosen as the pivot; 5 in this case. The partitioning
step of the algorithm will then place all elements less than 5 at the front of the array:

```
values: 2 3 1 5 8 7 6
flags:  T F F T T F F T
```

Furthermore, we mark the segment start of the pivot and the segment start of the
greater-than elements, by writing True to the matching indices.

Partition
---------

To illustrate how the partitioning works, we will go through the next partitioning of
this input step by step.

```
values:      2  3  1  5  8  7  6
flags:       T  F  F  T  T  F  F  T
```

Fist we propagate the start index of the segment to all values. This is done with a scan,
in propagateSegmentHead.

```
segStart :   0  0  0  3  4  4  4
```

Similarly, we propagate the first value of the segment, which will act as the pivot for
the segment.

```
Pivots:      2  2  2  5  8  8  8
```

We compare all values with the pivot and store the results in a boolean vector.

```
isLarger:    T  T  F  T  T  F  F
```

We compute postfix sums over these booleans, to compute the offsets of the larger and the
smaller elements. For the larger-than part, we treat a True as 1 and False as 0, for the
smaller-than the other way around. We start the sum at -1.

```
idxLarger:   0  1  1  0  0  0  0
idxSmaller: -1 -1  0 -1 -1  0  1
```

We propagate the last index of the smaller-than part and add one to it. This represents
the number of elements in the smaller-than section.

```
countSmall:  1  1  1  0  2  2  2
```

Now we can compute the new index for each element. For elements smaller than the pivot,
the new index is *segStart + idxSmaller*. Elements larger than the pivot are moved
to *segStart + countSmall + idxLarger*.

```
permutation: 1  2  0  3  6  4  5
```

Finally, we perform this permutation. We mark the new segment starts at the positions
of the pivots and the elements directly after the pivots.
```
permuted:    1  2  3  5  7  6  8
new flags:   T  T  T  T  T  F  T
```

We iterate this procedure until all flags are True. Instead of recursion on two segments,
which the classic recursive implementation, we iterate on the whole array.
