/* TODO: read q and n from file */
/* TODO: use batchload("file") to avoid expensive I/O */

/* set dimension of transient matrix q here */
n: 3;

/* define transient matrix q here */
q: matrix(
[1/2, 1/3, 0],
[1/4, 1/4, 0],
[0,   0,   3/4]
);

fund: invert(ident(n) - q);

stringout("output.m", fund);
