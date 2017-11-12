from __future__ import print_function
import sys
import numpy as np
from scipy import sparse
from scipy.sparse import linalg
import fileinput

debug = False

def eprint(*args, **kwargs):
    if debug:
        print(*args, file=sys.stderr, **kwargs)

def main():
    eprint("[python] waiting for input matrices ...")

    # load matrices
    (AP, (nq,nq_)) = read_matrix()
    eprint("[python] AP received (%dx%d)!" % (nq, nq_), flush=True)
    (X, (nx,nx_)) = read_matrix()
    eprint("[python] X received (%dx%d)!" % (nx, nx_), flush=True)
    (not_a, nr) = read_vector()
    eprint("[python] not_a received (%dx1)!" % nr, flush=True)
    assert(nq == nq_ == nx == nx_ == nr)
    n = nq
    eprint("[python] %dx%d matrices received!" % (n, n))

    # print received matrices
    eprint("[python] AP =\n", AP.toarray())
    eprint("[python] X =\n", X.toarray())
    eprint("[python] not_a =\n", not_a)

    # need to handle 1-dimensionoal case seperately
    if n == 1:
        if not_a[0] == 1:
            # return all-ones matrix
            write_matrix(sparse.eye(1, 1))
        else:
            # return all-zeros matrix
            write_matrix(sparse.dok_matrix((1,1), dtype='d'))
        exit(0)

    # first, check wich states can even reach an absorbing state ever
    (non_sing,) = np.nonzero(X.dot(not_a))
    eprint("[python] non-singular = ", non_sing)
    slice = np.ix_(non_sing, non_sing)

    # set up system just for the non singular states
    A = sparse.eye(non_sing.size) - AP[slice]
    A = A.tocsc()
    NA = sparse.diags(not_a[non_sing]).tocsc()
    X = linalg.spsolve(A, NA)
    XX = sparse.dok_matrix((n, n), dtype='d')
    XX[slice] = X

    # write matrix back
    write_matrix(XX)


def read_matrix():
    (M, N) = sys.stdin.readline().split()
    shape = (int(M), int(N))
    A = sparse.dok_matrix(shape, dtype='d')
    for line in sys.stdin:
        parts = line.split()
        if len(parts) == 0:
            # end of input
            return (A, shape)
        elif len(parts) == 3:
            (i, j, a) = parts
            A[int(i), int(j)] = np.float64(a)
        else:
            raise NameError("unepexted input line: %s" % line)
    raise NameError("reachead end of input stream before end of matrix!")

def read_vector():
    (M, N) = sys.stdin.readline().split()
    assert(M == N)
    shape = int(M)
    v = np.zeros(shape, dtype='d')
    for line in sys.stdin:
        # eprint("[python] received: \"%s\"" % line.strip())
        parts = line.split()
        if len(parts) == 0:
            # end of input
            return (v, shape)
        elif len(parts) == 3:
            (i, j, a) = parts
            i,j = int(i), int(j)
            assert(i == j)
            v[i] = np.float64(a)
        else:
            raise NameError("unepexted input line: %s" % line)
    raise NameError("reachead end of input stream before end of vector!")


def write_matrix(A):
    (M, N) = A.shape
    print('%d %d' % (M, N))
    A = A.tocoo()
    for i,j,v in zip(A.row, A.col, A.data):
        print('%d %d %f' % (i,j,v))


if __name__ == '__main__':
    if len(sys.argv) != 1:
        print("usage: %s" % sys.argv[0])
        exit(1)
    main()
