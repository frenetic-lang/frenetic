from __future__ import print_function
import sys
import numpy as np
from scipy import sparse
from scipy.sparse import linalg
import fileinput

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def main():
    eprint("[python] waiting for input matrices ...")

    # load matrices
    (Q, (nq,nq_)) = read_matrix()
    eprint("[python] Q received (%dx%d)!" % (nq, nq_), flush=True)
    (X, (nx,nx_)) = read_matrix()
    eprint("[python] X received (%dx%d)!" % (nx, nx_), flush=True)
    (R, (nr,nr_)) = read_matrix()
    eprint("[python] R received (%dx%d)!" % (nr, nr_), flush=True)
    assert(nq == nq_ == nx == nx_ == nr == nr_)
    n = nq
    eprint("[python] %dx%d matrices received!" % (n, n))

    # print received matrices
    eprint("[python] Q =\n", Q.toarray())
    eprint("[python] X =\n", X.toarray())
    eprint("[python] R =\n", R.toarray())

    # need to handle 1-dimensional case seperately
    if n == 1:
        if R[0] == 1:
            # return all-ones matrix
            write_matrix(R)
        else:
            # return all-zeros matrix
            write_matrix(sparse.dok_matrix((1,1), dtype='d'))
        exit(0)

    # first, check wich states can even reach an absorbing state ever
    non_sing = np.any(X, axis=1)
    eprint("[python] non-singular = non_sing", non_sing)

    # set up system just for the non singular states
    Q = Q[non_sing, non_sing]
    R = R[non_sing, non_sing]
    A = eqe(np.sum(non_sing)) - Q
    A.tocsc()
    R.tocsc()
    X = linalg.spsolve(A, R)
    XX = sparse.dok_matrix((n, n), dtype='d')
    XX[non_sing, non_sing] = X

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
