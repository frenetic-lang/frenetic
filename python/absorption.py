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

    # # load matrices
    (Q, (nt,nt_)) = read_matrix()
    (R, (nt__,na)) = read_matrix()
    assert(nt == nt_ == nt__ == na)
    eprint("[python] %dx%d matrices received!" % (nt, nt))


    # compute absorption probabilities: n_t x n_a matrix
    A = sparse.eye(nt) - Q
    A = A.tocsc();
    R = R.tocsc();
    X = linalg.spsolve(A, R)
    eprint("[python] X = " + str(X))

    # write matrix back
    write_matrix(X)


def read_matrix():
    (M, N) = sys.stdin.readline().split()
    shape = (int(M), int(N))
    A = sparse.dok_matrix(shape, dtype='d')
    for line in sys.stdin:
        parts = line.split()
        if len(parts) == 0:
            # end of matrix
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
