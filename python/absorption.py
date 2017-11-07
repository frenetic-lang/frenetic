import sys
import numpy as np
from scipy import sparse
import itertools

def main(q,r,x):
    print("called with q = \'%s\', r = \'%s\'" % (q, r))

    # load matrices
    (Q, (nt,nt_)) = load(q)
    (R, (nt__,na)) = load(r)
    assert(nt == nt_ == nt__)
    print("transiant = %d\nabsorbing = %d" % (nt, na))

    # compute absorption probabilities: n_t x n_a matrix
    A = sparse.eye(nt) - Q
    X = sparse.linalg.spsolve(A, R)

    # write matrix back
    store(x, X)


def load(file):
    with open(file, 'r') as f:
        (M, N) = f.readline().split()
        shape = (int(M), int(N))
        A = dok_matrix(shape, dtype='d')
        for line in f:
            (i, j, a) = line.split()
            A[int(i), int(j)] = np.float64(a)
        return (A, shape)

def store(file, A):
    with open(file, 'w') as f:
        (M, N) = A.shape()
        f.write('%d %d\n' % (M, N))
        A = A.tocoo()
        for i,j,v in itertools.izip(cx.row, cx.col, cx.data):
            f.write('%d %d %f\n' % (i,j,v))


if __name__ == '__main__':
    if len(sys.argv) != 4:
        print("usage: %s [file]" % sys.argv[0])
        exit(1)
    main(sys.argv[1], sys.argv[2], sys.argv[3])
