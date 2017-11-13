from __future__ import print_function
import sys
import numpy as np
from scipy import sparse
from scipy.sparse import linalg
import fileinput
import time

verbosity = 0

def eprint(*args, verb=0, **kwargs):
    if verb <= verbosity:
        print(*args, file=sys.stderr, **kwargs)

def main():
    eprint("[python] waiting for input matrices ...", verb=1)

    # load matrices
    (AP, (nq,nq_)) = read_matrix()
    eprint("[python] AP received (%dx%d)!" % (nq, nq_), flush=True, verb=2)
    (not_a, nr) = read_vector()
    eprint("[python] not_a received (%dx1)!" % nr, flush=True, verb=2)
    assert(nq == nq_ == nr)
    n = nq
    eprint("[python] %dx%d matrices received!" % (n, n), verb=1)

    # print received matrices
    eprint("[python] AP =\n", AP.toarray(), verb=3)
    eprint("[python] not_a =\n", not_a, verb=3)

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
    start = time.process_time()
    X = (AP + sparse.diags(not_a)).tocsc()
    n_abs = sum(not_a)
    n_trans_upper_bound = n - n_abs
    i = 1
    while i < n_trans_upper_bound:
        eprint("[python] i = ", i, verb=3)
        X = X.dot(X)
        i *= 2
    (absorbing,) = np.nonzero(not_a)
    (transient,) = np.nonzero((1 - not_a) * X.dot(not_a)) # reachabilty from a using X to not-a
    n_trans = transient.size
    tt_slice = np.ix_(transient, transient)
    ta_slice = np.ix_(transient, absorbing)
    eprint("--> python reachabilty & slice computation: %f seconds" % (time.process_time() - start), verb=0)
    eprint("[python] non-singular transient = ", transient, verb=2)
    eprint("[python] n = %d, n_abs = %d, n_trans = %d, n_singular = %d" 
           % (n, n_abs, n_trans, n - n_abs - n_trans), verb=1)

    # solve sparse linear system to compute absorption probabilities
    start = time.process_time()
    A = sparse.eye(transient.size) - AP[tt_slice]
    R = AP[ta_slice]
    eprint("--> python slicing time: %f seconds" % (time.process_time() - start), verb=0)

    start = time.process_time()
    X = linalg.spsolve(A, R)
    eprint("--> python solver time: %f seconds" % (time.process_time() - start), verb=0)
    XX = sparse.lil_matrix((n, n), dtype='d')
    XX[ta_slice] = X
    for i in absorbing:
        XX[i,i] = 1


    # write matrix back
    write_matrix(XX)


def read_matrix():
    (M, N) = sys.stdin.readline().split()
    shape = (int(M), int(N))
    A = sparse.lil_matrix(shape, dtype='d')
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
