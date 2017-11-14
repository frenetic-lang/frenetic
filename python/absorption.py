from __future__ import print_function
import sys
import numpy as np
from scipy import sparse
from scipy.sparse import linalg
import fileinput
import time

verbosity = 1

def eprint(*args, verb=0, **kwargs):
    if verb <= verbosity:
        print(*args, file=sys.stderr, **kwargs)

def get_transient(X, absorbing):
    transient = [False for _ in range(len(absorbing))]
    (worklist,) = np.nonzero(absorbing)
    worklist = worklist.tolist()
    
    preds = [[] for _ in range(len(absorbing))]
    ii,jj = X.nonzero()
    for i,j in zip(ii,jj):
        preds[j].append(i)

    while worklist:
        s = worklist.pop()
        for pred in preds[s]:
            if transient[pred]: continue
            transient[pred] = True
            worklist.append(pred)
    return np.nonzero(transient)



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
    # eprint("[python] AP =\n", AP.toarray(), verb=3)
    # eprint("[python] not_a =\n", not_a, verb=3)

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
    (transient,) = get_transient(AP, not_a)
    # X = (AP + sparse.diags(not_a)).tocsc()
    n_abs = sum(not_a)
    n_trans_upper_bound = n - n_abs
    # i = 1
    # while i < n_trans_upper_bound:
    #     eprint("[python] i = ", i, verb=3)
    #     X = X.dot(X)
    #     i *= 2
    eprint("--> python reachabilty computation: %f seconds" % (time.process_time() - start), verb=0)

    start = time.process_time()
    (absorbing,) = np.nonzero(not_a)
    # (transient,) = np.nonzero((1 - not_a) * X.dot(not_a)) # reachabilty from a using X to not-a
    n_trans = transient.size
    eprint("--> python index computation: %f seconds" % (time.process_time() - start), verb=0)
    eprint("[python] non-singular transient = ", transient, verb=2)
    eprint("[python] n = %d, n_abs = %d, n_trans = %d, n_singular = %d" 
           % (n, n_abs, n_trans, n - n_abs - n_trans), verb=1)

    # solve sparse linear system to compute absorption probabilities
    start = time.process_time()
    AP = AP.tocsr()[transient,:].tocsc()
    A = sparse.eye(transient.size).tocsc() - AP[:,transient]
    R = AP[:, absorbing]
    eprint("--> python slicing time: %f seconds" % (time.process_time() - start), verb=0)

    start = time.process_time()
    X = linalg.spsolve(A, R)
    eprint("--> python solver time: %f seconds" % (time.process_time() - start), verb=0)
    XX = sparse.lil_matrix((n, n), dtype='d')
    XX[np.ix_(transient, absorbing)] = X
    for i in absorbing:
        XX[i,i] = 1


    # write matrix back
    notify_ocaml()
    write_matrix(XX)

# send single byte. OCaml will discard it.
def notify_ocaml():
    sys.stdout.buffer.write(b"0")

def read_matrix():
    (M, N) = sys.stdin.readline().split()
    shape = (int(M), int(N))
    eprint("[python] receiving matrix of size %sx%s" % (M,N), verb=1)
    I = []; J = []; V = [];
    # A = sparse.lil_matrix(shape, dtype='d')
    for line in sys.stdin:
        parts = line.split()
        if len(parts) == 0:
            # end of input
            A = sparse.csr_matrix((V,(I,J)), shape=shape)
            return (A, shape)
        elif len(parts) == 3:
            (i, j, a) = parts
            I.append(int(i))
            J.append(int(j))
            V.append(np.float64(a))
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
