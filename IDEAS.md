## IDEAS
* sparse matrix inversion:
    - https://stackoverflow.com/questions/25929311/how-can-i-calculate-inverse-of-sparse-matrix-in-eigen-library
* Instead of using one global domain, refine domain during matrix compilation?
* local fields can help with state space explosion!
    - erased once they go out of scope
* exploit independence for better performance?
    - analyze blowup example
* sometimes all rows are the same
* solve kleene star one connected component at a time



# TODO:
* carefully examine allocation!
* pass all arguments explicit (-> less computation)
* parser

# OLD
* Use sub-stochastic matrices instead of stochastic ones?
    - this should work!
    - -> done!

BLAS (lacaml):
* predicates = vectors
* not a = 1 - a
