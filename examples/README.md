
To install dependencies:
```
./install-dependencies.ubuntu.sh
```

To run:
    ```
    sage -python network.py --topo fattree,4 -r spf,allsp,disjointtrees -o output/ex1
    ```
Four files -- ex1.dot (topology),  ex1-spf.trees, ex1-allsp.nexthops and ex1-disjointtrees.trees
(routing trees) -- will be generated in output directory
