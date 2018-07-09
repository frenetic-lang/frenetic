1) Install Bayonet and PSI with `./install_dependencies.sh`.
2) Generate .dot and .bayonet files of various sizes using generate.py. Take a look at the bottom of the file for details.
3) Run Bayonet and PSI using `./run_bayonet.sh output/<bayonet-file>`.
4) Run ProbNetKAT by running `jbuilder exec probnetkat.bayonet examples/bayonet/output/<dot-file>` from the project's root directory.
