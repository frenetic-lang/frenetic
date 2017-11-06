% matlab code invoking the C code generator "MATLAB Coder"
cfg = coder.config('exe');
codegen -c -config cfg -args [string] -d . main.m;