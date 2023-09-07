#!/bin/bash
#SBATCH --nodes=16
#SBATCH --time=12:00:00
#SBATCH --account=nstaff
#SBATCH --qos=regular
#SBATCH --constraint=cpu
#SBATCH --cpus-per-task=1

ml OpenCoarrays
for i in {1..3}; do
    for n in {1..16}; do
        fpm run --compiler gfortran-workaround.sh --flag "-O3 -fcoarray=lib" --example lu_decomp --runner "srun --nodes=${n} --ntasks=${n} --cpu-bind=cores" -- example/lu_decomp/100x100.dat
    done
done