#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=02:00:00
#SBATCH --account=nstaff
#SBATCH --qos=regular
#SBATCH --constraint=cpu
#SBATCH --cpus-per-task=1

ml PrgEnv-cray
for i in {1..3}; do
    for n in {1..100}; do
        fpm run --compiler crayftn-workaround.sh --flag "-O3" --example lu_decomp --runner srun -- example/lu_decomp/${n}x${n}.dat
    done
done