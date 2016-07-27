#!/bin/sh
#PBS -N SNOWMPI
#PBS -j oe
cd $PBS_O_WORKDIR
orterun -n 1 /usr/bin/R --slave -f mpi.R > mpi-$PBS_JOBID.out 2>&1
