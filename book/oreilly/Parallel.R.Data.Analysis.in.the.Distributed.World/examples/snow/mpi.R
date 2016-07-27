#!/usr/bin/Rscript
library(snow)
library(Rmpi)

# This script spawns workers when creating the cluster, so make sure
# that mpirun didn't start multiple copies of this script and that
# there are some process slots available for spawned workers.
if (mpi.comm.size(0) > 1 || mpi.universe.size() < 2) {
  if (mpi.comm.rank(0) == 0)
    cat('must use -n 1 and specify at least two slots\n', file=stderr())
  mpi.quit()
}

# Create a cluster and make sure it works
cl <- makeMPIcluster(mpi.universe.size() - 1)
r <- clusterEvalQ(cl, R.version.string)
print(unlist(r))

# Stop the cluster, finalize MPI and quit
stopCluster(cl)
mpi.quit()
