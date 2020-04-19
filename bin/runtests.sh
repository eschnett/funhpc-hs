#!/bin/sh

set -euxo pipefail

stack build --flag mpi-hs:mpich-macports --test --no-run-tests --bench --no-run-benchmarks --haddock --no-haddock-deps
mpiexec -n 3 stack exec funhpc

# stack build --test --no-run-tests
# mpiexec -n 3 stack exec -- $(stack path --dist-dir)/build/mpi-test/mpi-test
# mpiexec -n 3 stack exec -- $(stack path --dist-dir)/build/mpi-test-binary/mpi-test-binary
# mpiexec -n 3 stack exec -- $(stack path --dist-dir)/build/mpi-test-serialize/mpi-test-serialize
# mpiexec -n 3 stack exec -- $(stack path --dist-dir)/build/mpi-test-storable/mpi-test-storable
# mpiexec -n 3 stack exec -- $(stack path --dist-dir)/build/mpi-test-store/mpi-test-store

echo 'Done.'
