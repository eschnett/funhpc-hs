#!/bin/bash
exec "$@" >funhpc.$OMPI_COMM_WORLD_RANK.out 2>&1
