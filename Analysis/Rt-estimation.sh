#!/bin/bash
#
#$ -cwd
#$ -V
#$ -j y
#$ -S /bin/bash
#$ -M choover@berkeley.edu
#$ -m beas
#

mpirun -n 1 R --vanilla < 2-Rt-estimation.R > Rt_estimation.Rout