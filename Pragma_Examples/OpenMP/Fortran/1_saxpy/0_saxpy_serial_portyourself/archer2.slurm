#!/bin/bash

# Slurm job options (job-name, compute nodes, job time)
#SBATCH --account=ecseak03
#SBATCH --job-name=gpu_test
#SBATCH --output=jobout.%A
#SBATCH --error=joberr.%A
#SBATCH --time=0:10:0
#SBATCH --nodes=1
#--#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=1

#SBATCH --gpus=1
#SBATCH --partition=gpu
#SBATCH --qos=gpu-shd
##SBATCH --exclusive
###SBATCH --qos=gpu-shd

module load PrgEnv-amd
module load rocm
module load craype-accel-amd-gfx90a
module load craype-x86-milan

cd $SLURM_SUBMIT_DIR
export SRUN_CPUS_PER_TASK=$SLURM_CPUS_PER_TASK
export OMP_NUM_THREADS=1
export OMP_PLACES=cores

#export LD_LIBRARY_PATH=$CRAY_LD_LIBRARY_PATH:$LD_LIBRARY_PATH

EXECUTABLE=test
PATH_TO_EXECUTABLE=$EXECUTABLE

#srun --hint=nomultithread --distribution=block:block 

# Check assigned GPU
#srun --ntasks=1 rocm-smi

srun --ntasks=1 $PATH_TO_EXECUTABLE
