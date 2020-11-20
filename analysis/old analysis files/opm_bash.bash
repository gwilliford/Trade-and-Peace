#PBS -S /bin/bash
#PBS -q batch
#PBS -N opm
#PBS -l nodes=1:ppn=1:AMD
#PBS -l walltime=480:00:00
#PBS -l mem=10gb
#PBS -M gww17580@uga.edu 
#PBS -m abe
	
cd $PBS_O_WORKDIR

module load R

R CMD BATCH DM_opm.R