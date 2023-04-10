{\rtf1\ansi\ansicpg1252\cocoartf2708
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 HelveticaNeue;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red0\green0\blue0;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0\c84706;\cssrgb\c0\c0\c0;}
\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\sa240\partightenfactor0

\f0\fs24 \cf2 \expnd0\expndtw0\kerning0
#!/bin/bash -l        \
#SBATCH \'97ncodes=7\
#SBATCH \'97ntasks=7\
#SBATCH --mem=2gb\
#SBATCH \'97t 00:30:00\cf0 \
\cf2 #SBATCH --mail-type=ALL  \
#SBATCH --mail-user=chen6496@umn.edu \
#SBATCH \'97p amdsmall\cf0 \
\cf2 cd ~/psy8960-week11\cf0 \
\cf2 module load R/4.2.2-openblas\cf0 \
\cf2 Rscript script.R}