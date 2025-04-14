#!/bin/bash

for file in ./Simulation_output/*.vcf; do
    vcftools --vcf $file --thin 10000 --maf 0.05 --hap-r2 --out $file
done
