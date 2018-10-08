#!/bin/bash

(cd /opt/data/frim1; /usr/bin/q -H -O -d '	' "select f2.SampleID from plants.txt f1 JOIN samples.txt f2 ON (f1.PlantID=f2.PlantID) JOIN aliquots.txt f3 ON (f2.SampleID=f3.SampleID) JOIN enzymes.txt f5 ON (f3.AliquotID=f5.AliquotID) JOIN pools.txt f6 ON (f2.SampleID=f6.SampleID) JOIN qnmr_metabo.txt f8 ON (f6.PoolID=f8.PoolID)" | uniq)
