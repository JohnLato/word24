#!/bin/bash

ghc --make -i -i../src -hide-package word24 -odir . -hidir . testword24.hs QCUtils.hs -fhpc
