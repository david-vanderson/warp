#!/bin/sh
#racket -W 'error debug@GC' -l errortrace -t $1
racket -l errortrace -t $1
