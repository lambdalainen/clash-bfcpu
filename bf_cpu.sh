#!/bin/bash

if ! diff v/bf_cpu.v verilog/BF/bf_cpu.v
then
  echo "bf_cpu.v has changed"
else
  echo "bf_cpu.v hasn't changed, overwriting..."
  cp v/bf_cpu.w verilog/BF/bf_cpu.v
  rm verilog/BF/BF_testbench.v
fi
