# clash-bfcpu
Optimized &amp; pipelined Brainfuck CPU designed with CλaSH

This is an exercise project about FPGA programming. We use the functional hardware description language [CλaSH](http://www.clash-lang.org/) to design a Brainfuck CPU. Different approaches have be made before:

1. [brainfuck-cpu-fpga](https://gergo.erdi.hu/blog/2013-01-19-a_brainfuck_cpu_in_fpga/) (Kansas Lava)
1. [BF_CPU](http://nbviewer.jupyter.org/github/sandbender/BF_CPU/blob/master/BF_MYHDL_CPU_v2.ipynb) (MyHDL)
1. [bfcpu](http://www.clifford.at/bfcpu/bfcpu.html) (VHDL)
1. [brainfuckcpu](https://opencores.org/project,brainfuckcpu) (Verilog)
1. [The BrainFuck Machine](http://grapsus.net/74/) (7400 logic gates)

... and some others.

Our design has some new features:

1. The CPU includes a programming mode in which programs can be loaded directly through UART (more on this later)
1. A number of optimizations are exploited: we start with a very naïve implementation, then apply different optimizations (work in progress)
1. When running a program, the number of clock cycles is counted and displayed on the seven-segment display (we are using Nexys 4 from Digilent, but other boards with 8 seven-segment displays certainly can be used). This makes it easy to evaluate different optimizations.
