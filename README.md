# clash-bfcpu

This is an exercise project about FPGA programming. We use the functional hardware description language [CλaSH](http://www.clash-lang.org/) to design a Brainfuck CPU. This has been done before with different approaches:

1. [brainfuck-cpu-fpga](https://gergo.erdi.hu/blog/2013-01-19-a_brainfuck_cpu_in_fpga/) (Kansas Lava)
1. [BF_CPU](http://nbviewer.jupyter.org/github/sandbender/BF_CPU/blob/master/BF_MYHDL_CPU_v2.ipynb) (MyHDL)
1. [bfcpu](http://www.clifford.at/bfcpu/bfcpu.html) (VHDL)
1. [brainfuckcpu](https://opencores.org/project,brainfuckcpu) (Verilog)
1. [The BrainFuck Machine](http://grapsus.net/74/) (7400 logic gates)

... etc.

Our design has some new features:

1. The CPU includes a programming mode in which programs can be loaded directly through UART (more on this later)
1. A number of optimizations are exploited: we start with a very naïve implementation, then apply different optimizations (work in progress)
1. When running a program, the number of clock cycles is counted and displayed on the seven-segment display (we are using Nexys 4 from Digilent, but other boards with 8 seven-segment displays certainly can be used). This makes it easy to evaluate different optimizations.

![Brainfuck CPU on FPGA](https://github.com/aufheben/clash-bfcpu/raw/master/doc/video.gif "Brainfuck CPU on FPGA")

## Setup & Workflow

1. Clone the repository
2. Install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
3. Install clash

        stack setup --resolver=lts-6.24
        stack install clash-ghc
Note that lts-6.x (ghc-7.10.3) is required.
4. Generate Verilog

        ./runclash.sh
        CLaSH.Prelude> :l BF/CPU.hs
        CLaSH.Prelude> :verilog
5. Prepare top module for synthesis

        ./bf_cpu.sh
