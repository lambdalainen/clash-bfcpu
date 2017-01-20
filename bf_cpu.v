// Automatically generated Verilog-2001
module bf_cpu(RsRx
             ,clk
             ,btnCpuReset
             ,// clock
             system1000
             ,// asynchronous reset: active low
             system1000_rstn
             ,RsTx
             ,led_prog
             ,led_exec
             ,an
             ,seg);
  input [0:0] RsRx;
  input [0:0] clk;
  input [0:0] btnCpuReset;
  input system1000;
  input system1000_rstn;
  output [0:0] RsTx;
  output [0:0] led_prog;
  output [0:0] led_exec;
  output [7:0] an;
  output [7:0] seg;
  wire [18:0] output_0;
  wire [15:0] output_0_3;
  BF_topEntity BF_topEntity_inst
  (.rx (RsRx)
  ,.system1000 (system1000)
  ,.system1000_rstn (system1000_rstn)
  ,.result (output_0));
  
  assign RsTx = output_0[18:18];
  
  assign led_prog = output_0[17:17];
  
  assign led_exec = output_0[16:16];
  
  assign output_0_3 = output_0[15:0];
  
  assign an = output_0_3[15:8];
  
  assign seg = output_0_3[7:0];
endmodule
