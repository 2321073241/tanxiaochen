`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2022/10/30 15:33:52
// Design Name: 
// Module Name: xxx
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module xxx();
 
reg rstn;
reg clk;
reg en;
wire [7:0] cnt;
wire cout;
 
m60 u5(.clk(clk),.rstn(rstn), .en(en), .cnt(cnt), .cout(cout));
 
initial 
begin
  en <= 1;
  rstn <= 0;
  clk <= 0;
  #2 rstn <= 1;
  #2000 rstn <= 0;
end
always
  #1 clk = ~clk;
endmodule
