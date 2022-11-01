`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 10/30/2022 12:11:15 PM
// Design Name: 
// Module Name: m10
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


module m60(clk, rstn, cnt, en, cout);
 
input clk, rstn, en;
output[7:0] cnt;
output cout;
wire cout10, cout10_, cout6;
wire[3:0] cnt10, cnt6;
 
m10 u1(.clk(clk), .rstn(rstn), .en(en), .cnt(cnt10), .cout(cout10_)); //!10¡phÛM:co10_1
and u3(cout10, cout10_, en);
m6 u2(.clk(clk), .rstn(rstn), .en(cout10), .cnt(cnt6), .cout(cout6)); //co10_1en:co10,\:!6¡phýá÷
and u4(cout, cout10, cout6); //!6¡phÛM!6ýá÷co10\:!60¡phÛM
 
assign cnt = {cnt6, cnt10}; //!60¡phúØM:!6¡phúNM:!10¡phúûÕ/8421BCDûÕ
 
endmodule
 

module m6(
        //ïãI
        input                   rstn,   //MïN	H
        input                   clk,    //eö
        input                   en,
        output [3:0]    cnt,    //¡pú
        output                  cout);  //¢úM

        reg [3:0]               cnt_temp ;      //¡phÄXh
        always@(posedge clk or negedge rstn) begin
                if(! rstn)begin         //Mö¡öR0
                        cnt_temp        <= 4'b0 ;
                end
                else if(en) begin
                        if (cnt_temp==4'd5) begin  //¡ö10*cycleö¡öR0
                                cnt_temp        <=4'b000;
                        end
                        else begin                                      //¡ö 1
                                cnt_temp        <= cnt_temp + 1'b1 ;
                        end
                end
                else begin
                        cnt_temp <= cnt_temp;
                end
        end

        assign  cout = (cnt_temp==4'd5) ;       //úhM
        assign  cnt  = cnt_temp ;                       //úö¡öh

endmodule

module m10(
        //ïãI
        input                   rstn,   //MïN	H
        input                   clk,    //eö
        input                   en,
        output [3:0]    cnt,    //¡pú
        output                  cout);  //¢úM

        reg [3:0]               cnt_temp ;      //¡phÄXh
        always@(posedge clk or negedge rstn) begin
                if(! rstn)begin         //Mö¡öR0
                        cnt_temp        <= 4'b0 ;
                end
                else if(en) begin
                        if (cnt_temp==4'd9) begin  //¡ö10*cycleö¡öR0
                                cnt_temp        <=4'b000;
                        end
                        else begin                                      //¡ö 1
                                cnt_temp        <= cnt_temp + 1'b1 ;
                        end
                end
                else begin
                        cnt_temp <= cnt_temp;
                end
        end

        assign  cout = (cnt_temp==4'd9) ;       //úhM
        assign  cnt  = cnt_temp ;                       //úö¡öh

endmodule

