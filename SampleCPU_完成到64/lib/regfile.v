`include "defines.vh"
module regfile(
    input wire clk,
    input wire [4:0] raddr1,
    output wire [31:0] rdata1,
    input wire [4:0] raddr2,
    output wire [31:0] rdata2,
    
    input wire we,
    input wire [4:0] waddr,
    input wire [31:0] wdata
);
    reg [31:0] reg_array [31:0];
    // write
    always @ (posedge clk) begin
        if (we && waddr != 5'b0) begin
            reg_array[waddr] <= wdata;
        end
    end

    // read out 1
    assign rdata1 = (raddr1 == 5'b0) ? 32'b0 :
                    reg_array[raddr1];
    // read out2
    assign rdata2 = (raddr2 == 5'b0) ? 32'b0 :
                    reg_array[raddr2];
endmodule

module hilo_reg(
    input wire clk,
    input wire rst,
    input wire [`StallBus-1:0] stall,

    input wire [65:0] ex_bus,
    input wire [65:0] mem_bus,
    input wire [65:0] hilo_bus,

    output wire [31:0] hi_data,
    output wire [31:0] lo_data
);
    wire [31:0] hi,lo;

    wire ex_hi_we,ex_lo_we,mem_hi_we,mem_lo_we;
    wire [31:0] ex_hi_in,ex_lo_in,mem_hi_in,mem_lo_in;
    wire hi_we,lo_we;
    wire [31:0] hi_in,lo_in;
    assign {
        ex_hi_we,
        ex_lo_we,
        ex_hi_in,
        ex_lo_in
    } = ex_bus;
    assign {
        mem_hi_we,
        mem_lo_we,
        mem_hi_in,
        mem_lo_in
    } = mem_bus;
    assign {
        hi_we,
        lo_we,
        hi_in,
        lo_in
    } = hilo_bus;
    assign hi_data = ex_hi_we ? ex_hi_in:
                     mem_hi_we? mem_hi_in:
                     hi_we    ? hi_in:
                     hi;
    assign lo_data = ex_lo_we ? ex_lo_in:
                     mem_lo_we? mem_lo_in:
                     lo_we    ? lo_in:
                     lo;
    assign hi = hi_we ? hi_in : hi;
    assign lo = lo_we ? lo_in : lo;
endmodule