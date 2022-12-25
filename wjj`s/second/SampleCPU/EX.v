`include "lib/defines.vh"
module EX(
    input wire clk,
    input wire rst,
    // input wire flush,
    input wire [`StallBus-1:0] stall,

    input wire [`ID_TO_EX_WD-1:0] id_to_ex_bus,

    output wire [`EX_TO_MEM_WD-1:0] ex_to_mem_bus,

    output wire data_sram_en,
    output wire [3:0] data_sram_wen,
    output wire [31:0] data_sram_addr,
    output wire [31:0] data_sram_wdata,

    input wire [31:0] hi_data,
    input wire [31:0] lo_data,
    output wire stallreq_for_ex
);

    reg [`ID_TO_EX_WD-1:0] id_to_ex_bus_r;

    always @ (posedge clk) begin
        if (rst) begin
            id_to_ex_bus_r <= `ID_TO_EX_WD'b0;
        end
        // else if (flush) begin
        //     id_to_ex_bus_r <= `ID_TO_EX_WD'b0;
        // end
        else if (stall[2]==`Stop && stall[3]==`NoStop) begin
            id_to_ex_bus_r <= `ID_TO_EX_WD'b0;
        end
        else if (stall[2]==`NoStop) begin
            id_to_ex_bus_r <= id_to_ex_bus;
        end
    end

    wire [31:0] ex_pc, inst;
    wire [8:0] hilo_op;
    wire [4:0] mem_op;
    wire [11:0] alu_op;
    wire [2:0] sel_alu_src1;
    wire [3:0] sel_alu_src2;
    wire data_ram_en;
    wire [3:0] data_ram_wen;
    wire rf_we;
    wire [4:0] rf_waddr;
    wire sel_rf_res;
    wire [31:0] rf_rdata1, rf_rdata2;
    reg is_in_delayslot;

    assign {
        hilo_op,
        mem_op,
        ex_pc,          // 148:117
        inst,           // 116:85
        alu_op,         // 84:83
        sel_alu_src1,   // 82:80
        sel_alu_src2,   // 79:76
        data_ram_en,    // 75
        data_ram_wen,   // 74:71
        rf_we,          // 70
        rf_waddr,       // 69:65
        sel_rf_res,     // 64
        rf_rdata1,         // 63:32
        rf_rdata2          // 31:0
    } = id_to_ex_bus_r;

    wire [31:0] imm_sign_extend, imm_zero_extend, sa_zero_extend;
    assign imm_sign_extend = {{16{inst[15]}},inst[15:0]};
    assign imm_zero_extend = {16'b0, inst[15:0]};
    assign sa_zero_extend = {27'b0,inst[10:6]};

    wire [31:0] alu_src1, alu_src2;
    wire [31:0] alu_result, ex_result;
    wire [31:0] hilo_result;
    wire [65:0] hilo_bus;

    assign alu_src1 = sel_alu_src1[1] ? ex_pc :
                      sel_alu_src1[2] ? sa_zero_extend : rf_rdata1;

    assign alu_src2 = sel_alu_src2[1] ? imm_sign_extend :
                      sel_alu_src2[2] ? 32'd8 :
                      sel_alu_src2[3] ? imm_zero_extend : rf_rdata2;
    
    alu u_alu(
    	.alu_control (alu_op ),
        .alu_src1    (alu_src1    ),
        .alu_src2    (alu_src2    ),
        .alu_result  (alu_result  )
    );
    
    wire inst_sb, inst_sh, inst_sw;
    wire [3:0] data_sram_wen_r;
    wire [31:0] data_sram_wdata_r;

    assign {
        inst_sb,
        inst_sh,
        inst_sw
    } = data_ram_wen[2:0];

    assign data_sram_wen_r = inst_sw ? 4'b1111 : 4'b0000;
    assign data_sram_wdata_r = inst_sw ? rf_rdata2 : 32'b0;

    assign data_sram_en = data_ram_en;
    assign data_sram_wen = data_sram_wen_r;
    assign data_sram_addr = alu_result; 
    assign data_sram_wdata = data_sram_wdata_r;

    assign ex_to_mem_bus = {
        hilo_bus,
        mem_op,
        ex_pc,          // 75:44
        data_ram_en,    // 43
        data_ram_wen,   // 42:39
        sel_rf_res,     // 38
        rf_we,          // 37
        rf_waddr,       // 36:32
        ex_result       // 31:0
    };
    // hilo part
    wire inst_div,inst_mflo,inst_mfhi;
    wire inst_mthi,inst_mtlo;
    wire inst_divu;
    wire inst_mul,inst_mult,inst_multu;
    assign {
        inst_mfhi, inst_mflo, inst_mthi, inst_mtlo,
        inst_mult, inst_multu, inst_div, inst_divu,
        inst_mul
    } = hilo_op;

    reg stallreq_for_mul;
    reg stallreq_for_div;
    assign stallreq_for_ex = stallreq_for_div | stallreq_for_mul;

    wire [63:0] mul_result;
    wire mul_signed; // 有符号乘法标记

    wire [63:0] div_result;
    wire div_ready_i;
    reg [31:0] div_opdata1_o;
    reg [31:0] div_opdata2_o;
    reg div_start_o;
    reg signed_div_o;
    
    wire hi_we,lo_we;
    wire [31:0] hi_result,lo_result;

    wire op_mul = inst_mul | inst_mult | inst_multu;
    wire op_div = inst_div | inst_divu;

    assign hi_we = op_div | inst_mult | inst_multu | inst_mthi;
    assign lo_we = op_div | inst_mult | inst_multu | inst_mtlo;
    assign hi_result = inst_mthi ? rf_rdata1:
                       op_div ? div_result[63:32] :
                       op_mul ? mul_result[63:32] :
                       32'b0;
    assign lo_result = inst_mtlo ? rf_rdata1:
                       op_div ? div_result[31:0] :
                       op_mul ? mul_result[31:0] :
                       32'b0;

    assign hilo_result = inst_mfhi ? hi_data :
                         inst_mflo ? lo_data :
                         32'b0;
    assign hilo_bus = {hi_we,lo_we,hi_result,lo_result};
    assign ex_result = (inst_mfhi | inst_mflo) ? hilo_result : alu_result;

    // MUL part
    assign mul_signed = inst_mult;
    mul u_mul(
    	.clk        (clk            ),
        .resetn     (~rst           ),
        .mul_signed (mul_signed     ),
        .ina        (rf_rdata1      ), // 乘法源操作数1
        .inb        (rf_rdata2      ), // 乘法源操作数2
        .result     (mul_result     ) // 乘法结果 64bit
    );

    reg cnt;
    reg next_cnt;

    always @ (posedge clk) begin
        if (rst) begin
            cnt <= 1'b0;
        end
        else begin
            cnt <= next_cnt;
        end
    end

    always @ (posedge clk) begin
        if (clk) begin
            stallreq_for_mul <= 1'b0;
            next_cnt <= 1'b0;
        end
        else if ((inst_mult | inst_multu) & ~cnt) begin
            stallreq_for_mul <= 1'b1;
            next_cnt <= 1'b1;
        end
        else if ((inst_mult | inst_multu) & cnt) begin
            stallreq_for_mul <= 1'b0;
            next_cnt <= 1'b0;
        end
        else begin
            stallreq_for_mul <= 1'b0;
            next_cnt <= 1'b0;
        end
    end 

    // DIV part
    div u_div(
    	.rst          (rst          ),
        .clk          (clk          ),
        .signed_div_i (signed_div_o ),
        .opdata1_i    (div_opdata1_o    ),
        .opdata2_i    (div_opdata2_o    ),
        .start_i      (div_start_o      ),
        .annul_i      (1'b0      ),
        .result_o     (div_result     ), // 除法结果 64bit
        .ready_o      (div_ready_i      )
    );

    always @ (*) begin
        if (rst) begin
            stallreq_for_div = `NoStop;
            div_opdata1_o = `ZeroWord;
            div_opdata2_o = `ZeroWord;
            div_start_o = `DivStop;
            signed_div_o = 1'b0;
        end
        else begin
            stallreq_for_div = `NoStop;
            div_opdata1_o = `ZeroWord;
            div_opdata2_o = `ZeroWord;
            div_start_o = `DivStop;
            signed_div_o = 1'b0;
            case ({inst_div,inst_divu})
                2'b10:begin
                    if (div_ready_i == `DivResultNotReady) begin
                        div_opdata1_o = rf_rdata1;
                        div_opdata2_o = rf_rdata2;
                        div_start_o = `DivStart;
                        signed_div_o = 1'b1;
                        stallreq_for_div = `Stop;
                    end
                    else if (div_ready_i == `DivResultReady) begin
                        div_opdata1_o = rf_rdata1;
                        div_opdata2_o = rf_rdata2;
                        div_start_o = `DivStop;
                        signed_div_o = 1'b1;
                        stallreq_for_div = `NoStop;
                    end
                    else begin
                        div_opdata1_o = `ZeroWord;
                        div_opdata2_o = `ZeroWord;
                        div_start_o = `DivStop;
                        signed_div_o = 1'b0;
                        stallreq_for_div = `NoStop;
                    end
                end
                2'b01:begin
                    if (div_ready_i == `DivResultNotReady) begin
                        div_opdata1_o = rf_rdata1;
                        div_opdata2_o = rf_rdata2;
                        div_start_o = `DivStart;
                        signed_div_o = 1'b0;
                        stallreq_for_div = `Stop;
                    end
                    else if (div_ready_i == `DivResultReady) begin
                        div_opdata1_o = rf_rdata1;
                        div_opdata2_o = rf_rdata2;
                        div_start_o = `DivStop;
                        signed_div_o = 1'b0;
                        stallreq_for_div = `NoStop;
                    end
                    else begin
                        div_opdata1_o = `ZeroWord;
                        div_opdata2_o = `ZeroWord;
                        div_start_o = `DivStop;
                        signed_div_o = 1'b0;
                        stallreq_for_div = `NoStop;
                    end
                end
                default:begin
                end
            endcase
        end
    end

    // mul_result 和 div_result 可以直接使用
    
    
endmodule