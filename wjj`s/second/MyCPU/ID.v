`include "lib/defines.vh"
module ID(
    input wire clk,
    input wire rst,
    input wire flush,
    input wire [`StallBus-1:0] stall,
    
    output wire stallreq,

    input wire [`IF_TO_ID_WD-1:0] if_to_id_bus,

    input wire [31:0] inst_sram_rdata,

    input wire [`WB_TO_RF_WD-1:0] wb_to_rf_bus,

    output wire [`ID_TO_EX_WD-1:0] id_to_ex_bus,

    output wire [`BR_WD-1:0] br_bus ,
    input wire ex_we,
    input wire [4:0] ex_waddr,
    input wire [31:0] ex_wdata,
    input wire mem_we,
    input wire [4:0] mem_waddr,
    input wire [31:0] mem_wdata,
    output wire stall_for_load,
    input wire ex_ram_read
);

    reg [`IF_TO_ID_WD-1:0] if_to_id_bus_r;
    wire [31:0] inst;
    wire [31:0] id_pc;
    wire ce;

    wire wb_rf_we;
    wire [4:0] wb_rf_waddr;
    wire [31:0] wb_rf_wdata;

    wire [4:0] mem_op;

    reg is_stop;

    always @ (posedge clk) begin
        if (rst) begin
            if_to_id_bus_r <= `IF_TO_ID_WD'b0;  
            is_stop <= 1'b0;      
        end
        else if (flush) begin
            if_to_id_bus_r <= `IF_TO_ID_WD'b0;
            is_stop <= 1'b0; 
        end
        else if (stall[1]==`Stop && stall[2]==`NoStop) begin
            if_to_id_bus_r <= `IF_TO_ID_WD'b0;
            is_stop <= 1'b0;  
        end
        else if (stall[1]==`NoStop) begin
            if_to_id_bus_r <= if_to_id_bus;
            is_stop <= 1'b0;  
        end
        else if (stall[2]==`Stop) begin
            is_stop <= 1'b1;  
        end
    end
    
    assign inst = is_stop ? inst : inst_sram_rdata;
    assign {
        ce,
        id_pc
    } = if_to_id_bus_r;
    assign {
        wb_rf_we,
        wb_rf_waddr,
        wb_rf_wdata
    } = wb_to_rf_bus;

    wire [5:0] opcode;
    wire [4:0] rs,rt,rd,sa;
    wire [5:0] func;
    wire [15:0] imm;
    wire [25:0] instr_index;
    wire [19:0] code;
    wire [4:0] base;
    wire [15:0] offset;
    wire [2:0] sel;

    wire [63:0] op_d, func_d;
    wire [31:0] rs_d, rt_d, rd_d, sa_d;

    wire [2:0] sel_alu_src1;
    wire [3:0] sel_alu_src2;
    wire [11:0] alu_op;

    wire data_ram_en;
    wire [3:0] data_ram_wen;
    
    wire rf_we;
    wire [4:0] rf_waddr;
    wire sel_rf_res;
    wire [2:0] sel_rf_dst;

    wire [31:0] rdata1, rdata2;

    wire rs_ex_ok, rt_ex_ok;
    wire rs_mem_ok, rt_mem_ok;
    wire sel_rs_forward;
    wire sel_rt_forward;
    wire [31:0] rs_forward_data;
    wire [31:0] rt_forward_data;
    wire [31:0] rf_rdata1;
    wire [31:0] rf_rdata2;
    wire [31:0] rdata1_fd;
    wire [31:0] rdata2_fd;

    regfile u_regfile(
    	.clk    (clk    ),
        .raddr1 (rs ),
        .rdata1 (rdata1 ),
        .raddr2 (rt ),
        .rdata2 (rdata2 ),
        .we     (wb_rf_we     ),
        .waddr  (wb_rf_waddr  ),
        .wdata  (wb_rf_wdata  )
    );

    wire sel_r1_wdata;
    wire sel_r2_wdata;
    assign sel_r1_wdata = wb_rf_we & (wb_rf_waddr == rs);
    assign sel_r2_wdata = wb_rf_we & (wb_rf_waddr == rt);

    assign rf_rdata1 = sel_r1_wdata ? wb_rf_wdata : rdata1;
    assign rf_rdata2 = sel_r2_wdata ? wb_rf_wdata : rdata2;

    assign opcode = inst[31:26];
    assign rs = inst[25:21];
    assign rt = inst[20:16];
    assign rd = inst[15:11];
    assign sa = inst[10:6];
    assign func = inst[5:0];
    assign imm = inst[15:0];
    assign instr_index = inst[25:0];
    assign code = inst[25:6];
    assign base = inst[25:21];
    assign offset = inst[15:0];
    assign sel = inst[2:0];

    wire inst_ori, inst_lui, inst_addiu, inst_beq;
    wire inst_subu,inst_jal, inst_jr,    inst_addu;
    wire inst_bne, inst_sll, inst_or,    inst_lw;
    wire inst_lb,  inst_lbu, inst_lh,    inst_lhu;
    wire inst_xor, inst_sb , inst_sh,    inst_sw;
    wire inst_sltu;

    wire op_add, op_sub, op_slt, op_sltu;
    wire op_and, op_nor, op_or, op_xor;
    wire op_sll, op_srl, op_sra, op_lui;

    decoder_6_64 u0_decoder_6_64(
    	.in  (opcode  ),
        .out (op_d )
    );

    decoder_6_64 u1_decoder_6_64(
    	.in  (func  ),
        .out (func_d )
    );
    
    decoder_5_32 u0_decoder_5_32(
    	.in  (rs  ),
        .out (rs_d )
    );

    decoder_5_32 u1_decoder_5_32(
    	.in  (rt  ),
        .out (rt_d )
    );

    
    assign inst_ori     = op_d[6'b00_1101];
    assign inst_lui     = op_d[6'b00_1111];
    assign inst_addiu   = op_d[6'b00_1001];
    assign inst_beq     = op_d[6'b00_0100];
    assign inst_subu    = op_d[6'b00_0000] & func_d[6'b10_0011];
    assign inst_jal     = op_d[6'b00_0011];
    assign inst_jr      = op_d[6'b00_0000] & func_d[6'b00_1000];
    assign inst_addu    = op_d[6'b00_0000] & func_d[6'b10_0001];
    assign inst_bne     = op_d[6'b00_0101];
    assign inst_sll     = op_d[6'b00_0000] & func_d[6'b00_0000];
    assign inst_or      = op_d[6'b00_0000] & func_d[6'b10_0101];
    assign inst_lw      = op_d[6'b10_0011];
    assign inst_lbu     = op_d[6'b10_0100];
    assign inst_lb      = op_d[6'b10_0000];
    assign inst_lhu     = op_d[6'b10_0101];
    assign inst_lh      = op_d[6'b10_0001];
    assign inst_xor     = op_d[6'b00_0000] & func_d[6'b10_0110];
    assign inst_sb      = op_d[6'b10_1000];
    assign inst_sw      = op_d[6'b10_1011];
    assign inst_sh      = op_d[6'b10_1001];
    assign inst_sltu    = op_d[6'b00_0000] & func_d[6'b10_1011];

    // rs to reg1
    assign sel_alu_src1[0] = inst_ori | inst_addiu | inst_subu
                           | inst_addu| inst_or    | inst_lw
                           | inst_xor | inst_lb    | inst_lbu
                           | inst_lh  | inst_lhu   | inst_sb
                           | inst_sh  | inst_sw    | inst_sltu;

    // pc to reg1
    assign sel_alu_src1[1] = inst_jal;

    // sa_zero_extend to reg1
    assign sel_alu_src1[2] = inst_sll;

    
    // rt to reg2
    assign sel_alu_src2[0] = inst_subu | inst_addu | inst_sll
                            |inst_or   | inst_xor  | inst_sltu;
    
    // imm_sign_extend to reg2
    assign sel_alu_src2[1] = inst_lui | inst_addiu | inst_lw
                           | inst_lb  | inst_lbu   | inst_lh
                           | inst_lhu | inst_sb    | inst_sh
                           | inst_sw; 

    // 32'b8 to reg2
    assign sel_alu_src2[2] = inst_jal;

    // imm_zero_extend to reg2
    assign sel_alu_src2[3] = inst_ori;



    assign op_add = inst_addiu | inst_jal | inst_addu | inst_lw
                  | inst_lb    | inst_lbu | inst_lh   | inst_lhu
                  | inst_sb    | inst_sh  | inst_sw; 
    assign op_sub = inst_subu;
    assign op_slt = 1'b0;
    assign op_sltu = inst_sltu;
    assign op_and = 1'b0;
    assign op_nor = 1'b0;
    assign op_or = inst_ori | inst_or;
    assign op_xor = inst_xor;
    assign op_sll = inst_sll;
    assign op_srl = 1'b0;
    assign op_sra = 1'b0;
    assign op_lui = inst_lui;

    assign alu_op = {op_add, op_sub, op_slt, op_sltu,
                     op_and, op_nor, op_or, op_xor,
                     op_sll, op_srl, op_sra, op_lui};

    assign mem_op = {inst_lb, inst_lbu, inst_lh, inst_lhu, inst_lw};



    // load and store enable
    assign data_ram_en = inst_lw | inst_lb | inst_lbu | inst_lh
                       | inst_lhu| inst_sb | inst_sh  | inst_sw;

    // write enable
    assign data_ram_wen = {1'b0, inst_sb, inst_sh, inst_sw};



    // regfile store enable
    assign rf_we = inst_ori | inst_lui | inst_addiu | inst_subu
                |  inst_jal | inst_addu| inst_sll   | inst_or
                |  inst_lw  | inst_lb  | inst_lbu   | inst_lh
                |  inst_lhu | inst_xor | inst_sltu;



    // store in [rd]
    assign sel_rf_dst[0] = inst_subu | inst_addu | inst_sll | inst_or
                         | inst_xor  | inst_sltu;
    // store in [rt] 
    assign sel_rf_dst[1] = inst_ori | inst_lui | inst_addiu | inst_lw
                         | inst_lb  | inst_lbu | inst_lh    | inst_lhu;
    // store in [31]
    assign sel_rf_dst[2] = inst_jal;

    // sel for regfile address
    assign rf_waddr = {5{sel_rf_dst[0]}} & rd 
                    | {5{sel_rf_dst[1]}} & rt
                    | {5{sel_rf_dst[2]}} & 32'd31;

    // 0 from alu_res ; 1 from ld_res
    assign sel_rf_res = inst_lw | inst_lb | inst_lbu | inst_lh | inst_lhu; 

    assign rs_ex_ok = (rs == ex_waddr) && ex_we ? 1'b1 : 1'b0 ;
    assign rt_ex_ok = (rt == ex_waddr) && ex_we ? 1'b1 : 1'b0 ;

    assign rs_mem_ok = (rs == mem_waddr) && mem_we ? 1'b1 : 1'b0 ;
    assign rt_mem_ok = (rt == mem_waddr) && mem_we ? 1'b1 : 1'b0 ;

    assign sel_rs_forward = rs_ex_ok |rs_mem_ok;
    assign sel_rt_forward = rt_ex_ok |rt_mem_ok;

    assign rs_forward_data = rs_ex_ok ? ex_wdata :
                             rs_mem_ok? mem_wdata:
                             32'b0;
    assign rt_forward_data = rt_ex_ok ? ex_wdata :
                             rt_mem_ok? mem_wdata:
                             32'b0;
    assign rdata1_fd = sel_rs_forward ? rs_forward_data : rf_rdata1;
    assign rdata2_fd = sel_rt_forward ? rt_forward_data : rf_rdata2;

    assign stall_for_load = ex_ram_read & (rs_ex_ok | rt_ex_ok);

    assign id_to_ex_bus = {
        mem_op,
        id_pc,          // 158:127
        inst,           // 126:95
        alu_op,         // 94:83
        sel_alu_src1,   // 82:80
        sel_alu_src2,   // 79:76
        data_ram_en,    // 75
        data_ram_wen,   // 74:71
        rf_we,          // 70
        rf_waddr,       // 69:65
        sel_rf_res,     // 64
        rdata1_fd,         // 63:32
        rdata2_fd          // 31:0
    };


    wire br_e;
    wire [31:0] br_addr;
    wire rs_eq_rt;
    wire rs_ge_z;
    wire rs_gt_z;
    wire rs_le_z;
    wire rs_lt_z;
    wire [31:0] pc_plus_4;
    assign pc_plus_4 = id_pc + 32'h4;

    assign rs_eq_rt = (rdata1_fd == rdata2_fd);

    assign br_e = inst_beq & rs_eq_rt | inst_jal | inst_jr
                | inst_bne & ~rs_eq_rt;
    assign br_addr = inst_beq ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}) :
                     inst_bne ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}) :
                     inst_jal ? {pc_plus_4[31:28],inst[25:0],2'b0}:
                     inst_jr  ? rdata1_fd:
                     32'b0;

    assign br_bus = {
        br_e,
        br_addr
    };
    


endmodule