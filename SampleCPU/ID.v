`include "lib/defines.vh"
module ID(
    input wire clk,
    input wire rst,
    // input wire flush,
    input wire [`StallBus-1:0] stall,
    input wire [31:0] inst_sram_rdata,
    input wire [`IF_TO_ID_WD-1:0] if_to_id_bus,
    input wire [`WB_TO_RF_WD-1:0] wb_to_rf_bus,
    input wire [38:0] ex_to_rf_bus,
    input wire [37:0] mem_to_rf_bus,
    
    output wire stallreq,
    output wire stallreq_for_load,
    output wire [`ID_TO_EX_WD-1:0] id_to_ex_bus,
    output wire [`BR_WD-1:0] br_bus 
);

    reg [`IF_TO_ID_WD-1:0] if_to_id_bus_r;
    wire [31:0] inst;
    wire [31:0] id_pc;
    wire ce;

    wire ex_ram_read;
    wire ex_rf_we;
    wire [4:0] ex_rf_waddr;
    wire [31:0] ex_rf_wdata;

    wire mem_rf_we;
    wire [4:0] mem_rf_waddr;
    wire [31:0] mem_rf_wdata;

    wire wb_rf_we;
    wire [4:0] wb_rf_waddr;
    wire [31:0] wb_rf_wdata;

    reg is_stop;
    always @ (posedge clk) begin
        if (rst) begin
            if_to_id_bus_r <= `IF_TO_ID_WD'b0;  
            is_stop <= 1'b0;      
        end
        // else if (flush) begin
        //     ic_to_id_bus <= `IC_TO_ID_WD'b0;
        // end
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

    // forwarding
    assign {
        ex_ram_read,
        ex_rf_we,
        ex_rf_waddr,
        ex_rf_wdata
    } = ex_to_rf_bus;
    assign {
        mem_rf_we,
        mem_rf_waddr,
        mem_rf_wdata
    } = mem_to_rf_bus;
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

    wire [31:0] rf_rdata1, rf_rdata2;
    wire [31:0] rdata1, rdata2;

    regfile u_regfile(
    	.clk    (clk    ),
        .raddr1 (rs ),
        .rdata1 (rf_rdata1 ),
        .raddr2 (rt ),
        .rdata2 (rf_rdata2 ),
        .we     (wb_rf_we     ),
        .waddr  (wb_rf_waddr  ),
        .wdata  (wb_rf_wdata  )
    );

    // if previous rf_we and it's waddr == current, do forwarding
    assign rdata1 = (ex_rf_we && ex_rf_waddr == rs) ? ex_rf_wdata : 
                    (mem_rf_we && mem_rf_waddr == rs) ? mem_rf_wdata :
                    (wb_rf_we && wb_rf_waddr == rs) ? wb_rf_wdata : rf_rdata1;
    assign rdata2 = (ex_rf_we && ex_rf_waddr == rt) ? ex_rf_wdata : 
                    (mem_rf_we && mem_rf_waddr == rt) ? mem_rf_wdata : 
                    (wb_rf_we && wb_rf_waddr == rt) ? wb_rf_wdata : rf_rdata2;

    // if previous inst is load, check if stall is needed
    wire rs_in_ex, rt_in_ex;
    assign rs_in_ex = (ex_rf_we && ex_rf_waddr == rs) ? 1'b1 : 1'b0;
    assign rt_in_ex = (ex_rf_we && ex_rf_waddr == rt) ? 1'b1 : 1'b0;
    assign stallreq_for_load = ex_ram_read && (rs_in_ex | rt_in_ex);

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

    // calculate
    wire inst_add,  inst_addi,  inst_addu,  inst_addiu,
         inst_lui, 
         inst_sub,  inst_subu,
         inst_and,  inst_andi,
         inst_ori,  inst_or,    inst_xor,   inst_nor,   inst_xori,
         inst_sltu, inst_slt,   inst_slti,  inst_sltiu;

    // jump
    wire inst_bne,  inst_beq,  
         inst_jal,  inst_jr,    inst_j;

    // bitwise move
    wire inst_sll,  inst_sllv,  inst_sra,   inst_srav,  inst_srl,   inst_srlv;

    // memory
    wire inst_sb,   inst_sh,    inst_sw,
         inst_lb,   inst_lbu,   inst_lh,    inst_lhu,   inst_lw;

    // calculate
    assign inst_add     = op_d[6'b00_0000] & func_d[6'b10_0000];
    assign inst_addi    = op_d[6'b00_1000];
    assign inst_addu    = op_d[6'b00_0000] & func_d[6'b10_0001];
    assign inst_addiu   = op_d[6'b00_1001];
    assign inst_sub     = op_d[6'b00_0000] & func_d[6'b10_0010];
    assign inst_subu    = op_d[6'b00_0000] & func_d[6'b10_0011];
    assign inst_and     = op_d[6'b00_0000] & func_d[6'b10_0100];
    assign inst_andi    = op_d[6'b00_1100];
    assign inst_ori     = op_d[6'b00_1101];
    assign inst_or      = op_d[6'b00_0000] & func_d[6'b10_0101];
    assign inst_xor     = op_d[6'b00_0000] & func_d[6'b10_0110];
    assign inst_xori    = op_d[6'b00_1110];
    assign inst_nor     = op_d[6'b00_0000] & func_d[6'b10_0111];
    assign inst_lui     = op_d[6'b00_1111];
    assign inst_slt     = op_d[6'b00_0000] & func_d[6'b10_1010];
    assign inst_sltu    = op_d[6'b00_0000] & func_d[6'b10_1011];
    assign inst_slti    = op_d[6'b00_1010];
    assign inst_sltiu   = op_d[6'b00_1011];

    // memory
    assign inst_sb      = op_d[6'b10_1000];
    assign inst_sh      = op_d[6'b10_1001];
    assign inst_sw      = op_d[6'b10_1011];
    assign inst_lb      = op_d[6'b10_0000];
    assign inst_lbu     = op_d[6'b10_0100];
    assign inst_lh      = op_d[6'b10_0001];
    assign inst_lhu     = op_d[6'b10_0101];
    assign inst_lw      = op_d[6'b10_0011];

    // jump
    assign inst_bne     = op_d[6'b00_0101];
    assign inst_beq     = op_d[6'b00_0100];
    assign inst_jal     = op_d[6'b00_0011];
    assign inst_jr      = op_d[6'b00_0000] & func_d[6'b00_1000];
    assign inst_j       = op_d[6'b00_0010];

    // bitwise move
    assign inst_sll     = op_d[6'b00_0000] & func_d[6'b00_0000];
    assign inst_sllv    = op_d[6'b00_0000] & func_d[6'b00_0100];
    assign inst_sra     = op_d[6'b00_0000] & func_d[6'b00_0011];
    assign inst_srav    = op_d[6'b00_0000] & func_d[6'b00_0111];
    assign inst_srl     = op_d[6'b00_0000] & func_d[6'b00_0010];
    assign inst_srlv    = op_d[6'b00_0000] & func_d[6'b00_0110];

    // rs to reg1
    assign sel_alu_src1[0] = inst_add   | inst_addi | inst_addiu| inst_addu |
                             inst_sub   | inst_subu |
                             inst_and   | inst_andi |
                             inst_ori   | inst_or   | inst_xor  | inst_nor  | inst_xori |
                             inst_slt   | inst_sltu | inst_slti | inst_sltiu| inst_srl  | inst_srlv |
                             inst_jr    |
                             inst_sllv  | inst_srav | inst_srlv |
                             inst_lw;

    // pc to reg1
    assign sel_alu_src1[1] = inst_jal;

    // sa_zero_extend to reg1
    assign sel_alu_src1[2] = inst_sll   | inst_sra  | inst_srl;

    
    // rt to reg2
    assign sel_alu_src2[0] = inst_add   | inst_addu |
                             inst_sub   | inst_subu |
                             inst_and   |
                             inst_or    | inst_xor  | inst_nor  |
                             inst_slt   | inst_sltu |
                             inst_sll   | inst_sllv | inst_sra  | inst_srav | inst_srl  | inst_srlv |
                             inst_sw ;
    
    // imm_sign_extend to reg2
    assign sel_alu_src2[1] = inst_addi  | inst_addiu|
                             inst_andi  |
                             inst_lui   |
                             inst_slti  | inst_sltiu|
                             inst_lw;

    // 32'b8 to reg2
    assign sel_alu_src2[2] = inst_jal;

    // imm_zero_extend to reg2
    assign sel_alu_src2[3] = inst_ori   | inst_xori;

    wire op_add, op_sub, op_slt, op_sltu;
    wire op_and, op_nor, op_or, op_xor;
    wire op_sll, op_srl, op_sra, op_lui;

    assign op_add  = inst_add   | inst_addi | inst_addu | inst_addiu| 
                     inst_jal   |
                     inst_lw;
    assign op_sub  = inst_sub   | inst_subu;
    assign op_slt  = inst_slt   | inst_slti;
    assign op_sltu = inst_sltu  | inst_sltiu;
    assign op_and  = inst_and   | inst_andi;
    assign op_nor  = inst_nor;
    assign op_or   = inst_ori   | inst_or;
    assign op_xor  = inst_xor   | inst_xori;
    assign op_sll  = inst_sll   | inst_sllv;
    assign op_srl  = inst_srl   | inst_srlv;
    assign op_sra  = inst_sra   | inst_srav;
    assign op_lui  = inst_lui;

    assign alu_op = {op_add, op_sub, op_slt, op_sltu,
                     op_and, op_nor, op_or, op_xor,
                     op_sll, op_srl, op_sra, op_lui};

    wire [4:0] mem_op;
    assign mem_op = {inst_lb, inst_lbu, inst_lh, inst_lhu, inst_lw};

    // load and store enable
    assign data_ram_en = inst_sw | inst_lw;

    // write enable
    assign data_ram_wen = {1'b0, inst_sb, inst_sh, inst_sw};



    // regfile store enable
    assign rf_we = inst_add | inst_addi | inst_addu | inst_addiu| 
                   inst_sub | inst_subu | 
                   inst_and | inst_andi |
                   inst_ori | inst_or   | inst_xor  | inst_nor  | inst_xori |
                   inst_lui |
                   inst_slt | inst_sltu | inst_slti | inst_sltiu|
                   inst_jal |
                   inst_sll | inst_sllv | inst_sra  | inst_srav | inst_srl  | inst_srlv |
                   inst_lw;



    // store in [rd]
    assign sel_rf_dst[0] = inst_add | inst_addu |
                           inst_sub | inst_subu |
                           inst_and |
                           inst_or  | inst_xor  | inst_nor  |
                           inst_slt | inst_sltu |
                           inst_sll | inst_sllv | inst_sra  | inst_srav | inst_srl  | inst_srlv;
    // store in [rt] 
    assign sel_rf_dst[1] = inst_addi| inst_addiu|
                           inst_andi|
                           inst_ori | inst_xori | 
                           inst_lui |
                           inst_slti| inst_sltiu|
                           inst_lw;
    // store in [31]
    assign sel_rf_dst[2] = inst_jal;

    // sel for regfile address
    assign rf_waddr = {5{sel_rf_dst[0]}} & rd 
                    | {5{sel_rf_dst[1]}} & rt
                    | {5{sel_rf_dst[2]}} & 32'd31;

    // 0 from alu_res ; 1 from ld_res
    assign sel_rf_res = inst_lw; 

    assign id_to_ex_bus = {
        mem_op,         // 163:159
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
        rdata1,         // 63:32
        rdata2          // 31:0
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

    assign rs_eq_rt = (rdata1 == rdata2);

    assign br_e = inst_bne & !rs_eq_rt  |
                  inst_beq & rs_eq_rt   | 
                  inst_jal  | inst_jr   | inst_j;
    
    assign br_addr = inst_beq   ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}):
                     inst_bne   ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}):
                     inst_jal   ? {pc_plus_4[31:28], instr_index, 2'b0}         :
                     inst_j     ? {pc_plus_4[31:28], instr_index, 2'b0}         :
                     inst_jr    ? rdata1 : 
                     32'b0;

    assign br_bus = {
        br_e,
        br_addr
    };
    


endmodule