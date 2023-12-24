`timescale 1ns/1ps

module datapath(clk, reset, memdata, memadd, outdata, writeDataEN, stopf);
    input clk, reset; input [31:0]memdata;
    output reg [31:0] memadd, outdata; 
    output reg writeDataEN, stopf;

    logic [31:0] registers [0:31];
    reg [31:0] ireg, dreg, pcreg, areg, breg, alureg;

    wire [5:0]op,funct; wire [4:0]rs,rt,rd,shamt; wire[15:0]Iimm; wire [25:0] Jimm;

    //DECODIFICATION OF THE INSTRUCTION:
    
    assign {op, rs, rt, rd, shamt, funct} = ireg;
    assign Iimm = ireg[15:0]; assign Jimm = ireg[25:0];

    wire isRType = !(|op);
    wire isANDI = (op == 6'b001100), isORI  = (op == 6'b001101), isADDI = (op == 6'b001000);
    wire isXORI = (op == 6'b001110), isSLTI = (op == 6'b001010); 

    wire isJR = (isRType && funct == 6'b001000), isJAL = (op == 6'b000011), isJ = (op == 6'b000010);
    wire isBEQ = (op == 6'b000100),              isBNE = (op == 6'b000101);

    wire isEND = &(op) && &(funct);
    assign stopf = isEND;

    // DEFINITION OF ALU OPERATION
    wire [7:0] aluop = !isRType ? 8'h00 :
                       (funct == 6'b100000) ? 8'h01 : (funct == 6'b100010) ? 8'h02 : (funct == 6'b100100) ? 8'h04 :
                       (funct == 6'b100101) ? 8'h08 : (funct == 6'b100110) ? 8'h10 : (funct == 6'b101010) ? 8'h20:
                       (funct == 6'b000000) ? 8'h40 : (funct == 6'b000010) ? 8'h80 : 8'h00;    

    wire isALUreg = (isRType && !isJR), isALUimm = isADDI || isORI || isADDI || isXORI || isSLTI;
    wire isLoad   = (op == 6'b100010),  isStore  = (op == 6'b101011);
    wire isJump   = isJ || isJAL,       isBranch = isBEQ || isBNE;
    
    wire [4:0] rw = (isALUreg) ? rd : isJAL ? 5'd31 : rt;
    wire [31:0] bAdd = {{14{Iimm[15]}},Iimm,2'd0}, EIimm = {{16{Iimm[15]}}, Iimm}, Jpc = {pcreg[31:28], Jimm, 2'b00};

    //STATE MACHINE
    localparam FETCH_INST = 0;
    localparam FETCH_REGS = 1;
    localparam EXECUTE    = 2;
    localparam MEMORY     = 3;
    reg [2:0] state = FETCH_INST;

    wire [31:0] writeBackData = (isLoad) ? memdata : (isJAL) ? pcreg : aluout;
    wire writeBackEN = (state == MEMORY) || (state == EXECUTE && (isALUreg || isALUimm  || isJAL));

    assign writeDataEN = (isStore && state == EXECUTE);

    always@(posedge clk) begin
        if(reset) begin
            pcreg <= 0;    ireg <= 32'h00000020;
            state <= FETCH_INST;
        end else begin
            if(writeBackEN && rw!=0) begin
                registers[rw] <= writeBackData;
            end
                    
            case(state)
                FETCH_INST: begin
                    ireg <= memdata;
                    pcreg <= aluout;
                    state <= FETCH_REGS;
                end
                FETCH_REGS: begin
                    areg <= !(|rs) ? 0 : registers[rs];
                    breg <= !(|rt) ? 0 : registers[rt];

                    alureg <= aluout;
                    if(isJ || isJR ) pcreg <= nextpc;

                    state <= (isJ || isJR) ? FETCH_INST : EXECUTE;
                end
                EXECUTE: begin
                    dreg <= memdata;
                    if(isBranch || isJAL) pcreg <= nextpc;
                    
                    state = (isLoad) ? MEMORY : FETCH_INST;
                end
                MEMORY: begin
                    state <= FETCH_INST;
                end  
            endcase

            if(isEND)
                $writememh("regs.dat", registers);            
        end
    end

    // ALU INPUTS
    wire [31:0] aluIN1 = aluop[6] || aluop[7]                          ? {27'b0, shamt} :
                         (state == FETCH_REGS || state == FETCH_INST)  ? pcreg          :
                         areg                                                           ;

    wire [31:0] aluIN2 = (state == FETCH_INST)  ? 32'd4 : 
                         (state == FETCH_REGS)  ? bAdd  : 
                         (isALUreg || isBranch) ? breg  :
                         EIimm                          ;

    // ALU OUPUT/ZERO SIGNALS    
    reg [31:0] aluout;
    wire zero = !(|(aluout));

    // ALU
    always@(*) begin
        if( isADDI || aluop[0] || isLoad || isStore || isJAL || (state == FETCH_INST) || (state == FETCH_REGS))
            aluout = aluIN1 + aluIN2;
        else if( aluop[1] || isBranch)
            aluout = aluIN1 - aluIN2;
        else if( isANDI || aluop[2])
            aluout = aluIN1 & aluIN2;
        else if( isORI || aluop[3])
            aluout = aluIN1 | aluIN2;
        else if( isXORI || aluop[4])
            aluout = aluIN1 ^ aluIN2;
        else if(isSLTI || aluop[5])
            aluout = aluIN1 < aluIN2;
        else if(aluop[6])
            aluout = aluIN2 << aluIN1;
        else if(aluop[7])
            aluout = aluIN2 >> aluIN1;    
    end

    wire takebranch = ( isBEQ && zero) || ( isBNE && !zero);

    wire [31:0] nextpc = isJump     ? Jpc           :
                         takebranch ? alureg        :
                         isJR       ? registers[rs] : 
                         pcreg                      ;

    assign memadd = (state == FETCH_INST) ? pcreg : aluout;

    assign outdata = breg;

endmodule