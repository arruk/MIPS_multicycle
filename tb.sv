`timescale 1ns/1ps

`include "datapath.sv"

module tb;

    wire writeDataEN, stopf; reg reset, clk;
    reg [31:0]memdata, memadd, outdata; 

    datapath dut(clk, reset, memdata, memadd, outdata, writeDataEN, stopf);

    integer i;
    logic [31:0] mem [0:63]; // 0 - 31 INSTRUCTION MEM/ 32 - 64 DATA MEM


    always begin
        clk=0; #5; clk=1; #5;
    end

    initial begin
        i=0;
        reset = 1'b1; #10; reset = 1'b0;
        $display("initializing testbench!");
        $dumpfile("dump.vcd"); $dumpvars;
    end

    initial begin
        $readmemb("instr.dat", mem);
    end

    always@(posedge clk) begin
        if(writeDataEN)
            mem[{2'd0, memadd[31:2]}] <= outdata;
        
        i++;
        if(stopf || i>500) begin
            $display("finishing testbench");
            $writememb("memafter.dat", mem);
            $finish();
        end
    end

    assign memdata = mem[{2'd0, memadd[31:2]}];
endmodule