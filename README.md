`timescale 1ns / 1ps



module uart(
    input logic clk,
    input logic reset,
    input logic datain,
    input logic r_enable,
    output logic full,empty,
    output logic [7:0] fifo_data,  
    output logic dataout
    );
    
logic [7:0] seven_r;
logic [7:0] seven_W; 
 
 logic [7:0] rx_data;   
   
logic stp; 
logic [3:0] read_ptr,write_ptr;
logic full_history=0,empty_history=0;
logic ful,emp;


always_ff @(posedge clk)begin
    if(reset)begin
        full_history<=0;
        empty_history<=0;
    end else begin
        full_history<=full;
        empty_history<=empty;
        
        if(full ==1 && full_history==0) begin
            ful<=1;
        end else begin
            ful<=0;
        end
        if(empty==1 && empty_history==0)begin
            emp<=1;
         end else begin
            emp<=0;
         end
    end
end

 rx r(
    .clk(clk),
    .reset(reset),
    .rx_serial(datain),
    .stop(stp),
    .rx_dataout(rx_data)
 );
    
fifo f(
    .clk(clk),
    .reset(reset),
    .datain(rx_data),
    .w_en(stp),
    .r_en(r_enable),
    .r_ptr(read_ptr),
    .w_ptr(write_ptr),
    .full(full),
    .empty(empty),
    .dataout(fifo_data)
);

tx t(
    .clk(clk),
    .reset(reset),
    .full(ful),
    .empty(emp),
    .tx_datain(fifo_data),
    .tx_serial(dataout)    
);
    
seven_segment sg (
    .A(write_ptr),
    .segment(seven_w)
);

seven_segment sg1(
    .A(read_ptr),
    .segment(seven_r)
);

endmodule

module rx #(parameter clkdiv=100000000/115200)(
    input logic clk,
    input logic reset,
    input logic rx_serial,
    output logic stop,
    output logic[7:0] rx_dataout
    );
    

typedef enum logic [1:0] {
    IDLE,
    START,
    DATA,
    STOP
}data_state;

data_state state;
logic [15:0] counter=0;
logic[3:0] bitcounter=0;
logic[8:0] rx_buffer=0;

always_ff @(posedge clk) begin
    if(reset)begin
        state<=IDLE;
        counter<=0;
        bitcounter<=0;
        stop<=0;
    end else begin
        case(state)
        IDLE:begin
                stop<=0;
                if(!rx_serial)begin
                    state<=START;
                    counter<='0;
                end
            end
        START:begin
            counter<=counter+1;
                 if(counter==clkdiv[15:1])begin
                        state<=DATA;
                        counter<=0;
                        bitcounter<=0;
                    end
            end
       DATA:begin
                counter<=counter+1;
                if(counter==clkdiv)begin
                    counter<=0;
                    bitcounter<=bitcounter+1;
                    rx_buffer<= {rx_serial,rx_buffer[8:1]};
                    
                    if(bitcounter==4'd8)begin
                        rx_dataout<=rx_buffer[8:1];
                        stop<=1;
                        state<=IDLE;
                        bitcounter<=0;
                    end
                end
            end
        endcase
    end
end
    
    
endmodule


module tx #(parameter clkdiv=100000000/115200)(
    input logic clk,
    input logic reset,
    input logic full,empty,
    input logic [7:0] tx_datain,
    output logic tx_serial
    );
    
typedef enum logic {IDLE,TRANSFER} t_state;
t_state state;

logic [7:0]tx_data_e,tx_data_f;

assign tx_data_f = 8'b01100110;
assign tx_data_e = 8'b01100101;

logic tx_stop;
logic[15:0] counter;
logic[3:0] bitcounter;
logic [9:0] tx_buffer=0;

always_ff @(posedge clk)begin
    if(reset)begin
        state<=IDLE;
        counter<=0;
        bitcounter<=0;
        tx_stop<=0;
    end else begin
        case(state)
            IDLE:begin
                tx_serial<=1'b0;
                tx_stop<=1'b0;
                if(full==1 && empty==0) begin
                    state<=TRANSFER;
                    counter<=0;
                    bitcounter<=0;
                    tx_buffer<={1'b1,tx_data_f,1'b0};
                end
                if(empty==1 && full==0)begin
                    state<=TRANSFER;
                    counter<=0;
                    bitcounter<=0;
                    tx_buffer<={1'b1,tx_data_e,1'b0};                   
                end
            end
            
            TRANSFER:begin
                tx_serial<=tx_buffer[0];
                counter<=counter+1;
                if(counter==clkdiv)begin
                    counter<=0;
                    bitcounter<=bitcounter+1;
                    tx_buffer[8:0]<=tx_buffer[9:1];
                    if(bitcounter==4'd9)begin
                        bitcounter<=0;
                        state<=IDLE;
                        tx_stop<=1;
                        
                    end
                end
            end
        endcase
        
    end
end
endmodule

module fifo(
    input clk,
    input reset,
    input logic[7:0] datain,
    input logic r_en,w_en,
    output full,empty,
    output logic [3:0] w_ptr,r_ptr,
    output logic[7:0]dataout
);
    logic read_previous,button;
    logic[7:0] register[7:0];
    
    
    always_ff @(posedge clk) begin
        read_previous<=r_en;
        if(r_en==1 && read_previous==0)begin
            button<=1;
        end else begin
            button<=0;
        end
        if(reset)begin
            w_ptr<=0;
            r_ptr<=0;
        end else begin
            if(w_en && !full)begin
                register[w_ptr%8]<=datain;
                w_ptr<=w_ptr+1;
            end
            if(r_en && !empty)begin
                dataout<=register[r_ptr%8];
                r_ptr<=r_ptr+1;
            end
        end
    end
    
    assign full = w_ptr-r_ptr==4'd8;
    assign empty = r_ptr==w_ptr;

endmodule

module seven_segment (
    input logic A,
    output logic [7:0] segment
);

always_comb begin
    case(A)
        4'd0: segment=8'b11111100;
        4'd0: segment=8'b01100000;
        4'd0: segment=8'b11011010;
        4'd0: segment=8'b11110010;
        4'd0: segment=8'b01100110;
        4'd0: segment=8'b10110110;
        4'd0: segment=8'b10111110;
        4'd0: segment=8'b11100000;
        4'd0: segment=8'b11111110;
        4'd0: segment=8'b11110110;
        4'd0: segment=8'b11101110;
        4'd0: segment=8'b00111110;
        4'd0: segment=8'b10011100;
        4'd0: segment=8'b01111010;
        4'd0: segment=8'b10011110;
        4'd0: segment=8'b10001110;
        
    endcase
end
endmodule
