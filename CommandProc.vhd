library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.common_pack.all;

entity cmdProc is
    Port (  clk:		in std_logic;
            reset:		in std_logic;
            rxnow:		in std_logic;
            rxData:		in std_logic_vector (7 downto 0);
            txData:	    out std_logic_vector (7 downto 0);
            rxdone:		out std_logic;
            ovErr:		in std_logic;
            framErr:	    in std_logic;
            txnow:		out std_logic;
            txdone:		in std_logic;
            start:        out std_logic;
            numWords_bcd: out BCD_ARRAY_TYPE(2 downto 0);
            dataReady:    in std_logic;
            byte:         in std_logic_vector(7 downto 0);
            maxIndex:     in BCD_ARRAY_TYPE(2 downto 0);
            dataResults:  in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
            seqDone:      in std_logic
         );

end cmdProc;

architecture dataflow of cmdProc is

    type STATE_TYPE is (S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S3_5); -- custome data type for states
    
    -- internal signals
    signal curState, nextState: STATE_TYPE;  
    signal bcd1, bcd2, bcd3: std_logic_vector(3 downto 0);
    signal countEN, resCount: bit;
    signal count: integer; 
    SIGNAL buffer1 : STD_LOGIC_VECTOR(7 downto 0) := (others => '0');
    signal ascii_input: std_logic_vector(3 downto 0);
    signal reg_dataResults: CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
    signal reg_maxIndex: BCD_ARRAY_TYPE(2 downto 0);
    signal reg_bcd1, reg_bcd2, reg_bcd3: std_logic_vector(3 downto 0);
    signal reg_numWords: BCD_ARRAY_TYPE(2 downto 0);
    
    alias msd is byte(7 downto 4); -- first 4 bits of data
    alias lsd is byte(3 downto 0); -- last 4 bits of data
    alias msd_maxIndex is reg_maxIndex(2);
    alias middle_maxIndex is reg_maxIndex(1);
    alias lsd_maxIndex is reg_maxIndex(0);
    alias peakValue_MSD is reg_dataResults(3)(7 downto 4);
    alias peakValue_LSD is reg_dataResults(3)(3 downto 0);     
     
function ascii_converter(input: std_logic_vector(3 downto 0)) return std_logic_vector is -- returns ASCII representation of a 4 bit input
begin
    case input is
        when x"0" => return x"30";
        when x"1" => return x"31";
        when x"2" => return x"32";
        when x"3" => return x"33";
        when x"4" => return x"34";
        when x"5" => return x"35";
        when x"6" => return x"36";
        when x"7" => return x"37";
        when x"8" => return x"38";
        when x"9" => return x"39";
        when x"A" => return x"41";
        when x"B" => return x"42";
        when x"C" => return x"43";
        when x"D" => return x"44";
        when x"E" => return x"45";
        when x"F" => return x"46";     
        when others => return x"00"; 
    end case;
end;

begin

stateLogic: process(clk, reset, curState, rxData, count, seqDone, bcd1, bcd2, bcd3, rxnow, dataReady, byte, txdone, reg_dataResults, reg_maxIndex)
VARIABLE v_state: STATE_TYPE;
VARIABLE v_buffer: std_logic_vector(7 downto 0);
VARIABLE v_en, v_res: bit; -- clock enable and clock reset
VARIABLE v_numWords: BCD_ARRAY_TYPE (2 downto 0);
VARIABLE v_bcd1, v_bcd2, v_bcd3: std_logic_vector(3 downto 0);
begin
    v_state := S0; -- intialises variables default value
    v_buffer := x"00";
    v_en := '0';
    v_res := '0';
    v_bcd1 := x"0";
    v_bcd2 := x"0";
    v_bcd3 := x"0";
 
    start <= '0';
    if rxnow = '1' then
        rxDone <= '1';  -- informs receiver data has been received
    else
        rxDone <= '0';
    end if;

    case curState is
        when S0 =>
            v_res := '1';
            v_buffer := rxData;
            if rxnow = '1' then  -- only reads data if receiver is ready
                v_buffer := rxData;
                if rxData = x"41" or rxData = x"61" then
                    if txdone = '1' then
                        v_state := S1; -- A command
                    else
                        v_state := curState;
                    end if;    
                elsif rxData = x"50" or rxData = x"70" then
                    v_state := S10; --P command
                    
                    v_en := '1';
                elsif rxData = x"4c" or rxData = x"6c" then
                    v_state := S11; -- L command
                else
                    v_state := curState; -- if not a valid command, remains in S0
                end if;
            end if;
            
        when S1 =>
        v_buffer :=  rxData;
        
            if rxnow = '1' then
                v_buffer := rxData;
                if rxData >= x"30" and rxData <= x"39" then --input data must be within the ASCII integer range                   
                    case rxData is -- case statement assigns bcd variable to 4 bit bcd depending on ASCII integer
                        when x"30" => v_bcd1 := x"0";
                        when x"31" => v_bcd1 := x"1";
                        when x"32" => v_bcd1 := x"2";
                        when x"33" => v_bcd1 := x"3";
                        when x"34" => v_bcd1 := x"4";
                        when x"35" => v_bcd1 := x"5";
                        when x"36" => v_bcd1 := x"6";
                        when x"37" => v_bcd1 := x"7";
                        when x"38" => v_bcd1 := x"8";
                        when x"39" => v_bcd1 := x"9";
                        when others => NULL;
                    end case;
                    if txdone = '1' then
                        v_state := S2; 
                    else
                        v_state := curState;
                    end if; 
                elsif rxData = x"41" or rxData = x"61" then -- if A detected back to S0
                    v_state := curState;
                else
                    v_state := S0;
                end if;
            else
                v_state := curState;  
            end if;         
        
        when S2 =>
        v_buffer :=  rxData;
            if rxnow = '1' then
                v_buffer := rxData;
                if rxData >= x"30" and rxData <= x"39" then
                    
                    case rxData is
                        when x"30" => v_bcd2 := x"0";
                        when x"31" => v_bcd2 :=x"1";
                        when x"32" => v_bcd2 :=x"2";
                        when x"33" => v_bcd2 :=x"3";
                        when x"34" => v_bcd2 :=x"4";
                        when x"35" => v_bcd2 :=x"5";
                        when x"36" => v_bcd2 :=x"6";
                        when x"37" => v_bcd2 :=x"7";
                        when x"38" => v_bcd2 :=x"8";
                        when x"39" => v_bcd2 :=x"9";
                        when others => NULL;
                    end case;
                    if txdone = '1' then
                        v_state := S3; 
                    else
                        v_state := curState;
                    end if; 
                elsif rxData = x"41" or rxData = x"61" then
                    v_state := S1;
                else
                    v_state := S0;
                end if;
            else
                v_state := curState;  
            end if;    
            
        when S3 =>
        v_buffer :=  rxData;
            if rxnow = '1' then
                v_buffer := rxData;
                if rxData >= x"30" and rxData <= x"39" then
                    
                    start <= '1';
                    case rxData is
                        when x"30" => v_bcd3 := x"0";
                        when x"31" => v_bcd3 :=  x"1";
                        when x"32" => v_bcd3 :=  x"2";
                        when x"33" => v_bcd3 :=  x"3";
                        when x"34" => v_bcd3 :=  x"4";
                        when x"35" => v_bcd3 :=  x"5";
                        when x"36" => v_bcd3 :=  x"6";
                        when x"37" => v_bcd3 :=  x"7";
                        when x"38" => v_bcd3 :=  x"8";
                        when x"39" => v_bcd3 :=  x"9";
                        when others => NULL;
                    end case;
                    if txdone = '1' then
                        v_state := S3_5; -- full ANNN command has now been received so next state begins printing process
                    else
                        v_state := curState;
                    end if; 
                    
                elsif rxData = x"41" or rxData = x"61" then
                    v_state := S1;
                else
                    v_state := S0;
                end if;
            else
                v_state := curState;  
            end if;   
        
        when S3_5 =>
            v_buffer := x"20"; -- prints a space character so that there is a gap between the command and the data on the terminal
            start <= '1'; -- informs the data processor that the command processor is ready to receive bytes
            v_state := S4;
        
        when S4 =>
            if seqDone = '1' then -- if sequence is done, nextstate prints the last byte
                v_state := S8;
            elsif dataReady = '1' then -- otherwise if dataready is high then a byte is ready to be read
                v_state := S5;
            else
                v_state := curState;
            end if; 
            
        when S5 =>
            v_buffer :=  ascii_converter(msd); -- first 4 bits of byte are converted to relevant hex character and read to buffer
            if seqDone = '1' then
                v_state := S8;
            elsif txdone = '1' then -- when txdone is high, the ASCII has been read so nextstate is S6
                v_state := S6;
            else
                v_state := S5; -- otherwise remain in current state
            end if;
            
        when S6 =>
            v_buffer :=  ascii_converter(lsd); -- last 4 bits of byte are converted to relevant hex character and read to buffer
            if txdone = '1' then
                v_state := S7;
            else
                v_state := curState;
            end if;
            
        when S7 =>
            v_buffer :=  x"20"; -- buffer is assigned the space character so that there is a space in between every byte
            if txdone = '1' then
                v_state := S3_5;
            else 
                v_state := curState;
            end if;
            
        when S8 => -- this state is only entered when seqdone is high
            v_buffer :=  ascii_converter(msd); -- converts the first 4 bits of the last byte to ASCII
            if txdone = '1' then
                v_state := S9;
            else 
                v_state := curState;
            end if;
            
        when S9 =>
            v_buffer :=  ascii_converter(lsd); -- converts the last 4 bits of the last byte to ASCII
            if txdone = '1' then
                v_state := S0; -- entire sequence has now been printed so back to idle (S0)
            else 
                v_state := curState;
            end if;            
            
        when S10 => -- P command, prints peak byte and its position index
            v_en := '1'; -- count is enabled
            v_state := S10; -- loops state until case statement reassigns nextstate
            
            case count is -- uses count to go through the printing process
            when 0 => 
                v_buffer :=  x"20"; -- begins with a space character 
                v_state := S13;
            when 1 => 
                v_buffer := ascii_converter(peakValue_MSD); -- first 4 bits of peak byte
                v_state := S13;
            when 2 => 
                v_buffer := ascii_converter(peakValue_LSD); -- last 4 bits of peak byte
                v_state := S13;
            when 3 => 
                v_buffer := x"20"; -- space for presentation
                v_state := S13;
            when 4 => 
                v_buffer := ascii_converter(msd_maxIndex); --- first integer of map index
                v_state := S13;
            when 5 => 
                v_buffer := ascii_converter(middle_maxIndex); -- middle integer of map index
                v_state := S13;
            when 6 => 
                v_buffer := ascii_converter(lsd_maxIndex); -- last integer of map index
                v_state := S13;
            when others => v_state := s0;
            end case;
            

            
        when S11 => -- P command, prints the peak byte and the 3 bytes before and after it
        v_en := '1';
        v_state := S11;
        
                   case count is -- uses count to iterate through dataresutls
                when 0 => 
                    v_buffer := x"20"; -- prints space for presentation
                    v_state := S12;                                          
                when 1 => 
                    v_buffer := ascii_converter(reg_dataResults(0)(7 downto 4)); -- initial 4 bits of byte
                    v_state := S12;  
                when 2 =>
                     v_buffer := ascii_converter(reg_dataResults(0)(3 downto 0)); -- last 4 bits of byte
                     v_state := S12;  
                when 3 => 
                    v_buffer := x"20"; -- space 
                    v_state := S12;                                      
                when 4 => 
                    v_buffer := ascii_converter(reg_dataResults(1)(7 downto 4)); -- process repeats for different values of count and thus different bytes in dataresults
                    v_state := S12;  
                when 5 => 
                    v_buffer := ascii_converter(reg_dataResults(1)(3 downto 0));
                    v_state := S12;  
                when 6 => 
                    v_buffer := x"20"; 
                    v_state := S12;                                           
                when 7 => 
                    v_buffer := ascii_converter(reg_dataResults(2)(7 downto 4));
                    v_state := S12;  
                when 8 => 
                    v_buffer := ascii_converter(reg_dataResults(2)(3 downto 0));
                    v_state := S12;     
                when 9 => 
                    v_buffer := x"20";
                    v_state := S12;                                            
                when 10 => 
                    v_buffer := ascii_converter(reg_dataResults(3)(7 downto 4));
                    v_state := S12;    
                when 11 => 
                    v_buffer := ascii_converter(reg_dataResults(3)(3 downto 0));
                    v_state := S12;    
                when 12 => 
                    v_buffer := x"20";  
                    v_state := S12;                                            
                when 13 => 
                    v_buffer := ascii_converter(reg_dataResults(4)(7 downto 4)); 
                    v_state := S12;   
                when 14 => 
                    v_buffer := ascii_converter(reg_dataResults(4)(3 downto 0)); 
                    v_state := S12;   
                when 15 => 
                    v_buffer := x"20";     
                    v_state := S12;                                         
                when 16 => 
                    v_buffer := ascii_converter(reg_dataResults(5)(7 downto 4));  
                    v_state := S12;  
                when 17 => 
                    v_buffer := ascii_converter(reg_dataResults(5)(3 downto 0)); 
                    v_state := S12;   
                when 18 => 
                    v_buffer := x"20";   
                    v_state := S12;                                           
                when 19 => 
                    v_buffer := ascii_converter(reg_dataResults(6)(7 downto 4));  
                    v_state := S12;  
                when 20 => 
                    v_buffer := ascii_converter(reg_dataResults(6)(3 downto 0));   
                    v_state := S12;    
                when others =>
                     v_state := S0;
                     v_res := '0';
            end case;    
                        
        when S12 =>
        v_en := '0';
            if count = 0 then -- if statement ensures buffer1 retains its value
                v_buffer := x"20";
            elsif count = 1 then
                v_buffer := ascii_converter(reg_dataResults(0)(7 downto 4));
            
            elsif count = 2 then
                v_buffer := ascii_converter(reg_dataResults(0)(3 downto 0));
            elsif count = 3 then
                v_buffer := x"20";
            
            elsif count = 4 then
                v_buffer := ascii_converter(reg_dataResults(1)(7 downto 4));
            elsif count = 5 then
                v_buffer := ascii_converter(reg_dataResults(1)(3 downto 0));
            elsif count = 6 then
                v_buffer := x"20"; 
            elsif count = 7 then
                v_buffer := ascii_converter(reg_dataResults(2)(7 downto 4));
            elsif count = 8 then
                v_buffer := ascii_converter(reg_dataResults(2)(3 downto 0));
            elsif count = 9 then
                v_buffer := x"20";
            elsif count = 10 then
                v_buffer := ascii_converter(reg_dataResults(3)(7 downto 4));
            elsif count = 11 then
                v_buffer := ascii_converter(reg_dataResults(3)(3 downto 0));
            elsif count = 12 then
                v_buffer := x"20";
            elsif count = 13 then
                v_buffer := ascii_converter(reg_dataResults(4)(7 downto 4)); 
            elsif count = 14 then
                v_buffer := ascii_converter(reg_dataResults(4)(3 downto 0));
            elsif count = 15 then
                v_buffer := x"20"; 
            elsif count = 16 then
                 v_buffer := ascii_converter(reg_dataResults(5)(7 downto 4));
            elsif count = 17 then
                v_buffer := ascii_converter(reg_dataResults(5)(3 downto 0));
            elsif count = 18 then
                v_buffer := x"20";
            elsif count = 19 then
                v_buffer := ascii_converter(reg_dataResults(6)(7 downto 4)); 
            elsif count = 20 then
                v_buffer := ascii_converter(reg_dataResults(6)(3 downto 0));        
            end if;
            if txdone = '1' then -- this means transmitter has received data and so goes back to S11
                v_state := S11;
            else 
                v_state := curState;
            end if;
            
        when S13 =>    
            
            v_en := '0';
            if count = 0 then -- if statement ensures buffer1 retains its value
                v_buffer :=  x"20";
            elsif count = 1 then
               v_buffer := ascii_converter(peakValue_MSD);
            elsif count = 2 then
                v_buffer := ascii_converter(peakValue_LSD);
            elsif count = 3 then
                v_buffer := x"20";
            elsif count = 4 then
                v_buffer := ascii_converter(msd_maxIndex);
            elsif count = 5 then
                v_buffer := ascii_converter(middle_maxIndex);
            elsif count = 6 then
                v_buffer := ascii_converter(lsd_maxIndex);
            end if;    
            if txdone = '1' then -- this means transmitter has received data and so goes back to S10
                v_state := S10;
                
            else
                v_state := curState;
            end if;
            
        when others =>
            NULL;
            
    end case;
    nextState <= v_state; -- assigns signals to their variable's value. This method ensures now latches
    buffer1 <= v_buffer;
    countEN <= v_en;
    resCount <= v_res;
    bcd1 <= v_bcd1;
    bcd2 <= v_bcd2;
    bcd3 <= v_bcd3;
   
    
    
    
    
end process;

seq_state: process(clk, reset) -- nextstate logic. every clock cycle updates the state.
begin
    if reset = '1' then
        curState <= S0;
    elsif rising_edge(clk) then
        curState <= nextState;
    end if;
end process;

counter: process(reset, clk, countEN, resCount) -- synchronous clock
begin
    if reset = '1' or resCount = '1' then
        count <= 0;  
    elsif rising_edge(clk) then
        if countEN = '1' then
            count <= count + 1;
        end if;
    end if;
end process;

txLogic: process(curState, txdone) -- does not updates txnow until in printing states. currently does not fully work with data echoing.
begin
    txnow <= '0';
    case curState is
        when S0 => NULL;
        when S1 => NULL;
        when S2 => NULL;
        when S3 => NULL;
        when S4 => NULL;
        when others =>
            if txdone = '1' then -- if txdone is high then transmitter is ready to receive a byte so txnow is assigned to high
                txnow <= '1';
            else
                txnow <= '0';
            end if;
    end case;
end process;

dataEchoer: process(clk, reset) -- reads buffer1 into the transmitter on every clock cycle
begin
    if reset = '1' then
        txData <= x"00";
    elsif rising_edge(clk) then
        txData <= buffer1;
    end if;
end process;
    
registers: process(clk, reset, seqDone, reg_maxIndex, reg_dataResults, maxIndex, dataResults) -- following processes are registers that store certain values with a reset function and prevent latches
begin
    if reset = '1' then
        reg_maxIndex <= (x"0", x"0", x"0");
        reg_dataResults <= (x"00", x"00", x"00", x"00", x"00", x"00", x"00");
    elsif rising_edge(clk) and seqDone = '1' then 
        reg_maxIndex <= maxIndex;
        reg_dataResults <= dataResults; 
    end if;
end process; 


bcd_registers: process(clk, reset, bcd1, bcd2, bcd3, reg_bcd1, reg_bcd2, reg_bcd3)
begin
    if reset = '1' then 
        reg_bcd1 <= x"0";
        reg_bcd2<= x"0";
        reg_bcd3 <= x"0";
    elsif rising_edge(clk) THEN
        if curState = S1 THEN
            reg_bcd1 <= bcd1; 
        elsif curState = S2 then
            reg_bcd2<= bcd2;
        elsif curState = S3 then
            reg_bcd3 <= bcd3;
        END IF;        
    end if;
end process;

numWords_register: process(clk, reset, reg_numWords, reg_bcd1, reg_bcd2, reg_bcd3)
begin
    if reset = '1' then 
       reg_numWords <= (x"0", x"0", x"0"); 
    elsif rising_edge(clk) then 
        reg_numWords <= (reg_bcd1, reg_bcd2, reg_bcd3);
        numWords_bcd <= reg_numWords;     
    end if;
end process; 
     
end;
