library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.common_pack.ALL;
use work.dataGen;

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ENTITY dataConsume IS

-- PORT MAP OF THE DATA PROCESSOR
  Port (
    clk: in std_logic;
    reset: in std_logic := '0';
    start: in std_logic := '0';
    numWords_bcd: in BCD_ARRAY_TYPE(2 downto 0);
    ctrlIn: in std_logic := '0'; 
    data: in std_logic_vector(7 downto 0) := (OTHERS => 'X');
    
    ctrlOut: out std_logic := '0';  
    dataReady: out std_logic := '0';
    byte: out std_logic_vector(7 downto 0) := (OTHERS => 'X');
    maxIndex: out BCD_ARRAY_TYPE(2 downto 0);
    dataResults: out CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
    seqDone: out std_logic := '0'
    );
    
END dataConsume;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
architecture Behavioral of dataConsume is
    
    -- CREATE SIGNALS FOR THE NEXTSTATE LOGIC
    TYPE state_type IS (S0, S1, S2);
    SIGNAL curState, nextState: state_type;
    
    -- CREATE SIGNALS FOR THE COUNTERWORDS AND THE COUNTERAFTER
    SIGNAL counterWords: std_logic_vector(11 downto 0) := (OTHERS => '0');
    SIGNAL counterAfter: integer := 0;
    SIGNAL res_counterWords, res_counterAfter: BOOLEAN := FALSE;
    SIGNAL en_counterWords: std_logic := '0';
    SIGNAL en_CounterAfter: BOOLEAN := FALSE;
    SIGNAL DataIsReady: std_logic := '0';
    
    -- CREATE SIGNALS FOR THE REGISTERS

    SIGNAL reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10, reg11: std_logic_vector(7 DOWNTO 0) := (OTHERS => 'X');
    
    -- CREATE SIGNALS FOR THE COMPARATOR
    SIGNAL comp_en: BOOLEAN := FALSE;
    SIGNAL A, B: STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => 'X');
    SIGNAL CompResult: std_logic := '0';
    
    -- CREATE SIGNALS FOR THE BCD TO BINARY CONVERTER
    SIGNAL numWords_bin: std_logic_vector(10 downto 0) := (OTHERS => '0');
    SIGNAL BCD2bin_en: BOOLEAN := FALSE;
    -- CREATE SIGNALS FOR THE CTRL IN AND CTRL OUT REGISTERS TO COMMUNICATE WITH THE DATA GENERATOR
    SIGNAL ctrlOut_en, ctrlIn_en: BOOLEAN := FALSE;  --ctrl_1 = ctrlOut
    SIGNAL ctrlOut_in: std_logic := '0';    --ctrl_2 = ctrlIn
    SIGNAL ctrlOut_reg, ctrlIn_reg: std_logic := '0';
    SIGNAL edge_out, edge_in: std_logic := '0';
    SIGNAL ctrlIsOut: std_logic := '0';
    
    -- CREATE SIGNALS FOR THE BINARY TO BCD CONVERTER
    TYPE stages is (stage1, stage2, stage3);
    SIGNAL stage, stage_next: stages;
    SIGNAL binary, binary_next: std_logic_vector(11 downto 0) := (OTHERS => '0');
    SIGNAL bcds, bcds_reg, bcds_next: std_logic_vector(12 downto 0) := (OTHERS => '0');
    SIGNAL bcds_out_reg, bcds_out_reg_next: std_logic_vector(12 downto 0) := (OTHERS => '0');
    SIGNAL shift_counter, shift_counter_next: natural range 0 to 12;
    
    SIGNAL bin2BCD_en, maxIndexBin_en: BOOLEAN := FALSE;
    SIGNAL maxIndexBin: std_logic_vector(11 downto 0) := (OTHERS => '0');
    
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   
BEGIN
    -- THIS PROCESS CONVERTS A BINARY STRING INTO A 3-BIT BCD ARRAY
     bin2BCD_Converter: process(clk, bcds, binary, shift_counter, bcds_out_reg, stage, reset, bin2BCD_en, maxIndexBin, bcds_reg)
       begin
           
           -- Default assignments
           bcds_next <= bcds;
           binary_next <= binary;
           shift_counter_next <= shift_counter;
           bcds_out_reg_next <= bcds_out_reg;
           stage_next <= stage;
           bcds_reg <= (others => '0');
           
           IF reset = '1' then
               binary <= (others => '0');
               bcds <= (others => '0');
               bcds_out_reg <= (others => '0');
               shift_counter <= 0;
               stage_next <= stage1;
               bcds_reg <= (others => '0');
           
           ELSIF bin2BCD_en = TRUE THEN  
             
               IF clk'EVENT AND clk = '1' THEN
                   binary <= binary_next;
                   bcds <= bcds_next;
                   bcds_out_reg <= bcds_out_reg_next;
                   shift_counter <= shift_counter_next;  
                   stage <= stage_next;
                   bcds_reg <= (others => '0');
               END IF;
        
               case stage is
               
                   when stage1 =>
                   
                       stage_next <= stage2;
                       binary_next <= maxIndexBin;
                       bcds_next <= (others => '0');
                       shift_counter_next <= 0;
                       bcds_reg <= (others => '0');
                       
                   when stage2 =>
                       bcds_reg <= (others => '0');
                       
                       if shift_counter = 12 then
                           stage_next <= stage3; 
                          
                       else
                           binary_next <= binary(10 downto 0) & "0";
                           bcds_next <= bcds_reg(11 downto 0) & binary(11);
                           shift_counter_next <= shift_counter + 1;    
                       end if;
                       
                   when stage3 =>
                       bcds_reg <= (others => '0');
                       stage_next <= stage1;
                       
               end case;
               bcds_reg <= (others => '0');
               
               if bcds(11 downto 8) > 4 then
                   bcds_reg(11 downto 8) <= bcds(11 downto 8) + 3;
               else
                   bcds_reg(11 downto 8) <= bcds(11 downto 8);
               end if;
       
               if bcds(7 downto 4) > 4 then
                   bcds_reg(7 downto 4) <= bcds(7 downto 4) + 3;
               else
                   bcds_reg(7 downto 4) <= bcds(7 downto 4);
               end if;
       
               if bcds(3 downto 0) > 4 then
                   bcds_reg(3 downto 0) <= bcds(3 downto 0) + 3;
               else
                   bcds_reg(3 downto 0) <= bcds(3 downto 0);
               end if;
       
               if stage = stage3 then
                   bcds_out_reg_next <= bcds;
               else
                   bcds_out_reg_next <= bcds_out_reg;
               end if;
                              
           end if;    
       end process;
    
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------    
    -- THIS PROCESS STORES THE VALUES OF THE CURRENT MAX INDEX
    CurMaxIndex: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR counterWords = numWords_bin THEN
                maxIndexBin <= (OTHERS => 'X');       
            ELSIF maxIndexBin_en = TRUE AND counterWords < numWords_bin AND CompResult = '1' THEN
               maxIndexBin <= counterWords;
            END IF;
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   
    -- THIS PROCESS CONVERTS A 3-BIT BCD ARRAY INTO A BINARY STRING
    BCD2bin_Converter: PROCESS(clk, BCD2bin_en)
    BEGIN   
        IF clk'event AND clk='1' AND BCD2bin_en = TRUE THEN
            numWords_bin <= std_logic_vector(resize(unsigned(numWords_bcd(0)) * "0001",11)  --multiply by 1
                                   + resize(unsigned(numWords_bcd(1)) * "01010",11) --multiply by 10
                                   + resize(unsigned(numWords_bcd(2)) * "01100100",11)); --multiply by 100
        END IF;

    END PROCESS;
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    --THIS PROCESS COMPARES BINARY STRING 'A' AGAINST BINARY STRING 'B'
    comparatorData: PROCESS(clk)
    BEGIN       
    IF clk'EVENT AND clk = '1' THEN
        IF Comp_en = true THEN
            CompResult <= '0';
            IF (A(7) = '0' AND B(7) = '0') OR (A(7) = '1' AND B(7) = '1') THEN
                   CompResult <= '0';
                IF (A(6) = '0' AND B(6) = '0') OR (A(6) = '1' AND B(6) = '1') THEN
                       CompResult <= '0';
                    IF (A(5) = '0' AND B(5) = '0') OR (A(5) = '1' AND B(5) = '1') THEN
                           CompResult <= '0';
                        IF (A(4) = '0' AND B(4) = '0') OR (A(4) = '1' AND B(4) = '1') THEN
                               CompResult <= '0';
                            IF (A(3) = '0' AND B(3) = '0') OR (A(3) = '1' AND B(3) = '1') THEN
                                   CompResult <= '0';
                                IF (A(2) = '0' AND B(2) = '0') OR (A(2) = '1' AND B(2) = '1') THEN
                                       CompResult <= '0';
                                      IF (A(1) = '0' AND B(1) = '0') OR (A(1) = '1' AND B(1) = '1') THEN
                                             CompResult <= '0';                    
                                          IF (A(0) = '0' AND B(0) = '0') OR (A(0) = '1' AND B(0) = '1') THEN
                                                 CompResult <= '0';                                                               
                                          ELSIF A(0) = '0' AND B(0) = '1' THEN
                                                 CompResult <= '1';
                                          ELSIF A(0) = '1' AND B(0) = '0' THEN
                                                 CompResult <= '0';
                                          END IF;    
                                                                                                            
                                      ELSIF A(1) = '0' AND B(1) = '1' THEN
                                               CompResult <= '1';
                                      ELSIF A(1) = '1' AND B(1) = '0' THEN
                                               CompResult <= '0';
                                      END IF;    
                                                                   
                                ELSIF A(2) = '0' AND B(2) = '1' THEN
                                         CompResult <= '1';
                                ELSIF A(2) = '1' AND B(2) = '0' THEN
                                         CompResult <= '0';
                                END IF;     
                            
                            ELSIF A(3) = '0' AND B(3) = '1' THEN
                                     CompResult <= '1';
                            ELSIF A(3) = '1' AND B(3) = '0' THEN
                                     CompResult <= '0';
                            END IF;            
                                                
                        ELSIF A(4) = '0' AND B(4) = '1' THEN
                              CompResult <= '1';
                        ELSIF A(4) = '1' AND B(4) = '0' THEN
                              CompResult <= '0';
                        END IF;        
                
                    ELSIF A(5) = '0' AND B(5) = '1' THEN
                          CompResult <= '1';
                    ELSIF A(5) = '1' AND B(5) = '0' THEN
                          CompResult <= '0';
                    END IF;        
                            
                ELSIF A(6) = '0' AND B(6) = '1' THEN
                      CompResult <= '1';
                ELSIF A(6) = '1' AND B(6) = '0' THEN
                      CompResult <= '0';
                END IF;        
            
            ELSIF A(7) = '0' AND B(7) = '1' THEN
                CompResult <= '0';
            ELSIF A(7) = '1' AND B(7) = '0' THEN
                CompResult <= '1';
            END IF;        

        END IF;
    END IF;    
    END PROCESS; 
      
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
   
------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS MOVES REG3 INTO REG2 REGISTER    
    temp1: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR curState = S0 THEN
                reg2 <= (OTHERS => 'X');       
            ELSIF curState = S1 AND counterWords < numWords_bin AND edge_in = '1' THEN
               reg2 <= reg3;
            END IF;
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS MOVES REG4 INTO REG3 REGISTER    
    temp2: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR curState = S0 THEN
                reg3 <= (OTHERS => 'X');
            ELSIF curState = S1 AND counterWords < numWords_bin AND edge_in = '1' THEN
                reg3 <= reg4;
            END IF;
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS MOVES data INTO REG4 REGISTER    
    temp3: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR (curState = S0 AND start = '1' AND edge_in = '0') THEN
                reg4 <= (OTHERS => 'X');                  
            ELSIF (CurState = S0 AND edge_in = '1') OR (curState = S1 AND counterWords < numWords_bin AND edge_in = '1') THEN  
                reg4 <= data;
            END IF;
        END IF;
    END PROCESS;
    
    temp4: PROCESS(clk)
        BEGIN
            IF clk'event AND clk='1' THEN
                IF reset = '1' OR (curState = S0 AND start = '1' AND edge_in = '0') THEN
                    reg11 <= (OTHERS => 'X');                  
                ELSIF (CurState = S0 AND edge_in = '1') OR (curState = S1 AND counterWords < numWords_bin AND edge_in = '1') THEN  
                    reg11 <= reg2;
                END IF;
            END IF;
        END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS MOVES REG2 INTO THE REG5 REGISTER  
    PeakAddOne: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN    
            IF reset = '1' OR curState = S0 THEN
                reg5 <= (OTHERS => 'X'); 
            ELSIF curState = S2 AND counterAfter = 3 THEN
                reg5 <= reg2;
            END IF;
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS MOVES REG3 INTO REG6 REGISTER
    PeakAddTwo: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR curState = S0 THEN
                reg6 <= (OTHERS => 'X');
            ELSIF curState = S2 AND counterAfter = 3 THEN
                reg6 <= reg3;
            END IF;
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS MOVES REG4 INTO REG7 REGISTER
    PeakAddThree: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR curState = S0 THEN
                reg7 <= (OTHERS => 'X');
            ELSIF curState = S2 AND counterAfter = 3 THEN
                reg7 <= reg4;
            END IF;
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS MOVES REG4 INTO REG8 REGISTER
    PeakMinusOne: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR curState = S0 THEN
                reg8 <= (OTHERS => 'X');       
            ELSIF curState = S1 AND counterWords < numWords_bin AND CompResult = '1' THEN
                reg8 <= reg3;
            END IF;
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS MOVES REG3 INTO REG9 REGISTER
    PeakMinusTwo: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR curState = S0 THEN
                reg9 <= (OTHERS => 'X');       
            ELSIF curState = S1 AND counterWords < numWords_bin AND CompResult = '1' THEN
               reg9 <= reg2;
            END IF;
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS MOVES REG2 INTO REG10 REGISTER
    PeakMinusThree: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR curState = S0 THEN
                reg10 <= (OTHERS => 'X');       
            ELSIF curState = S1 AND counterWords < numWords_bin AND CompResult = '1' THEN
                reg10 <= reg11;
            END IF;
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- THIS PROCESS MOVES DATA INTO BYTE OUTPUT
    byteReg: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR (CurState = S0 AND start = '0') OR (CurState = S0 AND edge_in = '0') THEN
                byte <= (OTHERS => 'X');
            ELSIF (CurState = S0 AND edge_in = '1') OR curState = S1 THEN
                byte <= data;                
            END IF;
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------    
    -- THIS PROCESS MOVES DATA INTO REG1 REGISTER
    peak: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR (curState = S0 AND edge_in = '0') THEN
                reg1 <= (OTHERS => 'X');                       
            ELSIF (CurState = S0 AND edge_in = '1') OR (curState = S1 AND counterWords < numWords_bin AND CompResult = '1') THEN
                reg1 <= data;                
            END IF;               
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- THIS PROCESS MOVES REG1 INTO A TO BE COMPARED
    Areg: PROCESS(clk) --reset, curState, edge_in, counterWords, numWords_bin, reg1)
    BEGIN
        IF clk'EVENT AND clk = '1' THEN
                IF reset = '1' OR (curState = S0 AND edge_in = '0') THEN
                    A <= (OTHERS => 'X');
                ELSIF (curState = S1 OR curState = S2) AND counterWords < numWords_bin THEN
                    A <= reg1;           
                END IF;
        END IF;            
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- THIS PROCESS MOVES DATA INTO B TO BE COMPARED
    Breg: PROCESS(clk) --reset, curState, edge_in, counterWords, numWords_bin, data)
    BEGIN
        IF clk'EVENT AND clk = '1' THEN
                IF reset = '1' OR (curState = S0 AND edge_in = '0') THEN
                    B <= (OTHERS => 'X');
                ELSIF (curState = S1 OR curState = S2) AND counterWords < numWords_bin  THEN
                    B <= data;
                END IF;
        END IF;
    END PROCESS;      
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS MOVES NEXTSTATE INTO THE CURSTATE REGISTER ON THE RISING EDGE OF THE CLOCK
    StateReg: PROCESS(clk)
    BEGIN
        IF clk'event AND clk = '1' THEN
            IF reset = '1' THEN
                curstate <= S0;
            ELSE
                curState <= NextState;
            END IF;
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS DESCRIBES A COUNTER WHICH IS INCREMENTED EACH TIME A BYTE OF DATA IS PROCESSED 
    counterWords_proc: PROCESS(clk)
    BEGIN
        IF clk'EVENT AND clk = '1' THEN
            IF res_counterWords = TRUE OR reset = '1' THEN
                counterWords <= (OTHERS => '0');
            ELSIF dataIsReady = '1' THEN
                counterWords <= std_logic_vector(unsigned(counterWords) + 1);
            END IF;
        END IF;  
    END PROCESS;
   
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS DESCRIBES A COUNTER WHICH IS RESET WHEN A PEAK IS DETECTED AND INCREMENTED THEREAFTER TO COUNT THE 3 BYTES AFTER THE PEAK
    counterAfter_proc: PROCESS(clk)
    BEGIN
        IF clk'EVENT AND clk = '1' THEN
            IF res_counterAfter = TRUE OR reset = '1' THEN
                counterAfter <= 0;
            ELSIF dataisReady = '1' AND en_counterAfter = TRUE THEN
                counterAfter <= counterAfter + 1;                       
            END IF;
        END IF;
    END PROCESS;
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS DESCRIBES WHAT HAPPENS WHEN THE DATA PROCESSOR REQUESTS DATA FROM THE DATA GENERATOR
    
    -- This register takes the new ctrlOut input and compares it against the previous ctrlOut value. If they are different then
    -- edge_out goes high signifying a change in the ctrl1 signal. This then allows the new ctrlOut input to be stored in the register
    -- and also updates the value of ctrlOut and also enables the ctrlIn register. 
    
    ctrlOut_proc: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR (counterWords = numWords_bin AND ctrlOut_en = FALSE) THEN
                edge_out <= '0';
            ELSIF ctrlOut_en = TRUE THEN
                edge_out <= ctrlOut_reg XOR ctrlOut_in;
                IF edge_out = '1' THEN
                    ctrlOut <= ctrlOut_in;                  
                END IF;
                
                IF curState = S0 AND edge_in = '0' AND edge_out = '1' AND start = '0' THEN   
                    ctrlIsOut <= '1';
                ELSIF curState = S0 AND ctrlIn = '1' THEN
                    ctrlIsOut <= '0';
                END IF;                                 
            END IF;
        END IF;
    END PROCESS;   
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS DESCRIBES WHAT HAPPENS WHEN THE DATA GENERATOR SENDS DATA TO THE DATA PROCESSOR
    
    -- This register compares the stored value from ctrlIn_reg to the new value sent by the data generator. If these are different then
    -- edge_in is set to high which then allows ctrlIn to store its value in the register.
    
    ctrlIn_proc: PROCESS(clk)
    BEGIN
        IF clk'event AND clk='1' THEN
            IF reset = '1' OR (counterWords = numWords_bin AND ctrlIn_en = FALSE) THEN
                edge_in <= '0';
            ELSIF ctrlIn_en = TRUE THEN
                edge_in <= ctrlIn_reg XOR ctrlIn;
                IF edge_in = '1' THEN
                    ctrlIn_reg <= ctrlIn;
                END IF;
            END IF;
        END IF;
    END PROCESS;   
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       
    -- THIS PROCESS DESCRIBES THE STATES OF THE SEQUENTIAL LOGIC WHICH ENABLES VARIOUS PROCESSES
    NextStateLogic: PROCESS(curState, counterWords, numWords_bin, start, edge_out, ctrlOut_reg, edge_in, reg10, reg9, reg8, reg1, reg5, reg6, reg7, bcds_out_reg, ctrlIn, CompResult, counterAfter, ctrlIsOut)
        BEGIN
            CASE curState IS                                          
                -- This is the initialisation state. IF start = '1' then the process to take the first byte of data starts. 
                -- Once CtrlIn = '1', edge_in = '1' and the state moves to S1. Once the number of bytes we are required to 
                -- process has been reached, the current state returns to here, ready for a new number of bytes to process. 
                WHEN S0 =>
                    seqDone <= '0';
                    BCD2bin_en <= FALSE;
                    ctrlOut_en <= TRUE;
                    ctrlIn_en <= TRUE;
                    ctrlOut_in <= '0';
                    ctrlOut_reg <= '0';
                    dataReady <= '0';
                    DataIsReady <= '0';                   
                    en_counterWords <= '0';                   
                    dataResults(0) <= (OTHERS => 'X');
                    dataResults(1) <= (OTHERS => 'X');
                    dataResults(2) <= (OTHERS => 'X');
                    dataResults(3) <= (OTHERS => 'X');
                    dataResults(4) <= (OTHERS => 'X');
                    dataResults(5) <= (OTHERS => 'X');
                    dataResults(6) <= (OTHERS => 'X');
                    maxIndex(2) <= (OTHERS => 'X');
                    maxIndex(1) <= (OTHERS => 'X');
                    maxIndex(0) <= (OTHERS => 'X');                    

                    en_counterAfter <= FALSE;
                    Comp_en <= FALSE;
                    bin2BCD_en <= FALSE;
                    res_counterAfter <= FALSE;
                    res_counterWords <= FALSE; 
                    maxIndexBin_en <= FALSE;                   
                                                                                                  
                    NextState <= S0;
                    
                    IF counterWords = numWords_bin THEN
                        ctrlOut_en <= FALSE;
                        ctrlIn_en <= FALSE;
                    END IF;                      
                                       
                    IF start = '1' THEN
                        BCD2bin_en <= TRUE;
                        ctrlOut_en <= TRUE;
                        ctrlIn_en <= TRUE;                      
                        ctrlOut_in <= '1';
                        ctrlOut_reg <= '0';                        
                        seqDone <= '0';                      
                        dataReady <= '0';
                        DataIsReady <= '0';                       
                        en_counterWords <= '0';
                        
                        dataResults(0) <= (OTHERS => 'X');
                        dataResults(1) <= (OTHERS => 'X');
                        dataResults(2) <= (OTHERS => 'X');
                        dataResults(3) <= (OTHERS => 'X');
                        dataResults(4) <= (OTHERS => 'X');
                        dataResults(5) <= (OTHERS => 'X');
                        dataResults(6) <= (OTHERS => 'X');
                        maxIndex(2) <= (OTHERS => 'X');
                        maxIndex(1) <= (OTHERS => 'X');
                        maxIndex(0) <= (OTHERS => 'X');                         
                        bin2BCD_en <= FALSE;
                        en_counterAfter <= FALSE;
                        Comp_en <= FALSE;
                        bin2BCD_en <= TRUE;
                        res_counterAfter <= FALSE;
                        res_counterWords <= TRUE;
                        maxIndexBin_en <= FALSE;
                                                                   
                        NextState <= S0;
                        
                    END IF;
                    
                    IF edge_out = '1' THEN
                    
                        BCD2bin_en <= TRUE;
                        ctrlOut_en <= TRUE;
                        ctrlIn_en <= TRUE;                      
                        ctrlOut_in <= '1';
                        ctrlOut_reg <= '1';
                                                
                        seqDone <= '0';                      
                        dataReady <= '0';
                        DataIsReady <= '0';                       
                        en_counterWords <= '0';
                        
                        dataResults(0) <= (OTHERS => 'X');
                        dataResults(1) <= (OTHERS => 'X');
                        dataResults(2) <= (OTHERS => 'X');
                        dataResults(3) <= (OTHERS => 'X');
                        dataResults(4) <= (OTHERS => 'X');
                        dataResults(5) <= (OTHERS => 'X');
                        dataResults(6) <= (OTHERS => 'X');
                        maxIndex(2) <= (OTHERS => 'X');
                        maxIndex(1) <= (OTHERS => 'X');
                        maxIndex(0) <= (OTHERS => 'X');                         
                        bin2BCD_en <= FALSE;
                        en_counterAfter <= FALSE;
                        Comp_en <= FALSE;
                        bin2BCD_en <= TRUE;
                        res_counterAfter <= FALSE;
                        res_counterWords <= TRUE;
                        maxIndexBin_en <= FALSE;
                                                                   
                        NextState <= S0;                        
                    
                    END IF;
                    
                    IF ctrlIsOut = '1' THEN
                        ctrlOut_en <= TRUE;
                        ctrlIn_en <= TRUE; 
                        ctrlOut_reg <= '1';
                        ctrlOut_in <= '1';   
                    END IF;
                                                                                         
                    IF edge_in = '1' THEN                   
                                                  
                        dataReady <= '0';
                        DataIsReady <= '0';                       
                        en_counterWords <= '1';        
                        en_counterAfter <= TRUE;
                            
                        seqDone <= '0';
                        BCD2bin_en <= TRUE;
                        ctrlOut_en <= TRUE;
                        ctrlIn_en <= TRUE;  
                        ctrlOut_in <= '0';
                        ctrlOut_reg <= '0';
                        dataResults(0) <= (OTHERS => 'X');
                        dataResults(1) <= (OTHERS => 'X');
                        dataResults(2) <= (OTHERS => 'X');
                        dataResults(3) <= (OTHERS => 'X');
                        dataResults(4) <= (OTHERS => 'X');
                        dataResults(5) <= (OTHERS => 'X');
                        dataResults(6) <= (OTHERS => 'X');
                        maxIndex(2) <= (OTHERS => 'X');
                        maxIndex(1) <= (OTHERS => 'X');
                        maxIndex(0) <= (OTHERS => 'X');                             
                        Comp_en <= FALSE;
                        bin2BCD_en <= TRUE;
                        res_counterAfter <= FALSE;
                        maxIndexBin_en <= FALSE;                                                                                                                                               
                        res_counterWords <= FALSE;

                        NextState <= S1;
                            
                    END IF;
                                                                                                                               
                WHEN S1 =>
                -- In this state, the main data processing takes place. The process to take bytes of data continues and the new byte
                -- of data is compared with the current peak. If there is a new peak (compResult = '1') we stay in S1 for longer and 
                -- compare with data, but if not then we move to S2. If the number of bytes of data we want to process has been reached,
                -- seqDone = '1', the results are sent to the command processor and the state moves back to S0. 
                    en_counterAfter <= True;
                    seqDone <= '0';
                    BCD2bin_en <= TRUE;
                    ctrlOut_en <= TRUE;
                    ctrlIn_en <= TRUE;   
                    ctrlOut_in <= '0'; 
                    ctrlOut_reg <= '0';                   
                    dataReady <= '0';
                    DataIsReady <= '0';                                        
                    en_counterWords <= '1';                   
                    dataResults(0) <= (OTHERS => 'X');
                    dataResults(1) <= (OTHERS => 'X');
                    dataResults(2) <= (OTHERS => 'X');
                    dataResults(3) <= (OTHERS => 'X');
                    dataResults(4) <= (OTHERS => 'X');
                    dataResults(5) <= (OTHERS => 'X');
                    dataResults(6) <= (OTHERS => 'X');
                    maxIndex(2) <= (OTHERS => 'X');
                    maxIndex(1) <= (OTHERS => 'X');
                    maxIndex(0) <= (OTHERS => 'X');                     
                    Comp_en <= TRUE;
                    bin2BCD_en <= TRUE;
                    res_counterAfter <= FALSE;
                    res_counterWords <= FAlSE;
                    maxIndexBin_en <= FALSE;
                                                         
                    NextState <= S1;
                    
                    IF counterWords = numWords_bin THEN
                        dataResults(0) <= std_logic_vector(reg10);
                        dataResults(1) <= std_logic_vector(reg9);
                        dataResults(2) <= std_logic_vector(reg8);
                        dataResults(3) <= std_logic_vector(reg1);
                        dataResults(4) <= std_logic_vector(reg5);
                        dataResults(5) <= std_logic_vector(reg6);
                        dataResults(6) <= std_logic_vector(reg7);
                        
                        maxIndex(2) <= bcds_out_reg(11 downto 8);
                        maxIndex(1) <= bcds_out_reg(7 downto 4);
                        maxIndex(0) <= bcds_out_reg(3 downto 0);
                                                                                  
                        seqDone <= '1';                                                
                                                           
                        BCD2bin_en <= TRUE;
                        ctrlOut_en <= FALSE;
                        ctrlIn_en <= FALSE; 
                        ctrlOut_in <= '0';
                        ctrlOut_reg <= '0';
                        dataReady <= '0';
                        DataIsReady <= '0';
                        en_counterWords <= '1';
                        en_counterAfter <= TRUE;
                        Comp_en <= FALSE;
                        bin2BCD_en <= TRUE;
                        maxIndexBin_en <= FALSE;

                        res_counterAfter <= TRUE;
                        res_counterWords <= FALSE;                                                          
                                                
                        NextState <= S0;
                                                                                                       
                    END IF;                     
                                                                                                      
                    IF counterWords < numWords_bin  THEN 
                    
                        IF start = '1' THEN
                            ctrlOut_en <= TRUE;
                            ctrlIn_en <= TRUE;  
                            ctrlOut_in <= '1';
                            ctrlOut_reg <= '0';
                            dataReady <= '0';
                            DataIsReady <= '0';
                            en_counterWords <= '0';
                                
                            seqDone <= '0';
                            BCD2bin_en <= TRUE;
                                                          
                            dataResults(0) <= (OTHERS => 'X');
                            dataResults(1) <= (OTHERS => 'X');
                            dataResults(2) <= (OTHERS => 'X');
                            dataResults(3) <= (OTHERS => 'X');
                            dataResults(4) <= (OTHERS => 'X');
                            dataResults(5) <= (OTHERS => 'X');
                            dataResults(6) <= (OTHERS => 'X');
                            maxIndex(2) <= (OTHERS => 'X');
                            maxIndex(1) <= (OTHERS => 'X');
                            maxIndex(0) <= (OTHERS => 'X');                             
                            en_counterAfter <= TRUE;
                            Comp_en <= TRUE;
                            bin2BCD_en <= TRUE;
                            res_counterAfter <= FALSE; 
                            res_counterWords <= FAlSE;
                            maxIndexBin_en <= FALSE;
                                                            
                            NextState <= S1;  
                        END IF;
                                                        
                        IF edge_out = '1' THEN
                            BCD2bin_en <= TRUE;
                            ctrlOut_en <= TRUE;
                            ctrlIn_en <= TRUE;
                            
                            IF ctrlIn = '1' THEN                      
                                ctrlOut_in <= '0';
                                ctrlOut_reg <= '0';                                                                                                            
                            END IF;
                            
                            IF ctrlIn = '0' THEN
                                ctrlOut_in <= '1';
                                ctrlOut_reg <= '1';
                            END IF;
                                                                                                                                                          
                            seqDone <= '0';                      
                            dataReady <= '0';
                            DataIsReady <= '0';                       
                            en_counterWords <= '0';
                                                
                            dataResults(0) <= (OTHERS => 'X');
                            dataResults(1) <= (OTHERS => 'X');
                            dataResults(2) <= (OTHERS => 'X');
                            dataResults(3) <= (OTHERS => 'X');
                            dataResults(4) <= (OTHERS => 'X');
                            dataResults(5) <= (OTHERS => 'X');
                            dataResults(6) <= (OTHERS => 'X');
                            maxIndex(2) <= (OTHERS => 'X');
                            maxIndex(1) <= (OTHERS => 'X');
                            maxIndex(0) <= (OTHERS => 'X');                         
                            bin2BCD_en <= FALSE;
                            en_counterAfter <= FALSE;
                            Comp_en <= TRUE;
                            bin2BCD_en <= TRUE;
                            res_counterAfter <= FALSE;
                            res_counterWords <= FALSE;
                            maxIndexBin_en <= FALSE;
                            
                            NextState <= S1;
                        END IF;
                                                                                                            
                        IF edge_in = '1' THEN
                                                         
                            Comp_en <= TRUE;
                            en_counterAfter <= True;
                            en_counterWords <= '1';                                                                                                           
                            dataReady <= '0';
                            DataIsReady <= '1';                                                                                            
                            seqDone <= '0';
                            BCD2bin_en <= TRUE;
                            ctrlOut_en <= TRUE;
                            ctrlIn_en <= TRUE;   
                            ctrlOut_in <= '0';
                            ctrlOut_reg <= '0';
                                                        
                            dataResults(0) <= (OTHERS => 'X');
                            dataResults(1) <= (OTHERS => 'X');
                            dataResults(2) <= (OTHERS => 'X');
                            dataResults(3) <= (OTHERS => 'X');
                            dataResults(4) <= (OTHERS => 'X');
                            dataResults(5) <= (OTHERS => 'X');
                            dataResults(6) <= (OTHERS => 'X');
                            maxIndex(2) <= (OTHERS => 'X');
                            maxIndex(1) <= (OTHERS => 'X');
                            maxIndex(0) <= (OTHERS => 'X');                                 
                            res_counterAfter <= FALSE;
                            res_counterWords <= FAlSE;   
                            bin2BCD_en <= TRUE;
                            maxIndexBin_en <= FALSE;                           
                                                                    
                            NextState <= S1; 
                                                      
                        END IF; 
                                                       
                        IF compResult = '1' THEN
                                      
                            res_counterAfter <= TRUE;                         
                                                                            
                            seqDone <= '0';
                            ctrlOut_en <= TRUE;
                            ctrlIn_en <= TRUE;  
                            ctrlOut_in <= '0';
                            ctrlOut_reg <= '0';
                            dataReady <= '0';
                            DataIsReady <= '0';
                                        
                            en_counterWords <= '1';
                            dataResults(0) <= (OTHERS => 'X');
                            dataResults(1) <= (OTHERS => 'X');
                            dataResults(2) <= (OTHERS => 'X');
                            dataResults(3) <= (OTHERS => 'X');
                            dataResults(4) <= (OTHERS => 'X');
                            dataResults(5) <= (OTHERS => 'X');
                            dataResults(6) <= (OTHERS => 'X');
                            maxIndex(2) <= (OTHERS => 'X');
                            maxIndex(1) <= (OTHERS => 'X');
                            maxIndex(0) <= (OTHERS => 'X');                                     
                            en_counterAfter <= TRUE;
                            Comp_en <= TRUE;
                            bin2BCD_en <= TRUE;
                            res_counterWords <= FAlSE;
                            maxIndexBin_en <= TRUE;
                                           
                            NextState <= S1;
                                        
                       ELSIF edge_in = '1' THEN
                                                                                                                                                                                             
                            NextState <= S2;
            
                       END IF;                                                                                                                                                                                    
                                                                                                                                                                                                                                 
                    ELSE                       
                        NextState <= S0;                        
                    END IF;       
                            
                WHEN S2 =>
                -- In this state, dataReady = '1' to tell the command processor a new byte of data is ready to be taken.
                -- It also checks if there has been 3 bytes of data after a peak, which will stored in registers. It always
                -- returns to S1 after so it can process another byte of data. 
                    maxIndexBin_en <= FALSE;
                    
                    dataReady <= '1';
                    DataIsReady <= '0';
                    en_counterWords <= '0';
                    ctrlOut_en <= TRUE;
                    ctrlIn_en <= TRUE;   
                    
                    seqDone <= '0';
                    BCD2bin_en <= TRUE;
                    ctrlOut_in <= '0';
                    ctrlOut_reg <= '0';
                                     
                    dataResults(0) <= (OTHERS => 'X');
                    dataResults(1) <= (OTHERS => 'X');
                    dataResults(2) <= (OTHERS => 'X');
                    dataResults(3) <= (OTHERS => 'X');
                    dataResults(4) <= (OTHERS => 'X');
                    dataResults(5) <= (OTHERS => 'X');
                    dataResults(6) <= (OTHERS => 'X');
                    maxIndex(2) <= (OTHERS => 'X');
                    maxIndex(1) <= (OTHERS => 'X');
                    maxIndex(0) <= (OTHERS => 'X');                     
                    en_counterAfter <= TRUE;
                    Comp_en <= TRUE;
                    bin2BCD_en <= TRUE;
                    res_counterAfter <= FALSE;
                    res_counterWords <= FAlSE;                    
                                                          
                    NextState <= S2;    
                    
                        IF counterAfter = 3 THEN
                           
                            seqDone <= '0';
                            BCD2bin_en <= TRUE;
                            ctrlOut_en <= TRUE;
                            ctrlIn_en <= TRUE; 
                            ctrlOut_in <= '0';
                            ctrlOut_reg <= '0';

                            dataReady <= '1';
                            DataIsReady <= '0';
                            
                            en_counterWords <= '0';                            
                            dataResults(0) <= (OTHERS => 'X');
                            dataResults(1) <= (OTHERS => 'X');
                            dataResults(2) <= (OTHERS => 'X');
                            dataResults(3) <= (OTHERS => 'X');
                            dataResults(4) <= (OTHERS => 'X');
                            dataResults(5) <= (OTHERS => 'X');
                            dataResults(6) <= (OTHERS => 'X');
                            maxIndex(2) <= (OTHERS => 'X');
                            maxIndex(1) <= (OTHERS => 'X');
                            maxIndex(0) <= (OTHERS => 'X');                             
                            en_counterAfter <= TRUE;
                            Comp_en <= TRUE;
                            bin2BCD_en <= TRUE;
                            res_counterAfter <= FALSE;
                            res_counterWords <= FAlSE;
                            maxIndexBin_en <= FALSE;
                                      
                            NextState <= S1;
                        ELSE 
                            NextState <= S1;
                       
                        END IF;                                           
                
                WHEN OTHERS =>
                -- This will revert to default values if there is any errors.
                    seqDone <= '0';
                    BCD2bin_en <= FALSE;
                    ctrlOut_en <= FALSE;
                    ctrlIn_en <= FALSE; 
                    ctrlOut_in <= '0';
                    ctrlOut_reg <= '0';
                    dataReady <= '0';
                    DataIsReady <= '0';                   
                    en_counterWords <= '0';
                    dataResults(0) <= (OTHERS => 'X');
                    dataResults(1) <= (OTHERS => 'X');
                    dataResults(2) <= (OTHERS => 'X');
                    dataResults(3) <= (OTHERS => 'X');
                    dataResults(4) <= (OTHERS => 'X');
                    dataResults(5) <= (OTHERS => 'X');
                    dataResults(6) <= (OTHERS => 'X');
                    maxIndex(2) <= (OTHERS => 'X');
                    maxIndex(1) <= (OTHERS => 'X');
                    maxIndex(0) <= (OTHERS => 'X');                     
                    en_counterAfter <= FALSE;
                    Comp_en <= FALSE;
                    bin2BCD_en <= FALSE;
                    res_counterAfter <= FALSE;
                    res_counterWords <= TRUE;
                    maxIndexBin_en <= FALSE;
                    NextState <= S0;    
                                                    
            END CASE;
       END PROCESS;   
    
end Behavioral;
