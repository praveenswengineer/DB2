        IDENTIFICATION DIVISION.                            
        PROGRAM-ID.    STUDRECD.                            
        AUTHOR.        UTTAM.                               
        INSTALLATION.  IBM E&T.                             
        DATE-WRITTEN.  8/17/17.                             
        DATE-COMPILED.                                      
        SECURITY.      NONE.                                
        ENVIRONMENT DIVISION.                               
        INPUT-OUTPUT SECTION.                               
        FILE-CONTROL.                                       
            SELECT STUDMARK ASSIGN TO STUDMARK              
            ORGANIZATION IS SEQUENTIAL                      
            ACCESS IS SEQUENTIAL                            
            FILE STATUS IS WS-STUDMARK-ST.                  
        DATA DIVISION.                                      
        FILE SECTION.                                       
        FD STUDMARK                                  
          RECORDING MODE IS F.                       
        01 STUDMARK-LIST.                            
      05 F-STU-ID               PIC X(6).            
         05                      PIC X(1).           
         05 F-STU-CLASS            PIC X(1).         
         05                      PIC X(1).           
         05 F-STU-NAME             PIC X(20).        
         05                      PIC X(1).           
         05 F-MARKS-LANG           PIC X(2).         
         05                      PIC X(1).           
         05 F-MARKS-MATH           PIC X(2).         
         05                      PIC X(1).           
         05 F-MARKS-SCIENCE        PIC X(2).         
         05                      PIC X(1).           
         05 F-MARKS-SS             PIC X(2).         
         05                      PIC X(39).          
        WORKING-STORAGE SECTION.                                     
                  EXEC SQL                                           
                    INCLUDE SQLCA                                    
                  END-EXEC.                                          
                  EXEC SQL                                           
                    INCLUDE STUDRECD                                 
                  END-EXEC.                                          
        77   WS-STUDMARK-ST      PIC X(2).                           
        01 WS-SQLCODE PIC -9(9).                                     
        01   WS-STUDMARK-EOF PIC X VALUE 'N'.                        
             88 EOF-STUDMARK  VALUE 'Y'.                             
        PROCEDURE DIVISION.                                          
        000-MAIN-PARA.                                               
            PERFORM 100-OPEN-FILE-PARA.                              
              IF  WS-STUDMARK-ST = '00'                              
              PERFORM 200-READ-FILE-PARA UNTIL WS-STUDMARK-EOF = 'Y' 
              ELSE                                                   
               DISPLAY 'ERROR IN OPEN '  WS-STUDMARK-ST        
              END-IF.                                          
            PERFORM 400-CLOSE-FILE-PARA.                       
                         STOP RUN.                             
        100-OPEN-FILE-PARA.                                    
              OPEN INPUT STUDMARK.                             
        200-READ-FILE-PARA.                                    
          READ STUDMARK                                        
                  AT END                                       
                     SET EOF-STUDMARK TO TRUE                  
                  NOT AT END                                   
                     PERFORM 300-DISPLAY-READ                  
          END-READ.                                            
        300-DISPLAY-READ.                                      
              DISPLAY STUDMARK-LIST.                           
              PERFORM 500-INSERT-SQL.                          
        400-CLOSE-FILE-PARA.                                   
              CLOSE STUDMARK.                                      
        500-INSERT-SQL.                                            
                MOVE F-STU-ID TO STU-ID.                           
                MOVE F-STU-CLASS TO STU-CLASS.                     
                MOVE F-STU-NAME TO STU-NAME.                       
                MOVE F-MARKS-LANG TO MARKS-LANG.                   
                MOVE F-MARKS-MATH TO MARKS-MATH.                   
                MOVE F-MARKS-SCIENCE TO MARKS-SCIENCE.             
                MOVE F-MARKS-SS TO MARKS-SS.                       
                MOVE SQLCODE TO WS-SQLCODE.                        
                EXEC SQL                                           
                INSERT INTO STUDRECD                               
                (STU_ID,STU_CLASS,STU_NAME,MARKS_LANG,MARKS_MATH,  
                MARKS_SCIENCE,MARKS_SS)                            
                VALUES(:STU-ID,:STU-CLASS,:STU-NAME,:MARKS-LANG,   
                :MARKS-MATH,:MARKS-SCIENCE,:MARKS-SS)              
                END-EXEC.                                          
                 EVALUATE SQLCODE                   
                    WHEN 100                        
                      DISPLAY "ROWS ENDED"          
                    WHEN 0                          
                      DISPLAY "INSERTED SUCCESS"    
                    WHEN OTHER                      
                      DISPLAY "ERROR" WS-SQLCODE    
                END-EVALUATE.                       
