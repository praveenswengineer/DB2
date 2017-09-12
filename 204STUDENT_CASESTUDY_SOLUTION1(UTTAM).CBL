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
           SELECT STUDID ASSIGN TO STUDID                   
           ORGANIZATION IS SEQUENTIAL                       
          ACCESS IS SEQUENTIAL                            
          FILE STATUS IS WS-STUDID-ST.                    
          SELECT REPORT1 ASSIGN TO REPORT1                
          ORGANIZATION IS SEQUENTIAL                      
          ACCESS IS SEQUENTIAL                            
          FILE STATUS IS WS-REPORT1-ST.                   
          SELECT REPORT2 ASSIGN TO REPORT2                
          ORGANIZATION IS SEQUENTIAL                      
          ACCESS IS SEQUENTIAL                            
          FILE STATUS IS WS-REPORT2-ST.                   
      DATA DIVISION.                                      
      FILE SECTION.                                       
     * RECORD FORMAT FOR THE ENQUIRY STUDENT ID'S         
      FD STUDID                                           
          RECORDING MODE IS F.                            
      01 STUDID-LIST.                                     
        05 F-SID               PIC X(6).                  
        05                     PIC X(74).           
      FD REPORT2                                    
          RECORDING MODE IS F.                      
      01 REPORT2-LIST.                              
        05 REPORT2-ID          PIC X(80).           
      FD REPORT1                                    
          RECORDING MODE IS F.                      
      01 REPORT1-LIST.                              
        05 REPORT1-ID          PIC X(80).           
      FD STUDMARK                                   
          RECORDING MODE IS F.                      
      01 STUDMARK-LIST.                             
        05 F-STU-ID            PIC X(6).            
        05                     PIC X(1).            
        05 F-STU-CLASS            PIC X(1).         
        05                        PIC X(1).         
        05 F-STU-NAME             PIC X(20).        
        05                        PIC X(1).         
        05 F-MARKS-LANG           PIC X(2).         
        05                        PIC X(1).         
        05 F-MARKS-MATH           PIC X(2).         
        05                        PIC X(1).         
        05 F-MARKS-SCIENCE        PIC X(2).         
        05                        PIC X(1).         
        05 F-MARKS-SS             PIC X(2).         
        05                        PIC X(39).        
      WORKING-STORAGE SECTION.                      
      01 FAILSUB PIC 9(2) VALUE 32.                 
      01 TOTAL PIC 9(3) VALUE ZERO.                 
      01 WS-AST PIC X(80) VALUE ALL '*'.            
          EXEC SQL                                  
            INCLUDE SQLCA                           
          END-EXEC.                                 
          EXEC SQL                                  
            INCLUDE STUDRECD                                       
          END-EXEC.                                                
        77   WS-STUDMARK-ST      PIC X(2).                         
        77   WS-STUDID-ST        PIC X(2).                         
        77   WS-REPORT1-ST       PIC X(2).                         
        77   WS-REPORT2-ST       PIC X(2).                         
      01 WS-SQLCODE            PIC -9(9).                          
      01   WS-STUDMARK-EOF     PIC X     VALUE 'N'.                
             88 EOF-STUDMARK               VALUE 'Y'.              
     * FUNDA 1                                                     
      01   WS-STUDID-EOF       PIC X     VALUE 'N'.                
      PROCEDURE DIVISION.                                          
      000-MAIN-PARA.                                               
          PERFORM 100-OPEN-FILE-PARA.                              
          IF  WS-STUDMARK-ST = '00'                                
           PERFORM 200-READ-FILE-PARA UNTIL WS-STUDMARK-EOF = 'Y'  
          ELSE                                                     
           DISPLAY 'ERROR IN OPEN '  WS-STUDMARK-ST                 
          END-IF.                                                   
     * WRITE REPORT 1                                               
          OPEN OUTPUT REPORT1 OPEN INPUT STUDID.                    
          PERFORM 110-REPORT1-HEADER.                               
          IF WS-STUDID-ST = '00'                                    
           PERFORM 600-READ-FILE-PARA UNTIL WS-STUDID-EOF = 'Y'     
          ELSE                                                      
           DISPLAY 'ERROR IN REPORT 1' WS-STUDID-ST                 
          END-IF.                                                   
          MOVE WS-AST TO REPORT1-ID WRITE REPORT1-LIST.             
          CLOSE REPORT1, STUDID.                                    
     * WRITE REPORT 2                                               
          OPEN OUTPUT REPORT2 OPEN INPUT STUDID.                    
          MOVE 'N' TO WS-STUDID-EOF                                 
          IF WS-STUDID-ST = '00'                                    
           PERFORM 800-READ-FILE-PARA UNTIL WS-STUDID-EOF = 'Y'     
          ELSE                                                  
           DISPLAY 'ERROR IN REPORT 2 ' WS-STUDID-ST            
          END-IF.                                               
          CLOSE REPORT2, STUDID.                                
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
      600-READ-FILE-PARA.                                        
              READ STUDID                                        
                      AT END                                     
                        MOVE 'Y' TO WS-STUDID-EOF                
                      NOT AT END                                 
                         PERFORM 700-DISPLAY-READ-WRITE          
              END-READ.                                          
     * WRITE REPORT 1                                   
      700-DISPLAY-READ-WRITE.                           
              MOVE F-SID TO STU-ID.                     
              DISPLAY "SID: " F-SID.                    
              EXEC SQL                                  
              SELECT STU_ID    ,                        
                     STU_CLASS ,                        
                     STU_NAME  ,                        
                     MARKS_LANG,                        
                     MARKS_MATH,                        
                     MARKS_SCIENCE,                     
                     MARKS_SS                           
              INTO  :STU-ID    ,                        
                    :STU-CLASS ,                        
                    :STU-NAME  ,                        
                    :MARKS-LANG,                        
                    :MARKS-MATH,                        
                    :MARKS-SCIENCE,                         
                    :MARKS-SS                               
              FROM STUDRECD                                 
              WHERE STU_ID = :STU-ID                        
             END-EXEC.                                      
     * CHECK FOR SQL CODE                                   
              MOVE STU-ID TO REPORT1-ID(1:6).               
              MOVE STU-CLASS TO REPORT1-ID(15:1).           
              MOVE STU-NAME  TO REPORT1-ID(25:8).           
              MOVE MARKS-LANG TO REPORT1-ID(35:3).          
              MOVE MARKS-MATH TO REPORT1-ID(44:3).          
              MOVE MARKS-SCIENCE TO REPORT1-ID(53:3).       
              MOVE MARKS-SS TO REPORT1-ID(62:3).            
              COMPUTE TOTAL = MARKS-LANG + MARKS-MATH +     
                      MARKS-SCIENCE + MARKS-SS.             
              MOVE TOTAL TO REPORT1-ID(71:3).               
              IF SQLCODE = 0                                
               WRITE REPORT1-LIST                          
              ELSE                                         
               CONTINUE                                    
              END-IF.                                      
      800-READ-FILE-PARA.                                  
              READ STUDID                                  
                      AT END                               
                        MOVE 'Y' TO WS-STUDID-EOF          
                      NOT AT END                           
                         PERFORM 900-DISPLAY-READ-WRITE    
              END-READ.                                    
      900-DISPLAY-READ-WRITE.                              
              MOVE F-SID TO STU-ID.                        
              EXEC SQL                                     
              SELECT STU_ID    ,                           
                     STU_CLASS ,                           
                     STU_NAME  ,                           
                     MARKS_LANG,                            
                     MARKS_MATH,                            
                     MARKS_SCIENCE,                         
                     MARKS_SS                               
              INTO  :STU-ID    ,                            
                    :STU-CLASS ,                            
                    :STU-NAME  ,                            
                    :MARKS-LANG,                            
                    :MARKS-MATH,                            
                    :MARKS-SCIENCE,                         
                    :MARKS-SS                               
              FROM STUDRECD                                 
              WHERE STU_ID = :STU-ID                        
             END-EXEC.                                      
     * CHECK FOR SQL CODE                                   
              IF SQLCODE = 0                                
              MOVE WS-AST TO REPORT2-ID WRITE REPORT2-LIST  
              MOVE "MASTER/MISS" TO REPORT2-ID(3:11)                   
              MOVE STU-NAME TO REPORT2-ID(16:8) WRITE REPORT2-LIST     
              MOVE "HAVING        AS HIS/HER ID" TO REPORT2-ID(20:30)  
              MOVE STU-ID TO REPORT2-ID(27:6) WRITE REPORT2-LIST       
              MOVE "IN   CLASS                   " TO REPORT2-ID(20:30)
              MOVE STU-CLASS TO REPORT2-ID(23:1) WRITE REPORT2-LIST    
                IF MARKS-MATH >= 55 AND MARKS-LANG >= 55 AND           
                   MARKS-SCIENCE >= 55 AND MARKS-SS >= 55              
                MOVE "PASSED IN ALL SUBJECTS WITH" TO REPORT2-ID(20:30)
                ELSE                                                   
                  MOVE "FAILED IN " TO REPORT2-ID(20:30)               
                   IF MARKS-LANG < 55                                  
                      MOVE "LANG " TO REPORT2-ID(FAILSUB:5)            
                      COMPUTE FAILSUB = FAILSUB + 5                    
                   END-IF                                              
                   IF MARKS-MATH < 55                                  
                      MOVE "MATH " TO REPORT2-ID(FAILSUB:5)            
                      COMPUTE FAILSUB = FAILSUB + 5                  
                   END-IF                                            
                   IF MARKS-SCIENCE < 55                             
                      MOVE "SCIENCE " TO REPORT2-ID(FAILSUB:8)       
                      COMPUTE FAILSUB = FAILSUB + 8                  
                   END-IF                                            
                   IF MARKS-SS < 55                                  
                      MOVE "SS" TO REPORT2-ID(FAILSUB:2)             
                      COMPUTE FAILSUB = FAILSUB + 3                  
                   END-IF                                            
                   MOVE "WITH" TO REPORT2-ID(FAILSUB:4)              
                   MOVE 32 TO FAILSUB                                
                END-IF                                               
                WRITE REPORT2-LIST                                   
              MOVE ", LANG    MARKS =" TO REPORT2-ID(20:30)          
              MOVE MARKS-LANG TO REPORT2-ID(38:3) WRITE REPORT2-LIST 
              MOVE ", MATH    MARKS =" TO REPORT2-ID(20:30)          
              MOVE MARKS-MATH TO REPORT2-ID(38:3) WRITE REPORT2-LIST   
              MOVE ", SCIENCE MARKS =" TO REPORT2-ID(20:30)            
              MOVE MARKS-SCIENCE TO REPORT2-ID(38:3) WRITE REPORT2-LIST
              MOVE ", SS      MARKS =     AND" TO REPORT2-ID(20:30)    
              MOVE MARKS-SS      TO REPORT2-ID(38:3) WRITE REPORT2-LIST
              COMPUTE TOTAL = MARKS-LANG + MARKS-MATH +                
                      MARKS-SCIENCE + MARKS-SS                         
              MOVE "  TOTAL   MARKS =" TO REPORT2-ID(20:30)            
              MOVE TOTAL TO REPORT2-ID(38:3) WRITE REPORT2-LIST        
              ELSE                                                     
               DISPLAY "NOT EXISTING ID"                               
              END-IF.                                                  
      110-REPORT1-HEADER.                                              
              MOVE WS-AST TO REPORT1-ID WRITE REPORT1-LIST.            
              MOVE "REPORT ON STUDENTS RESULTS" TO REPORT1-ID(24:30)   
                   WRITE REPORT1-LIST.                                 
              MOVE "PREPARED BY UTTAM KUMAR P" TO REPORT1-ID(24:30)    
                   WRITE REPORT1-LIST.                              
              MOVE WS-AST TO REPORT1-ID WRITE REPORT1-LIST.         
              MOVE "STUDENT    STUDENT    STUDENT    MARKS    MARKS 
     -    "MARKS    MARKS    TOTAL" TO  REPORT1-ID(1:80)            
              WRITE REPORT1-LIST.                                   
              MOVE "  ID        CLASS      NAME      LANG     MATH  
     -    "SCIENCE    SS     MARKS" TO  REPORT1-ID(1:80)            
              WRITE REPORT1-LIST.                                   
              MOVE WS-AST TO REPORT1-ID WRITE REPORT1-LIST.         
