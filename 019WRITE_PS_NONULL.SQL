000000 IDENTIFICATION DIVISION.                            
       PROGRAM-ID.                                         
              PRWRIT.                                      
       AUTHOR.   UTTAM.                                    
       INSTALLATION.                                       
                 IBM E&T.                                  
       DATE-WRITTEN.                                       
                 12/31/98.                                 
       DATE-COMPILED.                                      
       SECURITY. NONE.                                     
       ENVIRONMENT DIVISION.                               
       INPUT-OUTPUT SECTION.                               
       FILE-CONTROL.                                       
           SELECT OUTFILE ASSIGN TO RELT                   
           ORGANIZATION IS SEQUENTIAL                      
           ACCESS IS SEQUENTIAL                            
          FILE STATUS IS WS-OUTFILE-ST.            
      DATA DIVISION.                               
      FILE SECTION.                                
                                                   
      FD OUTFILE                                   
          RECORDING MODE IS F.                     
      01 OUTFILEREC.                               
       05 OUT_DEPTNO PIC X(3).                     
       05            PIC X(5).                     
       05 OUT_DEPTNAME PIC X(36).                  
       05            PIC X(5).                     
       05 OUT_MGRNO PIC X(6).                      
       05            PIC X(5).                     
       05 OUT_ADMRDEPT PIC X(3).                   
       05            PIC X(5).                     
       05 OUT_LOCATION PIC X(5).                   
       05            PIC X(7).                     
      WORKING-STORAGE SECTION.                        
      77 WS-OUTFILE-ST PIC X(2).                      
      01 WS-SQLCODE PIC -9(9).                        
           EXEC SQL                                   
                INCLUDE SQLCA                         
           END-EXEC.                                  
           EXEC SQL                                   
                INCLUDE DEPTMENT                      
           END-EXEC.                                  
           EXEC SQL                                   
               DECLARE CUR1                           
               CURSOR FOR                             
               SELECT DEPTNO       ,                  
                      DEPTNAME     ,                  
                      MGRNO        ,                  
                      ADMRDEPT                        
               FROM DEPARTMENT                                      
               WHERE DEPTNO <> NULL                                 
          END-EXEC.                                                 
      PROCEDURE DIVISION.                                           
      000-MAIN-PARA.                                                
          PERFORM 100-OPEN-PARA.                                    
          MOVE "     " TO OUT_LOCATION.                             
          PERFORM 100-OPEN-FILE-PARA.                               
          IF SQLCODE = 0 AND WS-OUTFILE-ST = '00' THEN              
           PERFORM 200-SQL-PARA UNTIL SQLCODE = 100 OR SQLCODE < 0  
          ELSE                                                      
           DISPLAY "ERROR IN OPEN" WS-OUTFILE-ST                    
          END-IF.                                                   
          PERFORM 999-CLOSE-PARA.                                   
          PERFORM 300-CLOSE-FILE-PARA                               
            STOP RUN.                                               
      100-OPEN-PARA.                                                
          EXEC SQL                                         
            OPEN CUR1                                      
          END-EXEC.                                        
      200-SQL-PARA.                                        
          EXEC SQL                                         
           FETCH   CUR1                                    
           INTO :DEPTNO       ,                            
                :DEPTNAME     ,                            
                :MGRNO        ,                            
                :ADMRDEPT                                  
          END-EXEC.                                        
          PERFORM 300-EVAL-PARA.                           
      300-EVAL-PARA.                                       
          EVALUATE SQLCODE                                 
              WHEN 0                                       
                 MOVE DEPTNO TO OUT_DEPTNO;                
                 MOVE DEPTNAME TO OUT_DEPTNAME;            
                 MOVE MGRNO TO OUT_MGRNO;              
                 MOVE ADMRDEPT TO OUT_ADMRDEPT;        
     *           MOVE LOCATION TO OUT_LOCATION;        
                 PERFORM 400-WRITE-PARA                
              WHEN 100                                 
                 DISPLAY "NO ROWS FOUND"               
              WHEN OTHER                               
                 DISPLAY "ERROR" SQLCODE               
          END-EVALUATE.                                
      999-CLOSE-PARA.                                  
          EXEC SQL                                     
              CLOSE CUR1                               
          END-EXEC.                                    
      100-OPEN-FILE-PARA.                              
           OPEN OUTPUT OUTFILE.                        
      400-WRITE-PARA.                                  
           WRITE OUTFILEREC.                           
      300-CLOSE-FILE-PARA.      
           CLOSE OUTFILE.       
