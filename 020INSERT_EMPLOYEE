       IDENTIFICATION DIVISION.                
       PROGRAM-ID.     PROBLEM4.               
       AUTHOR.         LEELA.                  
       INSTALLATION.   IBM E&T.                
       DATE-WRITTEN.   17/08/17.               
       DATE-COMPILED.                          
       SECURITY.       NONE.                   
       ENVIRONMENT DIVISION.                   
       DATA DIVISION.                          
       WORKING-STORAGE SECTION.                
       01 WS-SQLCODE PIC -9(9).                
           EXEC SQL                            
             INCLUDE SQLCA                     
           END-EXEC.                           
           EXEC SQL                            
             INCLUDE EMPLOYEE                  
           END-EXEC.                           
      PROCEDURE DIVISION.                                             
      000-MAIN-PARA.                                                  
          PERFORM 111-SQL-PARA.                                       
          PERFORM 112-EVALUATE-PARA.                                  
          STOP RUN.                                                   
      111-SQL-PARA.                                                   
          MOVE '000069' TO  EMPNO.                                    
          MOVE  20 TO FIRSTNAME-LEN.                                  
          MOVE 'DELEROES' TO  FIRSTNAME-TEXT.                         
          MOVE 'S' TO  MIDINIT.                                       
          MOVE  15 TO LASTNAME-LEN.                                   
          MOVE 'QUITANA' TO  LASTNAME-TEXT.                           
          MOVE 'A00'    TO  WORKDEPT.                                 
          EXEC SQL                                                    
            INSERT INTO EMPLOYEEL                                     
            (EMPNO,FIRSTNAME,MIDINIT,LASTNAME,WORKDEPT)               
             VALUES( :EMPNO,:FIRSTNAME,:MIDINIT,:LASTNAME,:WORKDEPT ) 
          END-EXEC.                                                   
      112-EVALUATE-PARA.                                     
          MOVE SQLCODE TO WS-SQLCODE.                        
          EVALUATE SQLCODE                                   
            WHEN 0                                           
              DISPLAY 'INSERTED SUCESSFULLY' WS-SQLCODE      
            WHEN 100                                         
              DISPLAY 'NOT INSERTED' WS-SQLCODE              
            WHEN OTHER                                       
              DISPLAY 'ERROR' WS-SQLCODE                     
          END-EVALUATE.                                      
