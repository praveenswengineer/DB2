000000 IDENTIFICATION DIVISION.                              
       PROGRAM-ID.                                           
              SQLCBL01.                                      
       AUTHOR.   UTTAM.                                      
       INSTALLATION.                                         
                 IBM E&T.                                    
       DATE-WRITTEN.                                         
                 12/31/98.                                   
       DATE-COMPILED.                                        
       SECURITY. NONE.                                       
       ENVIRONMENT DIVISION.                                 
       DATA DIVISION.                                        
       WORKING-STORAGE SECTION.                              
       01 WS-SQLCODE PIC -9(9).                              
      *01 WS-SEX PIC X.                                      
           EXEC SQL                                          
               INCLUDE SQLCA                 
          END-EXEC.                          
          EXEC SQL                           
               INCLUDE EMPLOYEE              
          END-EXEC.                          
      PROCEDURE DIVISION.                    
          MOVE 'M' TO SEX.                   
            EXEC SQL                         
             SELECT  EMPNO     ,             
                     LASTNAME  ,             
                     BIRTHDATE ,             
                     SALARY                  
             INTO :EMPNO       ,             
                  :LASTNAME    ,             
                  :BIRTHDATE   ,             
                  :SALARY                    
             FROM EMPLOYEE                   
             WHERE SEX = :SEX                            
            END-EXEC.                                    
            MOVE SQLCODE TO WS-SQLCODE.                  
            PERFORM 000-MAIN-PARA.                       
            DISPLAY EMPNO     ,                          
                    LASTNAME  ,                          
                    BIRTHDATE ,                          
                    SALARY.                              
          STOP RUN.                                      
      000-MAIN-PARA.                                     
          EVALUATE SQLCODE                               
              WHEN 0                                     
                 DISPLAY "SUCCESS"                       
              WHEN 100                                   
                 DISPLAY "NO ROWS FOUND"                 
              WHEN OTHER                                 
                 DISPLAY "ERROR " WS-SQLCODE             
          END-EVALUATE.     
