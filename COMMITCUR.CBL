       IDENTIFICATION DIVISION.                 
       PROGRAM-ID.  CURUPDAT.                   
       ENVIRONMENT DIVISION.                    
       DATA DIVISION.                           
       WORKING-STORAGE SECTION.                 
        01  WS-SQLCODE PIC -9(9).               
           EXEC SQL                             
            INCLUDE SQLCA                       
           END-EXEC.                            
           EXEC SQL                             
            INCLUDE EMPLOYEE                    
           END-EXEC.                            
           EXEC SQL                             
            DECLARE EMP_CUR CURSOR FOR          
             SELECT EMPNO,                      
                   WORKDEPT,                    
                   SALARY                                      
            FROM EMPLOYEE                                      
            WHERE WORKDEPT=:WORKDEPT                           
            FOR UPDATE OF SALARY                               
            END-EXEC.                                          
       PROCEDURE DIVISION.                                     
        000-MAIN-PARA.                                         
          PERFORM 100-OPEN-EMP-PARA.                           
          PERFORM 110-FETCH-PARA.                              
          IF SQLCODE = 0                                       
             PERFORM 110-FETCH-PARA UNTIL SQLCODE = 100 OR     
                                               SQLCODE < 0     
          ELSE                                                 
             DISPLAY "ERROR IN OPEN"                           
          END-IF.                                              
          PERFORM 400-CLOSE-PARA.                              
              STOP RUN.                                        
        100-OPEN-EMP-PARA.                         
            MOVE 'A00' TO WORKDEPT                 
            EXEC SQL                               
              OPEN EMP_CUR                         
            END-EXEC.                              
        110-FETCH-PARA.                            
            EXEC SQL                               
              FETCH EMP_CUR                        
              INTO :EMPNO,:WORKDEPT,:SALARY        
            END-EXEC.                              
            PERFORM 200-EVALUATE-PARA.             
        200-EVALUATE-PARA.                         
              EVALUATE SQLCODE                     
                WHEN 0                             
                   PERFORM 120-UPDATE-PARA         
                   IF SQLCODE = 0                  
                    DISPLAY "UPDATED"              
                    EXEC SQL                   
                      COMMIT                   
                    END-EXEC                   
                   ELSE                        
                    DISPLAY "NOT UPDATED"      
                   END-IF                      
                WHEN 100                       
                   DISPLAY "FECTCH" SQLCODE    
                WHEN OTHER                     
                   DISPLAY "OTHER" SQLCODE     
              END-EVALUATE.                    
        120-UPDATE-PARA.                       
            EXEC SQL                           
              UPDATE EMPLOYEE                  
              SET SALARY = 50000               
              WHERE CURRENT OF EMP_CUR         
            END-EXEC.                          
         400-CLOSE-PARA.    
           EXEC SQL         
            CLOSE EMP_CUR   
           END-EXEC.        
