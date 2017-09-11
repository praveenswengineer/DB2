       IDENTIFICATION DIVISION.                      
       PROGRAM-ID.  CURUP141.                        
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
                   workdept,                                
                   salary                                   
            FROM EMPLOYEE                                   
            WHERE workdept=:workdept                        
            for update of salary                            
         END-EXEC.                                          
       PROCEDURE DIVISION.                                  
        000-MAIN-PARA.                                      
          PERFORM 100-OPEN-EMP-PARA.                        
          perform 110-fetch-para.                           
          IF SQLCODE = 0                                    
             PERFORM 110-fetch-para UNTIL SQLCODE = 100 OR  
                                               SQLCODE < 0  
          ELSE                                              
             DISPLAY "ERROR IN OPEN"              
          end-if.                                 
          PERFORM 400-CLOSE-PARA.                 
              STOP RUN.                           
        100-OPEN-EMP-PARA.                        
            MOVE 'a00' TO WORKDEPT                
          EXEC SQL                                
            OPEN EMP_CUR                          
          END-EXEC.                               
        110-fetch-para.                           
            EXEC SQL                              
              FETCH EMP_CUR                       
              INTO :EMPNO,:workdept,:SALARY       
            END-EXEC.                             
            perform 200-evaluate-para.            
        200-evaluate-para.                        
              evaluate sqlcode                    
                when 0                          
                   perform 120-update-para      
                when 100                        
                   display "fectch" sqlcode     
                when other                      
                   display "other" sqlcode      
              end-evaluate.                     
        120-update-para.                        
            exec sql                            
              update employee                   
              set salary = 50000                
              where current of emp_cur          
            end-exec.                           
         400-CLOSE-PARA.                        
           EXEC SQL                             
            CLOSE EMP_CUR                       
           END-EXEC.                            
