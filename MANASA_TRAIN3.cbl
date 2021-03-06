        IDENTIFICATION DIVISION.                             
        PROGRAM-ID.    TRAIN141.                             
        AUTHOR.        HONEY.                                
        INSTALLATION.  IBM E&T.                              
        DATE-WRITTEN.  8/17/17.                              
        DATE-COMPILED.                                       
        SECURITY.      NONE.                                 
        ENVIRONMENT DIVISION.                                
        INPUT-OUTPUT SECTION.                                
        FILE-CONTROL.                                        
            SELECT TRAIN3 ASSIGN TO TRAIN3DD                 
            ORGANIZATION IS SEQUENTIAL                       
            ACCESS IS SEQUENTIAL                             
            FILE STATUS IS WS-TRAIN3DD-ST.                   
             SELECT TRAIN2 ASSIGN TO TRAIN2DD                
            ORGANIZATION IS SEQUENTIAL                    
            ACCESS IS SEQUENTIAL                          
            FILE STATUS IS WS-TRAIN2DD-ST.                
         DATA DIVISION.                                   
         FILE SECTION.                                    
       FD TRAIN3                                          
          RECORDING MODE IS F.                            
       01 TRAIN3RE PIC X(80).                             
       FD TRAIN2                                          
          RECORDING MODE IS F.                            
       01 TRAIN2RE.                                       
         05 FS2-TRAIN-NUMBER PIC X(06).                   
         05                  PIC X(74).                   
         WORKING-STORAGE SECTION.                         
          EXEC SQL                                        
            INCLUDE SQLCA                                 
          END-EXEC.                                       
          EXEC SQL                                        
            INCLUDE TRAIN                             
          END-EXEC.                                   
      01 WS-SQLCODE PIC -9(9).                        
      01   WS-TRAIN3-EOF PIC X VALUE 'N'.             
         88 EOF-TRAIN3  VALUE 'Y'.                    
      01   WS-TRAIN2-EOF PIC X VALUE 'N'.             
         88 EOF-TRAIN2  VALUE 'Y'.                    
      77   WS-TRAIN2DD-ST      PIC X(2).              
      77   WS-TRAIN3DD-ST      PIC X(2).              
      77   WS-DATE       PIC X(8).                    
      77   WS-TIME       PIC X(4).                    
      01 WS-TRAIN-REC.                                
         05 FS1-TRAIN-NUMBER    PIC X(6).             
         05 FS1-TRAIN-TYPE   PIC X(1).                
         05 FS1-TRAIN-NAME  PIC X(20).                
         05 FS1-TRAIN-DEP-STN    PIC X(10).           
         05 FS1-TRAIN-DEP-TIME     PIC X(5).          
         05 FS1-TRAIN-ARR-STN   PIC X(10).            
         05 FS1-TRAIN-ARR-TIME    PIC X(05).                  
         05 FS1-TRAIN-FARE    PIC X(10).                      
         05              PIC X(13).                           
       01 WS-TRAIN-AST  PIC X(80) VALUE ALL '*'.              
       01 WS-TRAIN-TIT.                                       
         05 RS1-TRAINNO    PIC X(6) VALUE 'NO'.               
         05              PIC X(1).                            
         05 RS1-TRAINTYPE  PIC X(1) VALUE                     
                                'T'.                          
         05              PIC X(2).                            
         05 RS1-TRAINNAME   PIC X(20)  VALUE 'NAME'.          
         05              PIC X(2).                            
         05 RS1-TRAINDEP    PIC X(10) VALUE 'DEPSTN'.         
         05              PIC X(2).                            
         05 RS1-TRAINDEPT     PIC X(8) VALUE 'DEPTIME'.       
         05              PIC X(28).                           
      01 WS-TRAIN-DATE.                                       
         05 RS1-DATE PIC X(7) VALUE 'DATE:-'.                 
         05 RS1-ADATE PIC X(8).                                       
         05           PIC X(65).                                      
      01 WS-TRAIN-TIME.                                               
         05 RS1-DATE PIC X(7) VALUE 'TIME:-'.                         
         05 RS1-ATIME PIC X(4).                                       
         05           PIC X(69).                                      
      01 WS-TRAIN-RECNF.                                              
         05 WS-NFTRAIN-NUMBER PIC X(6).                               
         05         PIC X(5).                                         
         05         PIC X(69)  VALUE 'NOT FOUND'.                     
       PROCEDURE DIVISION.                                            
         000-MAIN-PARA.                                               
            PERFORM 100-OPEN-FILE-PARA.                               
              IF  WS-TRAIN2DD-ST = '00' AND                           
                      WS-TRAIN3DD-ST = '00'                           
                  PERFORM 400-WRITE-HDR-PARA                          
                  PERFORM 200-READ-FILE-PARA UNTIL WS-TRAIN2-EOF = 'Y'
                  PERFORM 500-WRITE-FOOTER-PARA                       
              ELSE                                                     
               DISPLAY 'ERROR IN OPEN '  WS-TRAIN2DD-ST  WS-TRAIN3DD-ST
              END-IF.                                                  
            PERFORM 300-CLOSE-FILE-PARA.                               
                         STOP RUN.                                     
         100-OPEN-FILE-PARA.                                           
                   OPEN INPUT TRAIN2.                                  
                   OPEN OUTPUT TRAIN3.                                 
         200-READ-FILE-PARA.                                           
                      READ TRAIN2                                      
                        AT END                                         
                            SET EOF-TRAIN2 TO TRUE                     
                        NOT AT END                                     
                            PERFORM 600-READ-SQL-PARA                  
             END-READ.                                                 
         300-CLOSE-FILE-PARA.                                          
                    CLOSE TRAIN2.                                      
                    CLOSE TRAIN3.                                      
         211-WRITE-PARA.                                           
                      DISPLAY "         ".                         
                   MOVE TRAINNO     TO  FS1-TRAIN-NUMBER.          
                   MOVE TRAINTYPE   TO  FS1-TRAIN-TYPE .           
                   MOVE TRAINNAME   TO  FS1-TRAIN-NAME .           
                   MOVE TRAINDEPSTN TO  FS1-TRAIN-DEP-STN.         
                   MOVE TRAINDEPTM  TO  FS1-TRAIN-DEP-TIME.        
                   MOVE TRAINARRSTN TO  FS1-TRAIN-ARR-STN .        
                   MOVE TRAINARRTM  TO  FS1-TRAIN-ARR-TIME .       
                   MOVE TRAINFARE   TO  FS1-TRAIN-FARE .           
                   WRITE TRAIN3RE FROM WS-TRAIN-REC.               
                       DISPLAY "WRITTEN TO THE FILE".              
      400-WRITE-HDR-PARA.                                          
                   WRITE TRAIN3RE FROM WS-TRAIN-AST.               
                   MOVE FUNCTION CURRENT-DATE(1:8) TO  WS-DATE.    
                   MOVE WS-DATE TO RS1-ADATE.                      
                   WRITE TRAIN3RE FROM WS-TRAIN-DATE.              
                   MOVE FUNCTION CURRENT-DATE(9:12) TO  WS-TIME.   
                   MOVE WS-TIME TO RS1-ATIME.                
                   WRITE TRAIN3RE FROM WS-TRAIN-TIME.        
                   WRITE TRAIN3RE FROM WS-TRAIN-AST.         
                   WRITE TRAIN3RE FROM WS-TRAIN-TIT.         
                   WRITE TRAIN3RE FROM WS-TRAIN-AST.         
      500-WRITE-FOOTER-PARA.                                 
                   WRITE TRAIN3RE FROM WS-TRAIN-AST.         
      600-READ-SQL-PARA.                                     
                 MOVE FS2-TRAIN-NUMBER TO TRAINNO.           
                 EXEC SQL                                    
                  SELECT TRAINNO,                            
                         TRAINTYPE,                          
                         TRAINNAME,                          
                         TRAINDEPSTN,                        
                         TRAINDEPTM,                         
                         TRAINARRSTN,                        
                         TRAINARRTM,                         
                         TRAINFARE                           
                    INTO :TRAINNO,                        
                         :TRAINTYPE,                      
                         :TRAINNAME,                      
                         :TRAINDEPSTN,                    
                         :TRAINDEPTM,                     
                         :TRAINARRSTN,                    
                         :TRAINARRTM,                     
                         :TRAINFARE                       
                   FROM TRAIN                             
                   WHERE TRAINNO = :TRAINNO               
                 END-EXEC.                                
                  EVALUATE SQLCODE                        
                     WHEN 100                             
                       PERFORM 700-NOTFOUND-WRITE-PARA    
                     WHEN 0                               
                       PERFORM 211-WRITE-PARA             
                     WHEN OTHER                           
                       DISPLAY "ERROR" SQLCODE            
                 END-EVALUATE.                       
      700-NOTFOUND-WRITE-PARA.                       
             MOVE TRAINNO TO WS-NFTRAIN-NUMBER.      
              WRITE TRAIN3RE FROM WS-TRAIN-RECNF.    
