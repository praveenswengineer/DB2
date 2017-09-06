CREATE TABLE PROJECT                    
(                                       
PROJNO    CHAR(6)     NOT NULL ,        
PROJNAME  VARCHAR(24) NOT NULL ,        
DEPTNO     CHAR(3)     NOT NULL ,       
RESPEMP   CHAR(6)     NOT NULL ,        
PRSTAFF   DECIMAL(5,2)         ,        
PRSTDATE  DATE                 ,        
PRENDATE  DATE                 ,        
MAJPROJ   CHAR(6)              ,        
PRIMARY KEY (PROJNO)                    
) IN DATABASE TRNGDB2;                  
