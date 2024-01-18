!***********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfixed
! %GROUP: ../fake_xlfutility_module.f intrimod63.f
! %VERIFY: intrimod63.out:../emptyout.vf
! %STDIN:
! %STDOUT: intrimod63.out
! %EXECARGS:
! %POSTCMD: rm -f xlfutility.mod
! %END
!**********************************************************************
      PROGRAM intrimod63
C**********************************************************************
C*  =================================================================== 
C*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY 
C*  =================================================================== 
C*                                                                     
C*  TEST CASE NAME             : intrimod63
C*  TEST CASE TITLE            : NTRINSIC Module nature with Service and
C*                             : Utility Procedure Interfaces
C*
C*  PROGRAMMER                 : Bahram Chehrazy
C*  DATE                       : January 13, 2004
C*  ORIGIN                     : AIX Compiler Development, 
C*                             : IBM SWS Toronto Lab     
C*                                                                      
C*  PRIMARY FUNCTIONS TESTED   : GMTIME_ & TIME_
C*  SECONDARY FUNCTIONS TESTED : None                                   
C*                                                                     
C*  DESCRIPTION                : Procedures GMTIME_ & TIME_,correct calls    
C*  KEYWORD(S)                 :                
C*  TARGET(S)                  : 
C*  NUMBER OF TESTS            :  5                                       
C*  STATUS                     :     
C*                                               
C*  STRUCTURE                  : Main program    
C*  EXECUTABLE                 : Yes           
C*                                           
C*  INPUTS                     : None        
C*  OUTPUTS                    : None      
C*                                       
C*  SETUP REQUIREMENTS         : N/A     
C*  DEPENDENCIES               :                
C*  REQUIRED COMPILER OPTIONS  : None                            
C*                                                             
C*  NORMAL COMPLETION          : Return code = 0        
C*  ABNORMAL COMPLETION        : Return code ^= 0         
C*                                                        
C*  RUN TIME ESTIMATE          : <60 SECS                   
C*                                                          
C*  CONDITIONS TESTED          : Listed below.                
C*                                                            
C/  COND.   DESCRIPTION
C/  -----   ------------------------------------------------------------
C/      1   Type of the arguments - INTEGER(4)
C/      2   An actual argument - HEXADECIMAL CONSTANT
C/      3   An actual argument - OCTAL CONSTANT
C/      4   An actual argument - BINARY CONSTANT
C/      5   An actual argument - HOLLERITH CONSTANT
C/
C* =================================================================== 
C*
C*  REVISION HISTORY            
C*  
C*  MM/DD/YY:  Init:  Comments:
C*  07/27/94   EB     -Initial Version (utility_proc/fxut083a_LDT.f)
C*  01/13/04   BC     -Modifyed and reused for INTRINSIC/NON_INTRINSIC module
C*                     nature.
C*                                                                    
C* =================================================================== 
C234567890123456789012345678901234567890123456789012345678901234567890

       use, intrinsic :: xlfutility

C DEFINES
       
       integer (kind=TIME_SIZE) TIME
       integer (4) tarray(9)

C CALLS OF TIME_ & GMTIME_

       TIME = time_()
       call gmtime_(TIME,tarray)
      
C  ACTUAL ARGUMENT - HEXADECIMAL CONSTANT

       call gmtime_(Z'1',tarray)
       call gmtime_(Z'a4580',tarray)
       call gmtime_(Z'1ba45680',tarray)

C  ACTUAL ARGUMENT - OCTAL CONSTANT

       call gmtime_(O'1',tarray)
       call gmtime_(O'670127',tarray)
       call gmtime_(O'12345670127',tarray)

C  ACTUAL ARGUMENT - BINARY CONSTANT

       call gmtime_(B"1",tarray)
       call gmtime_(B"1010100",tarray)
       call gmtime_(B"11010010110110010101011001101010",tarray)

       end 
