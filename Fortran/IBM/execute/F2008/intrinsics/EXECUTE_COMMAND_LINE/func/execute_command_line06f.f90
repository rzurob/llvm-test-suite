! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : execute_command_line06f.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-12-15
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : EXECUTE_COMMAND_LINE intrinsic
!*                             :
!*  SECONDARY FUNCTIONS TESTED :  
!*                                
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                :  
!*
!*  EXECUTE_COMMAND_LINE(COMMAND [, WAIT, EXITSTAT, CMDSTAT, CMDMSG ])
!*
!*  CMDSTAT and CMDMSG both present and an error condition occurs 
!*   => No termination of the program is initiated (the program completes successfully) 
!*   => CMDSTAT and CMDMSG values change
!*   * test both cases with EXITSTAT present or not  
!*   * No error message is emitted by the program (different fron the shell error message)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
PROGRAM execute_command_line06f
      IMPLICIT NONE
      CHARACTER(100) :: cmd, msg 
      INTEGER :: Icmd = 0, Istat = 0 

      cmd = "mv file1.txt file2.txt"
      msg = "success!" 

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDSTAT=Icmd, CMDMSG=msg)     
print*, '******************runtime test****************************'
print*, Icmd   ! a processor-dependent positive value
print*, msg    ! a processor-dependent explanatory message

      CALL EXECUTE_COMMAND_LINE(COMMAND="mv file1.txt file2.txt", CMDSTAT=Icmd, CMDMSG=msg, EXITSTAT=Istat)     
print*, '**************compipiletime test**************************'
print*, Icmd   ! processor-dependent positive value
print*, msg    ! processor-dependent explanatory message
print*, Istat  ! processor-dependent exit status
      IF( Istat .NE. Icmd )       ERROR STOP 10


      print*, "End of the program: Normal termination" 
END PROGRAM execute_command_line06f
