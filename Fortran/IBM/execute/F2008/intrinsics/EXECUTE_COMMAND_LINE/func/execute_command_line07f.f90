! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : execute_command_line07f.f
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
!*  Both CMDSTAT and CMDMSG present and no error condition occurs 
!*   => No termination of the program is initiated (the program completes successfully)
!*   => CMDSTAT and CMDMSG values are not changed
!*   test both if EXITSTAT is present or not
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
PROGRAM execute_command_line07f
      IMPLICIT NONE
      INTEGER, PARAMETER :: zero = 0
      CHARACTER(100) :: cmd, msg
      INTEGER :: Icmd, Istat

      cmd = "echo test command line execution"
      msg = "default!"

! compile time 
      CALL EXECUTE_COMMAND_LINE(COMMAND="echo test command line execution", CMDSTAT=Icmd, CMDMSG=msg)
      IF( Icmd .NE.       zero ) ERROR STOP 10
      IF( msg  .NE. "default!" ) ERROR STOP 11

! runtime 
      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDSTAT=Icmd, CMDMSG=msg, EXITSTAT=Istat)
      IF( Icmd  .NE.       zero ) ERROR STOP 12
      IF( Istat .NE.       Icmd ) ERROR STOP 13
      IF( msg   .NE. "default!" ) ERROR STOP 14

      print*, "End of the program: Normal termination"
END PROGRAM execute_command_line07f
