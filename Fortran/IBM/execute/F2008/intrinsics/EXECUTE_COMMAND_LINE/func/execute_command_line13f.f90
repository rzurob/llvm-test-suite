! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : execute_command_line13f.f
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
!*  run EXECUTE_COMMAND_LINE and SYSTEM with the echo command and verify they have the same behavior. 
!*  Call system('echo running on = `hostname` ')
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
PROGRAM execute_command_line13f
      IMPLICIT NONE
      CHARACTER(100) :: cmd, msg 
      INTEGER :: Icmd, Istat 

      cmd = "echo running on = `hostname` > test1 "
      msg = "default"
      Icmd = 0 
      Istat = 0 

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDMSG=msg, CMDSTAT=Icmd, EXITSTAT=Istat)
      IF( msg   .NE. "default" )  ERROR STOP 10
      IF( Icmd  .NE.         0 )  ERROR STOP 11
      IF( Istat .NE.         0 )  ERROR STOP 12

      CALL SYSTEM("echo running on = `hostname` > test2 ")
END PROGRAM execute_command_line13f
