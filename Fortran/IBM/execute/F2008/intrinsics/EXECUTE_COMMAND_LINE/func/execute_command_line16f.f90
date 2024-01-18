! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-12-14
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : EXECUTE_COMMAND_LINE intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  EXECUTE_COMMAND_LINE(COMMAND [, WAIT, EXITSTAT, CMDSTAT, CMDMSG ])
!*
!*  run EXECUTE_COMMAND_LINE and SYSTEM with the echo command and verify they have the same behavior.
!*  'uname -s' and 'uname -rv' command and verify they have the same behavior.
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
PROGRAM execute_command_line16f
      IMPLICIT NONE
      CHARACTER(100) :: cmd, msg
      INTEGER :: Icmd, Istat

      cmd = "uname -rv >> test1 "
      msg = "default"
      Icmd = 0
      Istat = 0

      CALL EXECUTE_COMMAND_LINE(COMMAND="uname -s > test1")
      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDMSG=msg, CMDSTAT=Icmd, EXITSTAT=Istat)
      IF( msg   .NE. "default" )  ERROR STOP 10
      IF( Icmd  .NE.         0 )  ERROR STOP 11
      IF( Istat .NE.         0 )  ERROR STOP 12

      CALL SYSTEM("uname -s > test2 ")
      CALL SYSTEM("uname -rv >> test2 ")

      CALL EXECUTE_COMMAND_LINE(COMMAND="diff test1 test2", EXITSTAT=Istat)

      CALL SYSTEM("diff test1 test2")
END PROGRAM execute_command_line16f
