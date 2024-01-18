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
!*  run "date" command and verify they have the same behavior.
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
PROGRAM execute_command_line17f
      IMPLICIT NONE

      CALL EXECUTE_COMMAND_LINE(COMMAND="date +%Y/%m/%d' '%H > test1")
      CALL SYSTEM("date +%Y/%m/%d' '%H > test2")
      CALL EXECUTE_COMMAND_LINE(COMMAND="diff test1 test2")
      CALL SYSTEM("diff test1 test2")
END PROGRAM execute_command_line17f
