! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-12-15
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : EXECUTE_COMMAND_LINE intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  EXECUTE_COMMAND_LINE(COMMAND [, WAIT, EXITSTAT, CMDSTAT, CMDMSG ])
!*
!*  CMDSTAT and CMDMSG not present and an error condition occurs
!*   => error termination of the program
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
PROGRAM execute_command_line03f
      IMPLICIT NONE

      CALL EXECUTE_COMMAND_LINE(COMMAND="mv file1.txt file2.txt")

      print*, "End of the program: Normal termination"
END PROGRAM execute_command_line03f
