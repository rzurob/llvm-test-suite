! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-12-15
!*
!*  PRIMARY FUNCTIONS TESTED   : EXECUTE_COMMAND_LINE intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  EXECUTE_COMMAND_LINE(COMMAND [, WAIT, EXITSTAT, CMDSTAT, CMDMSG ])
!*
!*  CMDSTAT not present but CMDMSG present and an error condition occurs
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
PROGRAM execute_command_line04f
      IMPLICIT NONE
      CHARACTER(100) :: cmd, msg
      INTEGER :: Istat

      cmd = "mv file1.txt file2.txt"
      msg = "success!"
      Istat = 0

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDMSG=msg, EXITSTAT=Istat)
print*, '******************runtime test****************************'
print*, msg    ! processor-dependent explanatory message
print*, Istat  ! processor-dependent exit status

      print*, "End of the program: Normal termination"
END PROGRAM execute_command_line04f