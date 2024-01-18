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
!*   CMDSTAT present but CMDMSG not present and an error condition occurs
!*   => No termination of the program is initiated (the program completes successfully)
!*   => CMDSTAT value changes
!*   * test both cases with EXITSTAT present or not
!*   * verify that the program doesn't issue an error message
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
PROGRAM execute_command_line05f
      IMPLICIT NONE
      CHARACTER(100) :: cmd, msg
      INTEGER :: Icmd, Istat

      cmd = "cp file1.txt file2.txt"
      msg = "success!"
      Icmd = 0
      Istat = 0

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDSTAT=Icmd)
      IF( msg  .NE. "success!" ) ERROR STOP 10
print*, '******************runtime test****************************'
print*, Icmd

      CALL EXECUTE_COMMAND_LINE(COMMAND="cp file1.txt file2.txt", CMDSTAT=Icmd, EXITSTAT=Istat)
      IF( msg   .NE. "success!" ) ERROR STOP 11
      IF( Istat .NE.       Icmd ) ERROR STOP 12
print*, '**************compipiletime test**************************'
print*, Icmd

      print*, "End of the program: Normal termination"
END PROGRAM execute_command_line05f
