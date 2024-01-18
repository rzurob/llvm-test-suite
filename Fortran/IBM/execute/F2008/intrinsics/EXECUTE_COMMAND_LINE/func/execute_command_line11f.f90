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
!*  Create an executable in the scenario file and run it

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
PROGRAM execute_command_line11f
      IMPLICIT NONE
      INTEGER :: I

      CALL EXECUTE_COMMAND_LINE('./execute_command_line11f_pre')

      open(unit=20, file='file02')
      DO I = 1, 100
         write(20, *) I
      END DO

      close(20)

      CALL EXECUTE_COMMAND_LINE('diff file01 file02 > test')
END PROGRAM execute_command_line11f
