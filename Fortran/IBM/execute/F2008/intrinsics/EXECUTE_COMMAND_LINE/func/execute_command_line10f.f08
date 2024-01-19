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
!*  Rename a Module
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
PROGRAM execute_command_line10f
      USE Mod
      IMPLICIT NONE
      CHARACTER(100) :: cmd, msg
      INTEGER :: I, Istat, Icmd, me


      CALL Sub1(10000)
      CALL Sub2(10000)

      CALL EXECUTE_COMMAND_LINE('diff file01 file02')
      CALL EXECUTE_COMMAND_LINE('mv mod.mod mod1.mod')
      CALL EXECUTE_COMMAND_LINE(COMMAND='ls mod.mod', CMDMSG=msg, EXITSTAT=Istat, CMDSTAT=Icmd)
print*, msg
print*, Icmd
print*, Istat


      CONTAINS

      SUBROUTINE Sub2(Arg)
        INTEGER :: I, Arg

        open(unit=20, file='file02')
        write(20, *) 1,' line written'

        DO I = 2, Arg
          write(20, *) I,' lines written'
        END DO
      END SUBROUTINE Sub2
END PROGRAM execute_command_line10f
