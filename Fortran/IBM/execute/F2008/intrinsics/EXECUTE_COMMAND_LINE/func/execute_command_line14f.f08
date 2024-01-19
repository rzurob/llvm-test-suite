! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-12-14
!*
!*  PRIMARY FUNCTIONS TESTED   : EXECUTE_COMMAND_LINE intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  EXECUTE_COMMAND_LINE(COMMAND [, WAIT, EXITSTAT, CMDSTAT, CMDMSG ])
!*
!*  run EXECUTE_COMMAND_LINE and SYSTEM with the echo command and verify they have the same behavior.
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
MODULE Mod
      CONTAINS

      SUBROUTINE Sub1(Arg)
        INTEGER :: Arg, I

        open(unit=10, file='file01')
        write(10, *) 1,' line written in file01'

        DO I = 2, Arg
          write(10, *) I,' lines written in file01'
        END DO
      END SUBROUTINE Sub1
END MODULE
PROGRAM execute_command_line14f
      USE Mod
      IMPLICIT NONE
      CHARACTER(100) :: cmd

      cmd = "diff file01 file02"

      CALL Sub1(100); CALL Sub2(100)

      CALL EXECUTE_COMMAND_LINE(cmd)
      CALL SYSTEM(cmd)

      CONTAINS

      SUBROUTINE Sub2(Arg)
        INTEGER :: Arg, I

        open(unit=20, file='file02')
        write(20, *) 1,' line written in file02'

        DO I = 2, Arg
          write(20, *) I,' lines written in file02'
        END DO
      END SUBROUTINE Sub2
END PROGRAM execute_command_line14f
