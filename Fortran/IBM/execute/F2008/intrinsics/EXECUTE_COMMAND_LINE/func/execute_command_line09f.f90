! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : execute_command_line09f.f
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
!*
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
      INTEGER, PARAMETER :: block1 = 10, block2 = 20, block3 = 30 

      CONTAINS

      SUBROUTINE Sub1
        INTEGER :: I

        open(unit=10, file='file01')
        write(10, *) 1,' line written in file01'

        DO I = 2, block1
          write(10, *) I,' lines written in file01'
        END DO

        close(10) 
      END SUBROUTINE Sub1
END MODULE

PROGRAM execute_command_line09f
      USE Mod
      IMPLICIT NONE

      CHARACTER(100) :: msg 
      INTEGER :: Icmd = 0 

      CALL Sub1()
      CALL Sub2()
      CALL Sub3()

      msg = "default"

      CALL EXECUTE_COMMAND_LINE(COMMAND='cat file01 file02 file03 > BigFile', CMDMSG=msg, CMDSTAT=Icmd)
      IF( msg  .NE. "default" )  ERROR STOP 10 
      IF( Icmd .NE.          0 )  ERROR STOP 11 

      CONTAINS

      SUBROUTINE Sub2
        INTEGER :: I

        open(unit=20, file='file02')

        DO I = block1, block2
          write(20, *) I,' lines written in file02'
        END DO
        close(20) 
      END SUBROUTINE Sub2
END PROGRAM execute_command_line09f
SUBROUTINE Sub3
 INTEGER :: I, lb, ub

 lb = 20; ub = 30 
 open(unit=30, file='file03')

 DO I = lb, ub-1, 1
   write(30, *) I,' lines written in file03'
 END DO
 close(30) 
END SUBROUTINE Sub3
