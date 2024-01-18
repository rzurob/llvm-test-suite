! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  SltForm.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltForm
!*
!*  DATE                       : Mar. 05, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector has the form a.b
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SltForm
  IMPLICIT NONE

  TYPE :: L0
    INTEGER :: I0=0
  END TYPE

  TYPE, EXTENDS(L0) :: L1
    INTEGER :: I1=1
  END TYPE

  TYPE, EXTENDS(L1) :: L2
    INTEGER :: I2=2
  END TYPE

  TYPE (L2) :: L

  ASSOCIATE ( As => L  )
    IF ( As.I2 .NE. 2  )      STOP 20
    As.I2 = -2

    ASSOCIATE ( As => As.L1  )
      IF ( As.I1 .NE. 1  )    STOP 21
      As.I1 = -1

      ASSOCIATE ( As => As.L0  )
        IF ( As.I0 .NE. 0  )  STOP 22
      END ASSOCIATE

      IF ( As.L0.I0 .NE. 0  ) STOP 23
      IF ( As.I1 .NE. -1  )   STOP 24

    END ASSOCIATE

    IF ( As.I2 .NE. -2  )     STOP 25

  END ASSOCIATE

  END
