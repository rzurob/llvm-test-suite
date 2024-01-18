!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : LEN
!*
!*  REFERENCE                  : Feature Number 289083
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  characteristics :: non initexp/non scalar/non int
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen
  IMPLICIT NONE

  INTEGER               :: I(2) = (/0,1/)
  INTEGER,    PARAMETER :: II(2)= (/1,1/)
  REAL,       PARAMETER :: R(2) = (/1.,1./)
  CHARACTER,  PARAMETER :: CC(2)= (/ACHAR(1), ACHAR(1)/)
  LOGICAL(8), PARAMETER :: L(2) = (/.TRUE., .FALSE./)
  COMPLEX(8), PARAMETER :: Z(2) = (/(1.,1.), (1.,1.)/)

  ENUM, BIND(C)
    ENUMERATOR :: ONE=1
  END ENUM

  INTEGER :: IC, IC1(2)

  IC  = LEN(" ", I(1))
  IC1 = LEN(" ", I)

  IC = LEN(STRING=(/" ", "A"/), KIND=II)

  IC1= LEN(STRING=" ", KIND=R(1))
  IC1= LEN(STRING=" ", KIND=CC(2))
  IC1= LEN(STRING=" ", KIND=L(1))
  IC1= LEN(STRING=" ", KIND=Z(2))

  IC = LEN(STRING=" ", KIND=KIND(ONE%KIND)) !ok

  END

