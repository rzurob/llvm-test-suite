!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgUbound
!*
!*  DATE                       : Jul. 05, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : UBOUND
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
!*  (322381)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgUbound
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

  INTEGER  :: IC(1)

  IC    = UBOUND(II, I(1), I(1))
  IC(1) = UBOUND(II, DIM=I(1), KIND=II)

  IC = UBOUND(ARRAY=II, KIND=II)

  IC = UBOUND(ARRAY=II, KIND=R(1))
  IC = UBOUND(ARRAY=II, KIND=CC(2))
  IC = UBOUND(ARRAY=II, KIND=L(1))
  IC = UBOUND(ARRAY=II, KIND=Z(2))

  IC    = UBOUND(ARRAY=II, KIND=ONE%KIND) !ok

  END

