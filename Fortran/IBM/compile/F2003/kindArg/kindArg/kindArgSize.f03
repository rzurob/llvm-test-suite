!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SIZE
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
!*  (324715)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgSize

  INTEGER               :: C, C1(2)

  INTEGER               :: I(2) = (/0,1/)
  INTEGER,    PARAMETER :: II(3)= (/1,1,1/)
  REAL,       PARAMETER :: R(2) = (/1.,1./)
  CHARACTER,  PARAMETER :: CC(2)= (/ACHAR(40), ACHAR(40)/)
  LOGICAL(8), PARAMETER :: L(2) = (/.TRUE., .FALSE./)
  COMPLEX(8), PARAMETER :: Z(2) = (/(1.,1.), (1.,1.)/)
  LOGICAL(1)            :: Mask(3) = (/.TRUE., .FALSE., .TRUE./)

  ENUM, BIND(C)
    ENUMERATOR :: ONE=1
  END ENUM

  C = SIZE(Mask,      KIND=I(1))
  C = SIZE(Mask, One, II)

  C1= SIZE(Mask, 1, R(1))
  C1= SIZE(Mask, 1, CC(2))
  C1= SIZE(Mask, 1, L(1))
  C1= SIZE(Mask, 1, Z(2))

  C = SIZE(ARRAY=Mask,          KIND=ONE) !ok
  C = SIZE(ARRAY=Mask, DIM=One, KIND=II(1)) !ok

  END

