!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgAchar
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ACHAR
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


  PROGRAM kindArgAchar

  CHARACTER :: C, C1(2)

  INTEGER               :: I(2) = (/0,1/)
  INTEGER,    PARAMETER :: II(2)= (/1,1/)
  REAL,       PARAMETER :: R(2) = (/1.,1./)
  CHARACTER,  PARAMETER :: CC(2)= (/ACHAR(1), ACHAR(1)/)
  LOGICAL(8), PARAMETER :: L(2) = (/.TRUE., .FALSE./)
  COMPLEX(8), PARAMETER :: Z(2) = (/(1.,1.), (1.,1.)/)

  ENUM, BIND(C)
    ENUMERATOR :: ONE=1
  END ENUM

  C  = ACHAR(40, I(1))
  C1 = ACHAR(40, I)

  C = ACHAR(40, II)

  C1= ACHAR(40, R(1))
  C1= ACHAR(40, CC(2))
  C1= ACHAR(40, L(1))
  C1= ACHAR(40, Z(2))

  C = ACHAR(40, ONE) !ok

  END

