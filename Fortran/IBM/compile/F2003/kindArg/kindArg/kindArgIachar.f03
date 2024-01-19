!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : IACHAR
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


  PROGRAM kindArgIachar

  INTEGER :: IC, IC1(2)

  INTEGER               :: I(2) = (/0,1/)
  INTEGER,    PARAMETER :: II(2)= (/1,1/)
  REAL,       PARAMETER :: R(2) = (/1.,1./)
  CHARACTER,  PARAMETER :: CC(2)= (/ACHAR(1), ACHAR(1)/)
  LOGICAL(8), PARAMETER :: L(2) = (/.TRUE., .FALSE./)
  COMPLEX(8), PARAMETER :: Z(2) = (/(1.,1.), (1.,1.)/)

  ENUM, BIND(C)
    ENUMERATOR :: ONE=1
  END ENUM

  IC  = IACHAR(" ", I(1))
  IC1 = IACHAR(" ", I)

  IC = IACHAR((/" ", "A"/), II)

  IC1= IACHAR(" ", R(1))
  IC1= IACHAR(" ", CC(2))
  IC1= IACHAR(" ", L(1))
  IC1= IACHAR(" ", Z(2))

  IC = IACHAR(" ", ONE%KIND) !ok

  END

