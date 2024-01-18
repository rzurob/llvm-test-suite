!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIndex
!*
!*  DATE                       : Jun. 20, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : INDEX
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


  PROGRAM kindArgIndex


  INTEGER               :: I(2) = (/0,1/)
  INTEGER,    PARAMETER :: II(1)= (/1/)
  REAL,       PARAMETER :: R(2) = (/1.,1./)
  CHARACTER,  PARAMETER :: CC(2)= (/ACHAR(1), ACHAR(1)/)
  LOGICAL(8), PARAMETER :: L(2) = (/.TRUE., .FALSE./)
  COMPLEX(8), PARAMETER :: Z(2) = (/(1.,1.), (1.,1.)/)

  INTEGER               :: IC(1)

  IC  = INDEX("  "," ", .TRUE., I(1))
  IC1 = INDEX((/character(2)::"  "/),(/character::" "/), KIND=I)

  IC = INDEX((/character(2)::"  "/),(/character::" "/), KIND=II)

  IC1= INDEX(STRING="  ",SUBSTRING=" ", KIND=R(1))
  IC1= INDEX(STRING="  ",SUBSTRING=" ", KIND=CC(2))
  IC1= INDEX(STRING="  ",SUBSTRING=" ", KIND=L(1))
  IC1= INDEX(STRING="  ",SUBSTRING=" ", KIND=Z(2))

  IC = INDEX((/character(2)::"  "/),(/character::" "/), KIND=KIND((/character(2)::"  "/))) !ok

  END

