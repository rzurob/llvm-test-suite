!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgMinloc
!*
!*  DATE                       : Jun. 27, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : MINLOC
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
!*  Characteristics :: non initexp/non scalar/non int
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMinloc


  INTEGER               :: I(2) = (/0,1/)
  INTEGER,    PARAMETER :: II(1)= (/1/)
  REAL,       PARAMETER :: R(2) = (/1.,1./)
  CHARACTER,  PARAMETER :: CC(2)= (/ACHAR(1), ACHAR(1)/)
  LOGICAL(8), PARAMETER :: L(2) = (/.TRUE., .FALSE./)
  COMPLEX(8), PARAMETER :: Z(2) = (/(1.,1.), (1.,1.)/)

  INTEGER               :: IC(1)

  IC  = MINLOC((/" "," "/), .FALSE., I(1))  !Shall not complain mask
  IC1 = MINLOC((/character(2)::"  "/),DIM=1, KIND=I)

  IC = MINLOC((/character(2)::" ", " "/), KIND=II)

  IC1= MINLOC(ARRAY=R,  KIND=R(1))
  IC1= MINLOC(ARRAY=CC, KIND=CC(2))
  IC1= MINLOC(ARRAY=I,  KIND=L(1))
  IC1= MINLOC(ARRAY=Z,  KIND=I(2))

  IC = MINLOC(ARRAY=R, KIND=L%KIND) !ok

  END

