! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_poly/selectType/CrossFeatures/Forall.f
! opt variations: -qnock

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 02, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* Forall
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Forall
  IMPLICIT CLASS(DT(4,1,3))(U)
  TYPE :: DT(K1,K2,N1)    ! (4,1,3)
    INTEGER, KIND             :: K1,K2
    INTEGER, LEN              :: N1
    INTEGER(K1)               :: Int
    CHARACTER(kind=K2,len=N1) :: C
  END TYPE
  INTEGER :: i

  CALL Sub((/(DT(4,1,3)(Int=-1, C="123"), i=1,16)/))

  CONTAINS

  SUBROUTINE Sub(U)
  DIMENSION :: U(:)

  SELECT TYPE (U)
  CLASS IS (DT(4,1,*))

    IF (ANY(U%Int   .NE. -1))      STOP 20
    IF (ANY(U%C     .NE. "123"))   STOP 21
    IF (ANY(SHAPE(U).NE. (/16/)))  STOP 22

    FORALL  (I=1:16 )
      U%Int = 1
      U%C="321"
    END FORALL

    IF (ANY(U%Int .NE. 1))     STOP 30
    IF (ANY(U%C   .NE. "321")) STOP 31

  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END



