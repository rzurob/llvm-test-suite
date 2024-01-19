! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/selectType/CrossFeatures/Where.f
! opt variations: -qnol

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
!* Where
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Where
  IMPLICIT CLASS(DT(20,4))(U)
  TYPE :: DT(N1,K1)    ! (20,4)
   INTEGER, KIND :: K1
   INTEGER, LEN  :: N1
   COMPLEX(K1)   :: Cplx=(1.0, -1.0)
  END TYPE
  INTEGER :: i

  CALL Sub((/(DT(20,4)(Cplx=(-1.0,1.0)), i=1,16)/))

  CONTAINS

  SUBROUTINE Sub(U)
  DIMENSION :: U(:)

  SELECT TYPE (U)
  CLASS IS (DT(*,4))

    IF (ANY(U%Cplx .NE. (-1.0, 1.0))) ERROR STOP 20
    IF (ANY(SHAPE(U).NE. (/16/))) ERROR STOP 21

    WHERE ( U%Cplx .EQ. (-1.0,1.0))
      U%Cplx = (1.0, -1.0)
    END WHERE

    IF (ANY(U%Cplx .NE. (1.0, -1.0))) ERROR STOP 22

  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END



