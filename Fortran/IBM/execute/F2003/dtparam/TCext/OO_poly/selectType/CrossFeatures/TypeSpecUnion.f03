! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/CrossFeatures/TypeSpecUnion.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 28, 2005
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
!*  Type Spec : Union and map
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  TypeSpecUnion
  IMPLICIT NONE

  STRUCTURE /S/
    UNION
      MAP
      INTEGER :: I=1
      END MAP

      MAP
        INTEGER :: J
      END MAP
    END UNION
  END STRUCTURE

  TYPE :: DT(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
    RECORD /S/ R(2,2,2)
  END TYPE

  TYPE(DT(4,20)) :: V

  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*)  :: Arg


S1: SELECT TYPE (S2 => Arg)
    CLASS DEFAULT

S2: SELECT TYPE (U => S2 )
    CLASS DEFAULT
      STOP 20
    TYPE IS (DT(4,*))

        IF (SIZE(U.R)       .NE. 8)            ERROR STOP 30
        IF (ANY(SHAPE(U.R)  .NE. (/2,2,2/) ))  ERROR STOP 31
        IF (KIND(U.R.I)   .NE. 4)  ERROR STOP 31
        IF (KIND(U.R.J)   .NE. 4)  ERROR STOP 32
        IF (ANY(U.R.I    .NE. 1))  ERROR STOP 33
        IF (ANY(U.R.J    .NE. 1))  ERROR STOP 34

        U.R.J = 2

        IF (ANY(U.R.I    .NE. 2))  ERROR STOP 43
        IF (ANY(U.R.J    .NE. 2))  ERROR STOP 44

    END SELECT S2
    END SELECT S1

  END SUBROUTINE

  END


