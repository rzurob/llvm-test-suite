! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/selectType/Quotes/ExecSeq1.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 27, 2005
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
!*  Diagnosis : Bad select type construct
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890





  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    CLASS(DT(4)), INTENT(IN) :: Arg
    INTEGER               :: GetId
      GetId = Arg%Id
    END FUNCTION
  END MODULE


  PROGRAM ExecSeq1
  USE M
  IMPLICIT NONE

  TYPE(DT(4)), TARGET   ::  DTV(3,3,3)
  CLASS(DT(4)), POINTER :: Ptr(:,:,:)
  INTEGER :: S(3)=(/1,2,3/), I, J

    Ptr => Dtv

    SELECT TYPE (U => Ptr(S,S,S))
    CLASS DEFAULT

    associate(a=>1)

      SELECT TYPE (U => U )
      CLASS DEFAULT

    end associate

      END SELECT

    END SELECT


  END


