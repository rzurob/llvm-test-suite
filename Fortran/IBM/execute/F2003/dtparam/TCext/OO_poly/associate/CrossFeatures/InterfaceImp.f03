! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_poly/associate/CrossFeatures/InterfaceImp.f
! opt variations: -qnock -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 10, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*   The implicit interface
!*    (ICE-301017)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1,K2,N1,K3)    ! (4,1,3,4)
      INTEGER, KIND             :: K1,K2,K3
      INTEGER, LEN              :: N1
      INTEGER(K1)               :: Id = 0
      CHARACTER(kind=K2,len=N1) :: C  = " "
      LOGICAL(K3)               :: L  = .FALSE.

      CONTAINS
      PROCEDURE, PASS   :: GetId
      PROCEDURE, PASS   :: GetC
      PROCEDURE, PASS   :: GetL
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    IMPLICIT CLASS(DT(4,1,*,4))(A)
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

    ELEMENTAL FUNCTION GetC(Arg)
    IMPLICIT CLASS(DT(4,1,*,4))(A)
    INTENT(IN) :: Arg
    CHARACTER  :: GetC
      GetC = Arg%C
    END FUNCTION

    ELEMENTAL FUNCTION GetL(Arg)
    IMPLICIT CLASS(DT(4,1,*,4))(A)
    INTENT(IN) :: Arg
    LOGICAL    :: GetL
      GetL = Arg%L
    END FUNCTION

  END MODULE

  PROGRAM InterfaceImp

  USE M
  IMPLICIT TYPE(DT(4,1,3,4))(F)

  interface
    FUNCTION Fun(Arg)
      import
      TYPE(DT(4,1,*,4)) :: Arg
      TYPE(DT(4,1,3,4)) :: Fun
    end function
  end interface

  ASSOCIATE ( As => (/ Fun(DT(4,1,3,4)(ID=-1, C="111", L=.TRUE.)), &
            &          Fun(DT(4,1,3,4)(ID=-2, C="222", L=.TRUE.)), &
            &          Fun(DT(4,1,3,4)(ID=-3, C="333", L=.TRUE.))  /) )

    IF ( Any(As%ID      .NE. (/-1,-2,-3/)) ) ERROR STOP 20
    IF ( ANY(As%GetID() .NE. (/-1,-2,-3/)) ) ERROR STOP 21

    IF ( Any(As%C      .NE. (/"111","222","333"/)) ) ERROR STOP 30
    IF ( ANY(As%GetC() .NE. (/"1","2","3"/)) )       ERROR STOP 31

    IF ( Any(As%L      .NEQV. .TRUE.) ) ERROR STOP 60
    IF ( ANY(As%GetL() .NEQV. .TRUE.) ) ERROR STOP 61


  END ASSOCIATE


  END

  FUNCTION Fun(Arg)
  USE M
  TYPE(DT(4,1,*,4)) :: Arg
  TYPE(DT(4,1,3,4)) :: Fun
    Fun = Arg
  END FUNCTION


