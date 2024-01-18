! GB DTP extension using:
! ftcx_dtp -qck -qreuse=self /tstdev/OO_poly/associate/CrossFeatures/InterfaceGen.f
! opt variations: -qnock -qreuse=none

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
!*   The generic interface
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1,K2,N1)    ! (4,1,3)
      INTEGER, KIND             :: K1,K2
      INTEGER, LEN              :: N1
      INTEGER(K1)               :: Id = 0
      CHARACTER(kind=K2,len=N1) :: C  = " "
      LOGICAL(K1)               :: L  = .FALSE.

      CONTAINS
      PROCEDURE, PASS   :: GetId
      PROCEDURE, PASS   :: GetC
      PROCEDURE, PASS   :: GetL
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    IMPLICIT CLASS(DT(4,1,*))(A)
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

    ELEMENTAL FUNCTION GetC(Arg)
    IMPLICIT CLASS(DT(4,1,*))(A)
    INTENT(IN) :: Arg
    CHARACTER  :: GetC
      GetC = Arg%C
    END FUNCTION

    ELEMENTAL FUNCTION GetL(Arg)
    IMPLICIT CLASS(DT(4,1,*))(A)
    INTENT(IN) :: Arg
    LOGICAL    :: GetL
      GetL = Arg%L
    END FUNCTION

  END MODULE

  PROGRAM InterfaceGen

  USE M
  IMPLICIT TYPE(DT(4,1,3))(F)

  INTERFACE Fun
    FUNCTION Fun1()
    IMPORT DT
    IMPLICIT TYPE(DT(4,1,3))(F)
    END FUNCTION

    FUNCTION Fun2(Arg)
    IMPORT DT
    TYPE(DT(4,1,*)) :: Arg
    TYPE(DT(4,1,3)) :: Fun2
    END FUNCTION
  END INTERFACE

  ASSOCIATE ( As => Fun(Fun()) )

    IF ( As%ID       .NE. -1 ) STOP 20
    IF ( As%GetID()  .NE. -1 ) STOP 21

    IF ( As%C       .NE. "!" ) STOP 30
    IF ( As%GetC()  .NE. "!" ) STOP 31

    IF ( As%L       .NEQV. .TRUE. ) STOP 60
    IF ( As%GetL()  .NEQV. .TRUE. ) STOP 61


  END ASSOCIATE


  END

  FUNCTION Fun1()
  USE M
  TYPE(DT(4,1,3)) :: Fun1
    Fun1 = DT(4,1,3)(ID=-1, C="!", L=.TRUE.)
  END FUNCTION

  FUNCTION Fun2(Arg)
  USE M
  TYPE(DT(4,1,*)) :: Arg
  TYPE(DT(4,1,3)) :: Fun2
    Fun2 = Arg
  END FUNCTION

