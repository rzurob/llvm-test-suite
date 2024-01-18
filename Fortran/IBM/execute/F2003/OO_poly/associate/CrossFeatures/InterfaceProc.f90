! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  InterfaceProc.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InterfaceProc
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
!*   The interface procedure
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER      :: Id = 0
      CHARACTER(3) :: C  = " "
      LOGICAL      :: L  = .FALSE.

      CONTAINS
      PROCEDURE, PASS   :: GetId
      PROCEDURE, PASS   :: GetC
      PROCEDURE, PASS   :: GetL
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    IMPLICIT CLASS(DT)(A)
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

    ELEMENTAL FUNCTION GetC(Arg)
    IMPLICIT CLASS(DT)(A)
    INTENT(IN) :: Arg
    CHARACTER  :: GetC
      GetC = Arg%C
    END FUNCTION

    ELEMENTAL FUNCTION GetL(Arg)
    IMPLICIT CLASS(DT)(A)
    INTENT(IN) :: Arg
    LOGICAL    :: GetL
      GetL = Arg%L
    END FUNCTION

  END MODULE

  PROGRAM InterfaceProc

  USE M
  IMPLICIT TYPE(DT)(F)

  TYPE(DT) :: V(128) =  DT(ID=-1, C="!", L=.TRUE.)

  INTERFACE Fun
    ELEMENTAL FUNCTION Fun1(Arg)
    IMPORT DT
    IMPLICIT TYPE(DT)(F)
    INTEGER, INTENT(IN) :: Arg
    END FUNCTION

    ELEMENTAL FUNCTION Fun2(Arg)
    IMPORT DT
    TYPE(DT), INTENT(IN) :: Arg
    TYPE(DT)             :: Fun2
    END FUNCTION
  END INTERFACE

  ASSOCIATE ( As => Fun(Fun(V)) )

    IF ( ANY(LBOUND(As) .NE. (/1/)) )   STOP 40
    IF ( ANY(SHAPE(As)  .NE. (/128/)) ) STOP 41

    IF ( ANY(As%ID       .NE. -1 )) STOP 20
    IF ( ANY(As%GetID()  .NE. -1 )) STOP 21

    IF ( ANY(As%C       .NE. "!" )) STOP 30
    IF ( ANY(As%GetC()  .NE. "!" )) STOP 31

    IF ( ANY(As%L       .NEQV. .TRUE. )) STOP 60
    IF ( ANY(As%GetL()  .NEQV. .TRUE. )) STOP 61


  END ASSOCIATE


  END

  ELEMENTAL FUNCTION Fun1(Arg)
  USE M
  TYPE(DT) :: Fun1
  INTEGER, INTENT(IN)  :: Arg
    Fun1 = DT(ID=-4, C="4", L=.TRUE.)
  END FUNCTION

  ELEMENTAL FUNCTION Fun2(Arg)
  USE M
  TYPE(DT), INTENT(IN) :: Arg
  TYPE(DT) :: Fun2
    Fun2 = Arg
  END FUNCTION

