! GB DTP extension using:
! ftcx_dtp -qreuse=self /tstdev/OO_poly/associate/CrossFeatures/ProcedureStmt.f
! opt variations: -qck -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ProcedureStmt.f
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ProcedureStmt 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 10, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*   The procedure stmt 
!*    (Comp failed) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 
  MODULE M
    TYPE :: DT(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id = 0
      CHARACTER(N1) :: C  = " "
      LOGICAL(K1)   :: L  = .FALSE.
 
      CONTAINS
      PROCEDURE, PASS   :: GetId
      PROCEDURE, PASS   :: GetC
      PROCEDURE, PASS   :: GetL
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    IMPLICIT CLASS(DT(4,*))(A) 
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

    ELEMENTAL FUNCTION GetC(Arg)
    IMPLICIT CLASS(DT(4,*))(A) 
    INTENT(IN) :: Arg
    CHARACTER  :: GetC
      GetC = Arg%C
    END FUNCTION

    ELEMENTAL FUNCTION GetL(Arg)
    IMPLICIT CLASS(DT(4,*))(A) 
    INTENT(IN) :: Arg
    LOGICAL    :: GetL
      GetL = Arg%L
    END FUNCTION

  END MODULE
 
  PROGRAM ProcedureStmt 

  USE M
  IMPLICIT TYPE(DT(4,3))(F) 

  TYPE(DT(4,3)) :: V =  DT(4,3)(ID=-1, C="!", L=.TRUE.) 

  INTERFACE 
    FUNCTION Func1(Arg)
    IMPORT DT
    IMPLICIT TYPE(DT(4,3))(F)
    INTEGER, INTENT(IN) :: Arg
    END FUNCTION

    FUNCTION Func2(Arg)
    IMPORT DT
    TYPE(DT(4,*)), INTENT(IN) :: Arg
    TYPE(DT(4,3))             :: Func2
    END FUNCTION 
  END INTERFACE

  PROCEDURE(Func1) :: Fun1
  PROCEDURE(Func2) :: Fun2

  CALL Sub(Fun2)

  CONTAINS

  SUBROUTINE Sub(Arg)
  PROCEDURE(Fun2) :: Arg

  ASSOCIATE ( As => Arg(V) ) 

    IF ( As%ID       .NE. -1 ) STOP 20
    IF ( As%GetID()  .NE. -1 ) STOP 21

    IF ( As%C       .NE. "!" ) STOP 30
    IF ( As%GetC()  .NE. "!" ) STOP 31

    IF ( As%L       .NEQV. .TRUE. ) STOP 60
    IF ( As%GetL()  .NEQV. .TRUE. ) STOP 61


  END ASSOCIATE

  END SUBROUTINE

  END 

  FUNCTION Fun1(Arg)
  USE M
  TYPE(DT(4,3)) :: Fun1
  INTEGER, INTENT(IN)  :: Arg
    Fun1 = DT(4,3)(ID=-4, C="4", L=.TRUE.) 
  END FUNCTION

  FUNCTION Fun2(Arg)
  USE M 
  TYPE(DT(4,*)), INTENT(IN) :: Arg
  TYPE(DT(4,3)) :: Fun2
    Fun2 = Arg
  END FUNCTION

