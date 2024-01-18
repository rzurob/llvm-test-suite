! GB DTP extension using:
! ftcx_dtp -qreuse=self /tstdev/OO_poly/associate/CrossFeatures/InterfaceElem.f
! opt variations: -qck -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  InterfaceElem.f
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
!*  TEST CASE NAME             : InterfaceElem
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
!*   The elemental function 
!*    () 
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
 
  PROGRAM InterfaceElem

  USE M
  IMPLICIT TYPE(DT(4,*))(F) 

  TYPE(DT(4,3)) :: V(128) =  DT(4,3)(ID=-1, C="!", L=.TRUE.) 

  INTERFACE Fun
    ELEMENTAL FUNCTION Fun1(Arg)
    IMPORT DT
    IMPLICIT TYPE(DT(4,3))(F)
    INTEGER, INTENT(IN) :: Arg
    END FUNCTION

    ELEMENTAL FUNCTION Fun2(Arg)
    IMPORT DT
    TYPE(DT(4,*)), INTENT(IN) :: Arg
    TYPE(DT(4,3))             :: Fun2
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
  TYPE(DT(4,3)) :: Fun1
  INTEGER, INTENT(IN)  :: Arg
    Fun1 = DT(4,3)(ID=-4, C="4", L=.TRUE.) 
  END FUNCTION

  ELEMENTAL FUNCTION Fun2(Arg)
  USE M 
  TYPE(DT(4,*)), INTENT(IN) :: Arg
  TYPE(DT(4,3)) :: Fun2
    Fun2 = Arg
  END FUNCTION

