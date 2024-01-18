! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/associate/CrossFeatures/FinalIntentOut.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=self -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  FinalIntentOut.f
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
!*  TEST CASE NAME             : FinalIntentOut
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 14, 2005
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
!*   The intent(out) attribute 
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 
  MODULE M

    INTEGER :: INDEX=0
    INTEGER :: Fin(6)=0

    TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CONTAINS
      FINAL :: FinalBase
    END TYPE

    TYPE, EXTENDS(Base) :: DT(K2,N2,K3)    ! (4,20,4,3,4)
      INTEGER, KIND :: K2,K3
      INTEGER, LEN  :: N2
      INTEGER(K2)   :: Id = 0
      CHARACTER(N2) :: C  = " "
      LOGICAL(K3)   :: L  = .FALSE.
 
      CONTAINS
      PROCEDURE, PASS   :: GetId
      PROCEDURE, PASS   :: GetC
      PROCEDURE, PASS   :: GetL

      FINAL :: Final
      FINAL :: FinalArr

    END TYPE

  CONTAINS

    SUBROUTINE FinalBase(Obj)
    TYPE(Base(4,*)) :: Obj
      Index = Index + 1
      Fin(Index) = 1
!     PRINT *, "Base Finalization"
    END SUBROUTINE
 
    SUBROUTINE Final(Obj)
    TYPE(DT(4,*,4,*,4)) :: Obj
      Index = Index + 1
      Fin(Index) = 2
!     PRINT *, "DT Finalization"
    END SUBROUTINE
 
    SUBROUTINE FinalArr(ObjArr)
    TYPE(DT(4,*,4,*,4)) :: ObjArr(:)
      Index = Index + 1
      Fin(Index) = 3
!     PRINT *, "DT ARR Finalization"
    END SUBROUTINE
 
    ELEMENTAL FUNCTION GetId(Arg)
    IMPLICIT CLASS(DT(4,*,4,*,4))(A) 
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

    ELEMENTAL FUNCTION GetC(Arg)
    IMPLICIT CLASS(DT(4,*,4,*,4))(A) 
    INTENT(IN) :: Arg
    CHARACTER  :: GetC
      GetC = Arg%C
    END FUNCTION

    ELEMENTAL FUNCTION GetL(Arg)
    IMPLICIT CLASS(DT(4,*,4,*,4))(A) 
    INTENT(IN) :: Arg
    LOGICAL    :: GetL
      GetL = Arg%L
    END FUNCTION

  END MODULE
 
  PROGRAM FinalIntentOut
  USE M
  IMPLICIT NONE 

  TYPE(DT(4,20,4,3,4)), TARGET :: V =  DT(4,20,4,3,4)(ID=-1, C="!", L=.TRUE.) 

  Fin = -1 

  ASSOCIATE ( As => Fun(V) ) 

    !FINALIZATION Finishes after evaluation of the selector
    IF ( ANY(Fin .NE. (/2,1,2,1,-1,-1/) ) ) STOP 79

    Fin = -1 
    ASSOCIATE ( As => As ) 

      IF ( As%ID       .NE. -2 ) STOP 20
      IF ( As%GetID()  .NE. -2 ) STOP 21

      IF ( As%C       .NE. "2" ) STOP 30
      IF ( As%GetC()  .NE. "2" ) STOP 31

      IF ( As%L       .NEQV. .TRUE. ) STOP 40
      IF ( As%GetL()  .NEQV. .TRUE. ) STOP 41
    
    END ASSOCIATE

    IF ( ANY(Fin .NE. -1 ) ) STOP 89 !  no finalization happen

    INDEX = 1
    Fin ( Index ) = 0  ! Finalization is about to start
!   PRINT *, "Finalization starts"
  END ASSOCIATE

  !FINALIZATION Finishes
! PRINT *, "Finalization finished"
! PRINT *, Fin
  IF ( ANY(Fin .NE. (/0,-1,-1,-1,-1,-1/) ) ) STOP 99
  ! no finalization happen for a return of a pointer

  Fin = -1 
  Index = 0
  ASSOCIATE ( As => V ) 

    ASSOCIATE ( As => Fun(As) ) 

      !FINALIZATION Finishes after evaluation of the selector
      IF ( ANY(Fin .NE. (/2,1,2,1,-1,-1/) ) ) STOP 119 

      IF ( As%ID       .NE. -2 ) STOP 50
      IF ( As%GetID()  .NE. -2 ) STOP 51

      IF ( As%C       .NE. "2" ) STOP 60
      IF ( As%GetC()  .NE. "2" ) STOP 61

      IF ( As%L       .NEQV. .TRUE. ) STOP 70
      IF ( As%GetL()  .NEQV. .TRUE. ) STOP 71

    END ASSOCIATE
   
    !FINALIZATION Finishes
!   PRINT *, "Finalization finished"
    IF ( ANY(Fin .NE. (/2,1,2,1,-1,-1/) ) ) STOP 89 
    ! no finalization happen for a return of a pointer

    Fin = -1
    INDEX = 1
    Fin ( Index ) = 0  

  END ASSOCIATE

  IF ( ANY(Fin .NE. (/0,-1,-1,-1,-1,-1/) ) ) STOP 109
  ! no finalization happen for a return of a pointer

  CONTAINS

  FUNCTION Fun(Arg)
  TYPE(DT(4,*,4,*,4)), INTENT(OUT) :: Arg
  CLASS(DT(4,:,4,:,4)), POINTER :: Fun
!   print*, "in fun"
    ALLOCATE(Fun, SOURCE= DT(4,20,4,3,4)(ID=-2, C="2", L=.TRUE.)) 
!   print*, "out fun"
  END FUNCTION


  END 

