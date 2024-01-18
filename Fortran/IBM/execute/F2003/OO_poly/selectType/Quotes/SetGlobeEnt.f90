! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  SetGlobeEnt.f
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
!*  TEST CASE NAME             : SetGlobeEnt 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 27, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     
!*  Set globe entities 
!* 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE, ABSTRACT :: DT0
      INTEGER(8) :: IArr(2) 
      CONTAINS
      PROCEDURE, PASS(Obj)   :: GetInt
    END TYPE

    TYPE, EXTENDS(DT0) :: DT
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0), INTENT(IN)    :: Obj 
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt 
      GetInt = Obj%IArr(Num) 
    END FUNCTION

  END MODULE


  PROGRAM SetGlobeEnt 
  USE M
  IMPLICIT NONE

  TYPE(DT), TARGET   ::  DTV(3,3,3)
  CLASS(DT0), POINTER :: PTR(:,:,:)
  INTEGER :: S(2)=(/1,2/), I, J
  TYPE(DT) :: U(2,2,2)=DT(IARR=-1)

    ALLOCATE(Ptr(2,2,2), SOURCE=DT(IArr=(/1_8, 2_8/)))

    ASSOCIATE ( U => U )

    SELECT TYPE (U => Ptr(:,:,:))
    CLASS IS (DT0)
      STOP 20 
    CLASS IS (DT)
      ASSOCIATE (U => U)

        IF (ANY(U(:,:,:)%IArr(1)  .NE. 1)) STOP 22
        IF (ANY(U(:,:,:)%IArr(2)  .NE. 2)) STOP 23
        IF (ANY(U(:,:,:)%GetInt(1).NE. 1)) STOP 24
        IF (ANY(U(:,:,:)%GetInt(2).NE. 2)) STOP 25

        Ptr%IArr(1) = -1
        Ptr%IArr(2) = -2

        IF (SIZE(U(2,2,2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(U(2,2,2)%IArr)   .NE. 8)  STOP 31
        IF (ANY(U(:,:,:)%IArr(1)  .NE. -1)) STOP 32
        IF (ANY(U(:,:,:)%IArr(2)  .NE. -2)) STOP 33
        IF (ANY(U(:,:,:)%GetInt(1).NE. -1)) STOP 34
        IF (ANY(U(:,:,:)%GetInt(2).NE. -2)) STOP 35
     
      END ASSOCIATE
    END SELECT

    END ASSOCIATE

  END


