! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  AssocNameComBlk.f
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
!*  TEST CASE NAME             : AssocNameComBlk
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 28, 2005
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
!*  Common block name 
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
  
    TYPE(DT), TARGET   ::  DTV(3,3,3)
    
  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0), INTENT(IN)    :: Obj 
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt 
      GetInt = Obj%IArr(Num) 
    END FUNCTION

  END MODULE


  PROGRAM AssocNameComBlk
  USE M
  IMPLICIT NONE
 
  CLASS(DT0), POINTER :: PTR(:,:,:)
  INTEGER :: S(2), I, J
  TYPE(DT) :: U(2,2,2)=DT(IARR=-1)
  COMMON /CB/S,I,J

    ALLOCATE(Ptr(2,2,2), SOURCE=DT(IArr=(/1_8, 2_8/)))

    SELECT TYPE (As => Ptr(I:,:J,I:J))
    CLASS DEFAULT

    SELECT TYPE (CB => As )
    CLASS IS (DT0)
      STOP 20 
    CLASS IS (DT)
        IF (ANY(CB(:,:,:)%IArr(1)  .NE. 1)) STOP 22
        IF (ANY(CB(:,:,:)%IArr(2)  .NE. 2)) STOP 23
        IF (ANY(CB(:,:,:)%GetInt(1).NE. 1)) STOP 24
        IF (ANY(CB(:,:,:)%GetInt(2).NE. 2)) STOP 25

        Ptr%IArr(1) = -1
        Ptr%IArr(2) = -2

        IF (SIZE(CB(2,2,2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(CB(2,2,2)%IArr)   .NE. 8)  STOP 31
        IF (ANY(CB(:,:,:)%IArr(1)  .NE. -1)) STOP 32
        IF (ANY(CB(:,:,:)%IArr(2)  .NE. -2)) STOP 33
        IF (ANY(CB(:,:,:)%GetInt(1).NE. -1)) STOP 34
        IF (ANY(CB(:,:,:)%GetInt(2).NE. -2)) STOP 35

    END SELECT
    END SELECT


  END

  BLOCK DATA INIT
  INTEGER :: S(2), I, J 
  COMMON /CB/S, I, J 
  DATA S /1,2/, I /1/, J /2/ 
  END BLOCK DATA INIT

