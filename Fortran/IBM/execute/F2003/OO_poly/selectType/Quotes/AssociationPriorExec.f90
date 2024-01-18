! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: AssociationPriorExec.f 
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
!*  TEST CASE NAME             : AssociationPriorExec
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
!*  The association is established prior to execution of
!*  the block
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
      CLASS(DT), POINTER :: Ptr(:,:)=>NULL()
    END TYPE
  
  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0), INTENT(IN)    :: Obj 
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt 
      GetInt = Obj%IArr(Num) 
    END FUNCTION

  END MODULE


  PROGRAM AssociationPriorExec
  USE M
  IMPLICIT NONE
 
  CLASS(DT0), POINTER :: Ptr(:,:),S1(:,:),S2(:,:)
  INTEGER :: S(2)=(/1,2/), I=1, J=2
  TYPE(DT) :: U(2,2)=DT(IARR=-1)

    ALLOCATE(Ptr(2,2), SOURCE=DT(IArr=(/1_8, 2_8/)))
    ALLOCATE(S1(2,2), SOURCE=DT(IArr=(/3_8, 4_8/)))
    ALLOCATE(S2(2,2), SOURCE=DT(IArr=(/5_8, 6_8/)))
     
    I = 1
    SELECT TYPE (Ptr => Ptr(I,:))
    CLASS DEFAULT
      STOP 40
    CLASS IS (DT0)
      STOP 41
    CLASS IS (DT)
        IF (ANY(Ptr(:)%IArr(1)  .NE. 1)) STOP 22
        IF (ANY(Ptr(:)%IArr(2)  .NE. 2)) STOP 23
        IF (ANY(Ptr(:)%GetInt(1).NE. 1)) STOP 24
        IF (ANY(Ptr(:)%GetInt(2).NE. 2)) STOP 25

        Ptr%IArr(1) = -1 
        Ptr%IArr(2) = -2 

        IF (SIZE(Ptr(2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(Ptr(2)%IArr)   .NE. 8)  STOP 31
        IF (ANY(Ptr(:)%IArr(1)  .NE. -1)) STOP 32
        IF (ANY(Ptr(:)%IArr(2)  .NE. -2)) STOP 33
        IF (ANY(Ptr(:)%GetInt(1).NE. -1)) STOP 34
        IF (ANY(Ptr(:)%GetInt(2).NE. -2)) STOP 35

        I = 2
        
        IF (ANY(Ptr(:)%IArr(1)  .NE. -1)) STOP 52
        IF (ANY(Ptr(:)%IArr(2)  .NE. -2)) STOP 53
        IF (ANY(Ptr(:)%GetInt(1).NE. -1)) STOP 54
        IF (ANY(Ptr(:)%GetInt(2).NE. -2)) STOP 55

    END SELECT  

        IF (ANY(Ptr(I,:)%IArr(1)  .NE. 1)) STOP 62
        IF (ANY(Ptr(I,:)%IArr(2)  .NE. 2)) STOP 63
        IF (ANY(Ptr(I,:)%GetInt(1).NE. 1)) STOP 64
        IF (ANY(Ptr(I,:)%GetInt(2).NE. 2)) STOP 65
 
  END


