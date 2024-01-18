! GB DTP extension using:
! ftcx_dtp -qck -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/VarDefContext1.f
! opt variations: -qnock -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp VarDefContext1.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  VarDefContext1 
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
!*   
!*  Diagnosis on varriable deinition context 
!* 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE, ABSTRACT :: DT0(K1,K2,N1)    ! (8,1,4)
      INTEGER, KIND             :: K1,K2
      INTEGER, LEN              :: N1
      INTEGER(K1)               :: IArr(2)
      CHARACTER(kind=K2,len=N1) :: C=""
      CONTAINS
      PROCEDURE, PASS(Obj)   :: GetInt
    END TYPE

    TYPE, EXTENDS(DT0) :: DT    ! (8,1,4)
      CLASS(DT(K1,K2,:)), POINTER :: Ptr(:,:)=>NULL()
    END TYPE
  
  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0(8,1,*)), INTENT(IN)    :: Obj 
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt 
      GetInt = Obj%IArr(Num) 
    END FUNCTION

  END MODULE


  PROGRAM  VarDefContext1 
  USE M
  IMPLICIT NONE

   
  CLASS(DT0(8,1,:)), POINTER :: Ptr(:,:)
  INTEGER :: S(2)=(/1,2/), I=1, J=2
  TYPE(DT(8,1,4)) :: U(2,2)=DT(8,1,4)(IARR=-1)

  ALLOCATE(Ptr(2,2), SOURCE=DT(8,1,4)(IArr=(/1_8, 2_8/)))
     
    I = 1
    SELECT TYPE (Ptr => Ptr(S,:))
    CLASS DEFAULT
      STOP 40
    CLASS IS (DT0(8,1,*))
      STOP 41
    CLASS IS (DT(8,1,*))

        IF (ANY(Ptr(:,:)%IArr(1)  .NE. 1)) STOP 22
        IF (ANY(Ptr(:,:)%IArr(2)  .NE. 2)) STOP 23
        IF (ANY(Ptr(:,:)%GetInt(1).NE. 1)) STOP 24
        IF (ANY(Ptr(:,:)%GetInt(2).NE. 2)) STOP 25
  
        CALL Sub(Ptr%C)

    END SELECT  

  CONTAINS

  SUBROUTINE  Sub(Arg)
    CLASS(*), INTENT(INOUT) ::  Arg(:,:)
    
    SELECT TYPE (Arg)
    TYPE IS (CHARACTER(*))
      Arg="111"
    CLASS DEFAULT
      STOP 111
    END SELECT 

  END SUBROUTINE

  END


