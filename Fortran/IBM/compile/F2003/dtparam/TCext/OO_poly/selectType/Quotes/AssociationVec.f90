! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_poly/selectType/Quotes/AssociationVec.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

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
! %POSTCMD: tcomp  AssociationVec.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : AssociationVec
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
!*  The selector with a vector subscript 
!* 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE, ABSTRACT :: DT0(N1,K1)    ! (20,8)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: IArr(2) 
      CONTAINS
      PROCEDURE, PASS(Obj)   :: GetInt
    END TYPE

    TYPE, EXTENDS(DT0) :: DT(K2,N2,K3)    ! (20,8,4,20,8)
      INTEGER, KIND                    :: K2,K3
      INTEGER, LEN                     :: N2
      CLASS(DT(:,K3,K2,:,K3)), POINTER :: Ptr(:,:)=>NULL()
    END TYPE
  
  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0(*,8)), INTENT(IN)    :: Obj 
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt 
      GetInt = Obj%IArr(Num) 
    END FUNCTION

  END MODULE


  PROGRAM AssociationPtr
  USE M
  IMPLICIT NONE
 
  CLASS(DT(:,8,4,:,8)), POINTER :: Ptr(:,:)
  INTEGER :: S(2)=(/1,2/), I=1, J=2
  TYPE(DT(20,8,4,20,8)) :: U(2,2)=DT(20,8,4,20,8)(IARR=-1)

    ALLOCATE(Ptr(2,2), SOURCE=DT(20,8,4,20,8)(IArr=(/1_8, 2_8/)))
     
     Ptr(1,1)%Ptr => Ptr
     Ptr(1,2)%Ptr => Ptr
     Ptr(2,1)%Ptr => Ptr
     Ptr(2,2)%Ptr => Ptr
  
S1: SELECT TYPE (S2 => Ptr(1,1)%Ptr(1,1)%Ptr(S,S))
    CLASS DEFAULT

S2: SELECT TYPE (Ptr => S2 )
  ! CLASS IS (DT0)
  !   STOP 20 
    CLASS IS (DT(*,8,4,*,8))
        IF (ANY(Ptr(:,:)%IArr(1)  .NE. 1)) STOP 22
        IF (ANY(Ptr(:,:)%IArr(2)  .NE. 2)) STOP 23
        IF (ANY(Ptr(:,:)%GetInt(1).NE. 1)) STOP 24
        IF (ANY(Ptr(:,:)%GetInt(2).NE. 2)) STOP 25

        Ptr%IArr(1) = -1 !Bad
        Ptr%IArr(2) = -2 !Bad

        IF (SIZE(Ptr(2,2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(Ptr(2,2)%IArr)   .NE. 8)  STOP 31
        IF (ANY(Ptr(:,:)%IArr(1)  .NE. -1)) STOP 32
        IF (ANY(Ptr(:,:)%IArr(2)  .NE. -2)) STOP 33
        IF (ANY(Ptr(:,:)%GetInt(1).NE. -1)) STOP 34
        IF (ANY(Ptr(:,:)%GetInt(2).NE. -2)) STOP 35


    END SELECT S2
    END SELECT S1

  END


