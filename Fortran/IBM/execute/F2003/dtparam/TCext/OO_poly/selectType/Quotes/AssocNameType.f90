! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/AssocNameType.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  AssocNameType.f
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
!*  TEST CASE NAME             : AssocNameType
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
!*  Type 
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

    TYPE, EXTENDS(DT0) :: DT    ! (20,8)
    END TYPE
  
    TYPE(DT(20,8)), TARGET   ::  DTV(3,3,3)
    
  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0(*,8)), INTENT(IN)    :: Obj 
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt 
      GetInt = Obj%IArr(Num) 
    END FUNCTION

  END MODULE


  PROGRAM AssocNameType
  USE M
  IMPLICIT NONE
 
  CLASS(DT0(:,8)), POINTER :: PTR(:,:,:)
  INTEGER :: S(2)=(/1,2/), I=1, J=2
  TYPE(DT(20,8)) :: U(2,2,2)=DT(20,8)(IARR=-1)
  NAMELIST /NL/S,I,J
  CHARACTER(100) :: C(3)

    ALLOCATE(Ptr(2,2,2), SOURCE=DT(20,8)(IArr=(/1_8, 2_8/)))

S1: SELECT TYPE (NL => Ptr(I:,:J,I:J))
    CLASS DEFAULT

S2: SELECT TYPE (DT => NL )
    CLASS IS (DT0(*,8))
      STOP 20 
    CLASS IS (DT(*,8))
        IF (ANY(DT(:,:,:)%IArr(1)  .NE. 1)) STOP 22
        IF (ANY(DT(:,:,:)%IArr(2)  .NE. 2)) STOP 23
        IF (ANY(DT(:,:,:)%GetInt(1).NE. 1)) STOP 24
        IF (ANY(DT(:,:,:)%GetInt(2).NE. 2)) STOP 25

        Ptr%IArr(1) = -1
        Ptr%IArr(2) = -2

        IF (SIZE(DT(2,2,2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(DT(2,2,2)%IArr)   .NE. 8)  STOP 31
        IF (ANY(DT(:,:,:)%IArr(1)  .NE. -1)) STOP 32
        IF (ANY(DT(:,:,:)%IArr(2)  .NE. -2)) STOP 33
        IF (ANY(DT(:,:,:)%GetInt(1).NE. -1)) STOP 34
        IF (ANY(DT(:,:,:)%GetInt(2).NE. -2)) STOP 35

    END SELECT S2
    END SELECT S1

    WRITE(C, NML=NL)
    PRINT*, C

  END


