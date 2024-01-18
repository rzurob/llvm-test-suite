! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/selectType/Quotes/AssocNameProc.f
! opt variations: -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  AssocNameProc.f
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
!*  TEST CASE NAME             : AssocNameProc
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
!*  Procedure entities' name 
!* 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE, ABSTRACT :: DT0(K1)    ! (8)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: IArr(2) 
      CONTAINS
      PROCEDURE, PASS(Obj)   :: GetInt
    END TYPE

    TYPE, EXTENDS(DT0) :: DT    ! (8)
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0(8)), INTENT(IN)    :: Obj 
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt 
      GetInt = Obj%IArr(Num) 
    END FUNCTION

  END MODULE


  PROGRAM AssocNameProc
  USE M
  IMPLICIT NONE

  TYPE(DT(8)), TARGET   ::  DTV(3,3,3)
  CLASS(DT0(8)), POINTER :: PTR(:,:,:)
  INTEGER :: S(2)=(/1,2/), I, J
  TYPE(DT(8)) :: U(2,2,2)=DT(8)(IARR=-1)

    ALLOCATE(Ptr(2,2,2), SOURCE=DT(8)(IArr=(/1_8, 2_8/)))

    ASSOCIATE ( REAL => U )

    SELECT TYPE (REAL => Ptr(:,:,:))
    CLASS IS (DT0(8))
      STOP 20 
    CLASS IS (DT(8))
      ASSOCIATE (GetInt => REAL)

        IF (ANY(GetInt(:,:,:)%IArr(1)  .NE. 1)) STOP 22
        IF (ANY(GetInt(:,:,:)%IArr(2)  .NE. 2)) STOP 23
        IF (ANY(GetInt(:,:,:)%GetInt(1).NE. 1)) STOP 24
        IF (ANY(GetInt(:,:,:)%GetInt(2).NE. 2)) STOP 25

        Ptr%IArr(1) = -1
        Ptr%IArr(2) = -2

        IF (SIZE(GetInt(2,2,2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(GetInt(2,2,2)%IArr)   .NE. 8)  STOP 31
        IF (ANY(GetInt(:,:,:)%IArr(1)  .NE. -1)) STOP 32
        IF (ANY(GetInt(:,:,:)%IArr(2)  .NE. -2)) STOP 33
        IF (ANY(GetInt(:,:,:)%GetInt(1).NE. -1)) STOP 34
        IF (ANY(GetInt(:,:,:)%GetInt(2).NE. -2)) STOP 35

        DTV = GetInt(2,2,2)
        PRINT*, DTV 
      END ASSOCIATE
    END SELECT

    END ASSOCIATE

  END

  SUBROUTINE REAL()
!   PRINT*, "OK"
  END SUBROUTINE

