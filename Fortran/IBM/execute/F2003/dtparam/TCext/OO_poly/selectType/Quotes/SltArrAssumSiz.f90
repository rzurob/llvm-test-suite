! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltArrAssumSiz.f
! opt variations: -qnok -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltArrAssumSiz.f
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
!*  TEST CASE NAME             : SltArrAssumSiz
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 07, 2005
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
!*   The selector is an assumed size  array 
!*    (Failed at stop 44)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE 

    TYPE, EXTENDS(Zero)  :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4,*)) :: Arg(:,:,:)
      Arg(:,:,:)%BaseId =  -Arg(:,:,:)%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4,*))  :: Arg(:,:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4,*))
          Arg(:,:,:)%ChildId = -Arg(:,:,:)%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrAssumSiz
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20)) :: Arr(18, 18,18)

  CALL Sub(Arr)

  CONTAINS

  SUBROUTINE Sub(Arr)
  CLASS(*) :: Arr(2:19, 2:19, 2:*)

  SELECT TYPE ( Ptr => Arr )
    CLASS DEFAULT
      SELECT TYPE (Arr)
      CLASS IS (Zero(4,*))
      SELECT TYPE (Ptr => Arr)
        TYPE IS (Child(4,*))
          IF ( ANY (LBOUND(Ptr)   .NE. (/2, 2, 2/) ) ) STOP 30
          IF ( UBOUND(Ptr, 1)     .NE. 19 )             STOP 31
          IF ( UBOUND(Ptr, 2)     .NE. 19 )             STOP 32

          IF ( ANY(Ptr(:,:,2)%Base%GetId() .NE. 1) ) STOP 34
          IF ( ANY(Ptr(:,:,2)%GetId()      .NE. 2) ) STOP 35
          IF ( ANY(Ptr(:,:,2)%BaseId       .NE. 1) ) STOP 36
          IF ( ANY(Ptr(:,:,2)%ChildId      .NE. 2) ) STOP 37


          CALL Ptr(2,2,2)%SetId(Ptr(:,:,2:8))
          CALL Ptr(2,2,2)%Base%SetId(Ptr(:,:,2:8)%Base)

          IF ( ANY(Ptr(:,:,2)%Base%GetId() .NE. -1 ) ) STOP 44
          IF ( ANY(Ptr(:,:,2)%GetId()      .NE. -2 ) ) STOP 45
          IF ( ANY(Ptr(:,:,2)%BaseId       .NE. -1 ) ) STOP 46
          IF ( ANY(Ptr(:,:,2)%ChildId      .NE. -2 ) ) STOP 47

       CLASS DEFAULT
          STOP 40
        CLASS is (Child(4,*))
          STOP 56
        TYPE is (Base(4,*))
          STOP 57
        TYPE IS (Zero(4,*))
          STOP 58
      END SELECT
      END SELECT

  END SELECT

  END SUBROUTINE

  END
  
