! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltArrHostULAlloc.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltArrHostULAlloc.f
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
!*  TEST CASE NAME             : SltArrHostAlloc
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 19, 2005
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
!*   The selector is a host associate name associating to an unlimited 
!*   poly allocatable array
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(K1)    ! (4)
        INTEGER, KIND :: K1
    CONTAINS
      PROCEDURE, NoPASS   :: Called
    END TYPE 

    TYPE, EXTENDS(Zero)  :: Base    ! (4)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    FUNCTION Called()
    LOGICAL :: Called
      Called =.true.
    END FUNCTION

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4)), INTENT(INOUT) :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4)), INTENT(INOUT)  :: Arg(:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrHostULAlloc
  USE M
  IMPLICIT NONE
  TYPE(Child(4)) :: V(4,4)
  
  V%BaseId = -1
  V%ChildId = -2

  CALL Sub(V(1:3:2,::2))

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(Base(4)) :: Arg(:, :)
  CLASS(*), ALLOCATABLE :: Arr(:,:)

    ALLOCATE (Arr(SIZE(Arg,1), SIZE(Arg,2)), SOURCE=Arg) 

    SELECT TYPE (U => Arr)
    CLASS IS (Child(4)) 
    SELECT TYPE (W => U)
    CLASS IS (Child(4)) 
      SELECT TYPE (V => W)
        TYPE IS (Child(4))
          IF ( SIZE(V)          .NE. 4 )          STOP 21
          IF ( ANY (LBOUND(V)   .NE. (/1, 1/) ) ) STOP 30
          IF ( ANY (UBOUND(V)   .NE. (/2, 2/) ) ) STOP 31
          IF ( ANY(SHAPE(V)     .NE. (/2,2/)) )   STOP 20

          IF ( ANY(V%Base%GetId() .NE. -1) ) STOP 34
          IF ( ANY(V%GetId()      .NE. -2) ) STOP 35
          IF ( ANY(V%BaseId       .NE. -1) ) STOP 36
          IF ( ANY(V%ChildId      .NE. -2) ) STOP 37

          IF ( .NOT. V%Called() ) STOP 45

          CALL V%SetId(U)
          CALL W%Base%SetId(W%Base)

          IF ( ANY (V%Base%GetId() .NE. 1 )) STOP 44
          IF ( ANY (V%GetId()      .NE. 2 )) STOP 45
          IF ( ANY (V%BaseId       .NE. 1 )) STOP 46
          IF ( ANY (V%ChildId      .NE. 2 )) STOP 47

       CLASS DEFAULT
          STOP 40
        CLASS is (Child(4))
          STOP 56
      END SELECT

  END SELECT
  END SELECT

  END SUBROUTINE 
 
 
  END



