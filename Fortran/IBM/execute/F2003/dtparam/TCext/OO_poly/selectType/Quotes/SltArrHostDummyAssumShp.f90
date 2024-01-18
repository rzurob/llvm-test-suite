! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltArrHostDummyAssumShp.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltArrHostDummyAssumShp.f
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
!*  TEST CASE NAME             : SltArrHostDummyAssumShp
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 20, 2005
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
!*   The selector is a host associate name associating to a 
!*   poly assumed shape dummy array 
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    CONTAINS
      PROCEDURE, NoPASS   :: Called
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

    FUNCTION Called()
    LOGICAL :: Called
      Called =.true.
    END FUNCTION

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
    CLASS(Base(4,*)), INTENT(INOUT) :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4,*)), INTENT(INOUT)  :: Arg(:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4,*))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrHostDummyAssumShp
  USE M
  IMPLICIT NONE
  CLASS(Base(4,:)), POINTER :: V(:,:)
  
  ALLOCATE(V(33,33), SOURCE=Child(4,20)(BaseId=-1, ChildId=-2)) 

  CALL Sub(V(1:2, 2:3), V(2:3, 1:2))
  SELECT TYPE  (W=>V(1:2, 2:3) )
  CLASS IS (Child(4,*))
    IF ( ANY(W%Base%GetId() .NE. 1) ) STOP 54
    IF ( ANY(W%GetId()      .NE. 2) ) STOP 55
    IF ( ANY(W%BaseId       .NE. 1) ) STOP 56
    IF ( ANY(W%ChildId      .NE. 2) ) STOP 57
  END SELECT 

  CONTAINS

  SUBROUTINE Sub(Arg1, Arg2)
  CLASS(Base(4,*)), OPTIONAL :: Arg1(-20:, 10:), Arg2(:,:) 

    IF ( .NOT. PRESENT(Arg1) ) STOP 11

    SELECT TYPE (U => Arg1(::1,:))
    CLASS IS (Child(4,*)) 
    SELECT TYPE (W => U(1:2,:) )
    CLASS IS (Child(4,*)) 
      SELECT TYPE (V => W)
        TYPE IS (Child(4,*))

          IF ( SIZE(V)          .NE. 4 )          STOP 21
          IF ( ANY (LBOUND(V)   .NE. (/1, 1/) ) ) STOP 30
          IF ( ANY (UBOUND(V)   .NE. (/2, 2/) ) ) STOP 31
          IF ( ANY(SHAPE(V)     .NE. (/2,2/)) )   STOP 20

          IF ( ANY(W%Base%GetId() .NE. -1) ) STOP 34
          IF ( ANY(W%GetId()      .NE. -2) ) STOP 35
          IF ( ANY(W%BaseId       .NE. -1) ) STOP 36
          IF ( ANY(W%ChildId      .NE. -2) ) STOP 37

          IF ( .NOT. V%Called() ) STOP 45

          CALL V%SetId(U)
          CALL W%Base%SetId(V%Base)

          IF ( ANY (U%Base%GetId() .NE. 1 )) STOP 44
          IF ( ANY (U%GetId()      .NE. 2 )) STOP 45
          IF ( ANY (U%BaseId       .NE. 1 )) STOP 46
          IF ( ANY (U%ChildId      .NE. 2 )) STOP 47

       CLASS DEFAULT
          STOP 40
        CLASS is (Child(4,*))
          STOP 56
      END SELECT

  END SELECT
  END SELECT

  END SUBROUTINE 
 
 
  END



