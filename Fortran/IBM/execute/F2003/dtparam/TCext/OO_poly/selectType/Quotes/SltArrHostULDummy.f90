! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltArrHostULDummy.f
! opt variations: -qnok -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltArrHostULDummy.f
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
!*  TEST CASE NAME             : SltArrHostULDummy
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
!*   poly dummy array 
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


  PROGRAM SltArrHostULDummy
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20)), TARGET :: V(4,4)
  
  V%BaseId = -1
  V%ChildId = -2

  CALL Sub(V(::2,1:2))
  ASSOCIATE (W=>V(::2,1:2))
    IF ( ANY(W%Base%GetId() .NE. 1) ) STOP 34
    IF ( ANY(W%GetId()      .NE. 2) ) STOP 35
    IF ( ANY(W%BaseId       .NE. 1) ) STOP 36
    IF ( ANY(W%ChildId      .NE. 2) ) STOP 37
  END ASSOCIATE 

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(Base(4,*)), TARGET, OPTIONAL :: Arg(:, :)

    IF ( .NOT. PRESENT(Arg) ) STOP 11

    SELECT TYPE (U => Arg)
    CLASS IS (Child(4,*)) 
    SELECT TYPE (W => U)
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



