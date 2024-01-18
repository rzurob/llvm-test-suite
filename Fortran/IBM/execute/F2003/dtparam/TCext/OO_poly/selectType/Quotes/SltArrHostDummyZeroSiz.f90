! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/SltArrHostDummyZeroSiz.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltArrHostDummyZeroSiz.f
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
!*  TEST CASE NAME             : SltArrHostDummyZeroSiz
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
!*   poly dummy array of zero size
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
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    FUNCTION Called()
    LOGICAL :: Called
      Called =.true.
    END FUNCTION

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = 2 
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = 1 
    END FUNCTION

  END MODULE


  PROGRAM SltArrHostDummyAssumShp
  USE M
  IMPLICIT NONE
  CLASS(Base(4,:)), POINTER :: V(:,:)
  
  ALLOCATE(V(-33:33,33:66), SOURCE=Child(4,20)()) 

  CALL Sub(V(-1:0, 34:35))

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(Base(4,*)), OPTIONAL :: Arg(20:, -10:) 

    IF ( .NOT. PRESENT(Arg) ) STOP 11

    SELECT TYPE (U => Arg(::1,:))
    CLASS IS (Child(4,*)) 
    SELECT TYPE (W => U(1:2,:) )
    CLASS IS (Child(4,*)) 
      SELECT TYPE (V => W)
        TYPE IS (Child(4,*))

          IF ( SIZEOF(V)        .NE. 0 )          STOP 41
          IF ( SIZE(V)          .NE. 4 )          STOP 21
          IF ( ANY (LBOUND(V)   .NE. (/1, 1/) ) ) STOP 30
          IF ( ANY (UBOUND(V)   .NE. (/2, 2/) ) ) STOP 31
          IF ( ANY(SHAPE(V)     .NE. (/2,2/)) )   STOP 20

          IF ( .NOT. V%Called() ) STOP 45

       CLASS DEFAULT
          STOP 40
        CLASS is (Child(4,*))
          STOP 56
      END SELECT

  END SELECT
  END SELECT

  END SUBROUTINE 
 
 
  END



