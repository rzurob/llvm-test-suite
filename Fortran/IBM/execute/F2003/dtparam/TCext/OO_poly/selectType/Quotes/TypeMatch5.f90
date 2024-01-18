! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/selectType/Quotes/TypeMatch5.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 24, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   The nearest type matchs that of selector
!*    Multiple CLASS IS/TYPE IS clauses mathch
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id=1
    END TYPE

    TYPE, EXTENDS(Zero)  :: One    ! (4)
    END TYPE

    TYPE, EXTENDS(One) :: Two    ! (4)
    END TYPE

    TYPE, EXTENDS(Two) :: Three    ! (4)
    END TYPE

  END MODULE

  PROGRAM TypeMatch4
  USE M,  Three=>One, One=>Three
  IMPLICIT NONE
  CLASS(*), ALLOCATABLE :: U(:,:)

    ALLOCATE(Two(4) :: U(2:3,3:4) )

    SELECT TYPE (One=>U)
    CLASS IS (Zero(4))
      STOP 40
    TYPE IS (Three(4))
      STOP 41
!   TYPE IS (One(4))
!     STOP 42
    CLASS IS (Three(4))
      STOP 43
    CLASS IS (Two(4))

      IF ( SIZE(U)          .NE. 4 )          STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   STOP 34
      IF ( ANY(One%Id       .NE. 1) )         STOP 35

    CLASS DEFAULT
       STOP 46
    END SELECT

  END



