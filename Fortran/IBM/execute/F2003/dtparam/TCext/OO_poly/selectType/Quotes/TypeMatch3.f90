! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/Quotes/TypeMatch3.f
! opt variations: -qnok -ql

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
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE

    TYPE, EXTENDS(Zero)  :: One    ! (4)
    END TYPE

    TYPE, EXTENDS(One) :: Two    ! (4)
    END TYPE

    TYPE, EXTENDS(Two) :: Three    ! (4)
    END TYPE

  END MODULE

  PROGRAM TypeMatch3
  USE M,  One => One
  IMPLICIT NONE
  CLASS(*), POINTER :: U(:,:)

    ALLOCATE(Two(4) :: U(2:3,3:4) )

    SELECT TYPE (U)
    CLASS IS (Zero(4))
       STOP 40
    CLASS IS (Three(4))
       STOP 41
    TYPE IS (Complex)
       STOP 42
    TYPE IS (INTEGER(2))
       STOP 43
    TYPE IS (INTEGER(4))
       STOP 44
    TYPE IS (One(4))
       STOP 47
    CLASS IS (One(4))

      IF ( SIZE(U)          .NE. 4 )          ERROR STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) ERROR STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) ERROR STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   ERROR STOP 34

    TYPE IS (REAL)
       STOP 45
    CLASS DEFAULT
       STOP 46
    END SELECT

  END



