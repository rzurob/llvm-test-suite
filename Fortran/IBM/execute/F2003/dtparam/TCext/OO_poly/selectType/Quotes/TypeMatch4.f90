! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/Quotes/TypeMatch4.f
! opt variations: -qnok -qnol

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
!*    Multiple CLASS IS clauses
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero)  :: One    ! (4,20)
    END TYPE

    TYPE, EXTENDS(One) :: Two    ! (4,20)
    END TYPE

    TYPE, EXTENDS(Two) :: Three    ! (4,20)
    END TYPE

  END MODULE

  PROGRAM TypeMatch4
  USE M,  Three=>One, One=>Three
  IMPLICIT NONE
  CLASS(*), ALLOCATABLE :: U(:,:)

    ALLOCATE(Two(4,20) :: U(2:3,3:4) )

    SELECT TYPE (U)
    CLASS IS (Zero(4,*))
       STOP 40
    TYPE IS (Three(4,*))
       STOP 41
    TYPE IS (One(4,*))
       STOP 47
    CLASS IS (Three(4,*))

      IF ( SIZE(U)          .NE. 4 )          STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   STOP 34

    CLASS DEFAULT
       STOP 46
    END SELECT

  END



