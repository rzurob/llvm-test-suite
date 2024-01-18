! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_poly/selectType/Quotes/TypeMatch7.f
! opt variations: -qnol -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: TypeMatch7.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : TypeMatch7
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
!*   Match with nested select type construct
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id = 0
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base(N2,K2)    ! (20,4,20,4)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      INTEGER(K2)   :: BaseId = 1
    END TYPE

    TYPE, EXTENDS(Base) :: Child(N3,K3)    ! (20,4,20,4,20,4)
      INTEGER, KIND :: K3
      INTEGER, LEN  :: N3
      INTEGER(K3)   :: ChildId = 2
    END TYPE

  END MODULE

  PROGRAM TypeMatch2
  USE M
  IMPLICIT NONE
  CLASS(*), POINTER :: V(:,:)

    ALLOCATE(V(2:3,3:4), SOURCE=Child(20,4,20,4,20,4)())

    SELECT TYPE (V)
    TYPE IS (INTEGER(8))
      STOP 43
    TYPE IS (INTEGER(2))
      STOP 43
    TYPE IS (INTEGER(4))
      STOP 44
    CLASS DEFAULT
      SELECT TYPE (V)
      CLASS IS (Zero(*,4))
      SELECT TYPE (V)
      CLASS IS (Base(*,4,*,4))

       SELECT TYPE (V)
       CLASS IS (Child(*,4,*,4,*,4))
         STOP 40
       CLASS IS (Base(*,4,*,4))
         STOP 41
       TYPE IS (Child(*,4,*,4,*,4))

         IF ( ANY(V%Id         .NE. 0))          STOP 30
         IF ( SIZE(V)          .NE. 4 )          STOP 31
         IF ( ANY (LBOUND(V)   .NE. (/2, 3/) ) ) STOP 32
         IF ( ANY (UBOUND(V)   .NE. (/3, 4/) ) ) STOP 33
         IF ( ANY(SHAPE(V)     .NE. (/2,2/)) )   STOP 34

       TYPE IS (Base(*,4,*,4))
         STOP 45
      CLASS DEFAULT
         STOP 46
      END SELECT

    END SELECT
    END SELECT
    END SELECT

  END



