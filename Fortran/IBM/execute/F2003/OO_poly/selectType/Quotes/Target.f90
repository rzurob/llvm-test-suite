! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Target.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Target
!*
!*  DATE                       : Jan. 25, 2005
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
!*  The associating entity's target attribute.
!*
!*  (ICE-298737)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    CLASS(DT), INTENT(IN) :: Arg
    INTEGER               :: GetId
      GetId = Arg%Id
    END FUNCTION
  END MODULE


  PROGRAM Target
  USE M
  IMPLICIT NONE

  INTEGER(2), TARGET   :: Int(2:3,3:4)=8_1
  INTEGER(2), POINTER  :: IntPtr(:,:)
  COMPLEX(4), TARGET   :: Cplx(2:3,3:4)=(-8.0_8, 8.0_8)
  COMPLEX(4), POINTER  :: CplxPtr(:,:)
  LOGICAL(2), TARGET   :: L(2:3,3:4)=.true._8
  LOGICAL(2), POINTER  :: LPtr(:,:)
  CHARACTER(1023), TARGET  :: C(2:3,3:4)
  CHARACTER(1023), POINTER :: CPtr(:,:)
  CHARACTER(1023) :: Str
  TYPE(DT),   TARGET   :: DTV(2:3,3:4)
  TYPE(DT),   POINTER  :: DTVPtr(:,:)


  Str(:) = '!'
  C = Str

  CALL Sub(Int(2:3,3:4))
  CALL Sub(Cplx(2:3,3:4))
  CALL Sub(L(2:3,3:4))
  CALL Sub(C(2:3,3:4))
  CALL Sub(DTV)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*), TARGET  :: Arg(2:3,3:4)

    SELECT TYPE (U => Arg)
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))       STOP 30
      IF ( SIZE(U)          .NE. 4 )          STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   STOP 34

    ASSOCIATE ( W => U )

      SELECT TYPE (U => W )

      TYPE IS (INTEGER(KIND(Int)))
        IntPtr => U
        IF ( ANY(U   .NE. IntPtr) )      STOP 35
        IF ( KIND(U) .NE. KIND(IntPtr) ) STOP 36

      TYPE IS (COMPLEX(KIND(Cplx)))
        CplxPtr => U
        IF ( ANY(U   .NE. CplxPtr ))     STOP 37
        IF ( KIND(U) .NE. KIND(CplxPtr) )STOP 38

      TYPE IS (LOGICAL(KIND(L)))
        LPtr => U
        IF ( ANY(U   .NEQV. LPtr) )      STOP 40
        IF ( KIND(U) .NE. KIND(LPtr) )   STOP 41

      TYPE IS (CHARACTER(*))
        CPtr => U
        IF ( ANY(U  .NE. CPtr) )       STOP 42
        IF ( LEN(U) .NE. LEN(CPtr) )   STOP 43

      TYPE IS (DT)
        DTVPtr => DTV
        IF ( ANY(U%Id      .NE. DTVPtr%Id ) )      STOP 42
        IF ( ANY(U%GetId() .NE. DTVPtr%GetId()))   STOP 43

      CLASS DEFAULT
        STOP 51
      END SELECT

    END ASSOCIATE
    END SELECT

  END SUBROUTINE

  END



