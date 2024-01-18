! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/Optional3.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Optional3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Optional3
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
!*  The associating entity's optional attribute.
!*  dummy array section
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    CLASS(DT(*,4)), INTENT(IN) :: Arg
    INTEGER               :: GetId
      GetId = Arg%Id
    END FUNCTION
  END MODULE


  PROGRAM Optional3
  USE M
  IMPLICIT NONE

  TYPE(DT(20,4)),   TARGET   ::    DTV(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
  TYPE(DT(:,4)),   POINTER  :: DTVPtr(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)

  CALL Sub()
  CALL Sub(DTV)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*), TARGET, OPTIONAL  :: Arg(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
  INTEGER :: S(2)=(/1,2/)

    IF ( .NOT. PRESENT(Arg)) RETURN

    SELECT TYPE (U => Arg(1:2,1:2,1:2,1:2,1:2,1:2,1:2,1:2,1:2,1:2,1:2,1:2,1:2,1:2,1:2,1:2,1:2,1:2))
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))        STOP 30
      IF ( SIZE(U)          .NE. 2**18 )      STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/) ) ) STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/) ) ) STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/)) )  STOP 34

    ASSOCIATE ( W => U )

      SELECT TYPE (U => W )

      TYPE IS (DT(*,4))
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



