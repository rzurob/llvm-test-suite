! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_poly/selectType/Quotes/OptionalArrPtr.f
! opt variations: -qnol -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 27, 2005
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
!*  The associating entity's optional attribute
!*  dummy with optional and pointer attributes
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


  PROGRAM OptionalArrPtr
  USE M
  IMPLICIT NONE

  CLASS(DT(20,4)), POINTER  ::  DTV(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
  TYPE(DT(20,4)),   POINTER :: DTVPtr(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)

  CALL Sub()
  CALL Sub(DTV)

    SELECT TYPE (U => DTV)
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, DT(20,4)()))        ERROR STOP 50
      IF ( SIZE(U)          .NE. 2**18 )       ERROR STOP 51
      IF ( ANY (LBOUND(U)   .NE. (/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/) ) ) ERROR STOP 52
      IF ( ANY (UBOUND(U)   .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/) ) ) ERROR STOP 53
      IF ( ANY(SHAPE(U)     .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/)) )  ERROR STOP 54

      SELECT TYPE ( U )

      TYPE IS (DT(*,4))
        IF ( ANY(U%Id      .NE. DTV%Id ) )      ERROR STOP 55
        IF ( ANY(U%GetId() .NE. DTV%GetId()))   ERROR STOP 56

      CLASS DEFAULT
        STOP 57
      END SELECT

    END SELECT

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(DT(*,4)), POINTER, OPTIONAL  :: Arg(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
  INTEGER :: S(2)=(/1,2/)

    IF ( .NOT. PRESENT(Arg)) RETURN

    ALLOCATE(Arg(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2))

    SELECT TYPE (U => Arg(:,S,:,S,:,S,:,S(:2),:,S,:,S,:,S,:,S,:,S(:)))
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))        ERROR STOP 30
      IF ( SIZE(U)          .NE. 2**18 )      ERROR STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/) ) ) ERROR STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/) ) ) ERROR STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/)) )  ERROR STOP 34

    ASSOCIATE ( W => U )

      SELECT TYPE (U => W )

      TYPE IS (DT(*,4))

        DTVPtr => DTV
        IF ( ANY(U%Id      .NE. DTVPtr%Id ) )      ERROR STOP 42
        IF ( ANY(U%GetId() .NE. DTVPtr%GetId()))   ERROR STOP 43

      CLASS DEFAULT
        STOP 51
      END SELECT

    END ASSOCIATE
    END SELECT

  END SUBROUTINE

  END



