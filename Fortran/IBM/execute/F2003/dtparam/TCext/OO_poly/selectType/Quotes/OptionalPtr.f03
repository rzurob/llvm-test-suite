! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/selectType/Quotes/OptionalPtr.f
! opt variations: -ql

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
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    CLASS(DT(4)), INTENT(IN) :: Arg
    INTEGER               :: GetId
      GetId = Arg%Id
    END FUNCTION
  END MODULE


  PROGRAM OptionalPtr
  USE M
  IMPLICIT NONE

  CLASS(DT(4)), POINTER  ::  DTV
  TYPE(DT(4)),   POINTER :: DTVPtr

  CALL Sub()
  CALL Sub(DTV)

    SELECT TYPE (U => DTV)
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, DT(4)()))        ERROR STOP 50

      SELECT TYPE ( U )

      TYPE IS (DT(4))
        IF ( U%Id      .NE. -1 )  ERROR STOP 55
        IF ( U%GetId() .NE. -1 )  ERROR STOP 56

      CLASS DEFAULT
        STOP 57
      END SELECT

    END SELECT

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(DT(4)), POINTER, OPTIONAL  :: Arg

    IF ( .NOT. PRESENT(Arg)) RETURN

    ALLOCATE(Arg, SOURCE=DT(4)(Id=-1))

    SELECT TYPE (U => Arg)
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))        ERROR STOP 30

    ASSOCIATE ( W => U )

      SELECT TYPE (U => W )

      TYPE IS (DT(4))

        IF ( U%Id      .NE. -1 )   ERROR STOP 42
        IF ( U%GetId() .NE. -1 )   ERROR STOP 43

      CLASS DEFAULT
        STOP 51
      END SELECT

    END ASSOCIATE
    END SELECT

  END SUBROUTINE

  END



