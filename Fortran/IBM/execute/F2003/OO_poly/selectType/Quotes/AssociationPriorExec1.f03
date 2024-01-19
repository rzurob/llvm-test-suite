! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 28, 2005
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
!*  The association is established prior to execution of
!*  the block
!*
!*  (ICE-298976)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM AssociationPriorExec1
  IMPLICIT NONE

  INTEGER :: I, J
  CLASS(*), POINTER :: Ptr(:,:)

    ALLOCATE(Ptr(2,2), SOURCE="1234")

    I = 1
    J = 4

    SELECT TYPE (Ptr => Ptr(I,:))
    CLASS DEFAULT
      STOP 40
    TYPE IS (CHARACTER(*))

      ASSOCIATE ( Ptr => Ptr(1:)(I:J))

        IF (SIZE(Ptr) .NE. 2)  ERROR STOP 30
        IF (LEN(Ptr)  .NE. 4)  ERROR STOP 31

        I = 4; J = 1
        Ptr = "4321"

        IF (ANY(Ptr .NE. "4321")) ERROR STOP 42

      END ASSOCIATE
    END SELECT

    SELECT TYPE (Ptr => Ptr)
    TYPE IS (CHARACTER(*))
      IF (ANY(Ptr(1,:)  .NE. "4321"))   ERROR STOP 52
      IF (ANY(Ptr(2:2,:)  .NE. "1234")) ERROR STOP 62
    END SELECT

  END

