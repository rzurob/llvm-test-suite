! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 05, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is of byte
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Byte
  IMPLICIT NONE

  CLASS(*),   POINTER :: BPtr
  CHARACTER,  TARGET  :: C="!"
  LOGICAL(1), TARGET  :: L=.TRUE.
  INTEGER(1), TARGET  :: I=-1
  BYTE,       TARGET  :: B=1_1

  ASSOCIATE ( As => B  )
    IF ( As .NE. 1_1 ) ERROR STOP 11
  END ASSOCIATE

  BPtr => C
  ASSOCIATE ( As => BPtr  )
    SELECT TYPE ( As )
    CLASS DEFAULT
      STOP 20
    TYPE IS (CHARACTER(*))
      IF ( As .NE. "!" ) ERROR STOP 21
      AS = "1"
      IF ( C .NE. "1" ) ERROR STOP 22
    END SELECT
  END ASSOCIATE

  BPtr => I
  ASSOCIATE ( As => BPtr  )
    SELECT TYPE ( As )
    CLASS DEFAULT
      STOP 30
    TYPE IS (INTEGER(1))
      IF ( As .NE. -1 ) ERROR STOP 31
      AS = 1
      IF ( I .NE. 1 ) ERROR STOP 32
    END SELECT
  END ASSOCIATE

  BPtr => L
  ASSOCIATE ( As => BPtr  )
    SELECT TYPE ( As )
    CLASS DEFAULT
      STOP 40
    TYPE IS (LOGICAL(1))
      IF ( .NOT. As ) ERROR STOP 41
      AS = .FALSE.
      IF ( L ) ERROR STOP 42
    END SELECT
  END ASSOCIATE

  BPtr => B
  ASSOCIATE ( As => BPtr  )
    SELECT TYPE ( As )
    CLASS DEFAULT
      STOP 50
    TYPE IS (CHARACTER(*))
      STOP 61
    TYPE IS (LOGICAL(1))
      STOP 62
    TYPE IS (INTEGER(1))
      STOP 63
    TYPE IS (BYTE)
      IF ( As .NE. 1_1 ) ERROR STOP 51
      AS = -1
      IF ( B .NE. -1 ) ERROR STOP 52
    END SELECT
  END ASSOCIATE

  END
