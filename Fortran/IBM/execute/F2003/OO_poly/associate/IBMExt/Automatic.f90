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
!*    The selector is an automatic entity
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Automatic
  IMPLICIT NONE

  CHARACTER   :: C="!"
  LOGICAL(1)  :: L=.TRUE.
  INTEGER(1)  :: I=-1
  BYTE        :: B=1_1

  CALL Sub()
  IF (C .NE. "!" .OR. .NOT. L .OR. I .NE. -1 .OR. B .NE. 1 ) STOP 99

  CONTAINS

  SUBROUTINE Sub()

  CLASS(*),   POINTER, AUTOMATIC :: BPtr(:)
  CHARACTER,  TARGET, AUTOMATIC  :: C(3)
  LOGICAL(1), TARGET, AUTOMATIC  :: L(3)
  INTEGER(1), TARGET, AUTOMATIC  :: I(3)
  BYTE,       TARGET, AUTOMATIC  :: B(3)

  C="!"
  L=.TRUE.
  I=-1
  B=1_1

  ASSOCIATE ( As => B  )
    IF ( ANY(As .NE. 1_1) ) STOP 11
  END ASSOCIATE

  BPtr => C
  ASSOCIATE ( As => BPtr  )
    SELECT TYPE ( As )
    CLASS DEFAULT
      STOP 20
    TYPE IS (CHARACTER(*))
      IF ( ANY(As .NE. "!") ) STOP 21
      AS = "1"
      IF ( ANY(C .NE. "1" ) )STOP 22
    END SELECT
  END ASSOCIATE

  BPtr => I
  ASSOCIATE ( As => BPtr  )
    SELECT TYPE ( As )
    CLASS DEFAULT
      STOP 30
    TYPE IS (INTEGER(1))
      IF ( ANY(As .NE. -1 )) STOP 31
      AS = 1
      IF ( ANY(I .NE. 1 )) STOP 32
    END SELECT
  END ASSOCIATE

  BPtr => L
  ASSOCIATE ( As => BPtr  )
    SELECT TYPE ( As )
    CLASS DEFAULT
      STOP 40
    TYPE IS (LOGICAL(1))
      IF ( ANY(.NOT. As )) STOP 41
      AS = .FALSE.
      IF ( ANY(L) ) STOP 42
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
      IF ( ANY(As .NE. 1_1 )) STOP 51
      AS = -1
      IF ( ANY(B .NE. -1 )) STOP 52
    END SELECT
  END ASSOCIATE

  END SUBROUTINE

  END
