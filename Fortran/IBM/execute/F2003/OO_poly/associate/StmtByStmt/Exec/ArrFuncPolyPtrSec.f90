! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 14, 2005
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
!*    The selector is a function call returning an array pointer of poly
!*    (Comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  CONTAINS
    FUNCTION ReturnArr(Arg)
    CLASS (*), Target :: Arg(:, :)
    CLASS (*), POINTER :: ReturnArr(:, :)
      ReturnArr => Arg
    END FUNCTION
  END MODULE

  PROGRAM ArrFuncPolyPtrSec
  USE M
  IMPLICIT NONE
  INTEGER :: i
  COMPLEX(8), TARGET :: Arr(2, 2)=(1.0,-1.0)
  INTEGER(8) :: S1(2)=(/5,4/), S2(2)=(/1,1/)

  ASSOCIATE ( As => ReturnArr(Arr(1:2:1, 1:2) ) )

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 32

    SELECT TYPE (As)
    TYPE IS (COMPLEX(8))
      IF ( ANY (As  .NE. RESHAPE((/((1.0, -1.0), i=1,4)/), (/2,2/)) ) ) STOP 33
    CLASS DEFAULT
      STOP 35
    END SELECT

    ASSOCIATE( As => SPREAD( As(:,1), 1, 2) )

      IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 40
      IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 41
      IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 42

      SELECT TYPE (As)
      TYPE IS (COMPLEX(8))
        IF ( ANY (As  .NE. RESHAPE((/((1.0, -1.0), i=1,4)/), (/2,2/)) ) ) STOP 33
      CLASS DEFAULT
        STOP 35
      END SELECT

    END ASSOCIATE

  END ASSOCIATE

  IF ( ANY ( Arr(1::1, ::1)   .NE. (1.0,-1.0) ) )        STOP 62

  END

