! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 02, 2004
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
!*    The associating entity associating to an pointer is used as actual argument
!*    (comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
    END TYPE

    TYPE, EXTENDS(Base) :: Child
    END TYPE


  END MODULE

  PROGRAM FuncArgPtr
  USE M
  TYPE(Child), TARGET  :: V
  TYPE(Child), POINTER  :: Ptr

  Ptr => V

  ASSOCIATE ( As => Ptr  )
     IF ( Func(As) .NE. 2 ) ERROR STOP 11
  END ASSOCIATE

  CONTAINS

  ELEMENTAL FUNCTION Func(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER :: Func

    SELECT TYPE (Arg)
      TYPE IS (Child)
        Func = 2
      TYPE IS (Base)
        Func = 1
      CLASS DEFAULT
        Func = -1
    END SELECT

  END FUNCTION

  END