! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/FuncArgPtr.f
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

    TYPE :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
    END TYPE


  END MODULE

  PROGRAM FuncArgPtr
  USE M
  TYPE(Child(4,20)), TARGET  :: V
  TYPE(Child(4,:)), POINTER  :: Ptr

  Ptr => V

  ASSOCIATE ( As => Ptr  )
     IF ( Func(As) .NE. 2 ) STOP 11
  END ASSOCIATE

  CONTAINS

  ELEMENTAL FUNCTION Func(Arg)
    CLASS(Base(4,*)), INTENT(IN)  :: Arg
    INTEGER :: Func

    SELECT TYPE (Arg)
      TYPE IS (Child(4,*))
        Func = 2
      TYPE IS (Base(4,*))
        Func = 1
      CLASS DEFAULT
        Func = -1
    END SELECT

  END FUNCTION

  END
