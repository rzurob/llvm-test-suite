! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/Misc/Misc4.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2004
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
!*    Associate Selector is a dummy in elemntal function
!*    (Comp failed: treat association as ptr assignment)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Misc4

  TYPE :: Base(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: BaseId = 1
  END TYPE

  CONTAINS

  ELEMENTAL FUNCTION Func(Arg)
    TYPE(Base(4)), INTENT(IN)  :: Arg
    TYPE(Base(4))              :: Func

    Func = Arg
    ASSOCIATE ( As => Arg)
    END ASSOCIATE
  END FUNCTION

  END
