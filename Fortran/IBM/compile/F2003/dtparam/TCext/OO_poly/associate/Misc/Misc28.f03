! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/Misc/Misc28.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 09, 2005
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
!*   Diagnosis on elemental subroutine
!*    (comp pass-300954)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: Id = 1
  END TYPE

  CLASS(*), POINTER :: T

  CONTAINS

    ELEMENTAL SUBROUTINE Set(Arg)
    CLASS(*), INTENT(IN) :: Arg
    ASSOCIATE( AS => Arg)
      ALLOCATE(T, SOURCE=Arg)
    END ASSOCIATE

  END SUBROUTINE

  END


