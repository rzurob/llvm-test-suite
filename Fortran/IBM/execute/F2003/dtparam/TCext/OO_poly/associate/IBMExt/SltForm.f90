! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/IBMExt/SltForm.f
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
!*    The selector has the form a.b
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SltForm
  IMPLICIT NONE

  TYPE :: L0(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: I0=0
  END TYPE

  TYPE, EXTENDS(L0) :: L1    ! (4)
    INTEGER(K1) :: I1=1
  END TYPE

  TYPE, EXTENDS(L1) :: L2    ! (4)
    INTEGER(K1) :: I2=2
  END TYPE

  TYPE (L2(4)) :: L

  ASSOCIATE ( As => L  )
    IF ( As.I2 .NE. 2  )      ERROR STOP 20
    As.I2 = -2

    ASSOCIATE ( As => As.L1  )
      IF ( As.I1 .NE. 1  )    ERROR STOP 21
      As.I1 = -1

      ASSOCIATE ( As => As.L0  )
        IF ( As.I0 .NE. 0  )  ERROR STOP 22
      END ASSOCIATE

      IF ( As.L0.I0 .NE. 0  ) ERROR STOP 23
      IF ( As.I1 .NE. -1  )   ERROR STOP 24

    END ASSOCIATE

    IF ( As.I2 .NE. -2  )     ERROR STOP 25

  END ASSOCIATE

  END
