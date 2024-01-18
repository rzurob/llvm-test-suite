! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/AssocNameAsOther5.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 28, 2005
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
!*    The associate construct name is the same as a specific interface name
!*   (300548-conflicting entity)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  TYPE :: DT(K1,N1)    ! (1,8)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CHARACTER(kind=K1,len=N1) :: C="12345678"
  END TYPE

  INTERFACE
    FUNCTION Fun(Arg)
    IMPORT DT
    CLASS(DT(1,*)) :: Arg
    TYPE(DT(1,8)) :: Fun
    END FUNCTION
  END INTERFACE

  INTERFACE
    FUNCTION F(Arg)
    INTEGER :: Arg, F
    END FUNCTION
  END INTERFACE

  END MODULE


  PROGRAM AssocNameAsOther5
  USE M

  ASSOCIATE ( Fun => Fun(DT(1,8)("87654321")) )
    IF ( Fun%C .NE. "87654321" ) ERROR STOP 11
     ASSOCIATE ( F => Fun%C )
       IF ( F .NE. "87654321" ) ERROR STOP 12
     END ASSOCIATE
     IF ( F(1) .NE. 1 ) ERROR STOP 13
  END ASSOCIATE


  END

  FUNCTION F(Arg)
  INTEGER :: Arg, F
    F = Arg
  END FUNCTION

  FUNCTION Fun(Arg)
  USE M, ONLY:DT
  CLASS(DT(1,*)) :: Arg
  TYPE(DT(1,8)) :: Fun
    Fun = Arg
  END FUNCTION

