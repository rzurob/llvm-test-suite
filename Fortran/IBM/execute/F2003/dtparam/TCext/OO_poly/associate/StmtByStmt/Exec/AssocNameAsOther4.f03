! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/AssocNameAsOther4.f
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
!*    The associate construct name is the same as an interface name
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  TYPE :: DT(K1,N1)    ! (1,8)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CHARACTER(kind=K1,len=N1) :: C="12345678"
  END TYPE

  INTERFACE Fun
    FUNCTION Fun(Arg)
    IMPORT DT
    CLASS(DT(1,*)) :: Arg
    TYPE(DT(1,8)) :: Fun
    END FUNCTION
  END INTERFACE

  CONTAINS

    FUNCTION F(Arg)
    INTEGER :: Arg, F
      F = Arg
    END FUNCTION

  END MODULE


  PROGRAM AssocNameAsOther4
  USE M
  PROCEDURE(F) :: Fun1

  ASSOCIATE ( Fun => Fun(DT(1,8)("87654321")) )
    IF ( Fun%C .NE. "87654321" ) ERROR STOP 11
  END ASSOCIATE

  ASSOCIATE ( F => Fun1(6) )
    IF ( F .NE. 6 ) ERROR STOP 11
  END ASSOCIATE


  END

  FUNCTION Fun1(Arg)
  INTEGER :: Arg, Fun1
    Fun1 = Arg
  END FUNCTION

  FUNCTION Fun(Arg)
  USE M, ONLY:DT
  CLASS(DT(1,*)) :: Arg
  TYPE(DT(1,8)) :: Fun
    Fun = Arg
  END FUNCTION
