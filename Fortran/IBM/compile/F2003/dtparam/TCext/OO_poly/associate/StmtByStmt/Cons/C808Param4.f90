! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Cons/C808Param4.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp C808Param4.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C808Param4
!*
!*  DATE                       : Oct. 26, 2004
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
!*    The selector is a parameter of a derived type
!*    (294598-Wrong syntax check on  As => V%i)
!*    (298114-redefinition on AS=>V%i)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM C808Param4
  IMPLICIT NONE

    TYPE T(K1)    ! (4)
      INTEGER, KIND        :: K1
      INTEGER(K1)          :: i
      INTEGER(K1), POINTER :: Ptr
    END TYPE

    TYPE(T(4)), PARAMETER :: V = T(4)(1, NULL())
    INTEGER, TARGET :: IntTar

    ASSOCIATE ( As => V )
      As = T(4)(0, NULL())
    END ASSOCIATE

    ASSOCIATE ( As => V%i )
      As = 1
    END ASSOCIATE

    ASSOCIATE ( As => T(4)(1, NULL()) )
      As = As
    END ASSOCIATE

  END
