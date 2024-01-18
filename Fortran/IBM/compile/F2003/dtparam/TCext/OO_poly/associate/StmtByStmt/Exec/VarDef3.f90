! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/VarDef3.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp VarDef3.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : VarDef3
!*
!*  DATE                       : Feb 22, 2005
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
!*
!*   Variable Definition Context on non variable selector
!*   - Pointer assignment
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarDef3

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND        :: K1
    INTEGER(K1), POINTER :: IntP
  END TYPE

  INTEGER, TARGET :: Tar = 1

  ASSOCIATE ( As => DT(4)(Tar) )

    As%IntP => NULL()

  END ASSOCIATE


  END

