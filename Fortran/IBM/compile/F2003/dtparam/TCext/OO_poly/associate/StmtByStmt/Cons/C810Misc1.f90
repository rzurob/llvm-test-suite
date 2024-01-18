! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Cons/C810Misc1.f
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
! %POSTCMD: tcomp C810Misc1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C810Misc1
!*
!*  DATE                       : Oct. 20, 2004
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
!*     C810 : Miscellaneous problems found
!*     - IMPLICIT does not work within select construct
!*    (Pass exec) :" V = 2" is not correct usage
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C810Misc1
  IMPLICIT NONE

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: i = 1
  END TYPE

  CLASS(DT(4)), ALLOCATABLE :: T

  ALLOCATE(T)

  SELECT TYPE ( T )
     TYPE IS (DT(4))
       v= 2
       print*, v
  END SELECT

  END

