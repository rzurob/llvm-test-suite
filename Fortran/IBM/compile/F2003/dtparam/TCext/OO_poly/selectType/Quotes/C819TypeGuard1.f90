! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/Quotes/C819TypeGuard1.f
! opt variations: -qnok -qnol

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
! %POSTCMD: tcomp C819TypeGuard1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C819TypeGuard1
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C819
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The inconsistency on construct name
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM C819TypeGuard1
  IMPLICIT NONE

  TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  END TYPE

  CLASS(*),   POINTER :: Ptr
  TYPE(Base(4,20)), TARGET  :: Tar

  Ptr => Tar

SELECT :  SELECT TYPE ( Ptr )
1   TYPE IS (Base(4,*))
! 11    PRINT*, "OK!"
2   CLASS IS (Base(4,*)) SLEC
22    STOP 20
3   CLASS DEFAULT
33    STOP 30
333 END SELECT  SELECT


  END

