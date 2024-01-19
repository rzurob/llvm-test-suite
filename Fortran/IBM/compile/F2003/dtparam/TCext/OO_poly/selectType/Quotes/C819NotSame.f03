! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/Quotes/C819NotSame.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
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
!*    The select type construct name is not consistent.
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM C819NotSame
  IMPLICIT NONE

  TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  END TYPE

  CLASS(*),   POINTER :: Ptr
  TYPE(Base(4,20)), TARGET  :: Tar

  Ptr => Tar

0 SELECT : SELECT TYPE ( Ptr )
1   TYPE IS (Base(4,*)) SELECT
11    PRINT*, "OK!"
2   CLASS IS (Base(4,*)) END
22    STOP 20
3   CLASS DEFAULT
33    STOP 30
4 END SELECT END


  END

