! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/Quotes/C819EndMissed.f
! opt variations: -qnok -ql

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
!*    The select type construct is missed from the end select statement
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM C819EndMissed
  IMPLICIT NONE

  TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
  END TYPE

  CLASS(*),   POINTER :: Ptr
  TYPE(Base(4)), TARGET  :: Tar

  Ptr => Tar

A: SELECT TYPE ( Ptr )
1   TYPE IS (Base(4)) A
!     PRINT*, "OK!"
2   CLASS IS (Base(4)) A
      STOP 20
3   CLASS DEFAULT
      STOP 30
  END SELECT


  END
