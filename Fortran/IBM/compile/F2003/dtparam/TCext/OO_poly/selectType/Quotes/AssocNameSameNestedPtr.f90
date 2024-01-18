! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/Quotes/AssocNameSameNestedPtr.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Associate name
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
!*
!*   The associate name is the same as the selector within associate/select type
!*   constructs
!*   Test the pointer attribute
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  AssocNameSameNestedPtr
  IMPLICIT NONE

  TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  END TYPE

  CLASS(*),   POINTER :: Ptr
  TYPE(Base(4,20)), TARGET  :: Tar

  Ptr => Tar

  ASSOCIATE ( Ptr => Ptr )
  SELECT TYPE (  Ptr )
    TYPE IS (Base(4,*))
!     PRINT*, "OK!"
      Ptr => Tar
    CLASS IS (Base(4,*))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT

  Ptr => Tar

  SELECT TYPE ( Ptr )
    TYPE IS (Base(4,*))
!     PRINT*, "OK!"
      Ptr => Tar
    CLASS IS (Base(4,*))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT
  END ASSOCIATE

  END

