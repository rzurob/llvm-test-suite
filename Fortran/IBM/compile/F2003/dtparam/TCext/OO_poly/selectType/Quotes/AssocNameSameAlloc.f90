! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/Quotes/AssocNameSameAlloc.f
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
!*   The associate name is the same as the selector
!*   Test the allocatable attribute
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  AssocNameSameAlloc
  IMPLICIT NONE

  TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  END TYPE

  CLASS(*),   ALLOCATABLE :: Alloc
  ALLOCATE(Base(4,20) :: Alloc)

  SELECT TYPE ( Alloc  => Alloc )
    TYPE IS (Base(4,*))
      PRINT*, "OK!"
      DEALLOCATE(Alloc)
    CLASS IS (Base(4,*))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT

  SELECT TYPE ( Alloc )
    TYPE IS (Base(4,*))
      PRINT*, "OK!"
      DEALLOCATE(Alloc)
    CLASS IS (Base(4,*))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT


  END

