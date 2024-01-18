! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/Quotes/AssocNameSameNestedAlloc.f
! opt variations: -qnok -ql

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
! %POSTCMD: tcomp AssocNameSameNestedAlloc.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  AssocNameSameNestedAlloc
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
!*   The associate name is the same as the selector within nested select type construct
!*   Test the allocatable attribute
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  AssocNameSameNestedAlloc
  IMPLICIT NONE

  TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
  END TYPE

  CLASS(*),   ALLOCATABLE :: Alloc
  ALLOCATE(Base(4) :: Alloc)

  SELECT TYPE ( Alloc )
  CLASS DEFAULT

  SELECT TYPE ( Alloc  => Alloc )
    TYPE IS (Base(4))
!     PRINT*, "OK!"
      DEALLOCATE(Alloc)
    CLASS IS (Base(4))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT

  DEALLOCATE(Alloc)

  SELECT TYPE ( Alloc )
    TYPE IS (Base(4))
!     PRINT*, "OK!"
      DEALLOCATE(Alloc)
    CLASS IS (Base(4))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT

  END SELECT

  END

