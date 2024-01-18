!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : BNoImportd
!*
!*  DATE                       : 2010-11-05
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : IMPORT statements are not allowed
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  IMPORT statements are part of the specification-part in the Fortran
!*  grammar, but are not allowed in BLOCK (in fact, they are only allowed in
!*  interface constructs).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod
  type bugs
  end type bugs
contains
  subroutine modsub
    block
      interface bunny
        subroutine carrot(a)
          import :: bugs     ! this use is okay
          type(bugs) :: a
        end subroutine carrot
      end interface bunny
    end block
  end subroutine modsub
end module mod

program BNoImportd
  use :: mod
  implicit none
  type elmer
  end type elmer
  block
    import :: elmer          ! can't use IMPORT here
  end block
end program BNoImportd

subroutine ext
  use :: mod
  implicit none
  type fudd
  end type fudd
  block
    import :: fudd           ! can't use IMPORT here
  end block
end subroutine ext

subroutine carrot(arg)
  use :: mod
  implicit none
  type(bugs) :: arg
end subroutine carrot
