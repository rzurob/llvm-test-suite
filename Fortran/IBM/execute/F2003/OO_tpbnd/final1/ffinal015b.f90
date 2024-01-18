!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal015b.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal015b.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      type t1
         integer, allocatable :: vector
      contains
         final :: finalize_t1s
      end type

      type(t1), allocatable :: dt1

   contains
      subroutine finalize_t1s(x)
         type(t1) :: x
         if (allocated(x%vector))    deallocate(x%vector)
      end subroutine
   end module

   use mod
   call example
   if(allocated(dt1%vector) .neqv. .false.)     error stop 2_4
   end

   subroutine example()
      use mod
      allocate(dt1)
      deallocate(dt1)
   end subroutine

