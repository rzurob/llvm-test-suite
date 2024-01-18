!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal018c.f
! %VERIFY: ffinal018c.out:ffinal018c.vf
! %STDIN:
! %STDOUT: ffinal018c.out
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal018c.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines: derived
!*                               type without data components: this
!*                               testcase ICE with 20040414 driver
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      type t1
      contains
         final :: finalize_t1
      end type

      type, extends(t1) :: t2
      contains
         final :: finalize_t2
      end type

   contains
      subroutine finalize_t1(x)
         type(t1), intent(inout) :: x
         print *, "finalize_t1"
      end subroutine

      subroutine finalize_t2(x)
         type(t2), intent(inout) :: x(:,:,:)
         print *, "finalize_t2"
      end subroutine
   end module

   use mod
   call sub()

   end

   subroutine sub()
   use mod
   type(t2), allocatable :: x(:,:,:)
   allocate(x(1,2,2))
   deallocate(x)
   end subroutine
