!**********************************************************************
!*  ===================================================================
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
         type(t2), intent(inout) :: x(1:3)
         print *, "finalize_t2"
      end subroutine
   end module

   use mod
   call sub()

   end

   subroutine sub()
   use mod
   type(t2), allocatable :: x(:)
   allocate(x(1:3))
   deallocate(x)
   end subroutine