!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ffinal015a.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal015a.f
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
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
         integer, pointer :: vector(:) => null()
      contains
         final :: finalize_t1s
      end type

      type t2
         integer, pointer :: vector(:) => null()
      contains
         final :: finalize_t1v 
      end type 

      type t3  
         integer, pointer :: vector(:) => null()
      contains
         final :: finalize_t2e
      end type

      type(t1), save :: a
      type(t2), save :: b(3)
      type(t3), save :: c(2, 2)

   contains
      subroutine finalize_t1s(x)
         type(t1) :: x
         if (associated(x%vector))    deallocate(x%vector)
      end subroutine
 
      subroutine finalize_t1v(x)
         type(t2) :: x(1:3)
         do i = lbound(x, 1), ubound(x, 1)
            if (associated(x(i)%vector)) &
               deallocate(x(i)%vector)
         end do  
      end subroutine
      elemental subroutine finalize_t2e(x)
         type(t3), intent(inout) :: x
         if (associated(x%vector))        deallocate(x%vector)
      end subroutine

   end module

   use mod
   call example(1)

   if(associated(a%vector))      error stop 1_4
   if(associated(b(1)%vector))   error stop 2_4
   if(associated(c(2,2)%vector)) error stop 3_4

   end

   subroutine example(n)
      use mod
      call finalize_t1s(a)
      call finalize_t1v(b)
      call finalize_t2e(c)
   end subroutine

