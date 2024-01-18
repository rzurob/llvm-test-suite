!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ffinal016a.f
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
!*  TEST CASE NAME             : ffinal016a.f
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
         real, pointer :: p1 
      end type

      type, extends(t1) :: t2
         real, pointer :: p2 
      contains
         final :: finalize_t2
      end type

      type, extends(t2) :: t3
         real, pointer :: p3 
      contains
         final :: finalize_t3
      end type 

      type(t1), allocatable :: dt1 
      type(t2), allocatable :: dt2 
      type(t3), allocatable :: dt3  

   contains
      subroutine finalize_t2(x)
         type(t2) :: x
         if (associated(x%p1))    deallocate(x%p1)
         if (associated(x%p2))    deallocate(x%p2)
      end subroutine
 
      subroutine finalize_t3(x)
         type(t3) :: x
         if (associated(x%p3))        deallocate(x%p3)
      end subroutine

   end module

   use mod
   call example
   if(associated(dt1%p1) .neqv. .false.)    error stop 2_4
   if(associated(dt2%p2) .neqv. .false.)    error stop 3_4
   if(associated(dt3%p3) .neqv. .false.)    error stop 4_4

   end

   subroutine example

   use mod

      allocate(dt1, dt2, dt3)
      deallocate(dt1, dt2, dt3)

   end subroutine

