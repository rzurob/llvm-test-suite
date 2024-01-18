!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal016b.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal016b.f
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
         real, allocatable :: p1
      end type

      type, extends(t1) :: t2
         real, allocatable :: p2
      contains
         final :: finalize_t2
      end type

      type, extends(t2) :: t3
         real, allocatable :: p3
      contains
         final :: finalize_t3
      end type

      type(t1), pointer :: dt1
      type(t2), pointer :: dt2
      type(t3), pointer :: dt3

   contains
      subroutine finalize_t2(x)
         type(t2) :: x
         if (allocated(x%p1))    deallocate(x%p1)
         if (allocated(x%p2))    deallocate(x%p2)
      end subroutine

      subroutine finalize_t3(x)
         type(t3) :: x
         if (allocated(x%p3))        deallocate(x%p3)
      end subroutine

   end module

   use mod
   call example

   if(allocated(dt1%p1) .neqv. .false.)    error stop 2_4
   if(allocated(dt2%p2) .neqv. .false.)    error stop 3_4
   if(allocated(dt3%p3) .neqv. .false.)    error stop 4_4

   end

   subroutine example

   use mod

      allocate(dt1, dt2, dt3)
      deallocate(dt1, dt2, dt3)

   end subroutine

