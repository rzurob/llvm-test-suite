!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-21 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing final subroutines
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      type t1 (kt1_1) ! kt1_1=4
         integer, kind :: kt1_1
         integer(kt1_1), allocatable :: vector
      contains
         final :: finalize_t1s
      end type

      type(t1(4)), allocatable :: dt1 ! tcx: (4)

   contains
      subroutine finalize_t1s(x)
         type(t1(4)) :: x ! tcx: (4)
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



! Extensions to introduce derived type parameters:
! type: t1 - added parameters (kt1_1) to invoke with (4) / declare with (4) - 2 changes
