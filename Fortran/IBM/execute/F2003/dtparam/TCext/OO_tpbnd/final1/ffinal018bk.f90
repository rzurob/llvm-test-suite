!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-26 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing final subroutines: derived
!*                               type without data components: this
!*                               testcase ICE with 20040414 driver
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      type t1 (kt1) ! kt1=1
         integer, kind :: kt1
      contains
         final :: finalize_t1
      end type

      type, extends(t1) :: t2
      contains
         final :: finalize_t2
      end type

   contains
      subroutine finalize_t1(x)
         type(t1(1)), intent(inout) :: x ! tcx: (1)
         print *, "finalize_t1"
      end subroutine

      subroutine finalize_t2(x)
         type(t2(1)), intent(inout) :: x(1,3) ! tcx: (1)
         print *, "finalize_t2"
      end subroutine
   end module

   use mod
   call sub()

   end

   subroutine sub()
   use mod
   type(t2(1)), allocatable :: x(:,:) ! tcx: (1)
   allocate(x(1,3))
   deallocate(x)
   end subroutine


! Extensions to introduce derived type parameters:
! type: t1 - added parameters (kt1) to invoke with (1) / declare with (1) - 1 changes
! type: t2 - added parameters () to invoke with (1) / declare with (1) - 2 changes
