!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal018akk.f
!*  TEST CASE NAME             : type-bound procedure ffinal018akk
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
      type t1 (kt1) ! kt1=2
         integer, kind :: kt1
      contains
         final :: finalize_t1
      end type

      type, extends(t1) :: t2 (kt2) ! kt2=2
         integer, kind :: kt2
      contains
         final :: finalize_t2
      end type

   contains
      subroutine finalize_t1(x)
         type(t1(2)), intent(inout) :: x ! tcx: (2)
         print *, "finalize_t1"
      end subroutine

      subroutine finalize_t2(x)
         type(t2(2,2)), intent(inout) :: x(1:3) ! tcx: (2,2)
         print *, "finalize_t2"
      end subroutine
   end module

   use mod
   call sub()

   end

   subroutine sub()
   use mod
   type(t2(2,2)), allocatable :: x(:) ! tcx: (2,2)
   allocate(x(1:3))
   deallocate(x)
   end subroutine


! Extensions to introduce derived type parameters:
! type: t1 - added parameters (kt1) to invoke with (2) / declare with (2) - 1 changes
! type: t2 - added parameters (kt2) to invoke with (2,2) / declare with (2,2) - 2 changes
