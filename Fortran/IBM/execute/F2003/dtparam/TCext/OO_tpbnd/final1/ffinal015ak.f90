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
         integer(kt1_1), pointer :: vector(:) => null()
      contains
         final :: finalize_t1s
      end type

      type t2 (kt2_1) ! kt2_1=4
         integer, kind :: kt2_1
         integer(kt2_1), pointer :: vector(:) => null()
      contains
         final :: finalize_t1v
      end type

      type t3 (kt3_1) ! kt3_1=4
         integer, kind :: kt3_1
         integer(kt3_1), pointer :: vector(:) => null()
      contains
         final :: finalize_t2e
      end type

      type(t1(4)), save :: a ! tcx: (4)
      type(t2(4)), save :: b(3) ! tcx: (4)
      type(t3(4)), save :: c(2, 2) ! tcx: (4)

   contains
      subroutine finalize_t1s(x)
         type(t1(4)) :: x ! tcx: (4)
         if (associated(x%vector))    deallocate(x%vector)
      end subroutine

      subroutine finalize_t1v(x)
         type(t2(4)) :: x(1:3) ! tcx: (4)
         do i = lbound(x, 1), ubound(x, 1)
            if (associated(x(i)%vector)) &
               deallocate(x(i)%vector)
         end do
      end subroutine
      elemental subroutine finalize_t2e(x)
         type(t3(4)), intent(inout) :: x ! tcx: (4)
         if (associated(x%vector))        deallocate(x%vector)
      end subroutine

   end module

   use mod
   call example(1)

   if(associated(a%vector))      error stop 101_4
   if(associated(b(1)%vector))   error stop 2_4
   if(associated(c(2,2)%vector)) error stop 3_4

   end

   subroutine example(n)
      use mod
      call finalize_t1s(a)
      call finalize_t1v(b)
      call finalize_t2e(c)
   end subroutine



! Extensions to introduce derived type parameters:
! type: t1 - added parameters (kt1_1) to invoke with (4) / declare with (4) - 2 changes
! type: t2 - added parameters (kt2_1) to invoke with (4) / declare with (4) - 2 changes
! type: t3 - added parameters (kt3_1) to invoke with (4) / declare with (4) - 2 changes
