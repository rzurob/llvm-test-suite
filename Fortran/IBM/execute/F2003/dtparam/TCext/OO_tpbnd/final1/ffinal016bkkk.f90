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
         real(kt1_1), allocatable :: p1
      end type

      type, extends(t1) :: t2 (kt2_1) ! kt2_1=4
         integer, kind :: kt2_1
         real(kt2_1), allocatable :: p2
      contains
         final :: finalize_t2
      end type

      type, extends(t2) :: t3 (kt3_1) ! kt3_1=4
         integer, kind :: kt3_1
         real(kt3_1), allocatable :: p3
      contains
         final :: finalize_t3
      end type

      type(t1(4)), pointer :: dt1  ! tcx: (4)
      type(t2(4,4)), pointer :: dt2  ! tcx: (4,4)
      type(t3(4,4,4)), pointer :: dt3   ! tcx: (4,4,4)

   contains
      subroutine finalize_t2(x)
         type(t2(4,4)) :: x ! tcx: (4,4)
         if (allocated(x%p1))    deallocate(x%p1)
         if (allocated(x%p2))    deallocate(x%p2)
      end subroutine

      subroutine finalize_t3(x)
         type(t3(4,4,4)) :: x ! tcx: (4,4,4)
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



! Extensions to introduce derived type parameters:
! type: t1 - added parameters (kt1_1) to invoke with (4) / declare with (4) - 1 changes
! type: t2 - added parameters (kt2_1) to invoke with (4,4) / declare with (4,4) - 2 changes
! type: t3 - added parameters (kt3_1) to invoke with (4,4,4) / declare with (4,4,4) - 2 changes
