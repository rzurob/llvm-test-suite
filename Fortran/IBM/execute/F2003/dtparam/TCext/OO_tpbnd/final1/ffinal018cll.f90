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
      type t1 (lt1) ! lt1=1
         integer, len :: lt1
      contains
         final :: finalize_t1
      end type

      type, extends(t1) :: t2 (lt2) ! lt2=1
         integer, len :: lt2
      contains
         final :: finalize_t2
      end type

   contains
      subroutine finalize_t1(x)
         type(t1(*)), intent(inout) :: x ! tcx: (*)
         print *, "finalize_t1"
      end subroutine

      subroutine finalize_t2(x)
         type(t2(*,*)), intent(inout) :: x(:,:,:) ! tcx: (*,*)
         print *, "finalize_t2"
      end subroutine
   end module

   use mod
   call sub()

   end

   subroutine sub()
   use mod
   type(t2(:,:)), allocatable :: x(:,:,:) ! tcx: (:,:)
   allocate(t2(1,1)::x(1,2,2)) ! tcx: t2(1,1)
   deallocate(x)
   end subroutine


! Extensions to introduce derived type parameters:
! type: t1 - added parameters (lt1) to invoke with (1) / declare with (*) - 1 changes
! type: t2 - added parameters (lt2) to invoke with (1,1) / declare with (*,*) - 2 changes
