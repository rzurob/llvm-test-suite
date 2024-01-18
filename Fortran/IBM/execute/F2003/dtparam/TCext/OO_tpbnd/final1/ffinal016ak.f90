!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal016ak.f
!*  TEST CASE NAME             : type-bound procedure ffinal016ak
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal016a by Catherine Sun)
!*  DATE                       : 2007-11-21 (original: )
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines 
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : testing final subroutines 
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      type t1 (kt1_1) ! kt1_1=4
         integer, kind :: kt1_1
         real(kt1_1), pointer :: p1 
      end type

      type, extends(t1) :: t2
         real(kt1_1), pointer :: p2 
      contains
         final :: finalize_t2
      end type

      type, extends(t2) :: t3
         real(kt1_1), pointer :: p3 
      contains
         final :: finalize_t3
      end type 

      type(t1(4)), allocatable :: dt1  ! tcx: (4)
      type(t2(4)), allocatable :: dt2  ! tcx: (4)
      type(t3(4)), allocatable :: dt3   ! tcx: (4)

   contains
      subroutine finalize_t2(x)
         type(t2(4)) :: x ! tcx: (4)
         if (associated(x%p1))    deallocate(x%p1)
         if (associated(x%p2))    deallocate(x%p2)
      end subroutine
 
      subroutine finalize_t3(x)
         type(t3(4)) :: x ! tcx: (4)
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



! Extensions to introduce derived type parameters:
! type: t1 - added parameters (kt1_1) to invoke with (4) / declare with (4) - 1 changes
! type: t2 - added parameters () to invoke with (4) / declare with (4) - 2 changes
! type: t3 - added parameters () to invoke with (4) / declare with (4) - 2 changes
