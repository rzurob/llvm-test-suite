!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt476fdk
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt476fd
!*                               by David Forster)
!*  DATE                       : 2008-01-22 (original: 2006-07-20)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array
!*                               Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement C476 (R455)
!*                               type-name must be accessible derived type
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : array constructor, accessible, derived type
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Constraint C476 on rule R455:
!*     "derived-type-spec is type-name [(type-param-spec-list)]"
!*  requires type-name to be an accessible derived type.  Types can be
!*  inaccessible because they have been renamed, and not just because they are
!*  private.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  implicit none

  type :: notUsed (knotUsed_1,knotUsed_2) ! knotUsed_1,knotUsed_2=8,4
     integer, kind :: knotUsed_1,knotUsed_2
  end type notUsed

  type :: isUsed (kisUsed_1) ! kisUsed_1=4
     integer, kind :: kisUsed_1
  end type isUsed

  type (isUsed(4)) :: iuArr(1), iuArr0(0) ! tcx: (4)
  type (notUsed(8,4)) :: nuArr(1), nuArr0(0), nuItem ! tcx: (8,4)

contains

  subroutine test(arr)
    class (*) :: arr(:)
  end subroutine test

end module mod

program acetdt476fdk

  use mod, only : isUsed, iuArr, iuArr0, nuArr, nuArr0, nuItem
  implicit none
  integer :: i
  class (*), allocatable :: al(:)

!!! No errors expected
  call test((/ isUsed(4):: /)) ! tcx: (4)
  call test((/ isUsed(4):: (isUsed(4)(),i=1,0) /)) ! tcx: (4) ! tcx: (4)
  call test((/ isUsed(4):: isUsed(4)() /)) ! tcx: (4) ! tcx: (4)
  iuArr0 = (/ isUsed(4):: /) ! tcx: (4)
  iuArr0 = (/ isUsed(4):: (isUsed(4)(),i=1,0) /) ! tcx: (4) ! tcx: (4)
  iuArr  = (/ isUsed(4):: isUsed(4)() /) ! tcx: (4) ! tcx: (4)
  allocate(al(1), source=(/isUsed(4):: isUsed(4)()/)) ! tcx: (4) ! tcx: (4)
  deallocate(al)
  allocate(al(0), source=(/isUsed(4)::/)) ! tcx: (4)
  deallocate(al)

  ! Also test okay []
  call test([ isUsed(4):: ]) ! tcx: (4)
  call test([ isUsed(4):: (isUsed(4)(),i=1,1) ]) ! tcx: (4) ! tcx: (4)
  iuArr0 =  [ isUsed(4):: ] ! tcx: (4)
  iuArr0 =  [ isUsed(4):: (isUsed(4)(),i=1,0) ] ! tcx: (4) ! tcx: (4)
  iuArr  =  [ isUsed(4):: (isUsed(4)(),i=1,1) ] ! tcx: (4) ! tcx: (4)
  allocate(al(1), source=[isUsed(4):: isUsed(4)()]) ! tcx: (4) ! tcx: (4)
  deallocate(al)

!!! Now try notUsed - full of errors to come:
  call test((/ notUsed(8,4):: /)) ! tcx: (8,4)
  call test((/ notUsed(8,4):: nuItem /)) ! tcx: (8,4)
  nuArr0 =  (/ notUsed(8,4):: /) ! tcx: (8,4)
  nuArr0 =  (/ notUsed(8,4):: (nuItem,i=1,0) /) ! tcx: (8,4)
  nuArr  =  (/ notUsed(8,4):: nuItem /) ! tcx: (8,4)
  allocate(al(0), source=(/notUsed(8,4)::/)) ! tcx: (8,4)
  deallocate(al)
  allocate(al(0), source=(/notUsed(8,4):: (nuItem,i=1,0)/)) ! tcx: (8,4)
  deallocate(al)
  allocate(al(1), source=(/notUsed(8,4):: nuItem/)) ! tcx: (8,4)
  deallocate(al)

  ! Also test bad []
  call test([ notUsed(8,4):: ]) ! tcx: (8,4)
  call test([ notUsed(8,4):: (nuItem,i=1,1) ]) ! tcx: (8,4)
  call test([ notUsed(8,4):: (nuItem,i=1,0) ]) ! tcx: (8,4)
  nuArr0 =  [ notUsed(8,4):: ] ! tcx: (8,4)
  nuArr0 =  [ notUsed(8,4):: (nuItem,i=1,0) ] ! tcx: (8,4)
  nuArr  =  [ notUsed(8,4):: (nuItem,i=1,1) ] ! tcx: (8,4)
  nuArr  =  [ notUsed(8,4):: nuItem ] ! tcx: (8,4)
  allocate(al(0), source=[notUsed(8,4)::]) ! tcx: (8,4)
  deallocate(al)
  allocate(al(0), source=[notUsed(8,4):: (nuItem,i=1,0)]) ! tcx: (8,4)
  deallocate(al)
  allocate(al(1), source=[notUsed(8,4):: nuItem]) ! tcx: (8,4)
  deallocate(al)

end program acetdt476fdk


! Extensions to introduce derived type parameters:
! type: notUsed - added parameters (knotUsed_1,knotUsed_2) to invoke with (8,4)/declare with (8,4) - 19 changes
! type: isUsed - added parameters (kisUsed_1) to invoke with (4)/declare with (4) - 24 changes
