!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-22 (original: 2006-07-20)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array
!*                               Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement C476 (R455)
!*                               type-name must be accessible derived type
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
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
!*  inaccessible because they have not been imported (USEd) properly, and not
!*  just because they are private.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  implicit none

  type :: oldName (koldName_1,loldName_1) ! koldName_1,loldName_1=4,13
     integer, kind :: koldName_1
     integer, len :: loldName_1
  end type oldName

  type (oldName(4,13)) :: onArr(1), onArr0(0) ! tcx: (4,13)

contains

  function oldie()
    type (oldName(4,13)) :: oldie, o ! tcx: (4,13)
    oldie = o
  end function oldie

  subroutine test(arr)
    class (*) :: arr(:)
  end subroutine test

end module mod

program acetdt476gdkl

  use mod, newName => oldName
  implicit none
  integer :: i
  type (newName(4,13)) :: nnArr(1), nnArr0(0) ! tcx: (4,13)
  class (*), allocatable :: al(:)

!!! No errors expected
  call test((/ newName(4,13):: /)) ! tcx: (4,13)
  call test((/ newName(4,13):: (newName(4,13)(),i=5,4) /)) ! tcx: (4,13) ! tcx: (4,13)
  call test((/ newName(4,13):: newName(4,13)() /)) ! tcx: (4,13) ! tcx: (4,13)
  call test((/ newName(4,13):: oldie() /)) ! tcx: (4,13)
  nnArr0 = (/ newName(4,13):: /) ! tcx: (4,13)
  nnArr0 = (/ newName(4,13):: (newName(4,13)(),i=5,4) /) ! tcx: (4,13) ! tcx: (4,13)
  nnArr  = (/ newName(4,13):: newName(4,13)() /) ! tcx: (4,13) ! tcx: (4,13)
  nnArr  = (/ newName(4,13):: oldie() /) ! tcx: (4,13)
  allocate(al(1), source=(/newName(4,13):: newName(4,13)()/)) ! tcx: (4,13) ! tcx: (4,13)
  deallocate(al)
  allocate(al(1), source=(/newName(4,13):: oldie()/)) ! tcx: (4,13)
  deallocate(al)

  ! Also test okay []
  call test([ newName(4,13):: ]) ! tcx: (4,13)
  call test([ newName(4,13):: newName(4,13)() ]) ! tcx: (4,13) ! tcx: (4,13)
  nnArr =   [ newName(4,13):: (newName(4,13)(),i=1,1) ] ! tcx: (4,13) ! tcx: (4,13)
  allocate(al(1), source=[newName(4,13):: newName(4,13)()]) ! tcx: (4,13) ! tcx: (4,13)
  deallocate(al)

!!! Now try oldName - full of errors to come:
  call test((/ oldName(4,13):: /)) ! tcx: (4,13)
  call test((/ oldName(4,13):: (oldie(),i=5,4) /)) ! tcx: (4,13)
  call test((/ oldName(4,13):: oldie() /)) ! tcx: (4,13)
  onArr0 =  (/ oldName(4,13):: /) ! tcx: (4,13)
  onArr  =  (/ oldName(4,13):: oldie() /) ! tcx: (4,13)
  allocate(al(0), source=(/oldName(4,13):: (oldie(),i=5,4)/)) ! tcx: (4,13)
  deallocate(al)
  allocate(al(0), source=(/oldName(4,13):: /)) ! tcx: (4,13)
  deallocate(al)
  allocate(al(1), source=(/oldName(4,13):: oldie()/)) ! tcx: (4,13)
  deallocate(al)

  ! Also test bad []
  call test([ oldName(4,13):: ]) ! tcx: (4,13)
  call test([ oldName(4,13):: oldie() ]) ! tcx: (4,13)
  onArr =   [ oldName(4,13):: (oldie(),i=1,1) ] ! tcx: (4,13)
  allocate(al(0), source=[oldName(4,13):: (oldie(),i=1,0)]) ! tcx: (4,13)
  deallocate(al)
  allocate(al(0), source=[oldName(4,13)::]) ! tcx: (4,13)
  deallocate(al)
  allocate(al(1), source=[oldName(4,13):: oldie()]) ! tcx: (4,13)
  deallocate(al)

end program acetdt476gdkl


! Extensions to introduce derived type parameters:
! type: oldName - added parameters (koldName_1,loldName_1) to invoke with (4,13)/declare with (4,*) - 39 changes
