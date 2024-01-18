!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt476gd
!*
!*  DATE                       : 2006-07-20
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : C476 (R455) type-name must be accessible derived type
!*
!*  REFERENCE                  : Feature Number 289053
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
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module mod

  implicit none

  type :: oldName
  end type oldName

  type (oldName) :: onArr(1), onArr0(0)

contains

  function oldie()
    type (oldName) :: oldie, o
    oldie = o
  end function oldie

  subroutine test(arr)
    class (*) :: arr(:)
  end subroutine test

end module mod

program acetdt476gd

  use mod, newName => oldName
  implicit none
  integer :: i
  type (newName) :: nnArr(1), nnArr0(0)
  class (*), allocatable :: al(:)

!!! No errors expected
  call test((/ newName:: /))
  call test((/ newName:: (newName(),i=5,4) /))
  call test((/ newName:: newName() /))
  call test((/ newName:: oldie() /))
  nnArr0 = (/ newName:: /)
  nnArr0 = (/ newName:: (newName(),i=5,4) /)
  nnArr  = (/ newName:: newName() /)
  nnArr  = (/ newName:: oldie() /)
  allocate(al(1), source=(/newName:: newName()/))
  deallocate(al)
  allocate(al(1), source=(/newName:: oldie()/))
  deallocate(al)

  ! Also test okay []
  call test([ newName:: ])
  call test([ newName:: newName() ])
  nnArr =   [ newName:: (newName(),i=1,1) ]
  allocate(al(1), source=[newName:: newName()])
  deallocate(al)

!!! Now try oldName - full of errors to come:
  call test((/ oldName:: /))
  call test((/ oldName:: (oldie(),i=5,4) /))
  call test((/ oldName:: oldie() /))
  onArr0 =  (/ oldName:: /)
  onArr  =  (/ oldName:: oldie() /)
  allocate(al(0), source=(/oldName:: (oldie(),i=5,4)/))
  deallocate(al)
  allocate(al(0), source=(/oldName:: /))
  deallocate(al)
  allocate(al(1), source=(/oldName:: oldie()/))
  deallocate(al)

  ! Also test bad []
  call test([ oldName:: ])
  call test([ oldName:: oldie() ])
  onArr =   [ oldName:: (oldie(),i=1,1) ]
  allocate(al(0), source=[oldName:: (oldie(),i=1,0)])
  deallocate(al)
  allocate(al(0), source=[oldName::])
  deallocate(al)
  allocate(al(1), source=[oldName:: oldie()])
  deallocate(al)

end program acetdt476gd
