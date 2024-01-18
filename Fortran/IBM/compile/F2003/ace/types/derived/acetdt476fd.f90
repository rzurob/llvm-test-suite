!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt476fd
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-20
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : C476 (R455) type-name must be accessible derived type
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
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
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module mod

  implicit none

  type :: notUsed
  end type notUsed

  type :: isUsed
  end type isUsed

  type (isUsed) :: iuArr(1), iuArr0(0)
  type (notUsed) :: nuArr(1), nuArr0(0), nuItem

contains

  subroutine test(arr)
    class (*) :: arr(:)
  end subroutine test

end module mod

program acetdt476fd

  use mod, only : isUsed, iuArr, iuArr0, nuArr, nuArr0, nuItem
  implicit none
  integer :: i
  class (*), allocatable :: al(:)

!!! No errors expected
  call test((/ isUsed:: /))
  call test((/ isUsed:: (isUsed(),i=1,0) /))
  call test((/ isUsed:: isUsed() /))
  iuArr0 = (/ isUsed:: /)
  iuArr0 = (/ isUsed:: (isUsed(),i=1,0) /)
  iuArr  = (/ isUsed:: isUsed() /)
  allocate(al(1), source=(/isUsed:: isUsed()/))
  deallocate(al)
  allocate(al(0), source=(/isUsed::/))
  deallocate(al)

  ! Also test okay []
  call test([ isUsed:: ])
  call test([ isUsed:: (isUsed(),i=1,1) ])
  iuArr0 =  [ isUsed:: ]
  iuArr0 =  [ isUsed:: (isUsed(),i=1,0) ]
  iuArr  =  [ isUsed:: (isUsed(),i=1,1) ]
  allocate(al(1), source=[isUsed:: isUsed()])
  deallocate(al)

!!! Now try notUsed - full of errors to come:
  call test((/ notUsed:: /))
  call test((/ notUsed:: nuItem /))
  nuArr0 =  (/ notUsed:: /)
  nuArr0 =  (/ notUsed:: (nuItem,i=1,0) /)
  nuArr  =  (/ notUsed:: nuItem /)
  allocate(al(0), source=(/notUsed::/))
  deallocate(al)
  allocate(al(0), source=(/notUsed:: (nuItem,i=1,0)/))
  deallocate(al)
  allocate(al(1), source=(/notUsed:: nuItem/))
  deallocate(al)

  ! Also test bad []
  call test([ notUsed:: ])
  call test([ notUsed:: (nuItem,i=1,1) ])
  call test([ notUsed:: (nuItem,i=1,0) ])
  nuArr0 =  [ notUsed:: ]
  nuArr0 =  [ notUsed:: (nuItem,i=1,0) ]
  nuArr  =  [ notUsed:: (nuItem,i=1,1) ]
  nuArr  =  [ notUsed:: nuItem ]
  allocate(al(0), source=[notUsed::])
  deallocate(al)
  allocate(al(0), source=[notUsed:: (nuItem,i=1,0)])
  deallocate(al)
  allocate(al(1), source=[notUsed:: nuItem])
  deallocate(al)

end program acetdt476fd
