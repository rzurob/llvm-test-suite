!*******************************************************************************
!*  ============================================================================
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
!*  requires type-name to be an accessible derived type.  Types can be made
!*  private explicitly, or as part of the default accessibility of a module
!*  (assuming the type is not then made public explicitly).  Here, we use a
!*  module with default private accessibility, and make two of the types
!*  public - one a superclass, and one a subclass.  To make manipulation of
!*  otherwise undeclarable variables possible, we declare them in the module,
!*  and make them public.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module mod

  implicit none
  private

  type :: priv
  end type priv

  type, extends(priv) :: pub
  end type pub

  type :: base
  end type base

  type, extends (base) :: ext
  end type ext

  type (priv), public :: privItem, privArr(1), privArr0(0)
  type (pub), public  :: pubItem, pubArr(1), pubArr0(0)
  type (base), public :: bItem, bArr(1), bArr0(0)
  type (ext), public  :: eItem, eArr(1), eArr0(0)

  public :: pub, base, test

contains

  subroutine test(arr)
    class (*) :: arr(:)
  end subroutine test

end module mod

program acetdt476ed

  use mod
  implicit none
  integer :: i
  class (*), allocatable :: al(:)

!!! No errors expected
  call test((/ base:: /))
  call test((/ base:: (bItem,i=5,4) /))
  call test((/ base:: bItem /))
  barr0 = (/ base:: /)
  barr  = (/ base:: bItem /)
  allocate(al(1), source=(/base:: bItem/))
  deallocate(al)

!!! Repeat with pub
  call test((/ pub:: /))
  call test((/ pub:: (pubItem,i=5,4) /))
  call test((/ pub:: pubItem /))
  pubArr0 = (/ pub:: /)
  pubArr  = (/ pub:: pubItem /)
  allocate(al(1), source=(/pub:: pubItem/))
  deallocate(al)

  ! Also test okay []
  call test([ base:: ])
  call test([ pub:: ])
  call test([ base:: (bItem,i=1,1) ])
  call test([ pub:: pubItem ])
  bArr =    [ base::(bItem,i=1,1) ]
  pubArr =  [ pub:: (pubItem,i=1,1) ]
  allocate(al(1), source=[pub::  pubItem])
  deallocate(al)
  allocate(al(1), source=[base:: bItem])
  deallocate(al)

!!! Try with priv and ext - lots of errors to come!
  call test((/ ext:: /))
  call test((/ ext:: (eItem,i=5,4) /))
  call test((/ ext:: eItem /))
  eArr0 =   (/ ext:: /)
  eArr  =   (/ ext:: eItem /)
  allocate(al(1), source=(/ext:: eItem/))
  deallocate(al)

  call test((/ priv:: /))
  call test((/ priv:: (privItem,i=5,4) /))
  call test((/ priv:: privItem /))
  privArr0= (/ priv:: /)
  privArr = (/ priv:: privItem /)
  allocate(al(1), source=(/priv:: privItem/))
  deallocate(al)

  ! Also test bad []
  call test([ ext:: ])
  call test([ priv:: ])
  call test([ ext:: eItem ])
  call test([ priv:: (privItem,i=1,1) ])
  privArr = [ priv:: (privItem,i=1,1) ]
  eArr =    [ ext:: (eItem,i=1,1) ]
  allocate(al(1), source=[priv:: privItem])
  deallocate(al)
  allocate(al(1), source=[ext:: eItem])
  deallocate(al)

end program acetdt476ed
