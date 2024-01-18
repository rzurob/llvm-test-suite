!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt476edlk
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-22 (original: 2006-07-20)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
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
!*  requires type-name to be an accessible derived type.  Types can be made
!*  private explicitly, or as part of the default accessibility of a module
!*  (assuming the type is not then made public explicitly).  Here, we use a
!*  module with default private accessibility, and make two of the types
!*  public - one a superclass, and one a subclass.  To make manipulation of
!*  otherwise undeclarable variables possible, we declare them in the module,
!*  and make them public.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  implicit none
  private

  type :: priv (lpriv_1) ! lpriv_1=11
     integer, len :: lpriv_1
  end type priv

  type, extends(priv) :: pub (kpub_1) ! kpub_1=4
     integer, kind :: kpub_1
  end type pub

  type :: base (lbase_1) ! lbase_1=3
     integer, len :: lbase_1
  end type base

  type, extends (base) :: ext
  end type ext

  type (priv(11)), public :: privItem, privArr(1), privArr0(0) ! tcx: (11)
  type (pub(11,4)), public  :: pubItem, pubArr(1), pubArr0(0) ! tcx: (11,4)
  type (base(3)), public :: bItem, bArr(1), bArr0(0) ! tcx: (3)
  type (ext(3)), public  :: eItem, eArr(1), eArr0(0) ! tcx: (3)

  public :: pub, base, test

contains

  subroutine test(arr)
    class (*) :: arr(:)
  end subroutine test

end module mod

program acetdt476edlk

  use mod
  implicit none
  integer :: i
  class (*), allocatable :: al(:)

!!! No errors expected
  call test((/ base(3):: /)) ! tcx: (3)
  call test((/ base(3):: (bItem,i=5,4) /)) ! tcx: (3)
  call test((/ base(3):: bItem /)) ! tcx: (3)
  barr0 = (/ base(3):: /) ! tcx: (3)
  barr  = (/ base(3):: bItem /) ! tcx: (3)
  allocate(al(1), source=(/base(3):: bItem/)) ! tcx: (3)
  deallocate(al)

!!! Repeat with pub
  call test((/ pub(11,4):: /)) ! tcx: (11,4)
  call test((/ pub(11,4):: (pubItem,i=5,4) /)) ! tcx: (11,4)
  call test((/ pub(11,4):: pubItem /)) ! tcx: (11,4)
  pubArr0 = (/ pub(11,4):: /) ! tcx: (11,4)
  pubArr  = (/ pub(11,4):: pubItem /) ! tcx: (11,4)
  allocate(al(1), source=(/pub(11,4):: pubItem/)) ! tcx: (11,4)
  deallocate(al)

  ! Also test okay []
  call test([ base(3):: ]) ! tcx: (3)
  call test([ pub(11,4):: ]) ! tcx: (11,4)
  call test([ base(3):: (bItem,i=1,1) ]) ! tcx: (3)
  call test([ pub(11,4):: pubItem ]) ! tcx: (11,4)
  bArr =    [ base(3)::(bItem,i=1,1) ] ! tcx: (3)
  pubArr =  [ pub(11,4):: (pubItem,i=1,1) ] ! tcx: (11,4)
  allocate(al(1), source=[pub(11,4)::  pubItem]) ! tcx: (11,4)
  deallocate(al)
  allocate(al(1), source=[base(3):: bItem]) ! tcx: (3)
  deallocate(al)

!!! Try with priv and ext - lots of errors to come!
  call test((/ ext(3):: /)) ! tcx: (3)
  call test((/ ext(3):: (eItem,i=5,4) /)) ! tcx: (3)
  call test((/ ext(3):: eItem /)) ! tcx: (3)
  eArr0 =   (/ ext(3):: /) ! tcx: (3)
  eArr  =   (/ ext(3):: eItem /) ! tcx: (3)
  allocate(al(1), source=(/ext(3):: eItem/)) ! tcx: (3)
  deallocate(al)

  call test((/ priv(11):: /)) ! tcx: (11)
  call test((/ priv(11):: (privItem,i=5,4) /)) ! tcx: (11)
  call test((/ priv(11):: privItem /)) ! tcx: (11)
  privArr0= (/ priv(11):: /) ! tcx: (11)
  privArr = (/ priv(11):: privItem /) ! tcx: (11)
  allocate(al(1), source=(/priv(11):: privItem/)) ! tcx: (11)
  deallocate(al)

  ! Also test bad []
  call test([ ext(3):: ]) ! tcx: (3)
  call test([ priv(11):: ]) ! tcx: (11)
  call test([ ext(3):: eItem ]) ! tcx: (3,)
  call test([ priv(11):: (privItem,i=1,1) ]) ! tcx: (11)
  privArr = [ priv(11):: (privItem,i=1,1) ] ! tcx: (11)
  eArr =    [ ext(3):: (eItem,i=1,1) ] ! tcx: (3)
  allocate(al(1), source=[priv(11):: privItem]) ! tcx: (11)
  deallocate(al)
  allocate(al(1), source=[ext(3):: eItem]) ! tcx: (3)
  deallocate(al)

end program acetdt476edlk


! Extensions to introduce derived type parameters:
! type: priv - added parameters (lpriv_1) to invoke with (11)/declare with (*) - 11 changes
! type: pub - added parameters (kpub_1) to invoke with (11,4)/declare with (*,4) - 11 changes
! type: base - added parameters (lbase_1) to invoke with (3)/declare with (*) - 11 changes
! type: ext - added parameters () to invoke with (3)/declare with (*) - 11 changes
