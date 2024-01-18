! GM DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/F2003/ace/types/derived/acetdt476a.f

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt476akl_rb
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-17 (original: 2006-07-19)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
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
!*  "derived-type-spec is type-name [(type-param-spec-list)]"
!*  requires type-name to be an accessible derived type.  Here, we verify that
!*  an accessible derived type is, in fact, permitted, and that the correct type
!*  is used.  We do this also for an extended type, and for a type which is
!*  explicitly public.  Testing variants - inaccessible and nonexistant - is
!*  tested elsewhere.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  implicit none

  type, private :: priv(k1,l1)    ! (4,20)
      integer, kind :: k1
      integer, len  :: l1
  end type priv

  type, extends(priv), public :: pub    ! (4,20)
  end type pub

  type, public :: base(l2,k2)    ! (20,4)
     integer, kind :: k2
     integer, len  :: l2
     integer(k2)   :: val = -1
  end type base

  type, extends (base) :: ext    ! (20,4)
     integer(k2) :: e
  end type ext

  enum, bind(C)
    enumerator :: UNKNOWN_TYPE, BASE_TYPE, EXT_TYPE, PRIV_TYPE, PUB_TYPE
  end enum

contains

  character(8) function expectedType(t)
    integer :: t
    select case (t)
       case(PRIV_TYPE); expectedType = 'priv'
       case(PUB_TYPE);  expectedType = 'pub'
       case(BASE_TYPE); expectedType = 'base'
       case(EXT_TYPE);  expectedType = 'ext'
       case default;    expectedType = 'unknown'
    end select
  end function expectedType

  integer(4) function errorIfUnexpectedType(t,exp)
    integer :: t, exp
    if (t /= exp) then
       print *, "Expected type '", trim(expectedType(t)), "', actual type is '", trim(expectedType(exp)), "'"
       errorIfUnexpectedType = 8
    else
       errorIfUnexpectedType = 0
    end if
  end function errorIfUnexpectedType

  subroutine test(arr,t,l,u,s,testNumber)
    class (*) :: arr(:)
    integer :: t, l, u, s
    integer(4) :: retcode, limitsError, testNumber

    retcode = 0
    if (l /= lbound(arr,1)) then
       print *, "Expected lbound of ", l, " found ", lbound(arr,1)
       retcode = retcode + 1
    end if
    if (u /= ubound(arr,1)) then
       print *, "Expected ubound of ", u, " found ", ubound(arr,1)
       retcode = retcode + 2
    end if
    if (s /= size(arr)) then
       print *, "Expected size of ", s, " found ", size(arr)
       retcode = retcode + 4
    end if


    select type (a => arr)

    type is (priv(4,*))
       retcode = retcode + errorIfUnexpectedType(t, PRIV_TYPE)

    type is (pub(4,*))
       retcode = retcode + errorIfUnexpectedType(t, PUB_TYPE)

    type is (base(*,4))
       retcode = retcode + errorIfUnexpectedType(t, BASE_TYPE)

    type is (ext(*,4))
       retcode = retcode + errorIfUnexpectedType(t, EXT_TYPE)

    class default
       retcode = retcode + errorIfUnexpectedType(t, UNKNOWN_TYPE)

    end select

    if (retcode == 0) return

    retcode = retcode + testNumber
    call zzrc(retcode)

  end subroutine test

end module mod

program acetdt476akl_rb

  use mod
  implicit none
  integer :: i
  class (*), allocatable :: al(:)
  type (base(20,4)) :: barr(3), bItem1, bItem2, bItem3, barr0(0)
  type (ext(20,4)) :: extarr(3), exItem1, exItem2, exItem3, extarr0(0)

  bItem1 = base(20,4)(1)
  bItem2 = base(20,4)(2)
  bItem3 = base(20,4)(3)

  call test((/ base(20,4):: /), BASE_TYPE, 1, 0, 0, 0_4)
  call test((/ base(20,4):: (base(20,4)(),i=5,4) /), BASE_TYPE, 1, 0, 0, 16_4)

  call test((/ base(20,4):: bItem1, bItem2, bItem3 /), BASE_TYPE, 1, 3, 3, 32_4)
  barr = (/ base(20,4):: bItem1, bItem2, bItem3 /)
  if (barr(1)%val /= 1 .or. barr(2)%val /= 2 .or. barr(3)%val /= 3) error stop 32_4

  call test((/ base(20,4):: (base(20,4)(i),i=1,3) /), BASE_TYPE, 1, 3, 3, 48_4)
  barr = (/ base(20,4):: (base(20,4)(i),i=1,3) /)
  if (barr(1)%val /= 1 .or. barr(2)%val /= 2 .or. barr(3)%val /= 3) error stop 48_4

  allocate(al(1), source=(/base(20,4):: bItem1/))
  call test(al, BASE_TYPE, 1, 1, 1, 64_4)
  deallocate(al)

  exItem1 = ext(20,4)(e=1)
  exItem2 = ext(20,4)(e=2)
  exItem3 = ext(20,4)(e=3)

  call test((/ ext(20,4):: /), EXT_TYPE, 1, 0, 0, 80_4)
  call test((/ ext(20,4):: (ext(20,4)(e=i),i=5,4) /), EXT_TYPE, 1, 0, 0, 96_4)

  call test((/ ext(20,4):: exItem1, exItem2, exItem3 /), EXT_TYPE, 1, 3, 3, 112_4)
  extarr = (/ ext(20,4):: exItem1, exItem2, exItem3 /)
  if (extarr(1)%e /= 1 .or. extarr(2)%e /= 2 .or. extarr(3)%e /= 3) error stop 112_4

  call test((/ ext(20,4):: (ext(20,4)(e=i),i=1,3) /), EXT_TYPE, 1, 3, 3, 128_4)
  extarr = (/ ext(20,4):: (ext(20,4)(e=i),i=1,3) /)
  if (extarr(1)%e /= 1 .or. extarr(2)%e /= 2 .or. extarr(3)%e /= 3) error stop 128_4

  call test((/ pub(4,20):: (pub(4,20)(),i=4,1) /), PUB_TYPE, 1,0,0, 144_4)
  call test((/ pub(4,20):: (pub(4,20)(),i=1,4) /), PUB_TYPE, 1,4,4, 160_4)

  allocate(al(1), source=(/ext(20,4):: ext(20,4)(e=1)/))
  call test(al, EXT_TYPE, 1, 1, 1, 176_4)
  deallocate(al)
  allocate(al(1), source=(/pub(4,20):: pub(4,20)()/))
  call test(al, PUB_TYPE, 1, 1, 1, 192_4)
  deallocate(al)

  ! Also test []:
  call test([ base(20,4):: ], BASE_TYPE, 1, 0, 0, 208_4)
  call test([ ext(20,4):: ], EXT_TYPE, 1, 0, 0, 224_4)
  call test([ base(20,4):: (base(20,4)(i),i=1,3) ], BASE_TYPE, 1, 3, 3, 240_4)
  call test([ ext(20,4):: exItem1, exItem2, exItem3 ], EXT_TYPE, 1, 3, 3, 256_4)
  extarr = [ ext(20,4):: (ext(20,4)(e=i),i=1,3) ]
  allocate(al(1), source=[pub(4,20):: pub(4,20)()])
  call test(al, PUB_TYPE, 1, 1, 1, 272_4)

end program acetdt476akl_rb
