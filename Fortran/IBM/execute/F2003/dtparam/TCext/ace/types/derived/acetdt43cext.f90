! GM DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/ace/types/derived/acetdt43c.f

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt43cext
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-17 (original: 2006-11-08)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : derived type, lookalike
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Similar to acetint43c, we invoke functions with names identical to derived
!*  types. FIX (07/10/17): specific function replaced by generic function using
!*  same name for interface.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt43cextmod

  implicit none

  type derived(k1)    ! (4)
     integer, kind :: k1
     integer(k1)   :: val = 4, v2 = 6
  end type derived

  interface derived
    module procedure derived_specific
  end interface

contains

  type (derived(4)) function derived_specific(a,b)
    integer :: a
    real :: b
    type (derived(4)) :: result
    result % val = -a
    result % v2  = -int(b)
    derived_specific = result
  end function derived_specific

end module acetdt43cextmod


program acetdt43cext

  use acetdt43cextmod
  implicit none

  type (derived(4)) :: darr(6), dt

  integer(4)     :: i

  print *, [derived(4)(1,2), derived(a=3,b=4.), derived(4)(5), derived(4)(val=7), derived(4)(v2=8), derived(4)(val=9,v2=10)]

  print *, [derived(4)(1,2), derived(a=3,b=4.), derived(4)(5), derived(4)(val=7), derived(4)(v2=8), derived(4)(val=9,v2=10)],&
           [derived(4)::     derived(4)(1,2), derived(a=3,b=4.), derived(4)(5), derived(4)(val=7), derived(4)(v2=8), derived(4)(val=9,v2=10)],&
           [derived(4)::     (derived(4)(1,2), derived(a=3,b=4.), derived(4)(5), derived(4)(val=7), derived(4)(v2=8), derived(4)(val=9,v2=10), i=1,1)]

  darr  =  [derived(4)::     derived(4)(1,2), derived(a=3,b=4.), derived(4)(5), derived(4)(val=7), derived(4)(v2=8), derived(4)(val=9,v2=10)]

  if (any([(darr(i) % val, darr(i) % v2, i=1,6)] /= [1, 2, -3, -4, 5, 6, 7, 6, 4, 8, 9, 10])) stop 2

  darr  =  [derived(4)::     (derived(4)(1,2), derived(a=3,b=4.), derived(4)(5), derived(4)(val=7), derived(4)(v2=8), derived(4)(val=9,v2=10), i=1,1)]

  if (any([(darr(i) % val, darr(i) % v2, i=1,6)] /= [1, 2, -3, -4, 5, 6, 7, 6, 4, 8, 9, 10])) stop 3

  darr  =  [derived(4)::     (derived(4)(i,i+1), derived(a=i+2,b=real(i+3)), derived(4)(i+4), derived(4)(val=i+6), derived(4)(v2=i*8), derived(4)(val=i*9,v2=i*10), i=1,1)]

  if (any([(darr(i) % val, darr(i) % v2, i=1,6)] /= [1, 2, -3, -4, 5, 6, 7, 6, 4, 8, 9, 10])) stop 4

end program acetdt43cext
