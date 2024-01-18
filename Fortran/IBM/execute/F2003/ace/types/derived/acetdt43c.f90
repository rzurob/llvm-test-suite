!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt43c
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-11-08
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : lookalike function invocations as ac_values
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : derived type, lookalike
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Similar to acetint43c, we invoke functions with names identical to derived types.
!*  FIX (07/10/17): specific function replaced by generic function using same name for interface.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt43cmod

  implicit none

  type derived
     integer :: val = 4, v2 = 6
  end type derived

  interface derived
    module procedure derived_specific
  end interface

contains

  type (derived) function derived_specific(a,b)
    integer :: a
    real :: b
    type (derived) :: result
    result % val = -a
    result % v2  = -int(b)
    derived_specific = result
  end function derived_specific

end module acetdt43cmod


program acetdt43c

  use acetdt43cmod
  implicit none

  type (derived) :: darr(6), dt

  integer(4)     :: i

  print *, [derived(1,2), derived(a=3,b=4.), derived(5), derived(val=7), derived(v2=8), derived(val=9,v2=10)]

  print *, [derived(1,2), derived(a=3,b=4.), derived(5), derived(val=7), derived(v2=8), derived(val=9,v2=10)],&
           [derived::     derived(1,2), derived(a=3,b=4.), derived(5), derived(val=7), derived(v2=8), derived(val=9,v2=10)],&
           [derived::     (derived(1,2), derived(a=3,b=4.), derived(5), derived(val=7), derived(v2=8), derived(val=9,v2=10), i=1,1)]

  darr  =  [derived::     derived(1,2), derived(a=3,b=4.), derived(5), derived(val=7), derived(v2=8), derived(val=9,v2=10)]

  if (any([(darr(i) % val, darr(i) % v2, i=1,6)] /= [1, 2, -3, -4, 5, 6, 7, 6, 4, 8, 9, 10])) stop 2

  darr  =  [derived::     (derived(1,2), derived(a=3,b=4.), derived(5), derived(val=7), derived(v2=8), derived(val=9,v2=10), i=1,1)]

  if (any([(darr(i) % val, darr(i) % v2, i=1,6)] /= [1, 2, -3, -4, 5, 6, 7, 6, 4, 8, 9, 10])) stop 3

  darr  =  [derived::     (derived(i,i+1), derived(a=i+2,b=real(i+3)), derived(i+4), derived(val=i+6), derived(v2=i*8), derived(val=i*9,v2=i*10), i=1,1)]

  if (any([(darr(i) % val, darr(i) % v2, i=1,6)] /= [1, 2, -3, -4, 5, 6, 7, 6, 4, 8, 9, 10])) stop 4

end program acetdt43c
