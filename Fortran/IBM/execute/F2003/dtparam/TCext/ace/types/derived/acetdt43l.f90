! GM DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/ace/types/derived/acetdt43.f

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt43l
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-17 (original: 2006-11-02)
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
!*  KEYWORD(S)                 : statement function
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Use a statement function identical to the constructor for a derived type in
!*  AC's, verifying the values.
!*  We print and assign the values, to test two different types of use.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt43lmod

  implicit none

  type derived(l1,k1)    ! (20,4)
     integer, kind :: k1
     integer, len  :: l1
     integer(k1)   :: val = 4, v2 = 6
  end type derived

end module acetdt43lmod


program acetdt43l

  use acetdt43lmod
  implicit none

  type (derived(20,4)) :: derived, darr(1)

  integer(4)     :: i

  ! The statement function:
  derived(i) = d2(-i,-6) ! can't refer directly to structure constructor, due to name conflict

  print *, [derived(3)], [derived(20,4):: derived(3)], [derived(20,4):: (derived(3), i=1,1)]

  darr  = [derived(20,4):: derived(3)]

  if (darr(1) % val /= -3 .or. darr(1) % v2 /= -6) stop 2

  darr  = [derived(20,4):: (derived(3), i=1,1)]

  if (darr(1) % val /= -3 .or. darr(1) % v2 /= -6) stop 3

  darr  = [derived(20,4):: (derived(i), i=1,1)]

  if (darr(1) % val /= -1 .or. darr(1) % v2 /= -6) stop 4

contains

  type (derived(20,4)) function d2(i,j)
    integer :: i, j
    d2 % val = i
    d2 % v2  = j
  end function d2

end program acetdt43l
