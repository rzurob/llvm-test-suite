!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BUse02
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-11-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : USE: definitions available to block
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  A block can use a host-associated module as well as a locally USEd module.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module m2
  character :: name2
contains
  subroutine ms
    print *, "ms: ", name2
  end subroutine ms
end module m2

module m1
  integer :: name1
contains
  subroutine msub
    print *, "msub:", name1
  end subroutine msub
end module m1


program BUse02
  use :: m2
  implicit none
  block
    use :: m1
    name1 = 9
    name2 = 'x'
    call msub ! msub: 9
    call ms   ! ms: x
    call checktype(name1) ! i: 9
    call checktype(name2) ! c: x
  end block
contains
  subroutine checktype(a)
    class(*) :: a
    select type (a)
      type is (integer)
           print *, 'i:', a
      type is (character(*))
           print *, 'c: ', a
      class default
           print *, 'unknown'
    end select
  end subroutine checktype
end program BUse02
