!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BVarRedefined
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-12-14
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : (re)definition of variable name
!*
!*  DESCRIPTION
!*
!*  Simple case of variable name redefined in a block.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BVarRedefined

  implicit none
  integer :: myvar

  myvar = 99
  print *, myvar
  call check(myvar)

  block
    character(20) :: myvar
    myvar = 'brillig & the slithy'
    print *, myvar
    call check(myvar)
  end block

  myvar = 102
  print *, myvar
  call check(myvar)

contains

  subroutine check(arg)
    class (*), intent(in) :: arg
    select type(arg)
       type is (integer);      print *, 'i', kind(arg), arg
       type is (character(*)); print *, 'c', len(arg), '>', arg, '<'
       class default;          print *, 'invalid'
    end select
  end subroutine check

end program BVarRedefined
