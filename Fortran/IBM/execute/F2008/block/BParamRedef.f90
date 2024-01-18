!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-12-14
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : parameters redefined in block
!*
!*  DESCRIPTION
!*
!*  Declare parameters for use in the main program, and similar ones in the
!*  block, reusing names, but sometimes changing values, type, or making them non-parameters.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BParamRedef

  implicit none
  integer, parameter :: NAME1 = 22
  character(8), parameter :: NAME2 = 'thisTest'
  integer(1) :: name3
  character(6) :: name4

  name3 = NAME1 + 11
  name4 = NAME2(2:7)

  call check(NAME1)
  call check(NAME2)
  call check(name3)
  call check(name4)

  block
    integer, parameter :: NAME4 = 44
    character(10), parameter :: NAME3 = 'finagleThis'
    integer(2) :: name1
    character(14) :: name2
    name1 = NAME4 / 2
    name2 = 'ab' // NAME3 // 'cd'
    call check(name1)
    call check(name2)
    call check(NAME3)
    call check(NAME4)
  end block

  call check(NAME1)
  call check(NAME2)
  call check(name3)
  call check(name4)

contains

  subroutine check(arg)
    class (*), intent(in) :: arg
    select type(arg)
       type is (integer(1));   print *, 'i1', kind(arg), arg
       type is (integer(2));   print *, 'i2', kind(arg), arg
       type is (integer);      print *, 'i4', kind(arg), arg
       type is (character(*)); print *, 'c', len(arg), '>', arg, '<'
       class default;          print *, 'invalid'
    end select
  end subroutine check

end program BParamRedef
