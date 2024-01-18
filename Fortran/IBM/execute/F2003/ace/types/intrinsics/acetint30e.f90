!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint30a
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-03
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : BOZ+hollerith literals + type spec (integer)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : intrinsic type, typeless, character, hollerith
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  BOZ literals and Hollerith constants are typeless, so they can be used to
!*  initialise any kind of intrinsic array.
!*  Hollerith constants are no longer part of the standard, but were once so
!*  important that they should still be tested.  They look like character
!*  strings, but are actually typeless, so there could be problems in declaring
!*  them, just like boz-literals.
!*  Strategy: Use boz-literals (hex), hollerith constants, and integers in
!*  various combinations, then different types of boz-literals, then different
!*  combos with hollerith constants.  Repeat select tests with different
!*  patterns and with implied-dos.
!*  This is an adaptation of acetint30a.
!*
!*  There are companion tests to these in types/none.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint30a

  implicit none

  integer    :: i
  integer(1) :: i1Arr(5)
  integer(2) :: i2Arr(5)
  integer(4) :: i4Arr(5)
  integer(8) :: i8Arr(5)
  logical    :: error
  error = .false.

  i1Arr = (/integer(1):: 0_1, b'00000000', o'000', z'00',   0_1/)
  call check1(1, i1Arr)
  call check1(2, (/integer(1):: 0_1, b'00000000', o'000', z'00',   0_1/))

  i2Arr = (/integer(2):: 0_2, b'0000000000000000', o'000000', z'0000',   0_2/)
  call check2(3, i2Arr)
  call check2(4, (/integer(2)::   0_2, b'0000000000000000', o'000000', z'0000',   0_2/))

  i4Arr = (/integer(4)::   0_4, b'00000000000000000000000000000000', o'00000000000', z'00000000',   0_4/)
  call check4(5, i4Arr)
  call check4(6, (/integer(4)::   0_4, b'00000000000000000000000000000000', o'00000000000', z'00000000',   0_4/))

  i8Arr = (/integer(8)::   0_8, b'0000000000000000000000000000000000000000000000000000000000000000', o'0000000000000000000000', z'0000000000000000',   0_8/)
  call check8(7, i8Arr)
  call check8(8, (/integer(8)::   0_8, b'0000000000000000000000000000000000000000000000000000000000000000', o'0000000000000000000000', z'0000000000000000',   0_8/))


  i1Arr = (/integer(1)::  32_1, b'00100000', o'040', z'20', 1h /)
  call check1(9, i1Arr)
  call check1(10, (/integer(1)::  32_1, b'00100000', o'040', z'20', 1h /))

  i2Arr = (/integer(2):: 8224_2, b'0010000000100000', o'020040', z'2020', 2h  /)
  call check2(11, i2Arr)
  call check2(12, (/integer(2):: 8224_2, b'0010000000100000', o'020040', z'2020', 2h  /))
 
  i4Arr = (/integer(4):: 538976288_4, b'00100000001000000010000000100000', o'4010020040', z'20202020', 4h    /)
  call check4(13, i4Arr)
  call check4(14, (/integer(4):: 538976288_4, b'00100000001000000010000000100000', o'4010020040', z'20202020', 4h    /))

  i8Arr = (/integer(8):: 2314885530818453536_8, b'0010000000100000001000000010000000100000001000000010000000100000', o'200401002004010020040', z'2020202020202020', 8h        /)
  call check8(15, i8Arr)
  call check8(16, (/integer(8):: 2314885530818453536_8, b'0010000000100000001000000010000000100000001000000010000000100000', o'200401002004010020040', z'2020202020202020', 8h        /))


  i1Arr = (/integer(1):: 126_1, b'01111110', o'176', z'7e', 1h~/)
  call check1(17, i1Arr)
  call check1(18, (/integer(1):: 126_1, b'01111110', o'176', z'7e', 1h~/))

  i2Arr = (/integer(2):: 32382_2, b'0111111001111110', o'77176', z'7e7e', 2h~~/)
  call check2(19, i2Arr)
  call check2(20, (/integer(2):: 32382_2, b'0111111001111110', o'77176', z'7e7e', 2h~~/))

  i4Arr = (/integer(4):: 2122219134_4, b'01111110011111100111111001111110', o'17637477176', z'7e7e7e7e', 4h~~~~/)
  call check4(21, i4Arr)
  call check4(22, (/integer(4):: 2122219134_4, b'01111110011111100111111001111110', o'17637477176', z'7e7e7e7e', 4h~~~~/))

  i8Arr = (/integer(8):: 9114861777597660798_8, b'0111111001111110011111100111111001111110011111100111111001111110', o'771763747717637477176', z'7E7E7E7E7E7E7E7E', 8h~~~~~~~~/)
  call check8(23, i8Arr)
  call check8(24, (/integer(8):: 9114861777597660798_8, b'0111111001111110011111100111111001111110011111100111111001111110', o'771763747717637477176', z'7E7E7E7E7E7E7E7E', 8h~~~~~~~~/))

  i1Arr = (/integer(1):: 127_1, b'01111111', o'177', z'7f', 127_1/)
  call check1(25, i1Arr)
  call check1(26, (/integer(1):: 127_1, b'01111111', o'177', z'7f', 127_1/))

  i2Arr = (/integer(2):: 32639_2, b'0111111101111111', o'77577', z'7f7f', 32639_2/)
  call check2(27, i2Arr)
  call check2(28, (/integer(2):: 32639_2, b'0111111101111111', o'77577', z'7f7f', 32639_2/))

  i4Arr = (/integer(4):: 2139062143_4, b'01111111011111110111111101111111', o'17737677577', z'7f7f7f7f', 2139062143_4/)
  call check4(29, i4Arr)
  call check4(30, (/integer(4):: 2139062143_4, b'01111111011111110111111101111111', o'17737677577', z'7f7f7f7f', 2139062143_4/))

  i8Arr = (/integer(8):: 9187201950435737471_8, b'0111111101111111011111110111111101111111011111110111111101111111', o'775773767757737677577', z'7f7f7f7f7f7f7f7f', 9187201950435737471_8/)
  call check8(31, i8Arr)
  call check8(32, (/integer(8):: 9187201950435737471_8, b'0111111101111111011111110111111101111111011111110111111101111111', o'775773767757737677577', z'7f7f7f7f7f7f7f7f', 9187201950435737471_8/))

  ! Of two's complement, one's complement, and sign-and-magnitude, all represent
  ! the positive integers the same way.  Negative numbers are a different story,
  ! so we have to determine what this machine uses before we can test the
  ! patterns.  We'll do this once, and never speak of it again!
  ! (Who still uses one's complement or sign-and-magnitude for integers, anyway?)
  !
  ! Model            -1  -0   0   1   min      max   huge()+1
  ! 2's complement:  ff n/a  00  01  80/-128  7f/127 min
  ! 1's complement:  fe  ff  00  01  80/-127  7f/127 min
  ! sign-and-magn.:  81  80  00  01  ff/-127  7f/127 -0

  if (not(0) == -1) then !! Must be two's complement

     i1Arr = (/integer(1):: -1_1, b'11111111', o'377', z'ff', -1_1/)
     call check1(33, i1Arr)
     call check1(34, (/integer(1):: -1_1, b'11111111', o'377', z'ff', -1_1/))

     i1Arr = (/integer(1):: -0_1, b'00000000', o'000', z'00', -0_1/)
     call check1(35, i1Arr)
     call check1(36, (/integer(1):: -0_1, b'00000000', o'000', z'00', -0_1/))

     i1Arr = (/integer(1):: -128_1, b'10000000', o'200', z'80', -128_1/)
     call check1(37, i1Arr)
     call check1(38, (/integer(1):: -128_1, b'10000000', o'200', z'80', -128_1/))


  else if (not(0) == 0) then !! Must be one's complement

     i1Arr = (/integer(1):: -1_1, b'11111110', o'376', z'fe', -1_1/)
     call check1(39, i1Arr)
     call check1(40, (/integer(1):: -1_1, b'11111110', o'376', z'fe', -1_1/))

     i1Arr = (/integer(1):: -0_1, b'11111111', o'377', z'ff', -0_1/)
     call check1(41, i1Arr)
     call check1(42, (/integer(1):: -0_1, b'11111111', o'377', z'ff', -0_1/))

     i1Arr = (/integer(1):: -127_1, b'10000000', o'200', z'80', -127_1/)
     call check1(43, i1Arr)
     call check1(44, (/integer(1):: -127_1, b'10000000', o'200', z'80', -127_1/))

  else if (int(z'80',1) == 0) then !! Must be sign-and-magnitude

     i1Arr = (/integer(1):: -1_1, b'10000001', o'201', z'81', -1_1/)
     call check1(45, i1Arr)
     call check1(46, (/integer(1):: -1_1, b'10000001', o'201', z'81', -1_1/))

     i1Arr = (/integer(1):: -0_1, b'10000000', o'200', z'80', -0_1/)
     call check1(47, i1Arr)
     call check1(48, (/integer(1):: -0_1, b'10000000', o'200', z'80', -0_1/))

     i1Arr = (/integer(1):: -127_1, b'11111111', o'377', z'ff', -127_1/)
     call check1(49, i1Arr)
     call check1(50, (/integer(1):: -127_1, b'11111111', o'377', z'ff', -127_1/))

  else ! if (signMag) then

     print *, "Error in determining number system"
     stop 2

  end if


  i4Arr = (/integer(4):: (0_4, b'00000000000000000000000000000000', o'00000000000', z'00000000', 0_4, i=1,1)/)
  call check4(51, i4Arr)
  call check4(52, (/integer(4):: (0_4, b'00000000000000000000000000000000', o'00000000000', z'00000000', 0_4, i=1,1)/))

  i4Arr = (/integer(4):: (538976288_4, b'00100000001000000010000000100000', o'4010020040', z'20202020', 4h    , i=1,1)/)
  call check4(53, i4Arr)
  call check4(54, (/integer(4):: (538976288_4, b'00100000001000000010000000100000', o'4010020040', z'20202020', 4h    , i=1,1)/))

  i4Arr = (/integer(4):: (2122219134_4, b'01111110011111100111111001111110', o'17637477176', z'7e7e7e7e', 4h~~~~, i=1,1)/)
  call check4(55, i4Arr)
  call check4(56, (/integer(4):: (2122219134_4, b'01111110011111100111111001111110', o'17637477176', z'7e7e7e7e', 4h~~~~, i=1,1)/))

  i4Arr = (/integer(4):: (2139062143_4, b'01111111011111110111111101111111', o'17737677577', z'7f7f7f7f', 2139062143_4, i=1,1)/)
  call check4(57, i4Arr)
  call check4(58, (/integer(4):: (2139062143_4, b'01111111011111110111111101111111', o'17737677577', z'7f7f7f7f', 2139062143_4, i=1,1)/))

  if (error) stop 3

contains

  subroutine check1(line, vals)
    integer :: line
    integer(1):: vals(:)
    if (any(vals(1) /= vals(2:))) then
       print *, "At line", line, "expected", vals(1), "got", vals(2:)
       error = .true.
    end if
  end subroutine check1

  subroutine check2(line, vals)
    integer :: line
    integer(2):: vals(:)
    if (any(vals(1) /= vals(2:))) then
       print *, "At line", line, "expected", vals(1), "got", vals(2:)
       error = .true.
    end if
  end subroutine check2

  subroutine check4(line, vals)
    integer :: line
    integer(4):: vals(:)
    if (any(vals(1) /= vals(2:))) then
       print *, "At line", line, "expected", vals(1), "got", vals(2:)
       error = .true.
    end if
  end subroutine check4

  subroutine check8(line, vals)
    integer :: line
    integer(8):: vals(:)
    if (any(vals(1) /= vals(2:))) then
       print *, "At line", line, "expected", vals(1), "got", vals(2:)
       error = .true.
    end if
  end subroutine check8

end program acetint30a
