!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-08-03
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : BOZ+hollerith literals + type spec (character)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type, typeless, character, hollerith
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Hollerith constants are no longer part of the standard, but were once so
!*  important that they should still be tested.  They look like character
!*  strings, but are actually typeless, so there could be problems in declaring
!*  them, just like boz-literals.  There are companion tests to these in
!*  types/none.  Note that the standard is silent on how to interpret the size
!*  of octal constants, i.e., is o'000' one byte or two?  Interpreted in the
!*  character context, XLF views o'43' as two bytes, with the leading byte null.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint30

  implicit none

  character(1) :: ch1Arr(3), ch1Base(3)
  character(4) :: ch4Arr(3), ch4Base(3)
  logical :: error

  error = .false.
  ch1Base = (/character:: '#', '#', '#'/)
  ch1Arr  = (/character:: '#', 1h#, z'23'/)
  call check(10_4, ch1Arr, ch1Base)
  ch1Arr  = (/character:: '#', 1h#, b'100011'/)
  call check(11_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: '#', 1h#, o'43'/)
  call check(12_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: '#', z'23', 1h#/)
  call check(13_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: 1h#, '#', z'23'/)
  call check(14_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: 1h#, z'23', '#'/)
  call check(15_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: z'23', '#', 1h#/)
  call check(16_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: z'23', 1h#, '#'/)
  call check(17_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: b'100011', 1h#, '#'/)
  call check(18_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: o'43', 1h#, '#'/)
  call check(19_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: 1h#, 1h#, 1h#/)
  call check(20_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: 1h#, 1h#, b'100011'/)
  call check(21_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: 1h#, 1h#, o'43'/)
  call check(22_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: 1h#, 1h#, z'23'/)
  call check(23_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: b'100011', b'100011', b'100011'/)
  call check(24_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: o'43', o'43', o'43'/)
  call check(25_4,ch1Arr,ch1Base)
  ch1Arr  = (/character:: z'23', z'23', z'23'/)
  call check(26_4,ch1Arr,ch1Base)

  ch1Base = (/character:: ',', ',', '/'/)
  ch1Arr  = (/character:: 1h,, 1h,, 1h//)
  call check(27_4,ch1Arr,ch1Base)

  ch1Base = (/character(1):: '#', '#', '#'/)
  ch1Arr  = (/character(1):: '#', 1h#, z'23'/)
  call check(30_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: '#', 1h#, b'100011'/)
  call check(31_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: '#', 1h#, o'43'/)
  call check(32_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: '#', z'23', 1h#/)
  call check(33_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: 1h#, '#', z'23'/)
  call check(34_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: 1h#, z'23', '#'/)
  call check(35_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: z'23', '#', 1h#/)
  call check(36_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: z'23', 1h#, '#'/)
  call check(37_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: b'100011', 1h#, '#'/)
  call check(38_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: o'43', 1h#, '#'/)
  call check(39_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: 1h#, 1h#, 1h#/)
  call check(40_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: 1h#, 1h#, b'100011'/)
  call check(41_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: 1h#, 1h#, o'43'/)
  call check(42_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: 1h#, 1h#, z'23'/)
  call check(43_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: b'100011', b'100011', b'100011'/)
  call check(44_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: o'43', o'43', o'43'/)
  call check(45_4,ch1Arr,ch1Base)
  ch1Arr  = (/character(1):: z'23', z'23', z'23'/)
  call check(46_4,ch1Arr,ch1Base)

  ch1Base = (/character(1):: ',', ',', '/'/)
  ch1Arr  = (/character(1):: 1h,, 1h,, 1h//)
  call check(47_4,ch1Arr,ch1Base)

  ch4Base = (/character(4):: '####', '####', '####'/)
  ch4Arr  = (/character(4):: '####', 4h####, z'23232323'/)
  call check(50_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: '####', 4h####, b'100011001000110010001100100011'/)
  call check(51_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: '####', 4h####, o'4310621443'/)
  call check(52_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: '####', z'23232323', 4h####/)
  call check(53_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: 4h####, '####', z'23232323'/)
  call check(54_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: 4h####, z'23232323', '####'/)
  call check(55_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: z'23232323', '####', 4h####/)
  call check(56_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: z'23232323', 4h####, '####'/)
  call check(57_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: b'100011001000110010001100100011', 4h####, '####'/)
  call check(58_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: o'4310621443', 4h####, '####'/)
  call check(59_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: 4h####, 4h####, 4h####/)
  call check(60_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: 4h####, 4h####, b'100011001000110010001100100011'/)
  call check(61_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: 4h####, 4h####, o'4310621443'/)
  call check(62_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: 4h####, 4h####, z'23232323'/)
  call check(63_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: b'100011001000110010001100100011', b'100011001000110010001100100011', b'100011001000110010001100100011'/)
  call check(64_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: o'4310621443', o'4310621443', o'4310621443'/)
  call check(65_4,ch4Arr,ch4Base)
  ch4Arr  = (/character(4):: z'23232323', z'23232323', z'23232323'/)
  call check(66_4,ch4Arr,ch4Base)

  ch4Base = (/character(4):: ',,,,', ',,,,', '////'/)
  ch4Arr  = (/character(4):: 4h,,,,, 4h,,,,, 4h/////)
  call check(67_4,ch4Arr,ch4Base)

  ch4Base = (/character(4):: '(/1h', '    ', '2h/)'/)
  ch4Arr  = (/character(4):: 4h(/1h, 4h    , 4h2h/)/)
  call check(68_4,ch4Arr,ch4Base)

  if (error) error stop 2_4

contains

  subroutine check(code, arr, expect)
    integer (4) :: code
    character(*) :: expect(:), arr(:)
    if (any(expect /= arr)) then
       print *, "At ", code, ", expected ", expect, "; got ", arr
       error = .true.
    end if
  end subroutine check

end program acetint30
