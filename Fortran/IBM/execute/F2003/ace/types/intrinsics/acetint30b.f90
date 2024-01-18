!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint30b
!*
!*  DATE                       : 2006-08-03
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : BOZ+hollerith literals + type spec (real 4)
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
!*  BOZ literals and Hollerith constants are typeless, so they can be used to
!*  initialise any kind of intrinsic array.
!*  Hollerith constants are no longer part of the standard, but were once so
!*  important that they should still be tested.  They look like character
!*  strings, but are actually typeless, so there could be problems in declaring
!*  them, just like boz-literals.
!*  Strategy: Use boz-literals (hex), hollerith constants, and real variables in
!*  various combinations, then different types of boz-literals, then different
!*  combos with hollerith constants.  Repeat select tests with different
!*  patterns and with implied-dos.
!*  This is an adaptation of acetint30a, using real(4):: in place of real::.
!*
!*  There are companion tests to these in types/none.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint30b

  use, intrinsic :: ieee_arithmetic
  implicit none

  integer    :: i
  real(4)    :: r4Arr(3), r4Base(3), r4
  logical    :: error

  error = .false.

  r4 = real(z'61616161', 4)  !!  0.2598458941E+21_4

  r4Base = (/real(4):: r4, r4, r4/)

  r4Arr  = (/real(4):: z'61616161', 4haaaa, r4/)
  call check(1, r4Arr)
  call check(2, (/real(4):: z'61616161', 4haaaa, r4/))

  r4Arr  = (/real(4):: z'61616161', r4, 4haaaa/)
  call check(3, r4Arr)
  call check(4, (/real(4):: z'61616161', r4, 4haaaa/))

  r4Arr  = (/real(4):: r4, 4haaaa, z'61616161'/)
  call check(5, r4Arr)
  call check(6, (/real(4):: r4, 4haaaa, z'61616161'/))

  r4Arr  = (/real(4):: r4, z'61616161', 4haaaa/)
  call check(7, r4Arr)
  call check(8, (/real(4):: r4, z'61616161', 4haaaa/))

  r4Arr  = (/real(4):: 4haaaa, r4, z'61616161'/)
  call check(9, r4Arr)
  call check(10, (/real(4):: 4haaaa, r4, z'61616161'/))

  r4Arr  = (/real(4):: 4haaaa, z'61616161', r4/)
  call check(11, r4Arr)
  call check(12, (/real(4):: 4haaaa, z'61616161', r4/))

  r4Arr  = (/real(4):: o'14130260541', 4haaaa, r4/)
  call check(15, r4Arr)
  call check(16, (/real(4):: o'14130260541', 4haaaa, r4/))

  r4Arr  = (/real(4):: b'1100001011000010110000101100001', 4haaaa, r4/)
  call check(17, r4Arr)
  call check(18, (/real(4):: o'14130260541', 4haaaa, r4/))

  r4Arr  = (/real(4):: 4haaaa, 4haaaa, 4haaaa/)
  call check(19, r4Arr)
  call check(20, (/real(4):: 4haaaa, 4haaaa, 4haaaa/))

  r4Arr  = (/real(4):: b'1100001011000010110000101100001', o'14130260541', z'61616161'/)
  call check(13, r4Arr)
  call check(14, (/real(4):: b'1100001011000010110000101100001', o'14130260541', z'61616161'/))


  r4Base = (/real(4):: 0.0_4, 1.0_4, IEEE_VALUE(0.0_4,IEEE_POSITIVE_INF)/)

  ! We can't represent the above values easily (at all?) with hollerith constants,
  ! which requires bytes with values of z'00', z'3f', z'7f', and z'80'.
  ! Although 3f is '?', 00 (NUL), 7f (DEL), and 80 have no associated ASCII
  ! graphic symbol. So we'll just test the boz literals:

  r4Arr = (/real(4):: b'00000000000000000000000000000000', &
                      b'00111111100000000000000000000000', &
                      b'01111111100000000000000000000000'/)
  call check(15, r4Arr)
  call check(16, (/real(4):: b'00000000000000000000000000000000', &
                             b'00111111100000000000000000000000', &
                             b'01111111100000000000000000000000'/))

  r4Arr = (/real(4):: o'00000000000', o'07740000000', o'17740000000'/)
  call check(17, r4Arr)
  call check(18, (/real(4):: o'00000000000', o'07740000000', o'17740000000'/))

  r4Arr = (/real(4):: z'00000000', z'3f800000', z'7f800000'/)
  call check(19, r4Arr)
  call check(20, (/real(4):: z'00000000', z'3f800000', z'7f800000'/))

  ! Now NaN:
  r4Base = (/ (IEEE_VALUE(0.0_4,IEEE_QUIET_NAN),i=1,3) /)
  r4Arr = (/real(4):: b'01111111110000000000000000000000', o'17760000000', z'7fe00000'/)
  if( .not.all(ieee_is_nan(r4Arr)) ) call reportError(21, r4Arr)
  if( .not.all(ieee_is_nan((/real(4):: b'01111111110000000000000000000000', o'17760000000', z'7fe00000'/))) ) &
       call reportError(22, (/real(4):: b'01111111110000000000000000000000', o'17760000000', z'7fe00000'/))

  ! Implied-do:
  r4Base = (/real(4):: r4, r4, r4/)

  r4Arr  = (/real(4):: (z'61616161', 4haaaa, r4, i=1,1)/)
  call check(101, r4Arr)
  call check(102, (/real(4):: (z'61616161', 4haaaa, r4, i=1,1)/))

  r4Arr  = (/real(4):: (z'61616161', r4, 4haaaa, i=1,1)/)
  call check(103, r4Arr)
  call check(104, (/real(4):: (z'61616161', r4, 4haaaa, i=1,1)/))

  r4Arr  = (/real(4):: (r4, 4haaaa, z'61616161', i=1,1)/)
  call check(105, r4Arr)
  call check(106, (/real(4):: (r4, 4haaaa, z'61616161', i=1,1)/))

  r4Arr  = (/real(4):: (r4, z'61616161', 4haaaa, i=1,1)/)
  call check(107, r4Arr)
  call check(108, (/real(4):: (r4, z'61616161', 4haaaa, i=1,1)/))

  r4Arr  = (/real(4):: (4haaaa, r4, z'61616161', i=1,1)/)
  call check(109, r4Arr)
  call check(110, (/real(4):: (4haaaa, r4, z'61616161', i=1,1)/))

  r4Arr  = (/real(4):: (4haaaa, z'61616161', r4, i=1,1)/)
  call check(111, r4Arr)
  call check(112, (/real(4):: (4haaaa, z'61616161', r4, i=1,1)/))

  r4Arr  = (/real(4):: (o'14130260541', 4haaaa, r4, i=1,1)/)
  call check(115, r4Arr)
  call check(116, (/real(4):: (o'14130260541', 4haaaa, r4, i=1,1)/))

  r4Arr  = (/real(4):: (b'1100001011000010110000101100001', 4haaaa, r4, i=1,1)/)
  call check(117, r4Arr)
  call check(118, (/real(4):: (o'14130260541', 4haaaa, r4, i=1,1)/))

  r4Arr  = (/real(4):: (4haaaa, 4haaaa, 4haaaa, i=1,1)/)
  call check(119, r4Arr)
  call check(120, (/real(4):: (4haaaa, 4haaaa, 4haaaa, i=1,1)/))

  r4Arr  = (/real(4):: (b'1100001011000010110000101100001', o'14130260541', z'61616161', i=1,1)/)
  call check(113, r4Arr)
  call check(114, (/real(4):: (b'1100001011000010110000101100001', o'14130260541', z'61616161', i=1,1)/))


  r4Base = (/real(4):: (0.0_4, 1.0_4, IEEE_VALUE(0.0_4,IEEE_POSITIVE_INF), i=1,1)/)

  ! We can't represent the above values easily (at all?) with hollerith constants,
  ! which requires bytes with values of z'00', z'3f', z'7f', and z'80'.
  ! Although 3f is '?', 00 (NUL), 7f (DEL), and 80 have no associated ASCII
  ! graphic symbol. So we'll just test the boz literals:

  r4Arr = (/real(4):: (b'00000000000000000000000000000000', &
                       b'00111111100000000000000000000000', &
                       b'01111111100000000000000000000000', i=1,1)/)
  call check(115, r4Arr)
  call check(116, (/real(4):: (b'00000000000000000000000000000000', &
                               b'00111111100000000000000000000000', &
                               b'01111111100000000000000000000000', i=1,1)/))

  r4Arr = (/real(4):: (o'00000000000', o'07740000000', o'17740000000', i=1,1)/)
  call check(117, r4Arr)
  call check(118, (/real(4):: (o'00000000000', o'07740000000', o'17740000000', i=1,1)/))

  r4Arr = (/real(4):: (z'00000000', z'3f800000', z'7f800000', i=1,1)/)
  call check(119, r4Arr)
  call check(120, (/real(4):: (z'00000000', z'3f800000', z'7f800000', i=1,1)/))

  ! Now NaN:
  r4Base = (/ (IEEE_VALUE(0.0_4,IEEE_QUIET_NAN),i=1,3) /)
  r4Arr = (/real(4):: (b'01111111110000000000000000000000', o'17760000000', z'7fe00000', i=1,1)/)
  if( .not.all(ieee_is_nan(r4Arr)) ) call reportError(121, r4Arr)
  if( .not.all(ieee_is_nan((/real(4):: (b'01111111110000000000000000000000', o'17760000000', z'7fe00000', i=1,1)/))) ) &
       call reportError(122, (/real(4):: (b'01111111110000000000000000000000', o'17760000000', z'7fe00000', i=1,1)/))

  if (error) error stop 2_4

contains

  subroutine check(line, vals)
    integer :: line
    real(4) :: vals(:)
    if (any(r4Base /= vals)) then
       call reportError(line, vals)
    end if
  end subroutine check

  subroutine reportError(line, vals)
    integer :: line
    real(4) :: vals(:)
    print *, "At line", line, "expected", r4Base, "got", vals
    error = .true.
  end subroutine reportError

end program acetint30b
