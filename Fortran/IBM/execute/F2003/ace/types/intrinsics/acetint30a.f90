!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint30a
!*
!*  DATE                       : 2006-08-03
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : BOZ+hollerith literals + type spec (real)
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
!*  acetint30b is a copy of this, using a real(4):: types specifier;
!*  acetint30c is a modification using 8-byte values.
!*
!*  There are companion tests to these in types/none.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint30a

  use, intrinsic :: ieee_arithmetic
  implicit none

  integer    :: i
  real       :: rArr(3), rBase(3), r
  logical    :: error

  error = .false.

  r = real(z'61616161')  !!  0.2598458941E+21_4

  rBase = (/real:: r, r, r/)

  rArr  = (/real:: z'61616161', 4haaaa, r/)
  call check(1, rArr)
  call check(2, (/real:: z'61616161', 4haaaa, r/))

  rArr  = (/real:: z'61616161', r, 4haaaa/)
  call check(3, rArr)
  call check(4, (/real:: z'61616161', r, 4haaaa/))

  rArr  = (/real:: r, 4haaaa, z'61616161'/)
  call check(5, rArr)
  call check(6, (/real:: r, 4haaaa, z'61616161'/))

  rArr  = (/real:: r, z'61616161', 4haaaa/)
  call check(7, rArr)
  call check(8, (/real:: r, z'61616161', 4haaaa/))

  rArr  = (/real:: 4haaaa, r, z'61616161'/)
  call check(9, rArr)
  call check(10, (/real:: 4haaaa, r, z'61616161'/))

  rArr  = (/real:: 4haaaa, z'61616161', r/)
  call check(11, rArr)
  call check(12, (/real:: 4haaaa, z'61616161', r/))

  rArr  = (/real:: o'14130260541', 4haaaa, r/)
  call check(15, rArr)
  call check(16, (/real:: o'14130260541', 4haaaa, r/))

  rArr  = (/real:: b'1100001011000010110000101100001', 4haaaa, r/)
  call check(17, rArr)
  call check(18, (/real:: o'14130260541', 4haaaa, r/))

  rArr  = (/real:: 4haaaa, 4haaaa, 4haaaa/)
  call check(19, rArr)
  call check(20, (/real:: 4haaaa, 4haaaa, 4haaaa/))

  rArr  = (/real:: b'1100001011000010110000101100001', o'14130260541', z'61616161'/)
  call check(13, rArr)
  call check(14, (/real:: b'1100001011000010110000101100001', o'14130260541', z'61616161'/))


  rBase = (/real:: 0.0_4, 1.0_4, IEEE_VALUE(0.0_4,IEEE_POSITIVE_INF)/)

  ! We can't represent the above values easily (at all?) with hollerith constants,
  ! which requires bytes with values of z'00', z'3f', z'7f', and z'80'.
  ! Although 3f is '?', 00 (NUL), 7f (DEL), and 80 have no associated ASCII
  ! graphic symbol. So we'll just test the boz literals:

  rArr = (/real:: b'00000000000000000000000000000000', &
                   b'00111111100000000000000000000000', &
                   b'01111111100000000000000000000000'/)
  call check(15, rArr)
  call check(16, (/real:: b'00000000000000000000000000000000', &
                          b'00111111100000000000000000000000', &
                          b'01111111100000000000000000000000'/))

  rArr = (/real:: o'00000000000', o'07740000000', o'17740000000'/)
  call check(17, rArr)
  call check(18, (/real:: o'00000000000', o'07740000000', o'17740000000'/))

  rArr = (/real:: z'00000000', z'3f800000', z'7f800000'/)
  call check(19, rArr)
  call check(20, (/real:: z'00000000', z'3f800000', z'7f800000'/))

  ! Now NaN:
  rBase = (/ (IEEE_VALUE(0.0_4,IEEE_QUIET_NAN),i=1,3) /)
  rArr = (/real:: b'01111111110000000000000000000000', o'17760000000', z'7fe00000'/)
  if( .not.all(ieee_is_nan(rArr)) ) call reportError(21, rArr)
  if( .not.all(ieee_is_nan((/real:: b'01111111110000000000000000000000', o'17760000000', z'7fe00000'/))) ) &
       call reportError(22, (/real:: b'01111111110000000000000000000000', o'17760000000', z'7fe00000'/))


  ! Implied-do:
  rBase = (/real:: r, r, r/)

  rArr  = (/real:: (z'61616161', 4haaaa, r, i=1,1)/)
  call check(101, rArr)
  call check(102, (/real:: (z'61616161', 4haaaa, r, i=1,1)/))

  rArr  = (/real:: (z'61616161', r, 4haaaa, i=1,1)/)
  call check(103, rArr)
  call check(104, (/real:: (z'61616161', r, 4haaaa, i=1,1)/))

  rArr  = (/real:: (r, 4haaaa, z'61616161', i=1,1)/)
  call check(105, rArr)
  call check(106, (/real:: (r, 4haaaa, z'61616161', i=1,1)/))

  rArr  = (/real:: (r, z'61616161', 4haaaa, i=1,1)/)
  call check(107, rArr)
  call check(108, (/real:: (r, z'61616161', 4haaaa, i=1,1)/))

  rArr  = (/real:: (4haaaa, r, z'61616161', i=1,1)/)
  call check(109, rArr)
  call check(110, (/real:: (4haaaa, r, z'61616161', i=1,1)/))

  rArr  = (/real:: (4haaaa, z'61616161', r, i=1,1)/)
  call check(111, rArr)
  call check(112, (/real:: (4haaaa, z'61616161', r, i=1,1)/))

  rArr  = (/real:: (o'14130260541', 4haaaa, r, i=1,1)/)
  call check(115, rArr)
  call check(116, (/real:: (o'14130260541', 4haaaa, r, i=1,1)/))

  rArr  = (/real:: (b'1100001011000010110000101100001', 4haaaa, r, i=1,1)/)
  call check(117, rArr)
  call check(118, (/real:: (o'14130260541', 4haaaa, r, i=1,1)/))

  rArr  = (/real:: (4haaaa, 4haaaa, 4haaaa, i=1,1)/)
  call check(119, rArr)
  call check(120, (/real:: (4haaaa, 4haaaa, 4haaaa, i=1,1)/))

  rArr  = (/real:: (b'1100001011000010110000101100001', o'14130260541', z'61616161', i=1,1)/)
  call check(113, rArr)
  call check(114, (/real:: (b'1100001011000010110000101100001', o'14130260541', z'61616161', i=1,1)/))


  rBase = (/real:: (0.0_4, 1.0_4, IEEE_VALUE(0.0_4,IEEE_POSITIVE_INF), i=1,1)/)

  ! We can't represent the above values easily (at all?) with hollerith constants,
  ! which requires bytes with values of z'00', z'3f', z'7f', and z'80'.
  ! Although 3f is '?', 00 (NUL), 7f (DEL), and 80 have no associated ASCII
  ! graphic symbol. So we'll just test the boz literals:

  rArr = (/real:: (b'00000000000000000000000000000000', &
                    b'00111111100000000000000000000000', &
                    b'01111111100000000000000000000000', i=1,1)/)
  call check(115, rArr)
  call check(116, (/real:: (b'00000000000000000000000000000000', &
                            b'00111111100000000000000000000000', &
                            b'01111111100000000000000000000000', i=1,1)/))

  rArr = (/real:: (o'00000000000', o'07740000000', o'17740000000', i=1,1)/)
  call check(117, rArr)
  call check(118, (/real:: (o'00000000000', o'07740000000', o'17740000000', i=1,1)/))

  rArr = (/real:: (z'00000000', z'3f800000', z'7f800000', i=1,1)/)
  call check(119, rArr)
  call check(120, (/real:: (z'00000000', z'3f800000', z'7f800000', i=1,1)/))

  ! Now NaN:
  rBase = (/ (IEEE_VALUE(0.0_4,IEEE_QUIET_NAN),i=1,3) /)
  rArr = (/real:: (b'01111111110000000000000000000000', o'17760000000', z'7fe00000', i=1,1)/)
  if( .not.all(ieee_is_nan(rArr)) ) call reportError(121, rArr)
  if( .not.all(ieee_is_nan((/real:: (b'01111111110000000000000000000000', o'17760000000', z'7fe00000', i=1,1)/))) ) &
       call reportError(122, (/real:: (b'01111111110000000000000000000000', o'17760000000', z'7fe00000', i=1,1)/))

  if (error) error stop 2_4

contains

  subroutine check(line, vals)
    integer :: line
    real :: vals(:)
    if (any(rBase /= vals)) then
       call reportError(line, vals)
    end if
  end subroutine check

  subroutine reportError(line, vals)
    integer :: line
    real :: vals(:)
    print *, "At line", line, "expected", rBase, "got", vals
    error = .true.
  end subroutine reportError

end program acetint30a
