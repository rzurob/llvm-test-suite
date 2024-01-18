!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint30c
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-03
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : BOZ+hollerith literals + type spec (real 8)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : intrinsic type, typeless, hollerith, real(8)
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
!*  This is an adaptation of acetint30a, using real(8):: in place of real::.
!*
!*  There are companion tests to these in types/none.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint30c

  use, intrinsic :: ieee_arithmetic
  implicit none

  integer    :: i
  real(8)    :: r8Arr(3), r8Base(3), r8
  logical    :: error

  error = .false.

  ! Repeat with doubles:
  r8 = real(z'6161616161616161', 8)  !! 0.122176384420437750e+162_8

  r8Base = (/real(8):: r8, r8, r8/)

  r8Arr  = (/real(8):: z'6161616161616161', 8haaaaaaaa, r8/)
  call check(1, r8Arr)
  call check(2, (/real(8):: z'6161616161616161', 8haaaaaaaa, r8/))

  r8Arr  = (/real(8):: z'6161616161616161', r8, 8haaaaaaaa/)
  call check(3, r8Arr)
  call check(4, (/real(8):: z'6161616161616161', r8, 8haaaaaaaa/))

  r8Arr  = (/real(8):: r8, 8haaaaaaaa, z'6161616161616161'/)
  call check(5, r8Arr)
  call check(6, (/real(8):: r8, 8haaaaaaaa, z'6161616161616161'/))

  r8Arr  = (/real(8):: r8, z'6161616161616161', 8haaaaaaaa/)
  call check(7, r8Arr)
  call check(8, (/real(8):: r8, z'6161616161616161', 8haaaaaaaa/))

  r8Arr  = (/real(8):: 8haaaaaaaa, r8, z'6161616161616161'/)
  call check(9, r8Arr)
  call check(10, (/real(8):: 8haaaaaaaa, r8, z'6161616161616161'/))

  r8Arr  = (/real(8):: 8haaaaaaaa, z'6161616161616161', r8/)
  call check(11, r8Arr)
  call check(12, (/real(8):: 8haaaaaaaa, z'6161616161616161', r8/))

  r8Arr  = (/real(8):: o'605413026054130260541', 8haaaaaaaa, r8/)
  call check(13, r8Arr)
  call check(14, (/real(8):: o'605413026054130260541', 8haaaaaaaa, r8/))

  r8Arr  = (/real(8):: b'110000101100001011000010110000101100001011000010110000101100001', &
                       8haaaaaaaa, r8/)
  call check(15, r8Arr)
  call check(16, (/real(8):: o'605413026054130260541', 8haaaaaaaa, r8/))

  r8Arr  = (/real(8):: 8haaaaaaaa, 8haaaaaaaa, 8haaaaaaaa/)
  call check(17, r8Arr)
  call check(18, (/real(8):: 8haaaaaaaa, 8haaaaaaaa, 8haaaaaaaa/))

  r8Arr  = (/real(8):: b'110000101100001011000010110000101100001011000010110000101100001', &
                       o'605413026054130260541', z'6161616161616161'/)
  call check(19, r8Arr)
  call check(20, (/real(8):: b'110000101100001011000010110000101100001011000010110000101100001', &
                             o'605413026054130260541', z'6161616161616161'/))

   
  r8Base = (/real(8):: 0.0_4, 1.0_4, IEEE_VALUE(0.0_4,IEEE_POSITIVE_INF)/)

  ! We can't represent the above values easily (at all?) with hollerith constants,
  ! which requires bytes with values of z'00', z'3f', z'7f', and z'f0'.
  ! Although 3f is '?', 00 (NUL), 7f (DEL), and f0 have no associated ASCII
  ! graphic symbol. So we'll just test the boz literals:

  r8Arr = (/real(8):: b'0000000000000000000000000000000000000000000000000000000000000000',&
                      b'0011111111110000000000000000000000000000000000000000000000000000',&
                      b'0111111111110000000000000000000000000000000000000000000000000000'/)
  call check(21, r8Arr)
  call check(22,  (/real(8):: b'0000000000000000000000000000000000000000000000000000000000000000',&
                              b'0011111111110000000000000000000000000000000000000000000000000000',&
                              b'0111111111110000000000000000000000000000000000000000000000000000'/))

  r8Arr = (/real(8):: o'0000000000000000000000', o'0377600000000000000000', o'0777600000000000000000'/)
  call check(23, r8Arr)
  call check(24, (/real(8):: o'0000000000000000000000', o'0377600000000000000000', o'0777600000000000000000'/))

  r8Arr = (/real(8):: z'0000000000000000', z'3ff0000000000000', z'7ff0000000000000'/)
  call check(25, r8Arr)
  call check(26, (/real(8):: z'0000000000000000', z'3ff0000000000000', z'7ff0000000000000'/))

  ! Now NaN:
  r8Base = (/ (IEEE_VALUE(0.0_8,IEEE_QUIET_NAN),i=1,3) /)
  r8Arr = (/real(8):: b'0111111111111000000000000000000000000000000000000000000000000000', &
                      o'0777700000000000000000', z'7ff8000000000000'/)
  if( .not.all(ieee_is_nan(r8Arr)) ) call reportError(27, r8Arr)
  if( .not.all(ieee_is_nan((/real(8):: b'0111111111111000000000000000000000000000000000000000000000000000', &
                                       o'0777700000000000000000', z'7ff8000000000000'/))) ) &
       call reportError(28, (/real(8):: b'0111111111111000000000000000000000000000000000000000000000000000', &
                                        o'0777700000000000000000', z'7ff8000000000000'/))


  r8Base = (/real(8):: (r8, r8, r8,i=1,1)/)

  r8Arr  = (/real(8):: (z'6161616161616161', 8haaaaaaaa, r8,i=1,1)/)
  call check(101, r8Arr)
  call check(102, (/real(8):: (z'6161616161616161', 8haaaaaaaa, r8,i=1,1)/))

  r8Arr  = (/real(8):: (z'6161616161616161', r8, 8haaaaaaaa,i=1,1)/)
  call check(103, r8Arr)
  call check(104, (/real(8):: (z'6161616161616161', r8, 8haaaaaaaa,i=1,1)/))

  r8Arr  = (/real(8):: (r8, 8haaaaaaaa, z'6161616161616161',i=1,1)/)
  call check(105, r8Arr)
  call check(106, (/real(8):: (r8, 8haaaaaaaa, z'6161616161616161',i=1,1)/))

  r8Arr  = (/real(8):: (r8, z'6161616161616161', 8haaaaaaaa,i=1,1)/)
  call check(107, r8Arr)
  call check(108, (/real(8):: (r8, z'6161616161616161', 8haaaaaaaa,i=1,1)/))

  r8Arr  = (/real(8):: (8haaaaaaaa, r8, z'6161616161616161',i=1,1)/)
  call check(109, r8Arr)
  call check(110, (/real(8):: (8haaaaaaaa, r8, z'6161616161616161',i=1,1)/))

  r8Arr  = (/real(8):: (8haaaaaaaa, z'6161616161616161', r8,i=1,1)/)
  call check(111, r8Arr)
  call check(112, (/real(8):: (8haaaaaaaa, z'6161616161616161', r8,i=1,1)/))

  r8Arr  = (/real(8):: (o'605413026054130260541', 8haaaaaaaa, r8,i=1,1)/)
  call check(113, r8Arr)
  call check(114, (/real(8):: (o'605413026054130260541', 8haaaaaaaa, r8,i=1,1)/))

  r8Arr  = (/real(8):: (b'110000101100001011000010110000101100001011000010110000101100001', &
                        8haaaaaaaa, r8,i=1,1)/)
  call check(115, r8Arr)
  call check(116, (/real(8):: (o'605413026054130260541', 8haaaaaaaa, r8,i=1,1)/))

  r8Arr  = (/real(8):: (8haaaaaaaa, 8haaaaaaaa, 8haaaaaaaa,i=1,1)/)
  call check(117, r8Arr)
  call check(118, (/real(8):: (8haaaaaaaa, 8haaaaaaaa, 8haaaaaaaa,i=1,1)/))

  r8Arr  = (/real(8):: (b'110000101100001011000010110000101100001011000010110000101100001', &
                        o'605413026054130260541', z'6161616161616161',i=1,1)/)
  call check(119, r8Arr)
  call check(120, (/real(8):: (b'110000101100001011000010110000101100001011000010110000101100001', &
                               o'605413026054130260541', z'6161616161616161',i=1,1)/))

   
  r8Base = (/real(8):: (0.0_4, 1.0_4, IEEE_VALUE(0.0_4,IEEE_POSITIVE_INF),i=1,1)/)

  ! We can't represent the above values easily (at all?) with hollerith constants,
  ! which requires bytes with values of z'00', z'3f', z'7f', and z'f0'.
  ! Although 3f is '?', 00 (NUL), 7f (DEL), and f0 have no associated ASCII
  ! graphic symbol. So we'll just test the boz literals:

  r8Arr = (/real(8):: (b'0000000000000000000000000000000000000000000000000000000000000000',&
                       b'0011111111110000000000000000000000000000000000000000000000000000',&
                       b'0111111111110000000000000000000000000000000000000000000000000000',i=1,1)/)
  call check(121, r8Arr)
  call check(122, (/real(8):: (b'0000000000000000000000000000000000000000000000000000000000000000',&
                               b'0011111111110000000000000000000000000000000000000000000000000000',&
                               b'0111111111110000000000000000000000000000000000000000000000000000',i=1,1)/))

  r8Arr = (/real(8):: (o'0000000000000000000000', o'0377600000000000000000', o'0777600000000000000000',i=1,1)/)
  call check(123, r8Arr)
  call check(124, (/real(8):: (o'0000000000000000000000', o'0377600000000000000000', o'0777600000000000000000',i=1,1)/))

  r8Arr = (/real(8):: (z'0000000000000000', z'3ff0000000000000', z'7ff0000000000000',i=1,1)/)
  call check(125, r8Arr)
  call check(126, (/real(8):: (z'0000000000000000', z'3ff0000000000000', z'7ff0000000000000',i=1,1)/))

  ! Now NaN:
  r8Base = (/ (IEEE_VALUE(0.0_8,IEEE_QUIET_NAN),i=1,3) /)
  r8Arr = (/real(8):: (b'0111111111111000000000000000000000000000000000000000000000000000', &
                       o'0777700000000000000000', z'7ff8000000000000',i=1,1)/)
  if( .not.all(ieee_is_nan(r8Arr)) ) call reportError(127, r8Arr)
  if( .not.all(ieee_is_nan((/real(8):: (b'0111111111111000000000000000000000000000000000000000000000000000', &
                                        o'0777700000000000000000', z'7ff8000000000000',i=1,1)/))) ) &
       call reportError(128, (/real(8):: (b'0111111111111000000000000000000000000000000000000000000000000000', &
                                          o'0777700000000000000000', z'7ff8000000000000',i=1,1)/))

  if (error) error stop 2_4

contains

  subroutine check(line, vals)
    integer :: line
    real(8) :: vals(:)
    if (any(r8Base /= vals)) then
       call reportError(line, vals)
    end if
  end subroutine check

  subroutine reportError(line, vals)
    integer :: line
    real(8) :: vals(:)
    print *, "At line", line, "expected", r8Base, "got", vals
    error = .true.
  end subroutine reportError

end program acetint30c
