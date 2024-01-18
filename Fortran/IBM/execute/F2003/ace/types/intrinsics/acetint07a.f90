!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint07a
!*
!*  DATE                       : 2007-09-07 (from original 2006-11-24)
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic type specifier combinations
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type, type specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  The type specifier gives the expected type of the data, but any data type can
!*  be used if it works with intrinsic assignment, e.g., (/ real:: (1,2) /) works.
!*  By the same token, we should be able to use nested AC's with their own type
!*  specifiers, providing they would work with intrinsic assignment, e.g.,
!*  (/ real:: (/ complex:: (1,2) /)/) should work.  This will work for any
!*  numeric type (integer, real, double precision, and complex), but not for
!*  logical or character types, since intrinsic assignments in these cases must
!*  be of logical to logical or character to character.
!*  Moreover, the order will matter:  (/real::(integer::(/real::2.1/)/)/)
!*  produces an array identical to (/real::2.0/) (integer:: truncates the data).
!*  Every place in which an array constructor can be used must be tested.  Here
!*  we test the appearance in an expression; other tests check the use in call
!*  statements, print statements, and assignments.
!*  (Identical to acetint07a, with the AC-IMP-DO's removed/converted.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint07a

  use, intrinsic :: ieee_arithmetic
  implicit none

  integer   :: iarr(5), iarr6(6), i
  real      :: rarr(5), var
  double precision :: darr(5)
  complex   :: zarr(5)
  integer(4) :: errorCount

  ! Test various types appearing in an integer AC in common forms, i.e., expressions,
  ! assignments, and print statements, and then repeat with other numeric types.
  ! We also test different kinds of logical or character data, like acetint05, which
  ! tests integers of different kinds.  Order should matter - if we go from least
  ! precise inside to most precise outside, we should lose nothing, but if we go from
  ! most to least precise, we lose details.
  ! real(4/8/16), complex(4/8), double precision, double complex
  ! character(1), ...(2), ...
  ! logical(1/2/4/8)

  errorCount = 0

  if (any([integer:: 1, [real:: 2.1], [double precision:: 3.1d0], [complex:: (4.1,5.1)], [integer::6]] &
          /= [integer:: 1, 2, 3, 4, 6]) ) call noteError(1)
  iarr = [integer:: 1, [real:: 2.1], [double precision:: 3.1d0], [complex:: (4.1,5.1)], [integer::6]]
  if (any(iarr /= [integer:: 1, 2, 3, 4, 6]) ) call noteError(2)
  print *, [integer:: 1, [real:: 2.1], [double precision:: 3.1d0], [complex:: (4.1,5.1)], [integer::6]]
  print *, 'x', [integer:: 1, 2, 3, 4, 6]

  if (.not.all(isSameReal4([real:: 1.2, [integer:: 2], [double precision:: 3.1d0], [complex:: (4.1,5.1)], [real::6.1]], &
                           [real:: 1.2, 2.0, 3.1, 4.1, 6.1]))) call noteError(5)
  rarr = [real:: 1.2, [integer:: 2], [double precision:: 3.1d0], [complex:: (4.1,5.1)], [real::6.1]]
  if (.not.all(isSameReal4(rarr, [real:: 1.2, 2.0, 3.1, 4.1, 6.1]))) call noteError(6)
  print *, [real:: 1.2, [integer:: 2], [double precision:: 3.1d0], [complex:: (4.1,5.1)], [real::6.1]]
  print *, 'x', [real:: 1.2_4, 2.0_4, 3.1_4, 4.1_4, 6.1_4]

  if (.not.all(isSameReal8([double precision:: 1.2d0, [integer:: 2], [real:: 3.1], [complex:: (4.1,5.1)], [double precision::6.1d0]], &
                           [double precision:: 1.2d0, 2.0_4, 3.1_4, 4.1_4, 6.1d0]))) call noteError(9)
  darr = [double precision:: 1.2d0, [integer:: 2], [real:: 3.1], [complex:: (4.1,5.1)], [double precision::6.1d0]]
  if (.not.all(isSameReal8(darr, [double precision:: 1.2d0, 2.0_4, 3.1_4, 4.1_4, 6.1d0]))) call noteError(10)
  print *, [double precision:: 1.2d0, [integer:: 2], [real:: 3.1], [complex:: (4.1,5.1)], [double precision::6.1d0]]
  print *, 'x', [double precision:: 1.2_8, 2.0_8, 3.1_4, 4.1_4, 6.1_8]

  if (.not.all(isSameComplex4([complex:: (4.1,5.1), [integer:: 2], [double precision:: 3.1d0], [real:: 1.2], [complex::(6.2,7.3)]], &
                              [complex:: (4.1,5.1), (2,0), (3.1,0), (1.2,0), (6.2,7.3)]))) call noteError(13)
  zarr = [complex:: (4.1,5.1), [integer:: 2], [double precision:: 3.1d0], [real:: 1.2], [complex::(6.2,7.3)]]
  if (.not.all(isSameComplex4(zarr, [complex:: (4.1,5.1), (2,0), (3.1,0), (1.2,0), (6.2,7.3)]))) call noteError(14)
  print *, [complex:: (4.1,5.1), [integer:: 2], [double precision:: 3.1d0], [real:: 1.2], [complex::(6.2,7.3)]]
  print *, 'x', [complex:: (4.1,5.1), 2.0_4, 3.1_4, 1.2_4, (6.2,7.3)]

  if (.not.all(isSameReal4([real:: 1.2, [integer:: 2, [real:: 6.1, [double precision:: 3.1d0, [complex:: (4.1,5.1)]]]]], &
                           [real:: 1.2, 2, 6, 3, 4]))) call noteError(17)
  print *, [real:: 1.2, [integer:: 2, [real:: 6.1, [double precision:: 3.1d0, [complex:: (4.1,5.1)]]]]]
  print *, 'x', [real:: 1.2_4, 2.0_4, 6.0_4, 3.0_4, 4.0_4]

  if (.not.all(isSameReal4([real:: 1.2, [double precision:: 3.1d0, [complex:: (4.1,5.1), [integer:: 2, [real:: 6.1]]]]], &
                           [real:: 1.2, 3.1, 4.1, 2.0, 6.0]))) call noteError(18)
  print *, [real:: 1.2, [double precision:: 3.1d0, [complex:: (4.1,5.1), [integer:: 2, [real:: 6.1]]]]]
  print *, 'x', [real:: 1.2_4, 3.1_4, 4.1_4, 2.0_4, 6.1_4]

  if (.not.all(isSameReal8([double precision:: 1.2d0, [integer:: 2, [real:: 3.1, [complex:: (4.1,5.1), [double precision:: 6.1d0]]]]], &
                           [double precision:: 1.2d0, 2.0d0, 3.0d0, 4.0d0, 6.0d0]))) call noteError(19)
  print *, [double precision:: 1.2d0, [integer:: 2, [real:: 3.1, [complex:: (4.1,5.1), [double precision:: 6.1d0]]]]]
  print *, 'x', [double precision:: 1.2_8, 2.0_8, 3.0_8, 4.0_8, 6.0_8]

  if (.not.all(isSameReal8([double precision:: 1.2d0, [double precision:: 6.1d0, [complex:: (4.1,5.1), [real:: 3.1, [integer:: 2]]]]], &
                           [double precision:: 1.2d0, 6.1d0, 4.1_4, 3.1_4, 2.0_4]))) call noteError(20)
  print *, [double precision:: 1.2d0, [double precision:: 6.1d0, [complex:: (4.1,5.1), [real:: 3.1, [integer:: 2]]]]]
  print *, 'x', [double precision:: 1.2_8, 6.1_8, 4.1_4, 3.1_4, 2.0_8]

  if (.not.all(isSameComplex4([complex:: (4.1,5.1), [double precision:: 3.1d0, [real:: 1.2, [complex:: (6.1,7.1), [integer:: 2]]]]], &
                              [complex:: (4.1,5.1), (3.1,0.0), (1.2,0.0), (6.1,0.0), (2.0,0.0)]))) call noteError(21)
  print *, [complex:: (4.1,5.1), [double precision:: 3.1d0, [real:: 1.2, [complex:: (6.1,7.1), [integer:: 2]]]]]
  print *, 'x', [complex:: (4.1,5.1), 3.1_4, 1.2_4, 6.1_4, 2.0_4]

  if (.not.all(isSameComplex8([double complex:: (4.1d0,5.1d0), [double precision:: 3.1d0, [real:: 1.2, [double complex:: (6.1,7.1), [integer:: 2]]]]], &
                              [double complex:: (4.1d0,5.1d0), (3.1d0,0.0d0), (1.2_4,0.0_4), (6.1_4,0.0_4), (2.0_4,0.0_4)]))) call noteError(22)
  print *, [double complex:: (4.1d0,5.1d0), [double precision:: 3.1d0, [real:: 1.2, [double complex:: (6.1,7.1), [integer:: 2]]]]]
  print *, 'x', [double complex:: (4.1d0,5.1d0), 3.1_8, 1.2_4, 6.1_4, 2]

  iarr = [integer:: 1, aimag([double complex:: [double precision:: 3.1d0, [real:: 2.1, [complex:: (4.1,5.1), [integer::6]]]]])]
  if (any(iarr /= [integer:: 1, 0, 0, 0, 0])) call noteError(23)
  print *, iarr

  iarr6 = [integer:: 1, aimag([complex:: (4.1,5.1), [double precision:: 3.1d0, [complex:: (7.1,8.1), [real:: 2.1, [integer::6]]]]])]
  if (any(iarr6 /= [integer:: 1, 5, 0, 0, 0, 0])) call noteError(25)
  print *, iarr6

  if (errorCount > 0) then
     print *, errorCount, "errors in total"
     stop 2
  end if

contains

  subroutine noteError(item)
    integer :: item
    print *, "Unexpected inequality at item", item
    errorCount = errorCount + 1
  end subroutine noteError

  ! Adapted from our standard precision_R4 to be elemental, and to handle NaN and Inf:
  elemental logical function isSameReal4(value,expected)
    real(4), intent(in) :: value, expected
    real(4) :: high, low, delta

    ! If they're both extreme - both NaN or Infinite with the same sign - return true
    if (ieee_is_nan(value) .and. ieee_is_nan(expected) .or. (value == expected)) then
       isSameReal4 = .true.
    else
       delta = expected * 0.00001
       high = delta + expected
       low = expected - delta
       ! This is still not perfect: we don't handle the range near Inf well:
       if (expected < 0.0E0) then
          isSameReal4 = ((value >= high) .and. (value <= low))
       else
          isSameReal4 = ((value <= high) .and. (value >= low))
       end if
    end if

  end function isSameReal4

  ! Adapted from our standard precision_R8 to be elemental, and to handle NaN and Inf:
  elemental logical function isSameReal8(value,expected)
    real(8), intent(in) :: value, expected
    real(8) :: high, low, delta

    ! If they're both extreme - both NaN or Infinite with the same sign - return true
    if (ieee_is_nan(value) .and. ieee_is_nan(expected) .or. (value == expected)) then
       isSameReal8 = .true.
    else
       delta = expected * 0.00000000000001D0
       high = delta + expected
       low = expected - delta
       ! This is still not perfect: we don't handle the range near Inf well:
       if (expected < 0.0D0) then
          isSameReal8 = ((value >= high) .and. (value <= low))
       else
          isSameReal8 = ((value <= high) .and. (value >= low))
       end if
    end if

  end function isSameReal8

  ! Adapted from our standard precision_R8 to be elemental, and to handle NaN and Inf:
  elemental logical function isSameReal16(value,expected)
    real(16), intent(in) :: value, expected
    real(16) :: high, low, delta

    ! If they're both extreme - both NaN or Infinite with the same sign - return true
    if (ieee_is_nan(value) .and. ieee_is_nan(expected) .or. (value == expected)) then
       isSameReal16 = .true.
    else
       delta = expected * 0.0000000000000000000000000000001Q0
       high = delta + expected
       low = expected - delta
       ! This is still not perfect: we don't handle the range near Inf well:
       if (expected < 0.0Q0) then
          isSameReal16 = ((value >= high) .and. (value <= low))
       else
          isSameReal16 = ((value <= high) .and. (value >= low))
       end if
    end if

  end function isSameReal16

  elemental logical function isSameComplex4(value,expected)
    complex(4), intent(in) :: value, expected
    isSameComplex4 = isSameReal4(real(value),real(expected)) .and. isSameReal4(aimag(value),aimag(expected))
  end function isSameComplex4

  elemental logical function isSameComplex8(value,expected)
    complex(8), intent(in) :: value, expected
    isSameComplex8 = isSameReal8(real(value),real(expected)) .and. isSameReal8(aimag(value),aimag(expected))
  end function isSameComplex8

  elemental logical function isSameComplex16(value,expected)
    complex(16), intent(in) :: value, expected
    isSameComplex16 = isSameReal16(real(value),real(expected)) .and. isSameReal16(aimag(value),aimag(expected))
  end function isSameComplex16

end program acetint07a
