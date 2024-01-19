!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-03
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : arithmetic and logical expressions
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Apply various arithmetic, relational, and logical expressions to AC's;
!*  at the same time, try passing the AC's to some simple user-defined
!*  functions, elemental functions, and intrinsics.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint44

  implicit none
  integer :: i
  logical(4) :: precision_r4
  logical, parameter :: F = .false., T = .true.

  ! Division by scalar
  print *, ([integer:: 10, 20, 30] / 10)
  if (any(([integer:: 10, 20, 30] / 10) /= [integer:: 1,2,3])) error stop 2
  if (sum(([integer:: 10, 20, 30] / 10)) /= 6) error stop 3
  if (.not. sameInt(([integer:: 10, 20, 30] / 10), [integer:: 1,2,3])) error stop 4
  if (.not. all(sameIntEl([integer:: 10, 20, 30] / 10, [integer:: 1,2,3]))) error stop 5

  ! Division by vector
  print *, ([integer:: 10, 20, 30] / [integer:: 2, 5, 10])
  if (any(([integer:: 10, 20, 30] / [integer:: 2, 5, 10]) /= [integer:: 5,4,3])) error stop 6
  if (sum(([integer:: 10, 20, 30] / [integer:: 2, 5, 10])) /= 12) error stop 7
  if (.not. sameInt(([integer:: 10, 20, 30] / [integer:: 2, 5, 10]), [integer:: 5,4,3])) error stop 8
  if (.not. all(sameIntEl([integer:: 10, 20, 30] / [integer:: 2, 5, 10], [integer:: 5,4,3]))) error stop 9

  ! Addition by vector
  print *, ([real:: 4.2e-2, 6.7e2, 1e16] + [real:: 3.3e-1, 3.2e1, 5e15])
  if (.not. precision_r4(product(([real:: 4.2e-2, 6.7e2, 1e16] + [real:: 3.3e-1, 3.2e1, 5e15])), (0.372*702*15e15))) error stop 10
  if (.not. sameReal(([real:: 4.2e-2, 6.7e2, 1e16] + [real:: 3.3e-1, 3.2e1, 5e15]), [real:: 0.372, 702.0, 15e15], 0.00001)) error stop 11
  if (.not. all(sameRealEl([real:: 4.2e-2, 6.7e2, 1e16] + [real:: 3.3e-1, 3.2e1, 5e15], [real:: 0.372, 702.0, 15e15], 0.00001))) error stop 12

  ! Repeat with subtraction:
  print *, ([real:: 4.2e-2, 6.7e2, 1e16] - [real:: 3.3e-1, 3.2e1, 5e15])
  if (.not. precision_r4(product(([real:: 4.2e-2, 6.7e2, 1e16] - [real:: 3.3e-1, 3.2e1, 5e15])), (-.288*638*5e15))) error stop 13
  if (.not. sameReal(([real:: 4.2e-2, 6.7e2, 1e16] - [real:: 3.3e-1, 3.2e1, 5e15]), [real:: -0.288, 638.0, 5e15], 0.00001)) error stop 14
  if (.not. all(sameRealEl([real:: 4.2e-2, 6.7e2, 1e16] - [real:: 3.3e-1, 3.2e1, 5e15], [real:: -0.288, 638.0, 5e15], 0.00001))) error stop 15

  ! Compare sizes:
  print *, ([integer:: 1, 5, 11] > [real:: 2.0, 5.00001, 10.0])
  if (any(([integer:: 1, 5, 11] > [real:: 2.0, 5.00001, 10.0]) .neqv. [logical:: F, F, T])) error stop 16
  if (any(merge([character:: 'a', 'b', 'c'], [character:: 'd', 'e', 'f'], [integer:: 1, 5, 11] > [real:: 2.0, 5.00001, 10.0]) &
           /= [character:: 'd', 'e', 'c'])) error stop 17
  if (.not. sameLog(([integer:: 1, 5, 11] > [real:: 2.0, 5.00001, 10.0]), [logical:: F, F, T])) error stop 18
  if (.not. all(sameLogEl([integer:: 1, 5, 11] > [real:: 2.0, 5.00001, 10.0], [logical:: F, F, T]))) error stop 19

  ! Try logical OR:

  print *, ([logical:: F, F, T, T] .or. [logical:: F, T, F, T])
  if (any(([logical:: F, F, T, T] .or. [logical:: F, T, F, T]) .neqv. [logical:: F, T, T, T])) error stop 20
  if (all([logical:: F, F, T, T] .or. [logical:: F, T, F, T])) error stop 21
  if (.not. sameLog(([logical:: F, F, T, T] .or. [logical:: F, T, F, T]), [logical:: F, T, T, T])) error stop 22
  if (.not. all(sameLogEl([logical:: F, F, T, T] .or. [logical:: F, T, F, T], [logical:: F, T, T, T]))) error stop 23

  ! Try logical equivalence:

  print *, ([logical:: F, F, T, T] .eqv. [logical:: F, T, F, T])
  if (.not. all(([logical:: F, F, T, T] .eqv. [logical:: F, T, F, T]) .eqv. [logical:: T, F, F, T])) error stop 24
  if (all([logical:: F, F, T, T] .eqv. [logical:: F, T, F, T])) error stop 25
  if (.not. sameLog(([logical:: F, F, T, T] .eqv. [logical:: F, T, F, T]), [logical:: T, F, F, T])) error stop 26
  if (.not. all(sameLogEl([logical:: F, F, T, T] .eqv. [logical:: F, T, F, T], [logical:: T, F, F, T]))) error stop 27

  ! Round out the logicals with and and not:

  print *, ((.not. [logical:: T, T, F, F]) .and. [logical:: F, T, F, T])
  if (.not. all(((.not. [logical:: T, T, F, F]) .and. [logical:: F, T, F, T]) .eqv. [logical:: F, F, F, T])) error stop 28
  if (all((.not. [logical:: T, T, F, F]) .and. [logical:: F, T, F, T])) error stop 29
  if (.not. sameLog(((.not. [logical:: T, T, F, F]) .and. [logical:: F, T, F, T]), [logical:: F, F, F, T])) error stop 30
  if (.not. all(sameLogEl((.not. [logical:: T, T, F, F]) .and. [logical:: F, T, F, T], [logical:: F, F, F, T]))) error stop 31

  ! Finish with characters:

  print *, [character(1):: 'a', 'bc', 'def'] // [character(2):: 'g', '''.,', 'hij']
  if (all([character(1):: 'a', 'bc', 'def'] > [character(2):: 'g', '''.,', 'hij'])) error stop 32
  if (any([character(1):: 'a', 'b', 'd'] /= [character(2):: 'a ', 'b ', 'd '])) error stop 33
  if (any(([character(1):: 'a', 'bc', 'def'] // [character(2):: 'g', '''.,', 'hij']) /= [character(3):: 'ag ', 'b''.', 'dhi'])) error stop 34
  if (.not. sameChar(([character(1):: 'a', 'bc', 'def'] // [character(2):: 'g', '''.,', 'hij']), [character(3):: 'ag ', 'b''.', 'dhi'])) error stop 35
  if (.not. all(sameCharEl(([character(1):: 'a', 'bc', 'def'] // [character(2):: 'g', '''.,', 'hij']), [character(3):: 'ag ', 'b''.', 'dhi']))) error stop 36

contains

  logical function sameInt(arr1, arr2)
    integer :: arr1(:), arr2(:)
    sameInt = all(arr1 == arr2)
  end function sameInt

  elemental logical function sameIntEl(v1, v2)
    integer, intent(in) :: v1, v2
    sameIntEl = (v1 == v2)
  end function sameIntEl

  logical function sameReal(arr1, arr2, err)
    real :: arr1(:), arr2(:), err
    sameReal = all(abs(arr1 - arr2) < abs(arr1*err))
  end function sameReal

  elemental logical function sameRealEl(v1, v2, err)
    real, intent(in) :: v1, v2, err
    sameRealEl = abs(v1-v2) < abs(v1*err)
  end function sameRealEl

  logical function sameLog(arr1, arr2)
    logical :: arr1(:), arr2(:)
    sameLog = all(arr1 .eqv. arr2)
  end function sameLog

  elemental logical function sameLogEl(v1, v2)
    logical, intent(in) :: v1, v2
    sameLogEl = v1 .eqv. v2
  end function sameLogEl

  logical function sameChar(arr1, arr2)
    character(*) :: arr1(:), arr2(:)
    sameChar = all(arr1 == arr2)
  end function sameChar

  elemental logical function sameCharEl(v1, v2)
    character(*), intent(in) :: v1, v2
    sameCharEl = v1 == v2
  end function sameCharEl

end program acetint44
