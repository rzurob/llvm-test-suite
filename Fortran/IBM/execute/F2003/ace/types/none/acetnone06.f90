!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone06
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-31
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Hollerith literals in AC's with no type specifier
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : typeless, hollerith
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Boz literals and Hollerith constants are typeless, so including them in an
!*  array constructor lacking a type specifier is a problem, because we don't
!*  know what type to assign to the AC.  Here we test Hollerith constants alone,
!*  assuming that the historical behaviour of treating them like characters will
!*  continue, but also testing their typeless property in the context of an
!*  integer array.
!*  Note: Hollerith constants are no longer part of the standard, but they were
!*  once so important that they should still be tested.
!*  Note 2: An AC containing Hollerith constants has no type, which is an oddity
!*  but it has the consequence that it can be passed as an argument to a
!*  procedure expecting any intrinsic array type without producing an error
!*  message.  We can also do arithmetic on the values, but that's arguably
!*  invalid (btw: 1h +1h ==z'20202020'+z'20202020' and not z'20'+z'20, so
!*  1h +1h ==z'40404040', or 538976288+538976288==1077952576 and not 32+32=64!).
!*  Note 3: An AC containing Hollerith constants is also not polymorphic, so it
!*  cannot be used in a select type directly.  Since it also has no proper type,
!*  trying to pass it to a procedure which takes a class(*) pointer will not
!*  compile, in contrast to an integer or character array, for example.
!*
!*  There are companion tests to these in types/intrinsics.
!*
!*  Boz literals are tested elsewhere.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone06

  implicit none
  integer :: i
  character(1) :: chArray(14)
  integer(1) :: intArray(14)

  print *, (/1ht, 1hh, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1he, 1hs, 1ht/)
  print *, (/1h(, 1h/, 1h', 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1h', 1h,, 1h/, 1h)/)
  print *, (/1h/, 1h/, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1h,, 1h/, 1h//)
  print *, (/ (1hr, i=1,14) /)
  print *, (/ (1h,, i=1,14) /)

  chArray = (/1ht, 1hh, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1he, 1hs, 1ht/)
  print *, chArray
  chArray = (/1h(, 1h/, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1h,, 1h/, 1h)/)
  print *, chArray
  chArray = (/1h/, 1h/, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1h,, 1h/, 1h//)
  print *, chArray

  intArray = (/1ht, 1hh, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1he, 1hs, 1ht/)
  print *, intArray
  intArray = (/1h(, 1h/, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1h,, 1h/, 1h)/)
  print *, intArray
  intArray = (/1h/, 1h/, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1h,, 1h/, 1h//)
  print *, intArray

  chArray = (/ (1hr, i=1,14) /)
  print *, chArray
  chArray = (/ (1h,, i=1,14) /)
  print *, chArray
  intArray = (/ (1hr, i=1,14) /)
  print *, intArray
  intArray = (/ (1h,, i=1,14) /)
  print *, intArray

  call chtest((/1ht, 1hh, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1he, 1hs, 1ht/))
  call chtest((/1h(, 1h/, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1h,, 1h/, 1h)/))
  call chtest((/1h/, 1h/, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1h,, 1h/, 1h//))

  call chtest((/4hthis, 4h is , 4ha te, 4hst  /))
  call chtest((/2hth, 2his, 2h i, 2hs , 2ha , 2hte, 2hst/))
  call chtest((/2h//, 2his, 2h i, 2hs , 2ha , 2ht,, 2h///))

  call itest((/1ht, 1hh, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1he, 1hs, 1ht/))
  call itest((/1h(, 1h/, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1h,, 1h/, 1h)/))
  call itest((/1h/, 1h/, 1hi, 1hs, 1h , 1hi, 1hs, 1h , 1ha, 1h , 1ht, 1h,, 1h/, 1h//))

  call chtest((/ (1hr, i=1,14) /))
  call chtest((/ (1h,, i=1,14) /))

  call itest((/ (1hr, i=1,14) /))
  call itest((/ (1h,, i=1,14) /))

contains
  subroutine chtest(arg)
    character(*) :: arg(:)
    print *, len(arg), kind(arg), size(arg), arg
  end subroutine chtest

  subroutine itest(arg)
    integer(1) :: arg(:)
    print *, size(arg), arg
  end subroutine itest

end program acetnone06
