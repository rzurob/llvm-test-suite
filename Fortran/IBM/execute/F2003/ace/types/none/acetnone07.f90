!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone07
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-31
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : BOZ literals in AC's with no type specifier (int+char)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : typeless, boz, integer, character
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Boz literals and Hollerith constants are typeless, so including them in an
!*  array constructor lacking a type specifier is a problem, because we don't
!*  know what type to assign to the AC.  Here we test boz literals alone.  The
!*  2003 standard only allows them in one of three intrinsics (int, real, and
!*  cmplx) and in data statements.  As an extension, they're allowed in some
!*  other contexts like those below.  Because they are typeless, they cannot
!*  appear in a select type or be passed as unlimited polymorphic dummy args.
!*  This source file is used for two test cases: verify the correctness of the
!*  extension, and verify the diagnostics generated in -qlanglvl=2003std.
!*
!*  Note: Unlike Hollerith constants, boz literals are part of the standard.
!*
!*  There are companion tests to these in types/intrinsics.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone07

  implicit none
  integer :: i
  character(1) :: chArray(3)
  integer(1) :: intArray(3)

  print *, ' 1:', (/b'100001', b'100010', b'100011'/) ! !"# / 33, 34, 35
  print *, ' 2:', (/o'41', o'42', o'43'/) ! ditto, octal
  print *, ' 3:', (/z'21', z'22', z'23'/) ! ditto, hex
  print *, ' 4:', (/b'100001', o'42', z'23'/) ! ditto, mixed

  print *, ' 5:', (/ (b'100001', i=1,3) /) ! !!! / 3 * 33
  print *, ' 6:', (/ (o'42', i=1,3) /) ! """ / 3 * 34
  print *, ' 7:', (/ (z'23', i=1,3) /) ! ### / 3 * 35

  chArray = (/b'100001', b'100010', b'100011'/) ! !"# / 33, 34, 35
  print *, ' 8:', chArray
  chArray = (/o'41', o'42', o'43'/) ! ditto, octal
  print *, ' 9:', chArray
  chArray = (/z'21', z'22', z'23'/) ! ditto, hex
  print *, '10:', chArray
  chArray = (/b'100001', o'42', z'23'/) ! ditto, mixed
  print *, '11:', chArray

  chArray = (/ (b'100001', i=1,3) /) ! !!!
  print *, '12:', chArray
  chArray = (/ (o'42', i=1,3) /) ! """
  print *, '13:', chArray
  chArray = (/ (z'23', i=1,3) /) ! ###
  print *, '14:', chArray

  intArray = (/b'100001', b'100010', b'100011'/) ! 33, 34, 35 / !"#
  print *, '15:', intArray
  intArray = (/o'41', o'42', o'43'/) ! ditto, octal
  print *, '16:', intArray
  intArray = (/z'21', z'22', z'23'/) ! ditto, hex
  print *, '17:', intArray
  intArray = (/b'100001', o'42', z'23'/) ! ditto, mixed
  print *, '18:', intArray

  intArray = (/ (b'100001', i=1,3) /) ! 3 * 33
  print *, '19:', intArray
  intArray = (/ (o'42', i=1,3) /) ! 3 * 34
  print *, '20:', intArray
  intArray = (/ (z'23', i=1,3) /) ! 3 * 35
  print *, '21:', intArray

  call chtest(22,(/b'100001', b'100010', b'100011'/)) ! !"# / 33, 34, 35
  call chtest(23,(/o'41', o'42', o'43'/)) ! ditto, octal
  call chtest(24,(/z'21', z'22', z'23'/)) ! ditto, hex
  call chtest(25,(/b'100001', o'42', z'23'/)) ! ditto, mixed

  call chtest(26,(/ (b'100001', i=1,3) /)) ! !!!
  call chtest(27,(/ (o'42', i=1,3) /)) ! """
  call chtest(28,(/ (z'23', i=1,3) /)) ! ###

  call itest(29,(/b'100001', b'100010', b'100011'/)) ! !"# / 33, 34, 35
  call itest(30,(/o'41', o'42', o'43'/)) ! ditto, octal
  call itest(31,(/z'21', z'22', z'23'/)) ! ditto, hex
  call itest(32,(/b'100001', o'42', z'23'/)) ! ditto, mixed

  call itest(33,(/ (b'100001', i=1,3) /)) ! 3 * 33
  call itest(34,(/ (o'42', i=1,3) /)) ! 3 * 34
  call itest(35,(/ (z'23', i=1,3) /)) ! 3 * 35

contains

  subroutine chtest(line,arg)
    integer :: line
    character(*) :: arg(:)
    print *, line, ':', len(arg), kind(arg), size(arg), arg
  end subroutine chtest

  subroutine itest(line,arg)
    integer :: line
    integer(1) :: arg(:)
    print *, line, ':', size(arg), arg
  end subroutine itest

end program acetnone07
