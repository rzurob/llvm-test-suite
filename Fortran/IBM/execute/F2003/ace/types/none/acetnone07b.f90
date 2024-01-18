!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone07b
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-28
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : BOZ literals in AC's with no type specifier (logical)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : typeless, boz, logical
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Here we test boz literals masquerading as logicals.
!*
!*  Boz literals and Hollerith constants are typeless, so including them in an
!*  array constructor lacking a type specifier is a problem, because we don't
!*  know what type to assign to the AC.  The 2003 standard only allows them in
!*  one of three intrinsics (int, real, and cmplx) and in data statements.
!*  As an extension, they're allowed in some other contexts like those below.
!*  Because they are typeless, they cannot appear in a select type or be passed
!*  as unlimited polymorphic dummy args.
!*
!*  Note 1: Unlike Hollerith constants, boz literals are part of the standard.
!*  Note 2: The language reference states that only 0 and 1 are defined.
!*
!*  There are companion tests to these in types/intrinsics.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone07b

  implicit none
  integer :: i
  logical(1) :: log1(4)
  logical(2) :: log2(4)
  logical(4) :: log4(4)
  logical(8) :: log8(4)

  print *, ' 1:', (/  b'1', b'0', .true., .false. /) ! T F T F
  print *, ' 2:', (/  o'1', o'0', .true., .false. /) ! ditto, octal
  print *, ' 3:', (/  z'1', z'0', .true., .false. /) ! ditto, hex
  print *, ' 4:', (/  b'1', o'0', z'1', .false. /) ! ditto, mixed
  print *, ' 5:', (/ (b'1', o'0', z'1', .false., i=1,1) /) ! T F T F, mixed

  log8 = (/b'1', b'0', b'1', b'0'/) ! T F T F
  print *, ' 6:', log8
  log4 = (/o'1', o'0', o'1', o'0'/) ! ditto, octal
  print *, ' 7:', log4
  log2 = (/z'1', z'0', z'1', z'0'/) ! ditto, hex
  print *, ' 8:', log2
  log1 = (/b'1', o'0', z'1', b'0'/) ! T F T F, mixed
  print *, ' 9:', log1

  log1 = (/ (b'1', b'0', i=1,2) /) ! T F T F
  print *, '10:', log1
  log2 = (/ (o'1', o'0', i=1,2) /) ! T F T F
  print *, '11:', log2
  log4 = (/ (z'1', z'0', i=1,2) /) ! T F T F
  print *, '12:', log4
  log8 = (/ (b'1', o'0', z'1', z'0', i=1,1) /) ! T F T F
  print *, '13:', log8

  log8 = (/b'1', b'0', .true., .false./) ! T F T F
  print *, '14:', log8
  log4 = (/o'1', o'0', .true., .false./) ! ditto, octal
  print *, '15:', log4
  log2 = (/z'1', z'0', .true., .false./) ! ditto, hex
  print *, '16:', log2
  log1 = (/b'1', o'0', .true., .false./) ! ditto, mixed
  print *, '17:', log1

  log1 = (/ (b'1', .false., i=1,2) /) ! T F T F
  print *, '18:', log1
  log2 = (/ (o'1', .false., i=1,2) /) ! T F T F
  print *, '19:', log2
  log4 = (/ (z'1', .false., i=1,2) /) ! T F T F
  print *, '20:', log4
  log8 = (/ (b'1', o'0', .true., .false., i=1,1) /) ! T F T F
  print *, '21:', log8

  call logtest1(22,(/b'1', b'0', b'1'/)) ! T F T
  call logtest2(23,(/o'1', o'0', o'1'/)) ! ditto, octal
  call logtest4(24,(/z'1', z'0', z'1'/)) ! ditto, hex
  call logtest8(25,(/b'1', o'0', z'1'/)) ! ditto, mixed

  call logtest1(26,(/ (b'1', b'0', i=1,3) /)) ! T F T F T F
  call logtest2(27,(/ (o'1', o'0', i=1,3) /)) ! T F T F T F
  call logtest4(28,(/ (z'1', z'0', i=1,3) /)) ! T F T F T F
  call logtest8(29,(/ (b'1', o'0', z'1', i=1,2) /)) ! T F T T F T

  call logtest1(30,(/b'1', b'0', .false._1/)) ! T F F
  call logtest2(31,(/o'1', o'0', .false._2/)) ! ditto, octal
  call logtest4(32,(/z'1', z'0', .false._4/)) ! ditto, hex
  call logtest8(33,(/b'1', o'0', .false._8/)) ! ditto, mixed

  call logtest1(34,(/ (b'1', .false._1, i=1,3) /)) ! T F T F T F
  call logtest2(35,(/ (o'1', .false._2, i=1,3) /)) ! T F T F T F
  call logtest4(36,(/ (z'1', .false._4, i=1,3) /)) ! T F T F T F
  call logtest8(37,(/ (b'1', o'0', z'1', .false._8, i=1,2) /)) ! T F T F T F T F

contains

  subroutine logtest1(line,arg)
    integer :: line
    logical(1) :: arg(:)
    print *, line, ':', size(arg), arg
  end subroutine logtest1

  subroutine logtest2(line,arg)
    integer :: line
    logical(2) :: arg(:)
    print *, line, ':', size(arg), arg
  end subroutine logtest2

  subroutine logtest4(line,arg)
    integer :: line
    logical(4) :: arg(:)
    print *, line, ':', size(arg), arg
  end subroutine logtest4

  subroutine logtest8(line,arg)
    integer :: line
    logical(8) :: arg(:)
    print *, line, ':', size(arg), arg
  end subroutine logtest8

end program acetnone07b
