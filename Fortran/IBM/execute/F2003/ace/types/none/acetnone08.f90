!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone08
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-03
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : BOZ+Hollerith literals in AC's with no type specifier
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
!*  know what type to assign to the AC.  Here we test a mix of boz literals and
!*  Hollerith constants.
!*
!*  Note: Unlike Hollerith constants, boz literals are part of the standard.
!*
!*  There are companion tests to these in types/intrinsics.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone08

  implicit none
  integer :: i
  character(1) :: chArray(2)
  integer(1)   :: intArray(2)

  print *, ' 1:', (/b'100011', 1h$/) ! 35, 36
  print *, ' 2:', (/o'43', 1h$/)     ! 35, 36
  print *, ' 3:', (/z'23', 1h$/)     ! 35, 36
  print *, ' 4:', (/(b'100011',1h#, i=1,1) /) ! 35 35
  print *, ' 5:', (/(o'44',1h$,     i=1,1) /) ! 36 36
  print *, ' 6:', (/(z'25',1h%,     i=1,1) /) ! 37 37

  print *, ' 7:', (/1h$, b'100011'/) ! 36, 35
  print *, ' 8:', (/1h$, o'43'/)     ! 36, 35
  print *, ' 9:', (/1h$, z'23'/)     ! 36, 35
  print *, '10:', (/(1h#, b'100011', i=1,1) /) ! 35 35
  print *, '11:', (/(1h$, o'44',     i=1,1) /) ! 36 36
  print *, '12:', (/(1h%, z'25',     i=1,1) /) ! 37 37

  chArray = (/b'100011', 1h$/)
  print *, '13:', chArray		! #$
  chArray = (/o'43', 1h$/)
  print *, '14:', chArray		! #$
  chArray = (/z'23',  1h$/)
  print *, '15:', chArray		! #$
  chArray = (/(b'100011',1h#, i=1,1) /)
  print *, '16:', chArray		! ##
  chArray = (/(o'44',1h$,     i=1,1) /)
  print *, '17:', chArray		! $$
  chArray = (/(z'25',1h%,     i=1,1) /)
  print *, '18:', chArray		! %%

  chArray = (/1h$, b'100011'/)
  print *, '19:', chArray		! $#
  chArray = (/1h$, o'43'/)
  print *, '20:', chArray		! $#
  chArray = (/1h$, z'23'/)
  print *, '21:', chArray		! $#
  chArray = (/(1h#, b'100011', i=1,1) /)
  print *, '22:', chArray		! ##
  chArray = (/(1h$, o'44',     i=1,1) /)
  print *, '23:', chArray		! $$
  chArray = (/(1h%, z'25',     i=1,1) /)
  print *, '24:', chArray		! %%

  intArray = (/b'100011', 1h$/)
  print *, '25:', intArray		! 35, 36
  intArray = (/o'43', 1h$/)
  print *, '26:', intArray		! 35, 36
  intArray = (/z'23',  1h$/)
  print *, '27:', intArray		! 35, 36
  intArray = (/(b'100011',1h#, i=1,1) /)
  print *, '28:', intArray		! 35 35
  intArray = (/(o'44',1h$,     i=1,1) /)
  print *, '29:', intArray		! 36 36
  intArray = (/(z'25',1h%,     i=1,1) /)
  print *, '30:', intArray		! 37 37

  intArray = (/1h$, b'100011'/)
  print *, '31:', intArray		! 36, 35
  intArray = (/1h$, o'43'/)
  print *, '32:', intArray		! 36, 35
  intArray = (/1h$, z'23'/)
  print *, '33:', intArray		! 36, 35
  intArray = (/(1h#, b'100011', i=1,1) /)
  print *, '34:', intArray		! 35 35
  intArray = (/(1h$, o'44',     i=1,1) /)
  print *, '35:', intArray		! 36 36
  intArray = (/(1h%, z'25',     i=1,1) /)
  print *, '36:', intArray		! 37 37

  call chtest(37,(/b'100011', 1h$/)) ! 35, 36
  call chtest(38,(/o'43', 1h$/))     ! 35, 36
  call chtest(39,(/z'23',  1h$/))     ! 35, 36
  call chtest(40,(/(b'100011',1h#, i=1,1) /)) ! 35 35
  call chtest(41,(/(o'44',1h$,     i=1,1) /)) ! 36 36
  call chtest(42,(/(z'25',1h%,     i=1,1) /)) ! 37 37

  call chtest(43,(/1h$, b'100011'/)) ! 36, 35
  call chtest(44,(/1h$, o'43'/))     ! 36, 35
  call chtest(45,(/1h$, z'23'/))     ! 36, 35
  call chtest(46,(/(1h#, b'100011', i=1,1) /)) ! 35 35
  call chtest(47,(/(1h$, o'44',     i=1,1) /)) ! 36 36
  call chtest(48,(/(1h%, z'25',     i=1,1) /)) ! 37 37

  call itest(49,(/b'100011', 1h$/)) ! 35, 36
  call itest(50,(/o'43', 1h$/))     ! 35, 36
  call itest(51,(/z'23',  1h$/))     ! 35, 36
  call itest(52,(/(b'100011',1h#, i=1,1) /)) ! 35 35
  call itest(53,(/(o'44',1h$,     i=1,1) /)) ! 36 36
  call itest(54,(/(z'25',1h%,     i=1,1) /)) ! 37 37

  call itest(55,(/1h$, b'100011'/)) ! 36, 35
  call itest(56,(/1h$, o'43'/))     ! 36, 35
  call itest(57,(/1h$, z'23'/))     ! 36, 35
  call itest(58,(/(1h#, b'100011', i=1,1) /)) ! 35 35
  call itest(59,(/(1h$, o'44',     i=1,1) /)) ! 36 36
  call itest(60,(/(1h%, z'25',     i=1,1) /)) ! 37 37

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

end program acetnone08
