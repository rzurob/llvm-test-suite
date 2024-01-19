!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-08-01
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Hollerith literals and characters in AC's with no type spec
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : typeless, hollerith, character
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Here we mix characters with Hollerith constants in an array constructor
!*  lacking a type specifier.  Historically, Hollerith constants were used
!*  to initialise numeric variables with character data (a separate character
!*  type did not then exist), so they're half-way between characters and
!*  numbers.  Verify that character data can be correctly initialised.
!*
!*  Note: Hollerith constants are no longer part of the standard, but they were
!*  once so important that they should still be tested.
!*
!*  There are companion tests to these in types/intrinsics.
!*  Boz literals are tested elsewhere.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone06c

  implicit none
  integer :: i

  character(4) :: ch4a(4)
  character(1) :: ch1a(4)

  ch4a = (/'this', ' tes', 4ht's , 4hokay/)
  print *, ch4a
  print *, (/'this', ' tes', 4ht's , 4hokay/)
  call test((/'this', ' tes', 4ht's , 4hokay/), 1)

  ch1a = (/'i', 's', 1ho, 1hk/)
  print *, ch1a
  print *, (/'i', 's', 1ho, 1hk/)
  call test((/'i', 's', 1ho, 1hk/), 2)

  ch4a = (/4hthis, ' tes', 4ht's , 'okay'/)
  print *, ch4a
  print *, (/4hthis, ' tes', 4ht's , 'okay'/)
  call test((/4hthis, ' tes', 4ht's , 'okay'/), 3)

  ch1a = (/1ho, 1hk, 'i', 's'/)
  print *, ch1a
  print *, (/1ho, 1hk, 'i', 's'/)
  call test((/1ho, 1hk, 'i', 's'/), 4)

contains

  subroutine test(arg,occ)
    class(*) :: arg(:)
    integer  :: occ
    select type(arg)
    type is (character(*)); return
    class default;          print *, 'Expected character, found unknown type at ', occ
    end select
  end subroutine test

end program acetnone06c
