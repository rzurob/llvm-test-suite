!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone06b
!*
!*  DATE                       : 2006-08-01
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Hollerith literals and numerics in AC's with no type spec
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : typeless, hollerith, numbers
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Here we mix numerics with Hollerith constants in an array constructor
!*  lacking a type specifier.  Historically, Hollerith constants were used
!*  to initialise numeric data with a character bit-pattern (a separate
!*  character type did not then exist), and could also be used to
!*  initialise logical data, so array constructors containing numeric or
!*  logical data mixed with hollerith constants are acceptable.
!*  Here we test logical data; note that logical data should be initialised
!*  with .true. and .false., and the compiler should warn about this.
!*
!*  Note: Hollerith constants are no longer part of the standard, but they were
!*  once so important that they should still be tested.
!*
!*  There are companion tests to these in types/intrinsics.
!*  Boz literals are tested elsewhere.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone06b

  implicit none
  integer :: i
  logical(1)   :: l1a(4)

  l1a = (/ .true._1, .false._1, 1ha, 1hb /)
  print *, l1a
  print *, (/ .true._1, .false._1, 1ha, 1hb /)
  call test((/ .true._1, .false._1, 1ha, 1hb /), 1)

  l1a = (/ 1ha, 1hb, .true._1, .false._1 /)
  print *, l1a
  print *, (/ 1ha, 1hb, .true._1, .false._1 /)
  call test((/ 1ha, 1hb, .true._1, .false._1 /), 2)

contains

  subroutine test(arg,occ)
    class(*) :: arg(:)
    integer  :: occ
    select type(arg)
    type is (logical(1)); ! do nothing
    class default;        print *, 'Expected logical, found unknown type at ', occ
    end select
  end subroutine test

end program acetnone06b
