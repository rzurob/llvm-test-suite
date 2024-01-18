!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone06a
!*
!*  DATE                       : 2006-08-09
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
!*  Here we focus on non-logical and non-character intrinsic types.
!*
!*  Note: Hollerith constants are no longer part of the standard, but they were
!*  once so important that they should still be tested.
!*
!*  There are companion tests to these in types/intrinsics.
!*  Boz literals are tested elsewhere.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone06a

  implicit none
  integer :: i

  integer(4)   :: i4a(4)
  integer(1)   :: i1a(4)
  real(4)      :: r4a(2), r
  complex(4)   :: z4a(2), z
  double precision :: da(2), d

  character(7) :: typeNames(5)

  enum, bind(c)
  enumerator :: ITYPE=1, RTYPE, ZTYPE, DTYPE, UTYPE
  end enum

  typeNames(ITYPE)  = 'integer'
  typeNames(RTYPE)  = 'real'
  typeNames(ZTYPE)  = 'complex'
  typeNames(DTYPE)  = 'double'
  typeNames(UTYPE)  = 'unknown'

#if __BIG_ENDIAN__
  i4a = (/104_4, 4htest, 115_4, 2hii/)
  print *, i4a
  print *, (/104_4, 4htest, 115_4, 2hii/)
  call test((/104_4, 4htest, 115_4, 2hii/), ITYPE, 1)

  i4a = (/4htest, 104_4, 2hii, 115_4/)
  print *, i4a
  print *, (/4htest, 104_4, 2hii, 115_4/)
  call test((/4htest, 104_4, 2hii, 115_4/), ITYPE, 2)
#else
  i4a = (/104_4, 4htset, 115_4, reverse_byte_order(2hii)/)
  print *, i4a
  print *, (/104_4, 4htset, 115_4, reverse_byte_order(2hii)/)
  call test((/104_4, 4htset, 115_4, reverse_byte_order(2hii)/), ITYPE, 1)

  i4a = (/4htset, 104_4, reverse_byte_order(2hii), 115_4/)
  print *, i4a
  print *, (/4htset, 104_4, reverse_byte_order(2hii), 115_4/)
  call test((/4htset, 104_4, reverse_byte_order(2hii), 115_4/), ITYPE, 2)
#endif

  i1a = (/104_1, 1ht, 115_1, 1hi/)
  print *, i1a
  print *, (/104_1, 1ht, 115_1, 1hi/)
  call test((/104_1, 1ht, 115_1, 1hi/), ITYPE, 3)

  r4a = (/ 0.0_4, 4haaaa /)
  print *, r4a
  print *, (/ 0.0_4, 4haaaa /)
  call test((/ 0.0_4, 4haaaa /), RTYPE, 4)

  r4a = (/ 4haaaa, 0.0_4 /)
  print *, r4a
  print *, (/ 4haaaa, 0.0_4 /)
  call test((/ 4haaaa, 0.0_4 /), RTYPE, 5)

  da = (/ 0.0d0, 8haaaaaaaa /)
  print *, da
  print *, (/ 0.0d0, 8haaaaaaaa /)
  call test((/ 0.0d0, 8haaaaaaaa /), DTYPE, 6)

  da = (/ 8haaaaaaaa, 0.0d0 /)
  print *, da
  print *, (/ 8haaaaaaaa, 0.0d0 /)
  call test((/ 8haaaaaaaa, 0.0d0 /), DTYPE, 7)

  ! No, 8haaaaaaaa is not a mistake: When using a Hollerith constant to
  ! initialise a complex number, a constant like 4haaaa would be extended
  ! to 8 characters by padding on the right with blanks, yielding a number
  ! like (0.26e21,.14e-18).  Using 8haaaaaaaa makes the result more
  ! understandable.

  z4a = (/ (0.0_4,1.0_4), 8haaaaaaaa /)
  print *, z4a
  print *, (/ (0.0_4,1.0_4), 8haaaaaaaa /)
  call test((/ (0.0_4,1.0_4), 8haaaaaaaa /), ZTYPE, 8)

  z4a = (/ 8haaaaaaaa, (0.0_4,1.0_4) /)
  print *, z4a
  print *, (/ 8haaaaaaaa, (0.0_4,1.0_4) /)
  call test((/ 8haaaaaaaa, (0.0_4,1.0_4) /), ZTYPE, 9)

contains

  subroutine test(arg,expect,occ)
    class(*) :: arg(:)
    integer  :: expect, occ, have
    select type(arg)
    type is (integer(4));   have = ITYPE
    type is (integer(1));   have = ITYPE
    type is (real(4));      have = RTYPE
    type is (double precision); have = DTYPE
    type is (complex(4));   have = ZTYPE
    class default;          have = UTYPE
    end select
    if (have == expect) return
    print *, 'Expected ', trim(typeNames(expect)),' at ', occ, ', got ',trim(typeNames(have))
  end subroutine test

end program acetnone06a
