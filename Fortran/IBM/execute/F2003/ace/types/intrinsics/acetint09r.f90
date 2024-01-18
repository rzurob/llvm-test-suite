!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint09r
!*
!*  DATE                       : 2006-10-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : data pointers in AC's with intrinsic type specifiers (real)
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
!*  Include initialised data pointer references in AC's and later test their
!*  values, to verify that they are allowed and have the correct value.
!*  The data pointers refer to variables apart from the array as well as to
!*  members and sections of the array.  Pointers and variables of different
!*  kinds are also used.
!*
!*  Note that the initial values for the arrays are chosen to be unique, down to
!*  the byte level (i.e., no byte in any part of any value is the same as any
!*  other byte in any other part), so that if a byte is incorrectly copied,
!*  we will notice it.  Also note that comparisons between values of the same
!*  kind can be exact, but that values converted from one kind to another should
!*  be treated carefully, due to different numbers of bits in exponents, layout,
!*  etc.  All values have been carefully chosen so as not to create Infinite, NaN,
!*  or denormalised numbers (so these should work on Blue Gene, for example).
!*
!*  After the pointers are set up and independent variables initialised, we
!*  create AC's, making sure that different pointers appear in different
!*  positions, and that some references are into parts of the array which are
!*  to be changed.  Section 7.4.1.3 of the standard tells us that "The execution
!*  of the assignment shall have the same effect as if the evaluation of all
!*  operations in expr [the RHS] and variable [the LHS] occurred before any
!*  portion of variable is defined by the assignment."  To verify this, we
!*  print the expected values before the assignment and the actual values after.
!*  We also use some pointers of different kinds, to verify that appropriate
!*  conversions are made.
!*  (We're testing reals in addition to integers and characters because this has
!*  been a productive strategy in the past, and because in involves multi-byte
!*  variables (exp+sig in fp, as opposed to integer values so small they fit in
!*  a single byte or two).)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint09r

  implicit none

  real(4), target   :: rtK4,  rarrK4(4)
  real(8), target   :: rtK8,  rarrK8(4), rarrD3K8(3,3,3), rt2K8
  real(16), target  :: rtK16, rarrK16(4)

  real(4),  parameter :: rcarK4(4)  = [real(z'40302010', 4), &                         ! 2.75195693970e00 [1-4][0567]
                                       real(z'47372717', 4), &                         ! 4.68870898438e04
                                       real(z'46362616', 4), &                         ! 1.16575214844e04
                                       real(z'45352515', 4)]                           ! 2.89831762695e03
  real(8),  parameter :: rcarK8(4)  = [real(z'415161718191A1B1', 8), &                 ! 4.55623002451e06 [4-9AB][1-4]
                                       real(z'425262728292A2B2', 8), &                 ! 3.15844332107e11
                                       real(z'435363738393A3B3', 8), &                 ! 2.18294893495e16
                                       real(z'445464748494A4B4', 8)]                   ! 1.50469375100e21
  real(16), parameter :: rcarK16(4) = [real(z'3A4A5A6A7A8A9AAABACADAEAFA0A1A2A',16), & !-1.72882078120E-25 [0-9A-F][A-D]
                                       real(z'3B4B5B6B7B8B9BABBBCBDBEBFB0B1B2B',16), & !-1.17535163453E-20
                                       real(z'3C4C5C6C7C8C9CACBCCCDCECFC0C1C2C',16), & !-7.98034022823E-16
                                       real(z'3D4D5D6D7D8D9DADBDCDDDEDFD0D1D2D',16)]   !-5.41189471233E-11

  real(4)   :: rindK4(4)
  real(8)   :: rindK8(4)
  real(16)  :: rindK16(4)

  real(4), pointer  :: rpK4,  rpaK4(:)
  real(8), pointer  :: rpK8,  rpaK8(:), rpaD3K8(:,:,:), rpa2D3K8(:,:,:), rp2K8, rp3K8
  real(16), pointer :: rpK16, rpaK16(:)

  character (150) :: line1, line2
  integer(4) :: i, j, k, errorCount

  rtK8  =  real(z'C58595A5B5D5E5F5', 8) ! -8.35012212264e26 [89A-F]5
  rt2K8 =  real(z'C68696A6B6D6E6F6', 8) ! -5.72686039011e31 [89A-F]6
  rpK8  => rtK8
  rp2K8 => rt2K8
  rp3K8 => rarrK8(2)
  rpaK8 => rarrK8(2:3)
  rpK4  => rtK4
  rpK16 => rtK16

  errorCount = 0
  ! Test simple copy
  rarrK8 = [rcarK8]
  if (any(rarrK8 /= rcarK8)) call noteFailure('A', rarrK8, rcarK8)

  ! Test use of pointer overlapping existing content
  rindK8 = (/ rtK8, rpK8, rp2K8, rp3K8 /)
  rarrK8 = (/ rtK8, rpK8, rp2K8, rp3K8 /)
  if (any(rarrK8 /= rindK8)) call noteFailure('B', rarrK8, rindK8)

  ! Repeat with TS:
  rarrK8 = rcarK8
  rindK8 = (/ real(8):: rtK8, rpK8, rp2K8, rp3K8 /)
  rarrK8 = (/ real(8):: rtK8, rpK8, rp2K8, rp3K8 /)
  if (any(rarrK8 /= rindK8)) call noteFailure('C', rarrK8, rindK8)


  ! Shuffle the order:
  rarrK8 = rcarK8
  rindK8 = (/ rp3K8, rtK8, rp2K8, rpK8 /)
  rarrK8 = (/ rp3K8, rtK8, rp2K8, rpK8 /)
  if (any(rarrK8 /= rindK8)) call noteFailure('D', rarrK8, rindK8)

  ! Repeat with TS:
  rarrK8 = rcarK8
  rindK8 = (/ real(8):: rp3K8, rtK8, rp2K8, rpK8 /)
  rarrK8 = (/ real(8):: rp3K8, rtK8, rp2K8, rpK8 /)
  if (any(rarrK8 /= rindK8)) call noteFailure('E', rarrK8, rindK8)


  ! Shuffle again:
  rarrK8 = rcarK8
  rindK8 = (/ rtK8, rp3K8, rp2K8, rpK8 /)
  rarrK8 = (/ rtK8, rp3K8, rp2K8, rpK8 /)
  if (any(rarrK8 /= rindK8)) call noteFailure('F', rarrK8, rindK8)

  ! Repeat with TS:
  rarrK8 = rcarK8
  rindK8 = (/ real(8):: rtK8, rp3K8, rp2K8, rpK8 /)
  rarrK8 = (/ real(8):: rtK8, rp3K8, rp2K8, rpK8 /)
  if (any(rarrK8 /= rindK8)) call noteFailure('G', rarrK8, rindK8)


  ! Two array sections:
  rarrK8 = rcarK8
  rindK8 = (/ rpaK8, rpaK8 /)
  rarrK8 = (/ rpaK8, rpaK8 /)
  if (any(rarrK8 /= rindK8)) call noteFailure('H', rarrK8, rindK8)

  ! Repeat with TS:
  rarrK8 = rcarK8
  rindK8 = (/ real(8):: rpaK8, rpaK8 /)
  rarrK8 = (/ real(8):: rpaK8, rpaK8 /)
  if (any(rarrK8 /= rindK8)) call noteFailure('I', rarrK8, rindK8)


  ! Array section in ac-implied-do:
  rarrK8 = rcarK8
  rindK8 = (/ (rpaK8, i=1,2) /)
  rarrK8 = (/ (rpaK8, i=1,2) /)
  if (any(rarrK8 /= rindK8)) call noteFailure('J', rarrK8, rindK8)

  rarrK8 = rcarK8
  rindK8 = (/ real(8):: (rpaK8, i=1,2) /)
  rarrK8 = (/ real(8):: (rpaK8, i=1,2) /)
  if (any(rarrK8 /= rindK8)) call noteFailure('K', rarrK8, rindK8)


  ! Flip the section, and repeat H-K:
  rpaK8 => rarrK8(3:2:-1)

  rarrK8 = rcarK8
  rindK8 = (/ rpaK8, rpaK8 /)
  rarrK8 = (/ rpaK8, rpaK8 /)
  if (any(rarrK8 /= rindK8)) call noteFailure('K', rarrK8, rindK8)

  rarrK8 = rcarK8
  rindK8 = (/ real(8):: rpaK8, rpaK8 /)
  rarrK8 = (/ real(8):: rpaK8, rpaK8 /)
  if (any(rarrK8 /= rindK8)) call noteFailure('L', rarrK8, rindK8)


  rarrK8 = rcarK8
  rindK8 = (/ (rpaK8, i=1,2) /)
  rarrK8 = (/ (rpaK8, i=1,2) /)
  if (any(rarrK8 /= rindK8)) call noteFailure('M', rarrK8, rindK8)

  rarrK8 = rcarK8
  rindK8 = (/ real(8):: (rpaK8, i=1,2) /)
  rarrK8 = (/ real(8):: (rpaK8, i=1,2) /)
  if (any(rarrK8 /= rindK8)) call noteFailure('N', rarrK8, rindK8)


  ! Play with different KINDs:
  rtK8  = real(z'3878182848586808', 8)                     ! 1.13291158195e-36
  rtK4  = real(z'39091929', 4)                             ! 1.30747110234e-04
  rtK16 = real(z'3E7E1E2E4e5E6E0E8E9EAEBECEDEEEFE', 16)    ! 1.12197898609e-07

  rarrK16 = rcarK16
  rindK16 = [ real(16):: rpK16, rtK16, rpK4, rp2K8 ]
  rarrK16 = [ real(16):: rpK16, rtK16, rpK4, rp2K8 ]
  if (any(rarrK16 /= rindK16)) call noteFailure('P', rarrK16, rindK16)

  rarrK8 = rcarK8
  rindK8(1:2) = [ real(8):: (rarrK16(i),i=3,4) ]
  rpaK8 = [ real(8):: (rarrK16(i),i=3,4) ]
  if (any(rpaK8 /= rindK8(1:2))) call noteFailure('Q', rpaK8, rindK8(1:2))

  rarrK16 = rcarK16
  rindK16 = [ real(16):: rpaK8, rpaK8 ]
  rarrK16 = [ real(16):: rpaK8, rpaK8 ]
  if (any(rarrK16 /= rindK16)) call noteFailure('R', rarrK16, rindK16)


  rarrK4 = rcarK4;    rpaK4 => rarrK4(1:2)
  rarrK8 = rcarK8;    rpaK8 => rarrK8(3:3)
  rarrK16 = rcarK16;  rpaK16 => rarrK16(4:4)

  rindK4 = [real(4):: rpaK4, rpaK8, rpaK16]
  rarrK4 = [real(4):: rpaK4, rpaK8, rpaK16]
  if (any(rarrK4 /= rindK4)) call noteFailure('S', rarrK4, rindK4)


  rarrK4 = rcarK4
  rindK4 = [real(4):: rpaK16, rpaK8, rpaK4]
  rarrK4 = [real(4):: rpaK16, rpaK8, rpaK4]
  if (any(rarrK4 /= rindK4)) call noteFailure('T', rarrK4, rindK4)


  rpaK4  => rarrK4;  rarrK4  = rcarK4
  rpaK8  => rarrK8;  rarrK8  = rcarK8
  rpaK16 => rarrK16; rarrK16 = rcarK16

  rarrD3K8 = reshape([real(8):: rarrK4, rarrK8, rarrK16, rtK4, rtK8, rtK16, rpaK16(4:1:-1), rpaK8(4:1:-1), rpaK4(4:1:-1)], [3,3,3])

  rpaD3K8  => rarrD3K8(1:3:2,1:3:2,1:1)                      ! 1,1,1; 3,1,1; 1,3,1; 3,3,1
  rpa2D3K8 => rarrD3K8(3:1:-2,3:1:-2,3:3)                    ! 3,3,3; 1,3,3; 3,1,3; 1,1,3
  if (any(rpaD3K8 /= rpa2D3K8)) then
     print *, "At U, unexpected difference:"
     print *, "  ", rpaD3K8
     print *, "should be"
     print *, "  ", rpa2D3K8
     errorCount = errorCount + 1
  end if

  if (errorCount /= 0) call zzrc(errorCount)

contains

  subroutine noteFailure(place, isValue, shouldBeValue)
    character(*) :: place
    class (*) :: isValue(:), shouldBeValue(:)
    integer :: i
    print *, "At ", place, ", unexpected difference:"
    select type (isValue)
    type is (real(4));      print *, "  ", isValue
    type is (real(8));      print *, "  ", isValue
    type is (real(16));     print *, "  ", isValue
    type is (character(*)); print *, "  ", [(trim(isValue(i)),i=1,size(isValue))]
    class default;          print *, "Unexpected type"
    end select
    print *, "should be"
    select type (shouldBeValue)
    type is (real(4));      print *, "  ", shouldBeValue
    type is (real(8));      print *, "  ", shouldBeValue
    type is (real(16));     print *, "  ", shouldBeValue
    type is (character(*)); print *, "  ", [(trim(shouldBeValue(i)),i=1,size(shouldBeValue))]
    class default;          print *, "Unexpected type"
    end select
    errorCount = errorCount + 1
  end subroutine noteFailure

end program acetint09r
