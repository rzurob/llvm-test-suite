!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-06-14 (YYYY-MM-DD)
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Assignment of A.C. to array variable
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type, assignment, array constructor
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that assignment of an integer array constructor to an integer
!*  array variable uses the correct values.  All the values within the
!*  constructor should be converted to match the type specifier before the
!*  constructor is used to initialise the variable, whether widening or
!*  narrowing, so an assignment of the form
!*    <i*2var> = (/ integer*2:: <i*1value>, <i*2value>, <i*4value> /)
!*  (e.g., i2arr = (/ integer*2:: 31, 31000, 3100000/))
!*  should leave only integer*2 values
!*  (so i2arr should be (/31, 31000, 19808/) (19808=3100000&z'FFFF')).
!*
!*  Note: This is identical to acetint04, except that INTEGER*<kind> is
!*  used in place of INTEGER(<kind>).
!*
!*  Use same mask scheme as acetint03:
!*  Use a mask with the top bit in each byte off, to avoid confusion with -1
!*  - this should work the same for all underlying integer representations
!*  but excess-notation, i.e., for two's-complement, one's-complement, and
!*  signed-magnitude; using -1 could invoke sign-extension, and confuse
!*  things. Using a mask of the form 7f7f7f... tests all bits without
!*  tripping over sign extension in any system.  In all three systems, these
!*  patterns correspond to the numbers:
!*
!*    8-bit:                 127 (7f)
!*   16-bit:               32639 (7f7f)
!*   32-bit:          2139062143 (7f7f7f7f)
!*   64-bit: 9187201950435737471 (7f7f7f7f7f7f7f7f)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint05

  implicit none
  integer    :: iarr (4), iarra (4)
  integer(1) :: iarr1(4), iarr1a(4)
  integer(2) :: iarr2(4), iarr2a(4)
  integer(4) :: iarr4(4), iarr4a(4)
  integer(8) :: iarr8(4), iarr8a(4)

  integer(1), parameter :: MASK1 = int(z'7F',1)
  integer(2), parameter :: MASK2 = int(z'7F7F',2)
  integer(4), parameter :: MASK4 = int(z'7F7F7F7F',4)
  integer(8), parameter :: MASK8 = int(z'7F7F7F7F7F7F7F7F',8)

  ! Make sure the values in each array are correct
  !   - assigning via an array constructor makes for a circular test.
  iarr1(1) = MASK1; iarr1(2) = MASK2; iarr1(3) = MASK4; iarr1(4) = MASK8
  iarr2(1) = MASK1; iarr2(2) = MASK2; iarr2(3) = MASK4; iarr2(4) = MASK8
  iarr4(1) = MASK1; iarr4(2) = MASK2; iarr4(3) = MASK4; iarr4(4) = MASK8
  iarr8(1) = MASK1; iarr8(2) = MASK2; iarr8(3) = MASK4; iarr8(4) = MASK8
  iarr(1)  = MASK1; iarr(2)  = MASK2; iarr(3)  = MASK4; iarr(4)  = MASK8

  iarr2a = (/integer*1:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr2a == iarr1)) then
     print *, "iarr2a (",iarr2a,") should be (",iarr1,")"
     call zzrc( 1_4)
  end if

  iarr4a = (/integer*1:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr4a == iarr1)) then
     print *, "iarr4a (",iarr4a,") should be (",iarr1,")"
     call zzrc( 2_4)
  end if

  iarr8a = (/integer*1:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr8a == iarr1)) then
     print *, "iarr8a (",iarr8a,") should be (",iarr1,")"
     call zzrc( 3_4)
  end if

  iarra  = (/integer*1:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarra  == iarr1)) then
     print *, "iarra (",iarra,") should be (", iarr1,")"
     call zzrc( 4_4)
  end if

  iarr1a = (/integer*2:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr1a == iarr1)) then
     print *, "iarr1a (",iarr1a,") should be (",iarr1,")"
     call zzrc( 5_4)
  end if

  iarr4a = (/integer*2:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr4a == iarr2)) then
     print *, "iarr4a (",iarr4a,") should be (",iarr2,")"
     call zzrc( 6_4)
  end if

  iarr8a = (/integer*2:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr8a == iarr2)) then
     print *, "iarr8a (",iarr8a,") should be (",iarr2,")"
     call zzrc( 7_4)
  end if

  iarra  = (/integer*2:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarra  == iarr2)) then
     print *, "iarra (",iarra,") should be (", iarr2,")"
     call zzrc( 8_4)
  end if

  iarr1a = (/integer*4:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr1a == iarr1)) then
     print *, "iarr1a (",iarr1a,") should be (",iarr1,")"
     call zzrc( 9_4)
  end if

  iarr2a = (/integer*4:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr2a == iarr2)) then
     print *, "iarr2a (",iarr2a,") should be (",iarr2,")"
     error stop 10_4
  end if

  iarr8a = (/integer*4:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr8a == iarr4)) then
     print *, "iarr8a (",iarr8a,") should be (",iarr4,")"
     error stop 11_4
  end if

  ! If -qintsize=2, then iarra is actually integer*2, i.e., like iarr2 and not iarr4:
  iarra  = (/integer*4:: MASK1, MASK2, MASK4, MASK8/)
  if (((kind(iarra)==2) .and. .not.all(iarra == iarr2)) &
       & .or. ((kind(iarra)>=4) .and. .not.all(iarra == iarr4))) then
     if (kind(iarra)==2) print *, "iarra (",iarra,") should be (",iarr2,")"
     if (kind(iarra)>=4) print *, "iarra (",iarra,") should be (",iarr4,")"
     error stop 12_4
  end if

  iarr1a = (/integer*8:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr1a == iarr1)) then
     print *, "iarr1a (",iarr1a,") should be (",iarr1,")"
     error stop 13_4
  end if

  iarr2a = (/integer*8:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr2a == iarr2)) then
     print *, "iarr2a (",iarr2a,") should be (",iarr2,")"
     error stop 14_4
  end if

  iarr4a = (/integer*8:: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr4a == iarr4)) then
     print *, "iarr4a (",iarr4a,") should be (",iarr4,")"
     error stop 15_4
  end if

  iarra  = (/integer*8:: MASK1, MASK2, MASK4, MASK8/)
  if (((kind(iarra)==2) .and. .not.all(iarra == iarr2)) &
       & .or. ((kind(iarra)==4) .and. .not.all(iarra == iarr4)) &
       & .or. ((kind(iarra)>=8) .and. .not.all(iarra == iarr8))) then
     if (kind(iarra)==2) print *, "iarra (",iarra,") should be (",iarr2,")"
     if (kind(iarra)==4) print *, "iarra (",iarra,") should be (",iarr4,")"
     if (kind(iarra)>=8) print *, "iarra (",iarra,") should be (",iarr8,")"
     error stop 16_4
  end if

  iarr1a = (/integer  :: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr1a == iarr1)) then
     print *, "iarr1a (",iarr1a,") should be (",iarr1,")"
     error stop 17_4
  end if

  iarr2a = (/integer  :: MASK1, MASK2, MASK4, MASK8/)
  if (.not.all(iarr2a == iarr2)) then
     print *, "iarr2a (",iarr2a,") should be (",iarr2,")"
     error stop 18_4
  end if

  iarr4a = (/integer  :: MASK1, MASK2, MASK4, MASK8/)
  if (((kind(0)==2) .and. .not.all(iarr4a == iarr2)) &
       & .or. ((kind(0)>=4) .and. .not.all(iarr4a == iarr4))) then
     if (kind(0)==2) print *, "iarr4a (",iarr4a,") should be (",iarr2,")"
     if (kind(0)>=4) print *, "iarr4a (",iarr4a,") should be (",iarr4,")"
     error stop 19_4
  end if

  iarr8a = (/integer  :: MASK1, MASK2, MASK4, MASK8/)
  if (((kind(0)==2) .and. .not.all(iarr8a == iarr2)) &
       & .or. ((kind(0)==4) .and. .not.all(iarr8a == iarr4)) &
       & .or. ((kind(0)>=8) .and. .not.all(iarr8a == iarr8))) then
     if (kind(0)==2) print *, "iarr8a (",iarr8a,") should be (",iarr2,")"
     if (kind(0)==4) print *, "iarr8a (",iarr8a,") should be (",iarr4,")"
     if (kind(0)>=8) print *, "iarr8a (",iarr8a,") should be (",iarr8,")"
     error stop 20_4
  end if

end program acetint05
