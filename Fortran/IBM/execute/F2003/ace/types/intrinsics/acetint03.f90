!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint03
!*
!*  DATE                       : 2006-06-13 (YYYY-MM-DD)
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic (integer) values correctly assigned
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that an integer array constructor, created from different
!*  sorts of elements (named constants, integer literals, and results of
!*  intrinsic functions) uses the correct values.  Type and kind are
!*  already checked in tests acetint01x and acetint02x.
!*
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

program acetint03

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

  ! Check named constants:
  call testValue((/integer*1:: MASK1, MASK2, MASK4, MASK8/),100_4)
  call testValue((/integer*2:: MASK1, MASK2, MASK4, MASK8/),200_4)
  call testValue((/integer*4:: MASK1, MASK2, MASK4, MASK8/),300_4)
  call testValue((/integer*8:: MASK1, MASK2, MASK4, MASK8/),400_4)
  call testValue((/integer  :: MASK1, MASK2, MASK4, MASK8/),500_4)

  ! Check literals (see note above about masks):
  call testValue((/integer*1:: 127_1, 32639_2, 2139062143_4, 9187201950435737471_8/),600_4)
  call testValue((/integer*2:: 127_1, 32639_2, 2139062143_4, 9187201950435737471_8/),700_4)
  call testValue((/integer*4:: 127_1, 32639_2, 2139062143_4, 9187201950435737471_8/),800_4)
  call testValue((/integer*8:: 127_1, 32639_2, 2139062143_4, 9187201950435737471_8/),900_4)
  call testValue((/integer  :: 127_1, 32639_2, 2139062143_4, 9187201950435737471_8/),1000_4)

  ! Check intrinsics:
  call testValue((/integer*1:: int(z'7F',1), int(z'7F7F',2), int(z'7F7F7F7F',4),&
                                & int(z'7F7F7F7F7F7F7F7F',8)/),1100_4)
  call testValue((/integer*2:: int(z'7F',1), int(z'7F7F',2), int(z'7F7F7F7F',4),&
                                & int(z'7F7F7F7F7F7F7F7F',8)/),1200_4)
  call testValue((/integer*4:: int(z'7F',1), int(z'7F7F',2), int(z'7F7F7F7F',4),&
                                & int(z'7F7F7F7F7F7F7F7F',8)/),1300_4)
  call testValue((/integer*8:: int(z'7F',1), int(z'7F7F',2), int(z'7F7F7F7F',4),&
                                & int(z'7F7F7F7F7F7F7F7F',8)/),1400_4)
  call testValue((/integer  :: int(z'7F',1), int(z'7F7F',2), int(z'7F7F7F7F',4),&
                                & int(z'7F7F7F7F7F7F7F7F',8)/),1500_4)

  ! If we get here, we know that the constructors which appear as actual
  ! arguments to our test function are correctly constructed, at least where
  ! the elements are named constants, literals, or intrinsic functions.
  ! Now test assignment to a variable of the same kind.

  iarr1 = (/integer*1:: MASK1, MASK2, MASK4, MASK8/)
  iarr2 = (/integer*2:: MASK1, MASK2, MASK4, MASK8/)
  iarr4 = (/integer*4:: MASK1, MASK2, MASK4, MASK8/)
  iarr8 = (/integer*8:: MASK1, MASK2, MASK4, MASK8/)
  iarr  = (/integer  :: MASK1, MASK2, MASK4, MASK8/)

  call testValue(iarr1,1600_4)
  call testValue(iarr2,1700_4)
  call testValue(iarr4,1800_4)
  call testValue(iarr8,1900_4)
  call testValue(iarr, 2000_4)

contains

  !* The array argument "arg" should have been created from the same array for
  !* each test: MASK1, MASK2, MASK4, MASK8.  Depending on the type of array
  !* the system created, we should see the values change from all MASK1 to
  !* each a different value, being MASK1, MASK2, MASK4, MASK8.

  subroutine testValue(arg, ecodeBase)
    class (*), intent(in) :: arg(:)
    integer(4), intent(in) :: ecodeBase
    integer(4) :: ecode
    character(120) :: message

    ecode = 0
    select type (obj => arg)

    type is (integer(1))
       write(message,"('Expecting',4(' ',i0),', got',4(' ',i0))") MASK1,MASK1,MASK1,MASK1,obj
       if (obj(1) /= MASK1) ecode = 1_4
       if (obj(2) /= MASK1) ecode = 2_4
       if (obj(3) /= MASK1) ecode = 3_4
       if (obj(4) /= MASK1) ecode = 4_4

    type is (integer(2))
       write(message,"('Expecting',4(' ',i0),', got',4(' ',i0))") MASK1,MASK2,MASK2,MASK2,obj
       if (obj(1) /= MASK1) ecode = 11_4
       if (obj(2) /= MASK2) ecode = 12_4
       if (obj(3) /= MASK2) ecode = 13_4
       if (obj(4) /= MASK2) ecode = 14_4

    type is (integer(4))
       write(message,"('Expecting',4(' ',i0),', got',4(' ',i0))") MASK1,MASK2,MASK4,MASK4,obj
       if (obj(1) /= MASK1) ecode = 21_4
       if (obj(2) /= MASK2) ecode = 22_4
       if (obj(3) /= MASK4) ecode = 23_4
       if (obj(4) /= MASK4) ecode = 24_4

    type is (integer(8))
       write(message,"('Expecting',4(' ',i0),', got',4(' ',i0))") MASK1,MASK2,MASK4,MASK8,obj
       if (obj(1) /= MASK1) ecode = 31_4
       if (obj(2) /= MASK2) ecode = 32_4
       if (obj(3) /= MASK4) ecode = 33_4
       if (obj(4) /= MASK8) ecode = 34_4

    class default
       message = "Unidentified type"
       ecode = 40_4
    end select

    if (ecode == 0) return

    print *, message
    call zzrc (ecode + ecodeBase)

  end subroutine testValue

end program acetint03
