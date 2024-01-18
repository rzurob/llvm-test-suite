!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint40za
!*
!*  DATE                       : 2006-10-18
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Assignment of A.C. to array variable (complex)
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
!*  Verify that assignment of a complex array constructor to a complex
!*  array variable uses the correct values.  All the values within the
!*  constructor should be converted to match the type specifier before the
!*  constructor is used to initialise the variable, whether widening or
!*  narrowing, so an assignment of the form
!*    <z*8var> = (/ complex*8:: <z*4value>, <z*8value>, <z*16value> /)
!*  (e.g., z8arr = (/ complex*8:: (1.1_4, 5.5_4), (3.3_8, -4.1_8), (7.7_16, 9.9_16)/))
!*  should leave only complex*8 values (and z*4 values cast as z*8 values)
!*  (so z8arr should be (/(1.1_4, 5.5_4), (3.3_8, -4.1_8), (7.7_8, 9.9_8)/)
!*  --- the first is actually (1.1_4, 5.5_4) stretched to complex*8, but without
!*  the precision given by *8).
!*
!*  Note: This is identical to acetint04, except complex variables are
!*  used in place of INTEGER variables.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint40za

  implicit none
  integer :: i
  complex(4) :: zarr4(12), zarr4a(12), zarr4b(12)
  complex(8) :: zarr8(12), zarr8a(12), zarr8b(12)
  complex(16):: zarr16(12), zarr16a(12), zarr16b(12)
  complex    :: zarr (12), zarra (12), zarrb (12)
  double complex :: dzarr(12), dzarra(12), dzarrb(12)

  complex(4),  parameter :: VAL1_4  = (1.1_4,1.1_4), VAL2_4 = (tiny(0.0_4),huge(0.0_4)), VAL3_4 = (huge(0.0_4),tiny(0.0_4))
  complex(8),  parameter :: VAL1_8  = (1.1_8,1.1_8), VAL2_8 = (tiny(0.0_8),huge(0.0_8)), VAL3_8 = (huge(0.0_8),tiny(0.0_8))
  complex(16), parameter :: VAL1_16  = (1.1_16,1.1_16), VAL2_16 = (tiny(0.0_16),huge(0.0_16)), VAL3_16 = (huge(0.0_16),tiny(0.0_16))
  integer, parameter  :: DP = kind(dzarr)
  double complex, parameter :: VAL1_DP  = (1.1_DP,1.1_DP), VAL2_DP = (tiny(0.0_DP),huge(0.0_DP)), VAL3_DP = (huge(0.0_DP),tiny(0.0_DP))

  ! Make sure the values in each array are correct
  !   - assigning via an array constructor makes for a circular test.
  zarr4(1) = VAL1_4; zarr4(2) = VAL2_4; zarr4(3) = VAL3_4;
  zarr4(4) = VAL1_8; zarr4(5) = VAL2_8; zarr4(6) = VAL3_8;
  zarr4(7) = VAL1_16; zarr4(8) = VAL2_16; zarr4(9) = VAL3_16;
  zarr4(10) = VAL1_DP; zarr4(11) = VAL2_DP; zarr4(12) = VAL3_DP

  zarr8(1) = VAL1_4; zarr8(2) = VAL2_4; zarr8(3) = VAL3_4;
  zarr8(4) = VAL1_8; zarr8(5) = VAL2_8; zarr8(6) = VAL3_8;
  zarr8(7) = VAL1_16; zarr8(8) = VAL2_16; zarr8(9) = VAL3_16;
  zarr8(10) = VAL1_DP; zarr8(11) = VAL2_DP; zarr8(12) = VAL3_DP

  zarr16(1) = VAL1_4; zarr16(2) = VAL2_4; zarr16(3) = VAL3_4;
  zarr16(4) = VAL1_8; zarr16(5) = VAL2_8; zarr16(6) = VAL3_8;
  zarr16(7) = VAL1_16; zarr16(8) = VAL2_16; zarr16(9) = VAL3_16;
  zarr16(10) = VAL1_DP; zarr16(11) = VAL2_DP; zarr16(12) = VAL3_DP

  zarr(1) = VAL1_4; zarr(2) = VAL2_4; zarr(3) = VAL3_4;
  zarr(4) = VAL1_8; zarr(5) = VAL2_8; zarr(6) = VAL3_8;
  zarr(7) = VAL1_16; zarr(8) = VAL2_16; zarr(9) = VAL3_16;
  zarr(10) = VAL1_DP; zarr(11) = VAL2_DP; zarr(12) = VAL3_DP

  dzarr(1) = VAL1_4; dzarr(2) = VAL2_4; dzarr(3) = VAL3_4;
  dzarr(4) = VAL1_8; dzarr(5) = VAL2_8; dzarr(6) = VAL3_8;
  dzarr(7) = VAL1_16; dzarr(8) = VAL2_16; dzarr(9) = VAL3_16;
  dzarr(10) = VAL1_DP; dzarr(11) = VAL2_DP; dzarr(12) = VAL3_DP


  zarr4a = (/complex(4):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr4b = (/complex(4):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarr4a == zarr4) .and. all(zarr4b == zarr4))) then
     print *, "zarr4a (",zarr4a,") should be (",zarr4,")"
     print *, "zarr4b (",zarr4b,") should be (",zarr4,")"
     error stop 1_4
  end if

  zarr8a = (/complex(4):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr8b = (/complex(4):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarr8a == zarr4) .and. all(zarr8b == zarr4))) then
     print *, "zarr8a (",zarr8a,") should be (",zarr4,")"
     print *, "zarr8b (",zarr8b,") should be (",zarr4,")"
     error stop 2_4
  end if

  zarr16a = (/complex(4):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr16b = (/complex(4):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarr16a == zarr4) .and. all(zarr16b == zarr4))) then
     print *, "zarr16a (",zarr16a,") should be (",zarr4,")"
     print *, "zarr16b (",zarr16b,") should be (",zarr4,")"
     error stop 3_4
  end if

  zarra  = (/complex(4):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarrb  = (/complex(4):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarra  == zarr4) .and. all(zarrb  == zarr4))) then
     print *, "zarra (",zarra,") should be (", zarr4,")"
     print *, "zarrb (",zarrb,") should be (", zarr4,")"
     error stop 4_4
  end if

  dzarra  = (/complex(4):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  dzarrb  = (/complex(4):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(dzarra  == zarr4) .and. all(dzarrb  == zarr4))) then
     print *, "dzarra (",dzarra,") should be (", zarr4,")"
     print *, "dzarrb (",dzarrb,") should be (", zarr4,")"
     error stop 5_4
  end if


  zarr4a = (/complex(8):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr4b = (/complex(8):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarr4a == zarr4) .and. all(zarr4b == zarr4))) then
     print *, "zarr4a (",zarr4a,") should be (",zarr4,")"
     print *, "zarr4b (",zarr4b,") should be (",zarr4,")"
     error stop 6_4
  end if

  zarr8a = (/complex(8):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr8b = (/complex(8):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarr8a == zarr8) .and. all(zarr8b == zarr8))) then
     print *, "zarr8a (",zarr8a,") should be (",zarr8,")"
     print *, "zarr8b (",zarr8b,") should be (",zarr8,")"
     error stop 7_4
  end if

  zarr16a = (/complex(8):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr16b = (/complex(8):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarr16a == zarr8) .and. all(zarr16b == zarr8))) then
     print *, "zarr16a (",zarr16a,") should be (",zarr8,")"
     print *, "zarr16b (",zarr16b,") should be (",zarr8,")"
     error stop 8_4
  end if

  ! If -qrealsize=8, then zarra is actually complex*8, i.e., like zarr8 and not zarr4:
  zarra  = (/complex(8):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarrb  = (/complex(8):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (((kind(zarra)==4) .and. .not.(all(zarra == zarr4) .and. all(zarrb == zarr4))) &
       & .or. ((kind(zarra)==8) .and. .not.(all(zarra == zarr8) .and. all(zarrb == zarr8))) &
       & .or. kind(zarra)>8) then
     if (kind(zarra)==4) then
        print *, "zarra (",zarra,") should be (",zarr4,")"
        print *, "zarrb (",zarrb,") should be (",zarr4,")"
     else if (kind(zarra)==8) then
        print *, "zarra (",zarra,") should be (",zarr8,")"
        print *, "zarrb (",zarrb,") should be (",zarr8,")"
     else
        print *, "Error in test case"
     end if
     error stop 9_4
  end if

  dzarra  = (/complex(8):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  dzarrb  = (/complex(8):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(dzarra == zarr8) .and. all(dzarrb == zarr8))) then
     print *, "dzarra (",dzarra,") should be (",zarr8,")"
     print *, "dzarrb (",dzarrb,") should be (",zarr8,")"
     error stop 10_4
  end if


  zarr4a = (/complex(16):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr4b = (/complex(16):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarr4a == zarr4) .and. all(zarr4b == zarr4))) then
     print *, "zarr4a (",zarr4a,") should be (",zarr4,")"
     print *, "zarr4b (",zarr4b,") should be (",zarr4,")"
     error stop 11_4
  end if

  zarr8a = (/complex(16):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr8b = (/complex(16):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarr8a == zarr8) .and. all(zarr8b == zarr8))) then
     print *, "zarr8a (",zarr8a,") should be (",zarr8,")"
     print *, "zarr8b (",zarr8b,") should be (",zarr8,")"
     error stop 12_4
  end if

  zarr16a = (/complex(16):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr16b = (/complex(16):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarr16a == zarr16) .and. all(zarr16b == zarr16))) then
     print *, "zarr16a (",zarr16a,") should be (",zarr16,")"
     print *, "zarr16b (",zarr16b,") should be (",zarr16,")"
     error stop 13_4
  end if

  zarra  = (/complex(16):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarrb  = (/complex(16):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (((kind(zarra)==4) .and. .not.(all(zarra == zarr4) .and. all(zarrb == zarr4))) &
       & .or. ((kind(zarra)==8) .and. .not.(all(zarra == zarr8) .and. all(zarrb == zarr8))) &
       & .or. kind(zarra)>8) then
     if (kind(zarra)==4) then
        print *, "zarra (",zarra,") should be (",zarr4,")"
        print *, "zarrb (",zarrb,") should be (",zarr4,")"
     else if (kind(zarra)==8) then
        print *, "zarra (",zarra,") should be (",zarr8,")"
        print *, "zarrb (",zarrb,") should be (",zarr8,")"
     else
        print *, "Error in test case"
     end if
     error stop 14_4
  end if

  dzarra  = (/complex(16):: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  dzarrb  = (/complex(16):: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (((kind(dzarra)==8) .and. .not.(all(dzarra == zarr8) .and. all(dzarrb == zarr8))) &
       & .or. ((kind(dzarra)==16) .and. .not.(all(dzarra == zarr16) .and. all(dzarrb == zarr16))) &
       & .or. kind(dzarra)>16) then
     if (kind(dzarra)==8) then
        print *, "dzarra (",dzarra,") should be (",zarr8,")"
        print *, "dzarrb (",dzarrb,") should be (",zarr8,")"
     else if (kind(dzarra)==16) then
        print *, "dzarra (",dzarra,") should be (",zarr16,")"
        print *, "dzarrb (",dzarrb,") should be (",zarr16,")"
     else
        print *, "Error in test case"
     end if
     error stop 15_4
  end if


  zarr4a = (/complex   :: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr4b = (/complex   :: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarr4a == zarr4) .and. all(zarr4b == zarr4))) then
     print *, "zarr4a (",zarr4a,") should be (",zarr4,")"
     print *, "zarr4b (",zarr4b,") should be (",zarr4,")"
     error stop 16_4
  end if

  zarr8a = (/complex   :: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr8b = (/complex   :: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (((kind(zarra)==4) .and. .not.(all(zarr8a == zarr4) .and. all(zarr8b == zarr4))) &
       & .or. ((kind(zarra)==8) .and. .not.(all(zarr8a == zarr8) .and. all(zarr8b == zarr8))) &
       & .or. kind(zarra)>8) then
     if (kind(zarra)==4) then
        print *, "zarr8a (",zarr8a,") should be (",zarr4,")"
        print *, "zarr8b (",zarr8b,") should be (",zarr4,")"
     else if (kind(zarra)==8) then
        print *, "zarr8a (",zarr8a,") should be (",zarr8,")"
        print *, "zarr8b (",zarr8b,") should be (",zarr8,")"
     else
        print *, "Error in test case"
     end if
     error stop 17_4
  end if

  zarr16a = (/complex   :: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr16b = (/complex   :: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (((kind(zarr)==4) .and. .not.(all(zarr16a == zarr4) .and. all(zarr16b == zarr4))) &
       & .or. ((kind(zarr)==8) .and. .not.(all(zarr16a == zarr8) .and. all(zarr16b == zarr8))) &
       & .or. kind(zarra)>8) then
     if (kind(zarr)==4) then
        print *, "zarr16a (",zarr16a,") should be (",zarr4,")"
        print *, "zarr16b (",zarr16b,") should be (",zarr4,")"
     else if (kind(zarr)==8) then
        print *, "zarr16a (",zarr16a,") should be (",zarr8,")"
        print *, "zarr16b (",zarr16b,") should be (",zarr8,")"
     else
        print *, "Error in test case"
     end if
     error stop 18_4
  end if

  zarra = (/complex   :: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarrb = (/complex   :: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarra == zarr) .and. all(zarrb == zarr))) then
     print *, "zarra (",zarra,") should be (",zarr,")"
     print *, "zarrb (",zarrb,") should be (",zarr,")"
     error stop 19_4
  end if

  ! -qrealsize can change the size of complex, so we have to check:
  dzarra  = (/complex    :: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  dzarrb  = (/complex    :: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (((kind(zarra)==4) .and. .not.(all(dzarra == zarr4) .and. all(dzarrb == zarr4))) &
       & .or. ((kind(zarra)==8) .and. .not.(all(dzarra == zarr8) .and. all(dzarrb == zarr8))) &
       & .or. kind(zarra)>8) then
     if (kind(zarra)==4) then
        print *, "dzarra (",dzarra,") should be (",zarr4,")"
        print *, "dzarrb (",dzarrb,") should be (",zarr4,")"
     else if (kind(zarra)==8) then
        print *, "dzarra (",dzarra,") should be (",zarr8,")"
        print *, "dzarrb (",dzarrb,") should be (",zarr8,")"
     else
        print *, "Error in test case"
     end if
     error stop 20_4
  end if


  zarr4a = (/double complex:: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr4b = (/double complex:: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarr4a == zarr4) .and. all(zarr4b == zarr4))) then
     print *, "zarr4a (",zarr4a,") should be (",zarr4,")"
     print *, "zarr4b (",zarr4b,") should be (",zarr4,")"
     error stop 21_4
  end if

  zarr8a = (/double complex:: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr8b = (/double complex:: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(zarr8a == zarr8) .and. all(zarr8b == zarr8))) then
     print *, "zarr8a (",zarr8a,") should be (",zarr8,")"
     print *, "zarr8b (",zarr8b,") should be (",zarr8,")"
     error stop 22_4
  end if

  zarr16a = (/double complex:: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarr16b = (/double complex:: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (((kind(dzarr)==8) .and. .not.(all(zarr16a == zarr8) .and. all(zarr16b == zarr8))) &
       & .or. ((kind(dzarr)==16) .and. .not.(all(zarr16a == zarr16) .and. all(zarr16b == zarr16))) &
       & .or. kind(dzarr)>16) then
     if (kind(dzarr)==8) then
        print *, "zarr16a (",zarr16a,") should be (",zarr8,")"
        print *, "zarr16b (",zarr16b,") should be (",zarr8,")"
     else if (kind(dzarr)==16) then
        print *, "zarr16a (",zarr16a,") should be (",zarr16,")"
        print *, "zarr16b (",zarr16b,") should be (",zarr16,")"
     else
        print *, "Error in test case"
     end if
     error stop 23_4
  end if

  zarra = (/double complex:: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  zarrb = (/double complex:: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (((kind(zarr)==4) .and. .not.(all(zarra == zarr4) .and. all(zarrb == zarr4))) &
       & .or. ((kind(zarr)==8) .and. .not.(all(zarra == zarr8) .and. all(zarrb == zarr8))) &
       & .or. kind(zarra)>8) then
     if (kind(zarr)==4) then
        print *, "zarra (",zarra,") should be (",zarr4,")"
        print *, "zarrb (",zarrb,") should be (",zarr4,")"
     else if (kind(zarr)==8) then
        print *, "zarra (",zarra,") should be (",zarr8,")"
        print *, "zarrb (",zarrb,") should be (",zarr8,")"
     else
        print *, "Error in test case"
     end if
     error stop 24_4
  end if

  ! -qrealsize can change the size of dzarr, so we have to check:
  dzarra  = (/double complex:: VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP/)
  dzarrb  = (/double complex:: (VAL1_4, VAL2_4, VAL3_4, VAL1_8, VAL2_8, VAL3_8, VAL1_16, VAL2_16, VAL3_16, VAL1_DP, VAL2_DP, VAL3_DP, i=1,1)/)
  if (.not.(all(dzarra == dzarr) .and. all(dzarrb == dzarr))) then
     print *, "dzarra (",dzarra,") should be (",dzarr,")"
     print *, "dzarrb (",dzarrb,") should be (",dzarr,")"
     error stop 25_4
  end if


end program acetint40za
