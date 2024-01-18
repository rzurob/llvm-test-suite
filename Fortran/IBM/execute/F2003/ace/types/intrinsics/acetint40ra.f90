!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint40ra
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-10-14
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Assignment of A.C. to array variable (real)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : intrinsic type, assignment, array constructor
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*

!*  Verify that assignment of a real array constructor to a real array variable
!*  uses the correct values.  All the values within the constructor should be
!*  converted to match the type specifier before the constructor is used to
!*  initialise the variable, whether widening or narrowing, so an assignment of
!*  the form
!*    <r*8var> = (/ real*8:: <r*4value>, <r*8value>, <r*16value> /)
!*  (e.g., r8arr = (/ real*8:: 1.1_4, 3.3_8, 7.7_16/))
!*  should leave only real*8 values (and r*4 values cast as r*8 values)
!*  (so r8arr should be (/1.1_4, 3.3_8, 7.7_8/) --- the first is actually 1.1_4
!*  stretched to 8 bytes, but without the precision given by _8).
!*
!*  Note: This is identical to acetint04, except real variables are
!*  used in place of INTEGER variables.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint40ra

  implicit none
  integer :: i
  real(4) :: rarr4(12), rarr4a(12), rarr4b(12)
  real(8) :: rarr8(12), rarr8a(12), rarr8b(12)
  real(16):: rarr16(12), rarr16a(12), rarr16b(12)
  real    :: rarr (12), rarra (12), rarrb (12)
  double precision :: dparr(12), dparra(12), dparrb(12)

  real(4),  parameter :: VAL4  = 1.1_4, SMALL4 = tiny(rarr4), HUGE4 = huge(rarr4)
  real(8),  parameter :: VAL8  = 1.1_8, SMALL8 = tiny(rarr8), HUGE8 = huge(rarr8)
  real(16), parameter :: VAL16  = 1.1_16, SMALL16 = tiny(rarr16), HUGE16 = huge(rarr16)
  integer, parameter  :: DP = kind(dparr)
  double precision, parameter :: VALDP  = 1.1_DP, SMALLDP = tiny(dparr), HUGEDP = huge(dparr)

  ! Make sure the values in each array are correct
  !   - assigning via an array constructor makes for a circular test.
  rarr4(1) = VAL4; rarr4(2) = SMALL4; rarr4(3) = HUGE4; rarr4(4) = VAL8; rarr4(5) = SMALL8; rarr4(6) = HUGE8;
  rarr4(7) = VAL16; rarr4(8) = SMALL16; rarr4(9) = HUGE16; rarr4(10) = VALDP; rarr4(11) = SMALLDP; rarr4(12) = HUGEDP

  rarr8(1) = VAL4; rarr8(2) = SMALL4; rarr8(3) = HUGE4; rarr8(4) = VAL8; rarr8(5) = SMALL8; rarr8(6) = HUGE8;
  rarr8(7) = VAL16; rarr8(8) = SMALL16; rarr8(9) = HUGE16; rarr8(10) = VALDP; rarr8(11) = SMALLDP; rarr8(12) = HUGEDP

  rarr16(1) = VAL4; rarr16(2) = SMALL4; rarr16(3) = HUGE4; rarr16(4) = VAL8; rarr16(5) = SMALL8; rarr16(6) = HUGE8;
  rarr16(7) = VAL16; rarr16(8) = SMALL16; rarr16(9) = HUGE16; rarr16(10) = VALDP; rarr16(11) = SMALLDP; rarr16(12) = HUGEDP

  rarr(1) = VAL4; rarr(2) = SMALL4; rarr(3) = HUGE4; rarr(4) = VAL8; rarr(5) = SMALL8; rarr(6) = HUGE8;
  rarr(7) = VAL16; rarr(8) = SMALL16; rarr(9) = HUGE16; rarr(10) = VALDP; rarr(11) = SMALLDP; rarr(12) = HUGEDP

  dparr(1) = VAL4; dparr(2) = SMALL4; dparr(3) = HUGE4; dparr(4) = VAL8; dparr(5) = SMALL8; dparr(6) = HUGE8;
  dparr(7) = VAL16; dparr(8) = SMALL16; dparr(9) = HUGE16; dparr(10) = VALDP; dparr(11) = SMALLDP; dparr(12) = HUGEDP


  rarr4a = (/real(4):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr4b = (/real(4):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarr4a == rarr4) .and. all(rarr4b == rarr4))) then
     print *, "rarr4a (",rarr4a,") should be (",rarr4,")"
     print *, "rarr4b (",rarr4b,") should be (",rarr4,")"
     error stop 1_4
  end if

  rarr8a = (/real(4):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr8b = (/real(4):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarr8a == rarr4) .and. all(rarr8b == rarr4))) then
     print *, "rarr8a (",rarr8a,") should be (",rarr4,")"
     print *, "rarr8b (",rarr8b,") should be (",rarr4,")"
     error stop 2_4
  end if

  rarr16a = (/real(4):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr16b = (/real(4):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarr16a == rarr4) .and. all(rarr16b == rarr4))) then
     print *, "rarr16a (",rarr16a,") should be (",rarr4,")"
     print *, "rarr16b (",rarr16b,") should be (",rarr4,")"
     error stop 3_4
  end if

  rarra  = (/real(4):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarrb  = (/real(4):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarra  == rarr4) .and. all(rarrb  == rarr4))) then
     print *, "rarra (",rarra,") should be (", rarr4,")"
     print *, "rarrb (",rarrb,") should be (", rarr4,")"
     error stop 4_4
  end if

  dparra  = (/real(4):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  dparrb  = (/real(4):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(dparra  == rarr4) .and. all(dparrb  == rarr4))) then
     print *, "dparra (",dparra,") should be (", rarr4,")"
     print *, "dparrb (",dparrb,") should be (", rarr4,")"
     error stop 5_4
  end if


  rarr4a = (/real(8):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr4b = (/real(8):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarr4a == rarr4) .and. all(rarr4b == rarr4))) then
     print *, "rarr4a (",rarr4a,") should be (",rarr4,")"
     print *, "rarr4b (",rarr4b,") should be (",rarr4,")"
     error stop 6_4
  end if

  rarr8a = (/real(8):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr8b = (/real(8):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarr8a == rarr8) .and. all(rarr8b == rarr8))) then
     print *, "rarr8a (",rarr8a,") should be (",rarr8,")"
     print *, "rarr8b (",rarr8b,") should be (",rarr8,")"
     error stop 7_4
  end if

  rarr16a = (/real(8):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr16b = (/real(8):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarr16a == rarr8) .and. all(rarr16b == rarr8))) then
     print *, "rarr16a (",rarr16a,") should be (",rarr8,")"
     print *, "rarr16b (",rarr16b,") should be (",rarr8,")"
     error stop 8_4
  end if

  ! If -qrealsize=8, then rarra is actually real*8, i.e., like rarr8 and not rarr4:
  rarra  = (/real(8):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarrb  = (/real(8):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (((kind(rarra)==4) .and. .not.(all(rarra == rarr4) .and. all(rarrb == rarr4))) &
       & .or. ((kind(rarra)==8) .and. .not.(all(rarra == rarr8) .and. all(rarrb == rarr8))) &
       & .or. kind(rarra)>8) then
     if (kind(rarra)==4) then
        print *, "rarra (",rarra,") should be (",rarr4,")"
        print *, "rarrb (",rarrb,") should be (",rarr4,")"
     else if (kind(rarra)==8) then
        print *, "rarra (",rarra,") should be (",rarr8,")"
        print *, "rarrb (",rarrb,") should be (",rarr8,")"
     else
        print *, "Error in test case"
     end if
     error stop 9_4
  end if

  dparra  = (/real(8):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  dparrb  = (/real(8):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(dparra == rarr8) .and. all(dparrb == rarr8))) then
     print *, "dparra (",dparra,") should be (",rarr8,")"
     print *, "dparrb (",dparrb,") should be (",rarr8,")"
     error stop 10_4
  end if


  rarr4a = (/real(16):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr4b = (/real(16):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarr4a == rarr4) .and. all(rarr4b == rarr4))) then
     print *, "rarr4a (",rarr4a,") should be (",rarr4,")"
     print *, "rarr4b (",rarr4b,") should be (",rarr4,")"
     error stop 11_4
  end if

  rarr8a = (/real(16):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr8b = (/real(16):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarr8a == rarr8) .and. all(rarr8b == rarr8))) then
     print *, "rarr8a (",rarr8a,") should be (",rarr8,")"
     print *, "rarr8b (",rarr8b,") should be (",rarr8,")"
     error stop 12_4
  end if

  rarr16a = (/real(16):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr16b = (/real(16):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarr16a == rarr16) .and. all(rarr16b == rarr16))) then
     print *, "rarr16a (",rarr16a,") should be (",rarr16,")"
     print *, "rarr16b (",rarr16b,") should be (",rarr16,")"
     error stop 13_4
  end if

  rarra  = (/real(16):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarrb  = (/real(16):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (((kind(rarra)==4) .and. .not.(all(rarra == rarr4) .and. all(rarrb == rarr4))) &
       & .or. ((kind(rarra)==8) .and. .not.(all(rarra == rarr8) .and. all(rarrb == rarr8))) &
       & .or. kind(rarra)>8) then
     if (kind(rarra)==4) then
        print *, "rarra (",rarra,") should be (",rarr4,")"
        print *, "rarrb (",rarrb,") should be (",rarr4,")"
     else if (kind(rarra)==8) then
        print *, "rarra (",rarra,") should be (",rarr8,")"
        print *, "rarrb (",rarrb,") should be (",rarr8,")"
     else
        print *, "Error in test case"
     end if
     error stop 14_4
  end if

  dparra  = (/real(16):: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  dparrb  = (/real(16):: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (((kind(dparra)==8) .and. .not.(all(dparra == rarr8) .and. all(dparrb == rarr8))) &
       & .or. ((kind(dparra)==16) .and. .not.(all(dparra == rarr16) .and. all(dparrb == rarr16))) &
       & .or. kind(dparra)>16) then
     if (kind(dparra)==8) then
        print *, "dparra (",dparra,") should be (",rarr8,")"
        print *, "dparrb (",dparrb,") should be (",rarr8,")"
     else if (kind(dparra)==16) then
        print *, "dparra (",dparra,") should be (",rarr16,")"
        print *, "dparrb (",dparrb,") should be (",rarr16,")"
     else
        print *, "Error in test case"
     end if
     error stop 15_4
  end if


  rarr4a = (/real   :: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr4b = (/real   :: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarr4a == rarr4) .and. all(rarr4b == rarr4))) then
     print *, "rarr4a (",rarr4a,") should be (",rarr4,")"
     print *, "rarr4b (",rarr4b,") should be (",rarr4,")"
     error stop 16_4
  end if

  rarr8a = (/real   :: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr8b = (/real   :: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (((kind(rarra)==4) .and. .not.(all(rarr8a == rarr4) .and. all(rarr8b == rarr4))) &
       & .or. ((kind(rarra)==8) .and. .not.(all(rarr8a == rarr8) .and. all(rarr8b == rarr8))) &
       & .or. kind(rarra)>8) then
     if (kind(rarra)==4) then
        print *, "rarr8a (",rarr8a,") should be (",rarr4,")"
        print *, "rarr8b (",rarr8b,") should be (",rarr4,")"
     else if (kind(rarra)==8) then
        print *, "rarr8a (",rarr8a,") should be (",rarr8,")"
        print *, "rarr8b (",rarr8b,") should be (",rarr8,")"
     else
        print *, "Error in test case"
     end if
     error stop 17_4
  end if

  rarr16a = (/real   :: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr16b = (/real   :: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (((kind(rarr)==4) .and. .not.(all(rarr16a == rarr4) .and. all(rarr16b == rarr4))) &
       & .or. ((kind(rarr)==8) .and. .not.(all(rarr16a == rarr8) .and. all(rarr16b == rarr8))) &
       & .or. kind(rarra)>8) then
     if (kind(rarr)==4) then
        print *, "rarr16a (",rarr16a,") should be (",rarr4,")"
        print *, "rarr16b (",rarr16b,") should be (",rarr4,")"
     else if (kind(rarr)==8) then
        print *, "rarr16a (",rarr16a,") should be (",rarr8,")"
        print *, "rarr16b (",rarr16b,") should be (",rarr8,")"
     else
        print *, "Error in test case"
     end if
     error stop 18_4
  end if

  rarra = (/real   :: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarrb = (/real   :: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarra == rarr) .and. all(rarrb == rarr))) then
     print *, "rarra (",rarra,") should be (",rarr,")"
     print *, "rarrb (",rarrb,") should be (",rarr,")"
     error stop 19_4
  end if

  ! -qrealsize can change the size of real, so we have to check:
  dparra  = (/real    :: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  dparrb  = (/real    :: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (((kind(rarra)==4) .and. .not.(all(dparra == rarr4) .and. all(dparrb == rarr4))) &
       & .or. ((kind(rarra)==8) .and. .not.(all(dparra == rarr8) .and. all(dparrb == rarr8))) &
       & .or. kind(rarra)>8) then
     if (kind(rarra)==4) then
        print *, "dparra (",dparra,") should be (",rarr4,")"
        print *, "dparrb (",dparrb,") should be (",rarr4,")"
     else if (kind(rarra)==8) then
        print *, "dparra (",dparra,") should be (",rarr8,")"
        print *, "dparrb (",dparrb,") should be (",rarr8,")"
     else
        print *, "Error in test case"
     end if
     error stop 20_4
  end if


  rarr4a = (/double precision:: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr4b = (/double precision:: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarr4a == rarr4) .and. all(rarr4b == rarr4))) then
     print *, "rarr4a (",rarr4a,") should be (",rarr4,")"
     print *, "rarr4b (",rarr4b,") should be (",rarr4,")"
     error stop 21_4
  end if

  rarr8a = (/double precision:: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr8b = (/double precision:: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(rarr8a == rarr8) .and. all(rarr8b == rarr8))) then
     print *, "rarr8a (",rarr8a,") should be (",rarr8,")"
     print *, "rarr8b (",rarr8b,") should be (",rarr8,")"
     error stop 22_4
  end if

  rarr16a = (/double precision:: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarr16b = (/double precision:: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (((kind(dparr)==8) .and. .not.(all(rarr16a == rarr8) .and. all(rarr16b == rarr8))) &
       & .or. ((kind(dparr)==16) .and. .not.(all(rarr16a == rarr16) .and. all(rarr16b == rarr16))) &
       & .or. kind(dparr)>16) then
     if (kind(dparr)==8) then
        print *, "rarr16a (",rarr16a,") should be (",rarr8,")"
        print *, "rarr16b (",rarr16b,") should be (",rarr8,")"
     else if (kind(dparr)==16) then
        print *, "rarr16a (",rarr16a,") should be (",rarr16,")"
        print *, "rarr16b (",rarr16b,") should be (",rarr16,")"
     else
        print *, "Error in test case"
     end if
     error stop 23_4
  end if

  rarra = (/double precision:: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  rarrb = (/double precision:: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (((kind(rarr)==4) .and. .not.(all(rarra == rarr4) .and. all(rarrb == rarr4))) &
       & .or. ((kind(rarr)==8) .and. .not.(all(rarra == rarr8) .and. all(rarrb == rarr8))) &
       & .or. kind(rarra)>8) then
     if (kind(rarr)==4) then
        print *, "rarra (",rarra,") should be (",rarr4,")"
        print *, "rarrb (",rarrb,") should be (",rarr4,")"
     else if (kind(rarr)==8) then
        print *, "rarra (",rarra,") should be (",rarr8,")"
        print *, "rarrb (",rarrb,") should be (",rarr8,")"
     else
        print *, "Error in test case"
     end if
     error stop 24_4
  end if

  ! -qrealsize can change the size of dparr, so we have to check:
  dparra  = (/double precision:: VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP/)
  dparrb  = (/double precision:: (VAL4, SMALL4, HUGE4, VAL8, SMALL8, HUGE8, VAL16, SMALL16, HUGE16, VALDP, SMALLDP, HUGEDP, i=1,1)/)
  if (.not.(all(dparra == dparr) .and. all(dparrb == dparr))) then
     print *, "dparra (",dparra,") should be (",dparr,")"
     print *, "dparrb (",dparrb,") should be (",dparr,")"
     error stop 25_4
  end if


end program acetint40ra
