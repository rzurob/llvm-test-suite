!*  ===================================================================
!*
!*  DATE                       : 01/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*  SECONDARY FUNCTIONS TESTED : binary-constant is B'digit [digit]...'
!*						or B"digit [digit]..."
!*				where digit shall have one of the
!*				values 0 or 1
!*
!*  DESCRIPTION                :
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program bozC408digit001d

   integer :: i1, i2, i3, i4, i5, i6, i7

   data i1 /B'100100"/
   data i2 /B"100101'/
   data i3 /B 100110/
   data i4 /B'100111/
   data i5 /B 101000'/
   data i6 /B"101001/
   data i7 /B 101010"/

 end
