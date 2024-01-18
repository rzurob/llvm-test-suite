!*  ===================================================================
!*
!*  DATE                       : 01/19/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*  SECONDARY FUNCTIONS TESTED : octal-constant	is O ’ digit [ digit ] ... ’
!*				or O " digit [ digit ] ... "
!*
!*				digit shall have one of the values 0 through 7.
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


program bozC409digit001d

    integer :: i1, i2, i3, i4, i5, i6, i7

    data i1 /O'80"/
    data i2 /O"693010'/
    data i3 /O 4459421/
    data i4 /O'11537121693/
    data i5 /O 2397610721574'/
    data i6 /O"2307626390676243/
    data i7 /O 1143712164337129643"/

  end
