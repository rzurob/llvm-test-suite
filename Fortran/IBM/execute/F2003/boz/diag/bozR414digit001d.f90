!*  ===================================================================
!*
!*  DATE                       : 01/19/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*  SECONDARY FUNCTIONS TESTED : hex-constant	is Z ’ hex-digit [ hex-digit ] ... ’
!*				or Z " hex-digit [ hex-digit ] ... "
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


program bozR414digit001d

    integer :: i1, i2, i3, i4, i5, i6, i7

    data i1 /Z'AF"/
    data i2 /Z"6608'/
    data i3 /Z 12F8A9/
    data i4 /Z'9AF94731/
    data i5 /Z 4FE23A37C'/
    data i6 /Z"98F96637CA3/
    data i7 /Z 98F947B6F95A3"/

  end
