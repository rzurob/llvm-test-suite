!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozC409digit004d.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 01/19/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*                             :
!*  SECONDARY FUNCTIONS TESTED : octal-constant	is O ’ digit [ digit ] ... ’
!*				or O " digit [ digit ] ... "
!*
!*				digit shall have one of the values 0 through 7.
!*
!*  DRIVER STANZA              : xlf2003
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


program bozC409digit004d

    integer :: i3, i4, i5, i6, i7, i8, i9, i10

    integer :: i1 = O"11"
    integer :: i2 = -O'11'

    print *, i1
    print *, i2

    print *, +O'22'
    data i4 /+o"22"/

    data i5 /-O'22'/
    data i6 /-o"22"/

    data i7 /+o""/
    data i8 /+O''/

    data i9 /-O""/
    data i10 /-o''/

  end
