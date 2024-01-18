!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozC408digit004d.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 01/19/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*                             :
!*  SECONDARY FUNCTIONS TESTED : octal-constant	is B ’ digit [ digit ] ... ’
!*				or B " digit [ digit ] ... "
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


program bozC408digit004d

    integer :: i3, i4, i5, i6, i7, i8, i9, i10

    integer :: i1 = B"101"
    integer :: i2 = -B'101'

    print *, i1
    print *, i2

    data i3 /+b'11'/
    data i4 /+B"11"/

    data i5 /-B'10'/
    data i6 /-b"10"/

    data i7 /+B""/
    data i8 /+b''/

    data i9 /-b""/
    data i10 /-B''/

  end
