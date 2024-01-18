!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozC409digit003d.f
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


program bozC409digit003d

    integer :: i1, i2
    data i1 /O""/
    data i1 /O''/

    print *, int(O'')

    print *, real(O"")

    print *, dble(o"")

    print *, cmplx(o'',O"")

  end
