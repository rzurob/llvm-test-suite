!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozC408digit003d.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 01/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*                             :
!*  SECONDARY FUNCTIONS TESTED : binary-constant is B'digit [digit]...'
!*						or B"digit [digit]..."
!*				where digit shall have one of the
!*				values 0 or 1
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


program bozC408digit003d

    integer :: i1, i2

    data i1 /B""/
    data i2 /B''/

    print *, int(B'')

    print *, real(B"")

    print *, dble(b"")

    print *, cmplx(b'',B"")

 end
