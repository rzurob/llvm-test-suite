!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozC408digit001d.f
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


  program bozC408digit001d

    integer :: i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11

    ! One Byte
    data i1 /B'100102'/

    !Two Bytes
    data i2 /B'110011002001'/

    !Three Bytes
    data i3 /B'10100101101100030001'/

    !Four Bytes
    data i4 /B'1001104011111001010001110100011'/

    !Five Bytes
    data i5 /B'100110001111100501000111010001101111100'/

    !Six Bytes
    data i6 /B'10011000111110010160011101000110111110010100011'/

    !Seven Bytes
    data i7 /B'1001100011171001010001110100011011111001010001110100011'/

    !Eight Bytes
    data i8 /B'100110001111100101000111010001101111108101000111010001100001001'/

    !Over Eight Bytes (65 bits)
    data i9 /B"91010101010101010101010101010101010101010101010101010101010101010"/
    data i10 /B"11010101010101010101010101010901010101010101010101010101010101010"/
    data i11 /B"10101010101010101010101010101010101010101010101010101010101010109"/

  end
