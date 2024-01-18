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


  program bozR414digit002d

    integer :: i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11

    ! One Byte
    data i1 /Z'H20'/

    !Two Bytes
    data i2 /Z'CC2G'/

    !Three Bytes
    data i3 /Z'A5BI3'/

    !Four Bytes
    data i4 /Z'Z7CA3A3'/

    !Five Bytes
    data i5 /Z'263E23A37CX'/

    !Six Bytes
    data i6 /Z'X4C7CA746F947'/

    !Seven Bytes
    data i7 /Z'263CA3A37CA3A3X'/

    !Eight Bytes
    data i8 /Z'263E51SD1BEA3A309'/

    !Over Eight Bytes (65 bits)
    data i9 /Z'GAAAAAAAAAAAAAAAA'/
    data i10 /Z'D5555552AAAAAAAAH'/
    data i11 /Z'AAAAAAAAAAAAAAAAO'/

  end
