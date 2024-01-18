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


  program bozC409digit002d

    integer :: i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11

    ! One Byte
    data i1 /O'80'/

    !Two Bytes
    data i2 /O'693010'/

    !Three Bytes
    data i3 /O'4459421'/

    !Four Bytes
    data i4 /O'11537121693'/

    !Five Bytes
    data i5 /O'2397610721574'/

    !Six Bytes
    data i6 /O'2307626390676243'/

    !Seven Bytes
    data i7 /O'1143712164337129643'/

    !Eight Bytes
    data i8 /O'230762495067650721411'/

    !Over Eight Bytes (65 bits)
    data i9 /O'9230762435067650721411'/
    data i10 /O'5761624650937120321511'/
    data i11 /O'3215014330620502174019'/

  end
