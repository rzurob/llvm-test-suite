!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE stmt with
!*  format declarations, and testing format SIGN selectors (SS,SP,S), mixed
!*  case, printing to the console zero values. Using integer variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign094

    integer(2) :: i2 = -0
    integer(4) :: i4 = -0
    integer :: i = -0
    integer(8) :: i8 = -0

13  format(I2,X,2(I4,X),I8)

    write (*,13,sign='processor_defined') i2, i, i4, i8
    write (*,13,sign='plus') i2, i, i4, i8
    write (*,13,sign='suppress') i2, i, i4, i8

14  format(I2,SS,X,2(SP,I4,X),S,I8)

    write (*,14,sign='processor_defined') i2, i, i4, i8
    write (*,14,sign='plus') i2, i, i4, i8
    write (*,14,sign='suppress') i2, i, i4, i8

15  format(S,I2,SP,X,2(I4,X),I8)

    write (*,15,sign='processor_defined') i2, i, i4, i8
    write (*,15,sign='plus') i2, i, i4, i8
    write (*,15,sign='suppress') i2, i, i4, i8

end program iosign094

