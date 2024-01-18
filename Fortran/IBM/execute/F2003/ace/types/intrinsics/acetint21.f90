!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint21
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-06-08
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : character type specifier variations
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : intrinsic type
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Verify that character type specifiers allow a wide range of forms (there
!*  are 9 different valid forms, including 2 obsolete forms).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod
contains
  pure integer function fun(arg)
    integer, intent(in) :: arg
    fun = arg
  end function fun
end module mod

program acetint21

  use mod
  implicit none

  ! We don't actually care how many chars the array has - "charr(1) = 'aa'" is legal, after all
  character :: charr(3)
  integer   :: ivar, iarr(2), len
  integer, parameter :: CONST1 = 1
  integer, parameter :: KIND = 1

  len = 2
  ivar = 1

  charr = (/character   :: 'a', 'b', 'c' /)
  charr = (/character   :: 'aa', 'bb', 'cc' /)
  charr = (/character*1 :: 'a', 'b', 'cd' /)
  charr = (/character*2 :: 'a', 'bb', 'ccd' /)

  !  charr = (/character( KIND = init-expr ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = 1 ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = CONST1*KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = KIND*CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = lbound(iarr(:),1) ) :: 'a', 'b', 'c' /)

  !  charr = (/character( LEN = expr , KIND = init-expr ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = 1,         KIND = 1 ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = CONST1,    KIND = CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = len,       KIND = KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = ivar,      KIND = CONST1*KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = KIND,      KIND = KIND*CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = lbound(iarr(:),1), KIND = lbound(iarr(:),1) ) :: 'a', 'b', 'c' /)

  charr = (/character( LEN = KIND*CONST1, KIND = 1 ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = ivar*1,    KIND = CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = 1*ivar,    KIND = KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = len-ivar,  KIND = CONST1*KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = CONST1*KIND, KIND = KIND*CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = fun(len),  KIND = lbound(iarr(:),1) ) :: 'a', 'b', 'c' /)

  !  charr = (/character( KIND = init-expr , LEN = expr ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = 1, LEN = 1 ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = CONST1, LEN = CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = KIND, LEN = len ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = CONST1*KIND, LEN = ivar ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = KIND*CONST1, LEN = KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = lbound(iarr(:),1), LEN = lbound(iarr(:),1) ) :: 'a', 'b', 'c' /)

  charr = (/character( KIND = 1, LEN = KIND*CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = CONST1, LEN = ivar*1 ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = KIND, LEN = 1*ivar ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = CONST1*KIND, LEN = len-ivar ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = KIND*CONST1, LEN = CONST1*KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND = lbound(iarr(:),1), LEN = fun(len) ) :: 'a', 'b', 'c' /)

  !  charr = (/character( expr , KIND = init-expr ) :: 'a', 'b', 'c' /)
  charr = (/character( 1,         KIND = 1 ) :: 'a', 'b', 'c' /)
  charr = (/character( CONST1,    KIND = CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( len,       KIND = KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( ivar,      KIND = CONST1*KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND,      KIND = KIND*CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( lbound(iarr(:),1), KIND = lbound(iarr(:),1) ) :: 'a', 'b', 'c' /)

  charr = (/character( KIND*CONST1, KIND = 1 ) :: 'a', 'b', 'c' /)
  charr = (/character( ivar*1,    KIND = CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( 1*ivar,    KIND = KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( len-ivar,  KIND = CONST1*KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( CONST1*KIND, KIND = KIND*CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( fun(len),  KIND = lbound(iarr(:),1) ) :: 'a', 'b', 'c' /)

  !  charr = (/character( expr , init-expr ) :: 'a', 'b', 'c' /)
  charr = (/character( 1,         1 ) :: 'a', 'b', 'c' /)
  charr = (/character( CONST1,    CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( len,       KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( ivar,      CONST1*KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND,      KIND*CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( lbound(iarr(:),1), lbound(iarr(:),1) ) :: 'a', 'b', 'c' /)

  charr = (/character( KIND*CONST1, 1 ) :: 'a', 'b', 'c' /)
  charr = (/character( ivar*1,    CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( 1*ivar,    KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( len-ivar,  CONST1*KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( CONST1*KIND, KIND*CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( fun(len),  lbound(iarr(:),1) ) :: 'a', 'b', 'c' /)

  !  charr = (/character( LEN = expr ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = 1 ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = len ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = ivar ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = lbound(iarr(:),1) ) :: 'a', 'b', 'c' /)

  charr = (/character( LEN = KIND*CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = ivar*1 ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = 1*ivar ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = len-ivar ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = CONST1*KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( LEN = fun(len) ) :: 'a', 'b', 'c' /)

  !  charr = (/character( expr ) :: 'a', 'b', 'c' /)
  charr = (/character( 2 ) :: 'a', 'b', 'c' /)
  charr = (/character( CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( len ) :: 'a', 'b', 'c' /)
  charr = (/character( ivar ) :: 'a', 'b', 'c' /)
  charr = (/character( KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( lbound(iarr(:),1) ) :: 'a', 'b', 'c' /)

  charr = (/character( KIND*CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character( ivar*1 ) :: 'a', 'b', 'c' /)
  charr = (/character( 1*ivar ) :: 'a', 'b', 'c' /)
  charr = (/character( len-ivar ) :: 'a', 'b', 'c' /)
  charr = (/character( CONST1*KIND ) :: 'a', 'b', 'c' /)
  charr = (/character( fun(len) ) :: 'a', 'b', 'c' /)

  !  charr = (/character*( expr ) :: 'a', 'b', 'c' /)
  charr = (/character*( 3 ) :: 'a', 'b', 'c' /)
  charr = (/character*( CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character*( len ) :: 'a', 'b', 'c' /)
  charr = (/character*( ivar ) :: 'a', 'b', 'c' /)
  charr = (/character*( KIND ) :: 'a', 'b', 'c' /)
  charr = (/character*( lbound(iarr(:),1) ) :: 'a', 'b', 'c' /)

  charr = (/character*( KIND*CONST1 ) :: 'a', 'b', 'c' /)
  charr = (/character*( ivar*1 ) :: 'a', 'b', 'c' /)
  charr = (/character*( 1*ivar ) :: 'a', 'b', 'c' /)
  charr = (/character*( len-ivar ) :: 'a', 'b', 'c' /)
  charr = (/character*( CONST1*KIND ) :: 'a', 'b', 'c' /)
  charr = (/character*( fun(len) ) :: 'a', 'b', 'c' /)

end program acetint21
