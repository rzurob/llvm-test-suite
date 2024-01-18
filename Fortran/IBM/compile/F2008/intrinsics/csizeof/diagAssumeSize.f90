! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-17
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - test diagnostic message when argument
!*                                 of c_sizeof is assumed-size array
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


  integer(C_SIZE_T) function get_arraySize(x) bind(c)
      use, intrinsic :: iso_c_binding
      integer(C_LONG) :: x(4,*)

      get_arraySize = c_sizeof(x)
  end function

