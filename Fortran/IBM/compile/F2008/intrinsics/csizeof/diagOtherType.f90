! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-17
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - test diagnostic message when argument
!*                                 of c_sizeof is not interoperable data entity
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


program main
      use, intrinsic :: iso_c_binding
      implicit none
      integer(C_SIZE_T)  :: size
      integer(C_LONG), pointer :: x
      byte :: y
      integer(C_INT), allocatable :: z(:)

      size = c_sizeof(x)
      size = c_sizeof(y)
      size = c_sizeof(z)
end
