! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-17
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - test result type is of integer(C_SIZE_T)
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
      integer(C_LONG) :: A

      call sub(c_sizeof(A))

      contains
          subroutine sub(x)
              use, intrinsic :: iso_c_binding
              integer(C_SIZE_T) :: x
              print *, x
          end subroutine

end program
