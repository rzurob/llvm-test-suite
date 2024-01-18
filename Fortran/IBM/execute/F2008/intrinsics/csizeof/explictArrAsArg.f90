! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-12-01
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - the actual arg of c_sizeof is explicit array
!*                               whose lb and ub are dummy args or variable

!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    integer(C_SIZE_T) function array1_size() bind(c)
         use, intrinsic :: iso_c_binding
         array1_size = get_size(11,20)

         contains
             integer(C_SIZE_T) function get_size(lb,ub)
                 use, intrinsic :: iso_c_binding
                 integer lb,ub
                 integer(C_LONG) :: arr1(lb:ub)
                 get_size = c_sizeof(arr1)
             end function

    end function array1_size

    integer(C_SIZE_T) function array2_size() bind(c)
         use, intrinsic :: iso_c_binding
         integer lb

         lb = 11

         call sub

         contains
             subroutine sub()
                 use, intrinsic :: iso_c_binding
                 real(C_LONG_DOUBLE) :: array2(lb:lb+9)
                 array2_size = c_sizeof(array2)
             end subroutine
    end function array2_size
