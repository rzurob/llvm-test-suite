! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : globalVar.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 2010-10-22
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - A C variable is interoperate with a variable
!*                               declared in the scope of module
!*                               - type integer(C_LONG) 
!*                               - type real(C_LONG_DOUBLE)
!*                               - complex(C_LONG_DOUBLE_COMPLEX) 
!*                               - array
!*                               - main program in C
 
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

    use, intrinsic :: iso_c_binding

    integer(C_LONG) :: arr1(10)
    real(C_LONG_DOUBLE) :: array2(100)
    complex(C_LONG_DOUBLE_COMPLEX) :: array3(20)

    bind(c,  name='array1') :: arr1

end module

    integer(C_SIZE_T) function array1_size() bind(c)
         use, intrinsic :: iso_c_binding
         use mod, only : arr1
         array1_size = c_sizeof(arr1)
    end function array1_size

    integer(C_SIZE_T) function array2_size() bind(c)
         use, intrinsic :: iso_c_binding
         use mod, only : array2
         array2_size = c_sizeof(array2)
    end function array2_size

    
    integer(C_SIZE_T) function array3_size() bind(c)
         use, intrinsic :: iso_c_binding
         use mod, only : array3
         array3_size = c_sizeof(array3)
    end function array3_size

