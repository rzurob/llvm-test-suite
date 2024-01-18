! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : nestDT.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 2010-12-01
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - derived-type's component is of another derived-type  
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

module mod

    use, intrinsic :: iso_c_binding

    type, bind(c) :: dT1 
         real(C_FLOAT)    :: a 
         integer(C_INT)     :: b 
    end type dT1

    type, bind(c) :: dT2
         type(dT1)         :: dt(2)
         complex(C_LONG_DOUBLE_COMPLEX) :: c 
    end type dT2

end module


    integer(C_SIZE_T) function getsize_dt(data) bind(c)
            use, intrinsic :: iso_c_binding
            use mod, only : dT2
            type(dT2) data
            getsize_dt = c_sizeof(data)
    end function

