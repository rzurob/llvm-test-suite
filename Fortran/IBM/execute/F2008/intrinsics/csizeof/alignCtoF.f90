! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-24
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - derived-type
!*                               - C main program
!*                               - Fortran option -qalign=bindc=packed|natural
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
         logical(C_BOOL)    :: d(2)
         integer(C_LONG_LONG)  :: e
    end type dT1

    type, bind(c) :: dT2
         integer(C_INT)    :: a(10)
         integer(C_SHORT)  :: b(10)
    end type dT2

    type, bind(c) :: dT3
         integer(C_SHORT)  :: a
         real(C_DOUBLE)    :: b
         character(c_char) :: c
         integer(C_INT)    :: d
         complex(C_LONG_DOUBLE_COMPLEX) :: e
         real(C_LONG_DOUBLE) :: f
         logical(C_BOOL)     :: g
    end type dT3

end module


    integer(C_SIZE_T) function getsize_dt1(data) bind(c)
            use, intrinsic :: iso_c_binding
            use mod, only : dT1
            type(dT1) data(2)
            getsize_dt1 = c_sizeof(data)
    end function

    integer(C_SIZE_T) function getsize_dt2(data) bind(c)
            use, intrinsic :: iso_c_binding
            use mod, only : dT2
            type(dT2) data(2,2)
            getsize_dt2 = c_sizeof(data)
    end function

    integer(C_SIZE_T) function getsize_dt3(data) bind(c)
            use, intrinsic :: iso_c_binding
            use mod, only : dT3
            type(dT3) data
            getsize_dt3 = c_sizeof(data)
    end function

