! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-18
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - derived-type components are scalars of
!*                                 type integer with all named constants
!*                               - Fortran main program
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
    type, bind(c) :: dType1
         integer(C_INT)  :: a
         integer(C_SHORT)  :: b
         integer(C_LONG)  :: c
         integer(C_LONG_LONG)  :: d
    end type dType1

    type, bind(c) :: dType2
         integer(C_INT8_T)  :: a
         integer(C_INT_LEAST8_T)  :: b
         integer(C_INT_FAST8_T)  :: c
         integer(C_SIGNED_CHAR)  :: d
    end type dType2

    type, bind(c) :: dType3
         integer(C_INT_FAST16_T)  :: a
         integer(C_INT_FAST32_T)  :: b
         integer(C_INT_FAST64_T)  :: c
    end type dType3

    type, bind(c) :: dType4
         integer(C_INT_LEAST64_T)  :: c
         integer(C_INT_LEAST16_T)  :: a
         integer(C_INT_LEAST32_T)  :: b
    end type dType4

    type, bind(c) :: dType5
         integer(C_BOOL)  :: a
         integer(C_CHAR)  :: b
         integer(C_INTMAX_T)  :: c
    end type dType5

    type, bind(c) :: dType6
         integer(C_INT16_T)  :: a
         integer(C_INT32_T)  :: b
         integer(C_INT64_T)  :: c
    end type dType6

    type, bind(c) :: dType7
         integer(C_SIZE_T)  :: a
         integer(C_INTPTR_T)  :: b
    end type dType7


    interface

        integer(C_SIZE_T) function get_size1(x) bind(c)
            use, intrinsic :: iso_c_binding
            import dType1
            type(dType1) x
        end function get_size1

        integer(C_SIZE_T) function get_size2(x) bind(c)
            use, intrinsic :: iso_c_binding
            import dType2
            type(dType2) x
        end function get_size2

        integer(C_SIZE_T) function get_size3(x) bind(c)
            use, intrinsic :: iso_c_binding
            import dType3
            type(dType3) x
        end function get_size3

        integer(C_SIZE_T) function get_size4(x) bind(c)
            use, intrinsic :: iso_c_binding
            import dType4
            type(dType4) x
        end function get_size4

        integer(C_SIZE_T) function get_size5(x) bind(c)
            use, intrinsic :: iso_c_binding
            import dType5
            type(dType5) x
        end function get_size5

        integer(C_SIZE_T) function get_size6(x) bind(c)
            use, intrinsic :: iso_c_binding
            import dType6
            type(dType6) x
        end function get_size6

        integer(C_SIZE_T) function get_size7(x) bind(c)
            use, intrinsic :: iso_c_binding
            import dType7
            type(dType7) x
        end function get_size7

    end interface

    type(dType1) :: dt1
    type(dType2) :: dt2
    type(dType3) :: dt3
    type(dType4) :: dt4
    type(dType5) :: dt5
    type(dType6) :: dt6
    type(dType7) :: dt7

    if ( c_sizeof(dt1) /= get_size1(dt1) ) error stop 10
    if ( c_sizeof(dt2) /= get_size2(dt2) ) error stop 20
    if ( c_sizeof(dt3) /= get_size3(dt3) ) error stop 30
    if ( c_sizeof(dt4) /= get_size4(dt4) ) error stop 40
    if ( c_sizeof(dt5) /= get_size5(dt5) ) error stop 50
    if ( c_sizeof(dt6) /= get_size6(dt6) ) error stop 60
    if ( c_sizeof(dt7) /= get_size7(dt7) ) error stop 70


end

