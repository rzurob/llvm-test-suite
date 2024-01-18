! *********************************************************************
!* ===================================================================
!*
!* DATE                         : June 25, 2012
!*
!* PRIMARY FUNCTIONS TESTED     : 399982 - C Interop: Optional Argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*
!* Calling a BIND(C) procedure from C, where the procedure is defined in Fortran
!*
!* Actual Argument:
!*   NULL Pointer, or corresponding C types
!*
!* Dummy Argument:
!*   derived type
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mod_test
        use iso_c_binding

        type, bind(c) :: dt1
                integer(c_short)   :: name(20)
                integer(C_INT)    :: id
        end type dt1

        type, bind(c) :: dt2
	        integer(c_int_fast8_t)     :: if8(4)
                integer(c_int_fast16_t)    :: if16
                integer(c_int_fast32_t)    :: if32
                integer(c_int_fast64_t)    :: if64
                real(c_long_double)        :: ldouble(5)
		complex(c_double_complex)  :: dc
                type(dt1) :: d1
	end type dt2

end module

      subroutine sub_test1(arg1, arg2, arg3) bind(C)
        use iso_c_binding
        use mod_test

        integer(c_int)           :: arg1
        integer(c_signed_char)   :: arg2
        type(dt2), optional      :: arg3

        print *, arg1
        print *, arg2

        if (present(arg3)) then
           print *, "arg for dt2 is ", arg3
        else
           print *, "arg for dt2 is not present"
        end if
      end


      subroutine sub_test2(arg1, arg2, arg3) bind(C)
        use iso_c_binding
        use mod_test

        integer(c_int)          :: arg1
        type(dt1), optional     :: arg3
        type(dt2), optional     :: arg2

        print *, arg1

        if (present(arg2)) then
           print *, "arg for dt2 is ", arg2
        else
           print *, "arg for dt2 is not present"
        end if

        if (present(arg3)) then
           print *, "arg for dt1 is ", arg3
        else
           print *, "arg for dt1 is not present"
        end if

      end

