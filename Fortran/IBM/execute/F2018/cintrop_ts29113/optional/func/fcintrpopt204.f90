! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fcintrpopt204.f
!*
!* PROGRAMMER                   : Ying Zhang
!* DATE                         : June 25, 2012
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : 399982 - C Interop: Optional Argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*
!* Calling a BIND(C) procedure from Fortran, (where the procedure is defined in C)
!*
!* Actual Argument:
!*   the actual argument is itself an optional dummy argument that is present or not present.
!*
!* Dummy Argument:
!*   nested optional procedure, procedure itself has optional dummy arg
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module testmod

  interface
    subroutine c_func_1(func_co_1) bind(c)
        use, intrinsic :: iso_c_binding
        optional func_co_1
        interface
            integer(c_int) function func_co_1(arg) bind(c)
            import
            integer(c_int), optional, intent(inout) :: arg
            end function func_co_1
        end interface
     end subroutine c_func_1

    subroutine c_func_2(func_co_2) bind(c)
        use, intrinsic :: iso_c_binding
        optional func_co_2
        interface
            integer(c_int) function func_co_2(arg) bind(c)
            import
            integer(c_int), optional, intent(inout) :: arg
            end function func_co_2
        end interface
     end subroutine c_func_2

     integer(c_int) function realfunc(arg) bind(c)
        use, intrinsic :: iso_c_binding
        integer(c_int), optional, intent(inout) :: arg
     end function realfunc
  end interface

end module testmod

program test
  use iso_c_binding
  use testmod
  implicit none
  
  call c_func_1()
  call c_func_1(realfunc)

  call c_func_2()
  call c_func_2(realfunc)
end program test
