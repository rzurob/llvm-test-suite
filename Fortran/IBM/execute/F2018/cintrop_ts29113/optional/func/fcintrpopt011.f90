! *********************************************************************
!* ===================================================================
!*
!* DATE                         : June 25, 2012
!* ORIGIN                       : AIX Complier Development
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

subroutine sub_testf_1(code, func_co) bind(c)
   use, intrinsic :: iso_c_binding
   implicit none

   character(c_char), intent(inout) :: code
   optional func_co
   interface
       character(c_char) function func_co(arg) bind(c)
            use, intrinsic :: iso_c_binding
            character(c_char), intent(inout) :: arg
       end function
   end interface

   code = '1'

   print *, code

   if (present(func_co)) then
	code = func_co(code)
   else
        code = 'n'
        print *, "sub_testf_1 func_co not present"
   endif

   print *, code

end subroutine

subroutine sub_testf_2(code,func_co_2) bind(c)
   use, intrinsic :: iso_c_binding
   implicit none
   character(c_char), intent(inout) :: code
   optional func_co_2
   interface
        character(c_char) function func_co_2(arg) bind(c)
        use, intrinsic :: iso_c_binding
        character(c_char), intent(inout)::arg
        end function

	subroutine sub_testf_1(code1, func_co) bind(c)
   	use, intrinsic :: iso_c_binding
   	character(c_char), intent(inout) :: code1
   	optional func_co
   	interface
          character(c_char) function func_co(arg1) bind(c)
            use, intrinsic :: iso_c_binding
            character(c_char), intent(inout) :: arg1
          end function
   	end interface
end subroutine

   end interface

   code = '2'

   print *, code

   if (present(func_co_2)) then
        call sub_testf_1(code, func_co_2)
   else
        call sub_testf_1(code)
        print *, "sub_testf_2 func_co_2 not present"
   endif

   print *, code

end subroutine

character(c_char) function realfunc(a1) bind(c)
  use, intrinsic :: iso_c_binding
  character(c_char), intent(inout):: a1

  print *, a1
  realfunc='r'

end function

