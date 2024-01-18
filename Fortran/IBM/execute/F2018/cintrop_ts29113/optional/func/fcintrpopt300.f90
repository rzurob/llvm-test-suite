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
!* Calling a BIND(C) procedure from C, where the procedure is defined in Fortran, and
!* calling a BIND(C) procedure from Fortran, (where the procedure is defined in C)
!*
!* Actual Argument:
!* FORTRAN procedure actual arg: NULL pointer, or corresponding C types.
!* C procedure actual arg: no actual argument or correponding derived types.
!*
!* Dummy Argument:
!* FORTRAN procedure dummy arg: assumed size array of intrinsic types
!* C procedure dummy arg: pointers to a structure, a c type
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module test
  use iso_c_binding
  implicit none

  type, bind(c) ::  dt1
	integer(c_int) :: i2(101:102,150:151)
  end type

end module test

program fcintrpopt300
      use iso_c_binding
      use test
      implicit none

      interface

         subroutine c_func_test(arg1, arg2) bind(c)
           import
           integer(c_int), optional, intent(inout) :: arg1
           type(dt1), optional :: arg2
         end subroutine c_func_test

         subroutine f_func_test(num, arg) bind(c)
           import
           integer(c_int), optional :: num
           integer(c_int_fast8_t) , dimension(*), optional :: arg
         end subroutine f_func_test

      end interface

      integer i, j, realarg
      type(dt1), pointer :: pdt1
      type(dt1), target :: tdt1

      call c_func_test()

      realarg = 100
      call c_func_test(realarg)

      do j=1, 2
        do i=1, 2
      	    tdt1%i2(i+100,j+149) = i + j + realarg
        enddo
      enddo

      pdt1 => tdt1

      call c_func_test(realarg, pdt1)
end program

subroutine f_func_test(num, arg) bind(c)
  use iso_c_binding
  implicit none

     integer(c_int), optional :: num
     integer(c_int_fast8_t), dimension(*), optional :: arg
     integer i

     if (present(num)) then
             print *, "num is present in f_func_test"
	     if (present(arg)) then
 	      	 print *, "arg is present in f_func_test"
                 do i=1, num
  	             print *, arg(i)
		 enddo
	     else
 	      	 print *, "arg is not present in f_func_test"
             end if
     else
             print *, "num is not present in f_func_test"
     end if

end subroutine

