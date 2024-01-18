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
!* Calling a BIND(C) procedure from Fortran, where the procedure is defined in Fortran
!*
!* Actual Argument:
!*   When have multiple optional dummy arguments in a function or subroutine, supply
!*   with no actual argument, or actual arguments for all the optional ones, or
!*   actual arguments for some of the optional arguments
!*
!* Dummy Argument:
!*   optional procedure + derived type
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
  use iso_c_binding

  type, bind(c) :: dt0
	integer(c_int64_t) :: i1
	integer(c_intptr_t) :: i2
  end type

end module testmod

program test
  use testmod
  implicit none

 interface
  real(c_float) function arg1(k) bind(c)
     use iso_c_binding
     integer(c_int)  k
  end function
  subroutine arg2(y,z) bind(c)
     use iso_c_binding
     real(c_float) y, z
     optional z
  end subroutine
 end interface

 interface
  subroutine sub2(func1,func2,dt) bind(c)
      import dt0
      optional func1,func2
      type(dt0), optional :: dt
      interface
          real(c_float) function func1(j) bind(c)
		use iso_c_binding
		integer(c_int)  j
          end function
          subroutine func2(x1,x2) bind(c)
		use iso_c_binding
		real(c_float) x1, x2
		optional x2
          end subroutine
      end interface
  end subroutine
 end interface

  type(dt0) :: dt_i1

  dt_i1%i1 = 10
  dt_i1%i2 = 20

  call sub2(arg1)
  call sub2(func2=arg2)
  call sub2(dt=dt_i1)
  call sub2(func1=arg1,dt=dt_i1)
  call sub2(arg1,arg2,dt_i1)


end program

subroutine sub2(func1,func2, dt) bind(c)
  use iso_c_binding
  use testmod

        integer(c_int) i, rc
        optional func1,func2
        type(dt0), optional :: dt
        interface
          real(c_float) function func1(j) bind(c)
                use iso_c_binding
                integer(c_int)  j
          end function
          subroutine func2(x1,x2) bind(c)
                use iso_c_binding
                real(c_float) x1, x2
                optional x2
          end subroutine
        end interface

        if(present(func1)) then
            i=10
            if(int(func1(i)+0.5).ne.20) print *, "error in func1"
        else
	    print *, "func1 is not presented"
        endif

        if(present(func2)) then
            call func2(9.0)
            call func2(9.0,1.0)
        else
	    print *, "func2 is not presented"
        endif

        if(present(dt)) then
	    print *, dt
        else
            print *, "dt is not presented"
        endif

end subroutine sub2

real(c_float) function arg1(k) bind(c)
  use iso_c_binding

  integer(c_int)  k
  arg1=2*k

end function

subroutine arg2(x,y) bind(c)
  use iso_c_binding

  real(c_float) x,y
  optional y
  if(present(y)) then
	print *, "y present"
	print *, 3.0*x+y
  else
	print *, "y is not presnet"
        print *, 3.0*x
  endif
end subroutine arg2

