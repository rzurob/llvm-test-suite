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
!*   explicit-shape array of derived type
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module derivemod
  use iso_c_binding
  implicit none

  type, bind(c) :: dt0
     integer(c_intmax_t) :: a
  end type dt0

  type, bind(c) :: dt1
     type(dt0) :: d0
     integer(c_intptr_t) :: a(2)
  end type dt1

  type, bind(c) :: dt2
     type(dt1) :: d1
     integer(c_long_long) :: a(2)
  end type dt2

end module derivemod

integer(c_int) function func_dt_arr1(dt_a0, dt_a1, dt_a2) bind(c)
  use iso_c_binding
  use derivemod
  implicit none

  type(dt0), value, intent(in) :: dt_a0
  type(dt1), optional :: dt_a1(2,2)
  type(dt2), intent(inout) :: dt_a2(2,2)
  integer i, j, k

  if ( dt_a0%a /= 50 ) then
      print *, "F error: dt_a0%a = ", dt_a0%a
      error stop 10
  endif

  if (present(dt_a1)) then
           func_dt_arr1 = 100
  else
           func_dt_arr1 = 10
  end if

  do j = 1, 2
    do i = 1, 2
      do k = 1, 2
         if ( dt_a2(i, j)%a(k) /= (i+j+k) ) then
             print *, "F error dt2:a : ", i, j, k, dt_a2(i, j)%a(k)
             error stop 20
         endif
         dt_a2(i, j)%a(k) = i + j + k + 1

         if ( dt_a2(i, j)%d1%a(k) /= (i+j+k) ) then
                 print *, "F error dt2.dt1.a : ", i, j, k, dt_a2(i, j)%d1%a(k)
		 error stop 30
         endif
         if ( dt_a2(i, j)%d1%d0%a /= (i+j+998) ) then
                print *, "F error dt2.dt1.dt0.a : ", i, j, dt_a2(i, j)%d1%d0%a
		error stop 40
         endif
      end do
    end do
  end do

end function func_dt_arr1
