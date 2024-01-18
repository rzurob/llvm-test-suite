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
!* Calling a BIND(C) procedure from Fortran, where the procedure is defined in Fortran
!*
!* Actual Argument:
!*   the actual argument is itself an optional dummy argument that is present or not present.
!*
!* Dummy Argument:
!*   intrinsic type + assumed-size array of derived type
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module dtmod
   use iso_c_binding

   type, bind(c) :: dt0
      real(16) :: a
   end type

   type, bind(c) :: dt1
      real(16) :: a
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      real(16) :: a
      type(dt1) :: d1
   end type

end module dtmod

module subsmod
   interface
      subroutine sub1(dim, size, dt) bind(c)
         use dtmod
         integer, optional, intent(in) :: dim
         integer, optional, intent(in) :: size
         type(dt2), optional, intent(inout), dimension(*) :: dt
      end subroutine sub1
      subroutine sub2(dim, size, dt) bind(c)
         use dtmod
         integer, optional, intent(in) :: dim
         integer, optional, intent(in) :: size
         type(dt2), optional, intent(inout), dimension(*) :: dt
      end subroutine sub2
      subroutine sub3(dim, size, dt) bind(c)
         use dtmod
         integer, optional, intent(in) :: dim
         integer, optional, intent(in) :: size
         type(dt2), optional, intent(inout), dimension(*) :: dt
      end subroutine sub3
      subroutine sub4(dim, size, dt) bind(c)
         use dtmod
         integer, optional, intent(in) :: dim
         integer, optional, intent(in) :: size
         type(dt2), optional, intent(inout), dimension(*) :: dt
      end subroutine sub4
   end interface
end module subsmod

program testprogram
  use iso_c_binding
  use dtmod
  use subsmod
  implicit none

  integer i, j

  type(dt2) dt2_a(2,2)

  do j=1, 2
    do i=1, 2
      dt2_a(i, j).a = 5.0q0
      dt2_a(i, j).d1.a = 10.0q0
      dt2_a(i, j).d1.d0.a = 15.0q0
    enddo
  enddo

  print *, "Testing 1..................."
  call sub1()

  print *, "Testing 2..................."
  call sub1(size=4, dt=dt2_a)

  print *, "Testing 3..................."
  call sub1(2, 2, dt2_a)

  print *, "Testing 4..................."
  call sub2(dt=dt2_a, dim=4)

  print *, "Testing 5..................."
  call sub3(dt=dt2_a)

end program

subroutine sub1(dim, size, dt) bind(c)
  use dtmod
  integer, optional, intent(in) :: dim
  integer, optional, intent(in) :: size
  type(dt2), optional, intent(inout), dimension(*) :: dt

  call sub2(dim, size, dt)

end subroutine sub1

subroutine sub2(dim, size, dt) bind(c)
  use dtmod
  integer, optional, intent(in) :: dim
  integer, optional, intent(in) :: size
  type(dt2), optional, intent(inout), dimension(*) :: dt

  call sub3(dim, size, dt)

end subroutine sub2

subroutine sub3(dim, size, dt) bind(c)
  use dtmod
  integer, optional, intent(in) :: dim
  integer, optional, intent(in) :: size
  type(dt2), optional, intent(inout), dimension(*) :: dt

  call sub4(dim, size, dt)

end subroutine sub3

subroutine sub4(dim, size, dt) bind(c)
  use dtmod
  integer, optional, intent(in) :: dim
  integer, optional, intent(in) :: size
  type(dt2), optional, intent(inout), dimension(*) :: dt

  integer :: dim_val, size_val, i, j

  dim_val = 1
  size_val = 1

  if (present(dim)) then
	print *, "  dim ", dim
        dim_val = dim
  else
	print *, "  dim not present"
  endif

  if (present(size)) then
	print *, "  size ", size
        size_val = size
  else
        print *, "  size not present"
  endif

  if (present(dt)) then
	print *, "  printing array of dimension ", dim_val, ", size ", size_val
        do i=1, size_val*dim_val
		print *, dt(i)
		dt(i).a = dt(i).a + 1.0q0
		dt(i).d1.a = dt(i).d1.a + 1.0q0
		dt(i).d1.d0.a = dt(i).d1.d0.a + 1.0q0
	enddo
  else
	print *, "  dt not present"
  endif

end subroutine sub4

