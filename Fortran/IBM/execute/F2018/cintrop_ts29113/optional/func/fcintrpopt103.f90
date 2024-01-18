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
!*   allocated POINTER
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


module testmod
  use iso_c_binding

  type, bind(c) :: dt0
      integer(c_short) :: a
  end type

  type, bind(c) :: dt1
      integer(c_short) :: a
      type(dt0) :: d0
  end type

  type, bind(c) :: dt2
      integer(c_short) :: a
      type(dt1) :: d1
  end type

  interface
      subroutine sub1(dt) bind(c)
         import dt2
         type(dt2), optional, intent(inout), dimension(5) :: dt
      end subroutine sub1
      subroutine sub2(dt) bind(c)
         import dt1
         type(dt1), optional, intent(inout), dimension(5, 5) :: dt
      end subroutine sub2
      subroutine sub3(dt) bind(c)
         import dt2
         type(dt2), optional, intent(inout), dimension(5, 5, 5) :: dt
      end subroutine sub3
      subroutine sub4(arg1, arg2) bind(c)
         import dt0
         type(dt0), optional, intent(in) :: arg1
         type(dt0), optional, dimension(5), intent(in) :: arg2
      end subroutine sub4
  end interface

end module testmod

program testprogram
  use iso_c_binding
  use testmod
  implicit none

  integer i, j, k

  type(dt2), allocatable, target :: pt_dt2_rk1_arr(:)
  type(dt1), allocatable, target :: pt_dt1_rk2_arr(:,:)
  type(dt2), allocatable, target :: pt_dt2_rk3_arr(:,:,:)
  type(dt0), allocatable, target :: pt_dt0
  type(dt0), allocatable, target :: pt_dt0_rk1_arr(:)

  allocate(pt_dt2_rk1_arr(5))
  allocate(pt_dt1_rk2_arr(5, 5))
  allocate(pt_dt2_rk3_arr(5, 5, 5))
  allocate(pt_dt0)
  allocate(pt_dt0_rk1_arr(5))

  print *, "Testing sub1..................."

  do i=1, 5
        pt_dt2_rk1_arr(i).a = 5
        pt_dt2_rk1_arr(i).d1.a = 15
        pt_dt2_rk1_arr(i).d1.d0.a = 25
  enddo

  call sub1()
  call sub1(pt_dt2_rk1_arr)

  print *, "Testing sub2..................."

  do i=1, 5
    do j=1, 5
        pt_dt1_rk2_arr(i, j).a = 35
        pt_dt1_rk2_arr(i, j).d0.a = 45
    enddo
  enddo

  call sub2()
  call sub2(pt_dt1_rk2_arr)

  print *, "Testing sub3..................."

  do i=1, 5
    do j=1, 5
      do k=1, 5
        pt_dt2_rk3_arr(i, j, k).a = 55
        pt_dt2_rk3_arr(i, j, k).d1.a = 65
        pt_dt2_rk3_arr(i, j, k).d1.d0.a = 75
      enddo
    enddo
  enddo

  call sub3()
  call sub3(pt_dt2_rk3_arr)

  print *, "Testing sub4..................."

  pt_dt0.a = 85

  do i=1, 5
        pt_dt0_rk1_arr(i).a = 95
  enddo

  call sub4()
  call sub4(pt_dt0)
  call sub4(arg2=pt_dt0_rk1_arr)
  call sub4(pt_dt0, pt_dt0_rk1_arr)

  deallocate(pt_dt2_rk1_arr)
  deallocate(pt_dt1_rk2_arr)
  deallocate(pt_dt2_rk3_arr)
  deallocate(pt_dt0)
  deallocate(pt_dt0_rk1_arr)

end program

subroutine sub1(dt) bind(c)
        use testmod, only : dt2
        type(dt2), optional, intent(inout), dimension(5) :: dt

	if (present(dt)) then
		print *, dt
	else
		print *, "sub1 dt not present"
	endif

end subroutine sub1


subroutine sub2(dt) bind(c)
        use testmod, only : dt1
         type(dt1), optional, intent(inout), dimension(5, 5) :: dt

        if (present(dt)) then
                print *, dt
        else
                print *, "sub2 dt not present"
        endif

end subroutine sub2

subroutine sub3(dt) bind(c)
        use testmod, only : dt2
         type(dt2), optional, intent(inout), dimension(5, 5, 5) :: dt

        if (present(dt)) then
                print *, dt
        else
                print *, "sub3 dt not present"
        endif

end subroutine sub3

subroutine sub4(arg1, arg2) bind(c)
        use testmod, only : dt0
         type(dt0), optional, intent(in) :: arg1
         type(dt0), optional, dimension(5), intent(in) :: arg2

        if (present(arg1)) then
                print *, arg1
        else
                print *, "sub4 arg1 not present"
        endif

        if (present(arg2)) then
                print *, arg2
        else
                print *, "sub4 arg2 not present"
        endif

end subroutine sub4


