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
!*   assumed-size array of intrinsic type and derived type
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

integer(c_int) function func_test1(size, buffer) bind(c)
    use iso_c_binding
    implicit none

    logical, external :: precision_r4

    integer(c_int) :: size
    real(c_float), dimension( * ), optional :: buffer
    integer    :: i, actual_size

    actual_size = 0

    if (present(buffer)) then
        do i = 1, size
           if(precision_r4(buffer(i), i*1.0_4)) then
		actual_size = actual_size + 1
           end if
        end do
    end if

    print *, actual_size

    func_test1 = actual_size

end function func_test1

