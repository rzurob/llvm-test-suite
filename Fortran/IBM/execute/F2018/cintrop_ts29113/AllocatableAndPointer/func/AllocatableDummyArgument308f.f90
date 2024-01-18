!*********************************************************************
!* ===================================================================
!*
!* DATE                         : January 25, 2013
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a Fortran BIND(C) procedure from C
!*
!*                                - Allocate in Fortran / deallocate in C
!*                                - Set values in Fortran
!*                                - Verify values both in Fortran and C
!*                                - type c_float
!*                                - Matmul: the last dimension of the first
!*                                          array must be equal to the first
!*                                          dimension of the second array
!* Fortran array:
!*   - dim 1 is number of rows
!*   - dim2 is number of columns
!*   - Column order
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine sub_alloc(n, m, arg) bind(C)
    use iso_c_binding
    implicit none

    integer(c_int) :: n, m
    real(c_float), allocatable :: arg(:,:)

    if ( allocated(arg) ) then
      print*, "argument is already allocated!"
      ERROR STOP 10
    else
      allocate(arg(n,m))
    endif

    if ( .not. allocated(arg) ) ERROR STOP 11
end subroutine sub_alloc
subroutine assign_value(arg) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), allocatable :: arg(:,:)

!*********************************************
!   x = 1 2 3       y = 3 2 1
!       3 2 1          -2 4 1
!      -4 3 1
!*********************************************

    if( .not. allocated(arg) ) ERROR STOP 15

     ! depending on the size of arg x or y will be assigned to it
    If( size(arg) == 9 ) then
       arg = reshape([1, 3, -4, 2, 2, 3, 3, 1, 1], [3,3])
       If( lbound(arg,1) /= 1 )  ERROR STOP 16
       If( lbound(arg,2) /= 1 )  ERROR STOP 17
       If( ubound(arg,1) /= 3 )  ERROR STOP 18
       If( ubound(arg,2) /= 3 )  ERROR STOP 19
    elseif( size(arg) == 6 ) then
       arg = reshape([3, -2, 2, 4, 1, 1], [2,3])
       If( lbound(arg,1) /= 1 )  ERROR STOP 20
       If( lbound(arg,2) /= 1 )  ERROR STOP 21
       If( ubound(arg,1) /= 2 )  ERROR STOP 22
       If( ubound(arg,2) /= 3 )  ERROR STOP 23
    else
       ERROR STOP 24
    endif
end subroutine assign_value
subroutine my_matmul(a, b, res) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), allocatable :: a(:,:), b(:,:), res(:,:)
    integer :: ub1, ub2, i

    ub1 = ubound(a,1)-lbound(a,1)+1
    ub2 = ubound(b,2)-lbound(b,2)+1

    allocate(res(ub1, ub2))
    res = matmul(a, b)
    do i = 1, ub1, 1
      print*, res (i,:)
    end do
end subroutine my_matmul
