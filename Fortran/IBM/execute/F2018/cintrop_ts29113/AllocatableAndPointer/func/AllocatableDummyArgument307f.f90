!*********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument303f.f
!*
!* PROGRAMMER                   : Dorra Bouchiha
!* DATE                         : January 25, 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
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

subroutine sub_alloc_clean(n, m, arg) bind(C)
    use iso_c_binding
    implicit none

    real(c_float), allocatable :: arg(:,:)
    integer(c_int)             :: n, m 
    integer                    :: st
    character(200)             :: msg

    allocate(arg(n,m), stat=st, errmsg=msg)
    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 13
    endif

    if ( .not. allocated(arg) ) ERROR STOP 14
end subroutine sub_alloc_clean

subroutine assign_value(arg) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), allocatable :: arg(:,:)
    integer :: x(3,3), y(2,3)  ! automatic storage 


!*********************************************
!   x = 1 2 3       y = 3 2 1     
!       3 2 1          -2 4 1         
!      -4 3 1                
!*********************************************

    if( .not. allocated(arg) ) ERROR STOP 15

    x = reshape([1, 3, -4, 2, 2, 3, 3, 1, 1], [3,3])
    y = reshape([3, -2, 2, 4, 1, 1], [2,3])

     ! depending on the size of arg x or y will be assigned to it
    If( size(arg) == size(x) ) then 
       arg = x
       If( lbound(arg,1) /= lbound(x,1) )  ERROR STOP 16
       If( lbound(arg,2) /= lbound(x,2) )  ERROR STOP 17
       If( ubound(arg,1) /= ubound(x,1) )  ERROR STOP 18
       If( ubound(arg,2) /= ubound(x,2) )  ERROR STOP 19
       print*, arg(1,:)
       print*, arg(2,:)
       print*, arg(3,:)
    elseif( size(arg) == size(y) ) then
       arg = y
       If( lbound(arg,1) /= lbound(y,1) )  ERROR STOP 20
       If( lbound(arg,2) /= lbound(y,2) )  ERROR STOP 21
       If( ubound(arg,1) /= ubound(y,1) )  ERROR STOP 22
       If( ubound(arg,2) /= ubound(y,2) )  ERROR STOP 23
       print*, arg(1,:)
       print*, arg(2,:)
    else 
       ERROR STOP 24
    endif 
end subroutine assign_value

subroutine my_matmul(a,b) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), allocatable :: a(:,:), b(:,:)

    print*, "Matrix multiplication result:"
    print*, matmul(a,b)
!*********************************************
! expected result: for matmul(x,x)
!  -5 15  8 
!   5 13 12
!   1  1 -8
!*********************************************
!*********************************************
! expected result: for matmul(y,x)
!  5 13 12
!  6  7 -1
!*********************************************
end subroutine my_matmul
