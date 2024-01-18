!*********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument309f.f
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
!*                                - Allocate (a, source=b) with a and b both having a C descriptor
!*                                - Assumed-shape dummy argument
!*                                - type c_float 
!*                                - Nesting of calls
!*                                   Bind(c) ==> Non-bind(c)
!*                                   Bind(c) ==> bind(c) ==> Non-bind(c)
!*                                   Bind(c) ==> bind(c) ==> bind(c)
!*                                - Matmul: the last dimension of the first 
!*                                          array must be equal to the first 
!*                                          dimension of the second array
!*                                - Verify values both in Fortran and C
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
subroutine sub_alloc(tgt, src) bind(C)
    use iso_c_binding
    implicit none

    real(c_float), allocatable :: src(:,:), tgt(:,:)
    real(c_float), allocatable :: tgt_1(:)
    integer                    :: st
    character(200)             :: msg
    logical                    :: res

    interface
       subroutine modify_values(arg) 
           implicit none
           real, allocatable :: arg(:,:)
       end subroutine
       logical(c_bool) function func(a,b) bind(C)
          use iso_c_binding
          implicit none
          real(c_float), allocatable :: a(:), b(:,:)
       end function func
    end interface

    if (      allocated(tgt))  ERROR STOP 10
    if (.not. allocated(src))  ERROR STOP 11
    if ( lbound(src,1) /= 1 )  ERROR STOP 12
    if ( lbound(src,2) /= 1 )  ERROR STOP 13
    if ( ubound(src,1) /= 2 )  ERROR STOP 14
    if ( ubound(src,2) /= 3 )  ERROR STOP 15
    if ( size(src)     /= 6 )  ERROR STOP 16

    allocate(tgt, source=src, stat=st, errmsg=msg)

    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 17
    endif

    call modify_values(tgt)
    if (.not. allocated(tgt))  ERROR STOP 18
    if ( lbound(tgt,1) /= 1 )  ERROR STOP 19
    if ( lbound(tgt,2) /= 1 )  ERROR STOP 20
    if ( ubound(tgt,1) /= 2 )  ERROR STOP 21
    if ( ubound(tgt,2) /= 3 )  ERROR STOP 22
    if ( size(tgt)     /= 6 )  ERROR STOP 23

    allocate(tgt_1(size(tgt,2)), source=tgt(1,:))

    res = func(tgt_1, src)
    if ( .not. res )  ERROR STOP 24

end subroutine sub_alloc

subroutine sub_dealloc(arg) bind(C)
    use iso_c_binding
    implicit none

    real(c_float), allocatable :: arg(:,:)
    integer                     :: st
    character(200)              :: msg

    deallocate( arg, stat=st, errmsg=msg )
    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 30
    endif

    if ( allocated(arg) ) ERROR STOP 31
end subroutine sub_dealloc

subroutine modify_values(arg) 
    implicit none
    real, allocatable :: arg(:,:)
    real :: y(2,3) 
    integer :: i, j

!*********************************************
!          y = 3 2 1     
!             -2 4 1         
!*********************************************

    if( .not. allocated(arg) ) ERROR STOP 40

    y = reshape([3., -2., 2., 4., 1., 1.], [2,3])

    if( lbound(arg,1) /= lbound(y,1) )  ERROR STOP 41
    if( lbound(arg,2) /= lbound(y,2) )  ERROR STOP 42
    if( ubound(arg,1) /= ubound(y,1) )  ERROR STOP 43
    if( ubound(arg,2) /= ubound(y,2) )  ERROR STOP 44
    if( any(arg       /=          y) )  ERROR STOP 45

    !modify an arbitary value 
    arg(1,3) = -arg(2,3)
end subroutine modify_values

logical(c_bool) function func(a,b) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), allocatable :: a(:), b(:,:), r(:)
    real :: x(3), y(2,3) 

    interface 
       subroutine my_matmul(a,b,r)
           implicit none
           real(4), allocatable :: a(:), b(:,:), r(:)
       end subroutine my_matmul
       subroutine my_matmul_c(a,b,r) bind(C)
           implicit none
           real(4), allocatable :: a(:), b(:,:), r(:)
       end subroutine my_matmul_c
    end interface 

    func = .true.

    y = reshape([3., -2., 2., 4., 1., 1.], [2,3])
    x = [3., 2., -1.]
    if (any(a /= x) ) ERROR STOP 50
    if (any(b /= y) ) ERROR STOP 51

    call my_matmul(a,b,r)
! if result is good, return true. Otherwise False 
!*********************************************
! expected result: 
!   12.  1. 
!*********************************************
    if (any(r /= [12., 1.]) ) then 
        func = .false.
        ERROR STOP 52
   end if

    call my_matmul_c(a,b,r)
    if (any(r /= [12., 1.]) ) then 
        func = .false.
        ERROR STOP 53
   end if
end function func
!***************************************************
! If a dummy argument in an interoperable interface is allocatable, 
! assumed-shape, (...)  or a data pointer, the corresponding formal 
! parameter is interpreted as the address of a C descriptor
! for the effecctive argument in a reference to the procedure.
!***************************************************
logical(c_bool) function ffunc(a,b) bind(C)
    use iso_c_binding
    implicit none
    !real(c_float) :: a(:)   ! assumed shape array not supported yet
    real(c_float), pointer :: a(:)
    real(c_float), allocatable :: b(:,:), r(:)
    real :: x(3), y(2,3) 


    ffunc = .true.
    y = reshape([3., -2., 2., 4., -1., 1.], [2,3])
    x = [3., 2., 1.]
    if (any(a /= x) ) ERROR STOP 60
    if (any(b /= y) ) ERROR STOP 61

    r = matmul(b,a)
    if (any(r /= [12., 3.]) ) then 
        ffunc = .false.
        ERROR STOP 62
   end if
end function ffunc

subroutine my_matmul(a,b,r)
    implicit none
    real(4), allocatable :: a(:), b(:,:), r(:)

    r = matmul(b,a)
end subroutine my_matmul

subroutine my_matmul_c(a,b,r) bind(C)
    implicit none
    real(4), allocatable :: a(:), b(:,:), r(:)

    r = matmul(b,a)
end subroutine my_matmul_c
