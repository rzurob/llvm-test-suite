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
!*                                - Allocate on the Fortran side/ Deallocate on the Fortran side
!*                                - Verify values both in Fortran and C
!*                                - type c_float 
!*                                - Nesting of calls
!*                                   Bind(c) ==> bind(c)
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
subroutine sub_alloc_1(arg) bind(C)
    use iso_c_binding
    implicit none

    real(c_float), allocatable :: arg(:,:)
    integer                    :: st
    character(200)             :: msg

    if (      allocated(arg))  ERROR STOP 10

    allocate(arg(2,3), stat=st, errmsg=msg)

    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 11
    endif

    if (.not. allocated(arg))  ERROR STOP 12
    if ( lbound(arg,1) /= 1 )  ERROR STOP 13
    if ( lbound(arg,2) /= 1 )  ERROR STOP 14
    if ( ubound(arg,1) /= 2 )  ERROR STOP 15
    if ( ubound(arg,2) /= 3 )  ERROR STOP 16
    if ( size(arg)     /= 6 )  ERROR STOP 17
end subroutine sub_alloc_1

subroutine sub_alloc_2(tgt, src) bind(C)
    use iso_c_binding
    implicit none

    real(c_float), allocatable :: src(:,:), tgt(:,:)
    integer                    :: st
    character(200)             :: msg

    interface
       subroutine modify_values(arg) bind(C)
           implicit none
           real, allocatable :: arg(:,:)
       end subroutine
    end interface

    if (      allocated(tgt))  ERROR STOP 20
    if (.not. allocated(src))  ERROR STOP 21
    if ( lbound(src,1) /= 1 )  ERROR STOP 22
    if ( lbound(src,2) /= 1 )  ERROR STOP 23
    if ( ubound(src,1) /= 2 )  ERROR STOP 24
    if ( ubound(src,2) /= 3 )  ERROR STOP 25
    if ( size(src)     /= 6 )  ERROR STOP 26

    allocate(tgt, source=src, stat=st, errmsg=msg)

    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 27
    endif

    call modify_values(tgt)
    if (.not. allocated(tgt))  ERROR STOP 28
    if ( lbound(tgt,1) /= 1 )  ERROR STOP 29
    if ( lbound(tgt,2) /= 1 )  ERROR STOP 30
    if ( ubound(tgt,1) /= 2 )  ERROR STOP 31
    if ( ubound(tgt,2) /= 3 )  ERROR STOP 32
    if ( size(tgt)     /= 6 )  ERROR STOP 33

    print*, "In allocate:"
    print*, "src:", src
    print*, "tgt:", tgt
end subroutine sub_alloc_2

subroutine sub_dealloc(arg) bind(C)
    use iso_c_binding
    implicit none

    real(c_float), allocatable :: arg(:,:)
    integer                     :: st
    character(200)              :: msg

    print*, "in deallocate"
    deallocate( arg, stat=st, errmsg=msg )
    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 40
    endif

    if ( allocated(arg) ) ERROR STOP 41
end subroutine sub_dealloc

subroutine modify_values(arg) bind(C)
    implicit none
    real, allocatable :: arg(:,:)
    real :: y(2,3) 

!*********************************************
!          y = 3 2 1     
!             -2 4 1         
!*********************************************

    if( .not. allocated(arg) ) ERROR STOP 50

    y = reshape([3., -2., 2., 4., 1., 1.], [2,3])

    if( lbound(arg,1) /= lbound(y,1) )  ERROR STOP 51
    if( lbound(arg,2) /= lbound(y,2) )  ERROR STOP 52
    if( ubound(arg,1) /= ubound(y,1) )  ERROR STOP 53
    if( ubound(arg,2) /= ubound(y,2) )  ERROR STOP 54
    if( any(arg       /=          y) )  ERROR STOP 55

    !modify an arbitary value 
    arg(1,3) = -arg(2,3)
end subroutine modify_values

logical(c_bool) function ffunc(a,b) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), pointer     :: a(:)
    real(c_float), allocatable :: b(:,:), r(:)
    real :: x(3), y(2,3) 

    interface 
       subroutine my_matmul(a,b,r)
           implicit none
           real(4), pointer     :: a(:)
           real(4), allocatable :: b(:,:), r(:)

       end subroutine my_matmul
    end interface 

   ffunc = .true.

    y = reshape([3., -2., 2., 4., -1., 1.], [2,3])
    x = [3., 2., 1.]
    print*, "y:", y
    print*, "x:", x
    print*, "expected results"
    print*, matmul(y,x)
    print*, "b(compare to y):", b
    print*, "a(compare to x):", a
    print*, "expected results with objects having c-desc"
    print*, matmul(b,a)

    print*, "call my_matmul:"
    call my_matmul(a,b,r)
    print*, "actual results"
    print*, r

    if (any(r /= matmul(y,x)) ) then 
    !if (any(r /= [12., 1.]) ) then 
        ffunc = .false.
        ERROR STOP 60
   end if
!*********************************************
! expected result: 
!   12.  1. 
!*********************************************
end function ffunc

subroutine my_matmul(a,b,r)
    implicit none
    real(4), pointer     :: a(:)
    real(4), allocatable :: b(:,:), r(:)

    print*, "Matrix multiplication result:"
    r = matmul(b,a)
    print*, r
end subroutine my_matmul

subroutine my_transpose(arg) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), dimension(:,:), pointer :: arg(:,:)
    real(c_float), dimension(size(arg,2),size(arg,1)) :: new
    real(c_float), dimension(2,3) :: y

    y = reshape([1., 1., 4., 2., -2., 3.], [2,3])

    if(.not. associated(arg) )  ERROR STOP 70
    if( lbound(arg,1)  /=  0 )  ERROR STOP 71
    if( lbound(arg,2)  /=  0 )  ERROR STOP 72
    if( ubound(arg,1)  /=  1 )  ERROR STOP 73
    if( ubound(arg,2)  /=  2 )  ERROR STOP 74
    if( size(arg)      /=  6 )  ERROR STOP 75
    if( any(arg        /= y) )  ERROR STOP 76

    new = transpose(arg)
    if( lbound(new,1)      /=  1 )  ERROR STOP 77
    if( lbound(new,2)      /=  1 )  ERROR STOP 78
    if( ubound(new,1)      /=  3 )  ERROR STOP 79
    if( ubound(new,2)      /=  2 )  ERROR STOP 80
    if( size(new)          /=  6 )  ERROR STOP 81
    if( any(new /= transpose(y)) )  ERROR STOP 82
end subroutine my_transpose
