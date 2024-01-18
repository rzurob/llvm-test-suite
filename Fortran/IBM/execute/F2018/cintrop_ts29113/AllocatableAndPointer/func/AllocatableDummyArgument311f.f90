!*********************************************************************
!* ===================================================================
!*
!* DATE                         : January 25, 2013
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a Fortran BIND(C) procedure from C
!*
!*                                - Allocate(a, source=b) with a and b both having a C descriptor
!*                                - Verify values both in Fortran and C
!*                                - type c_float
!*                                - Nesting of calls
!*                                   Bind(c) ==> Non-bind(c)
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
subroutine sub_alloc(tgt, src) bind(C)
    use iso_c_binding
    implicit none

    real(c_float), allocatable :: src(:,:), tgt(:,:)
    integer                    :: st
    character(200)             :: msg

    interface
       subroutine check(arg)
           implicit none
           real, allocatable :: arg(:,:)
       end subroutine
    end interface

    if (      allocated(tgt))  ERROR STOP 10
    if (.not. allocated(src))  ERROR STOP 11
    if ( lbound(src,1) /= 1 )  ERROR STOP 12
    if ( lbound(src,2) /= 1 )  ERROR STOP 13
    if ( ubound(src,1) /= 2 )  ERROR STOP 14
    if ( ubound(src,2) /= 3 )  ERROR STOP 15
    if ( size(src)     /= 6 )  ERROR STOP 16
    call check(src)

    allocate(tgt, source=src, stat=st, errmsg=msg)

    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 17
    endif

    if (.not. allocated(tgt))  ERROR STOP 18
    if ( lbound(tgt,1) /= 1 )  ERROR STOP 19
    if ( lbound(tgt,2) /= 1 )  ERROR STOP 20
    if ( ubound(tgt,1) /= 2 )  ERROR STOP 21
    if ( ubound(tgt,2) /= 3 )  ERROR STOP 22
    if ( size(tgt)     /= 6 )  ERROR STOP 23
    call check(tgt)

    print*, "src:", src
    print*, "tgt:", tgt

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

subroutine check(arg)
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

end subroutine check

logical(c_bool) function ffunc(a,b) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), pointer :: a(:)
    real(c_float), allocatable :: b(:,:), r(:)

    interface
       subroutine check(arg)
           implicit none
           real, allocatable :: arg(:,:)
       end subroutine
    end interface

    call check(b)
    if( .not. associated(a) ) ERROR STOP 50
    if( size(a) /=        3 ) ERROR STOP 51
    ffunc = .true.
    r = matmul(b,a)
    if (any(r /= [14., 3.]) ) then
        ffunc = .false.
        ERROR STOP 52
   end if
end function ffunc
