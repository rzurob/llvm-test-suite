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
!*                                - Allocate (a, source=b) with only a having a C descriptor
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
subroutine sub_alloc(arg) bind(C)
    use iso_c_binding
    implicit none

    real(c_float), allocatable :: src(:,:), arg(:,:)
    integer                    :: st
    character(200)             :: msg

    interface
       subroutine check(arg) bind(C)
          use iso_c_binding
          implicit none
          real(c_float), allocatable :: arg(:,:)
       end subroutine
    end interface

    src = reshape([3., -2., 2., 4., 1., 1.], [2,3])

    if (      allocated(arg))  ERROR STOP 10
    if (.not. allocated(src))  ERROR STOP 11
    if ( lbound(src,1) /= 1 )  ERROR STOP 12
    if ( lbound(src,2) /= 1 )  ERROR STOP 13
    if ( ubound(src,1) /= 2 )  ERROR STOP 14
    if ( ubound(src,2) /= 3 )  ERROR STOP 15
    if ( size(src)     /= 6 )  ERROR STOP 16

    allocate(arg, source=src, stat=st, errmsg=msg)

    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 17
    endif

    if (.not. allocated(arg))  ERROR STOP 18
    if ( lbound(arg,1) /= 1 )  ERROR STOP 19
    if ( lbound(arg,2) /= 1 )  ERROR STOP 20
    if ( ubound(arg,1) /= 2 )  ERROR STOP 21
    if ( ubound(arg,2) /= 3 )  ERROR STOP 22
    if ( size(arg)     /= 6 )  ERROR STOP 23
    call check(arg)
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

subroutine check(arg) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), allocatable :: arg(:,:)
    real :: y(2,3)

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

real(c_float) function find_max(arg) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), allocatable :: arg(:,:)

    print*, maxval(arg)
    find_max = maxval(arg)

end function find_max

real(c_float) function find_min(arg) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), allocatable :: arg(:,:)

    print*, minval(arg)
    find_min = minval(arg)
end function find_min

subroutine locate_max(arg) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), allocatable :: arg(:,:)

    print*, "First occurence of the of the maximum value:"
    print*, maxloc(arg)
end subroutine locate_max

subroutine locate_min(arg) bind(C)
    use iso_c_binding
    implicit none
    real(c_float), allocatable :: arg(:,:)

    print*, "First occurence of the of the minimum value:"
    print*, minloc(arg)
end subroutine locate_min
