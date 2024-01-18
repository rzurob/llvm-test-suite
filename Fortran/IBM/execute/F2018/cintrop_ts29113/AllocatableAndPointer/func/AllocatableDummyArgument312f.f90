!*********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument312f.f
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
!*                                - Allocate (a, source=b) with only b having a C descriptor
!*                                - type c_float 
!*                                - I/O: read values from files 
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
subroutine check_all(arg) bind(c)
    use iso_c_binding
    implicit none
    real(c_float), allocatable, dimension(:,:) :: arg
    integer  i, j, k 

    interface
      subroutine my_read(a, unit) bind(c)
         use iso_c_binding
         implicit none
         real(c_float), allocatable, dimension(:,:), intent(inout) :: a
         integer, intent(in) :: unit
         integer :: iostat
         character(256) :: iomsg
      end subroutine my_read
    end interface

    if(          .not. allocated(arg) ) ERROR STOP 10
    if( size(arg)       .ne.      100 ) ERROR STOP 11
    if( any(shape(arg)  .ne. [10,10]) ) ERROR STOP 12
    if( any(lbound(arg) .ne. [-4,-4]) ) ERROR STOP 13
    if( any(ubound(arg) .ne. [ 5, 5]) ) ERROR STOP 14

    do i =1, size(arg)
       write (10,*) i*10.5 
    end do

    rewind(10)

    ! read value from file 
    call my_read(arg, 10)
    
    k = 1
    do i = lbound(arg,1), ubound(arg,1)
        do j = lbound(arg,2), ubound(arg,2)
           if (arg(j,i)  /= k*10.5) then 
             print*, arg(j,i), k*10.5
             ERROR STOP 15
           endif 
           k = k+1
        end do
    end do
end subroutine check_all

subroutine my_read(a, unit) bind(c)
    use iso_c_binding
    implicit none
    real(c_float), allocatable, dimension(:,:), intent(inout) :: a
    integer, intent(in) :: unit
    integer :: iostat, i, j
    character(256) :: iomsg

    do i = lbound(a,1), ubound(a,1)
        do j = lbound(a,2), ubound(a,2)
           read (unit, *, iostat=iostat, iomsg=iomsg) a(j,i)
           if (iostat  /= 0) then 
             print*, iomsg
             ERROR STOP 16
           endif 
        end do
    end do
end subroutine my_read

subroutine alloc_new_obj(src) bind(C)
    use iso_c_binding
    implicit none

    real(c_float), allocatable :: src(:,:)
    real(c_float), allocatable :: tgt(:,:)
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
    print*, src 
    print*, tgt 
    if (.not. allocated(tgt))  ERROR STOP 28
    if ( lbound(tgt,1) /= 1 )  ERROR STOP 29
    if ( lbound(tgt,2) /= 1 )  ERROR STOP 30
    if ( ubound(tgt,1) /= 2 )  ERROR STOP 31
    if ( ubound(tgt,2) /= 3 )  ERROR STOP 32
    if ( size(tgt)     /= 6 )  ERROR STOP 33

    allocate(tgt_1(size(tgt,2)), source=tgt(1,:))

    res = func(tgt_1, src)
    if ( .not. res )  ERROR STOP 34
end subroutine alloc_new_obj

subroutine associate_new_obj(src) bind(C)
    use iso_c_binding
    implicit none

    real(c_float), allocatable :: tgt(:)
    real(c_float), pointer     :: src(:)
    integer                    :: st
    character(200)             :: msg
    logical                    :: res

    if ( .not.   associated(src) )  ERROR STOP 40
    if (          allocated(tgt) )  ERROR STOP 41
    if ( any(lbound(src) /= [1]) )  ERROR STOP 42
    if ( any(ubound(src) /= [3]) )  ERROR STOP 43
    if ( size(src)       /= 3    )  ERROR STOP 44

    allocate(tgt, source=src, stat=st, errmsg=msg)

    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 45
    endif

    print*, src 
    print*, tgt 
    if ( .not. allocated(tgt)    )  ERROR STOP 46
    if ( any(lbound(tgt) /= [1]) )  ERROR STOP 47
    if ( any(ubound(tgt) /= [3]) )  ERROR STOP 48
    if ( size(tgt)       /= 3    )  ERROR STOP 49

! Do something more with array section received from C
! Locate Max, min...
end subroutine associate_new_obj

subroutine modify_values(arg) 
    implicit none
    real, allocatable :: arg(:,:)
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
