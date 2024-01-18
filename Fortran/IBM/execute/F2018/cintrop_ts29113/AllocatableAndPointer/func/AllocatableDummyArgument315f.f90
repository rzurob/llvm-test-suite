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
!*                                - Allocate (a, source=b) with only b having a C descriptor
!*                                - complex numbers
!*                                - Nesting of calls
!*                                   Bind(c) ==> Non-bind(c)
!*                                - Verify values both in Fortran and C
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine check_all(all) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none
    complex(c_float_complex), allocatable :: all(:)

    if(         .not. allocated(all) ) ERROR STOP 10
    if(     .not. is_contiguous(all) ) ERROR STOP 11
    if( size(all)      .ne.        5 ) ERROR STOP 12
    if( any(shape(all) .ne.     [5]) ) ERROR STOP 13
    if( lbound(all,1)  .ne.        1 ) ERROR STOP 14
    if( ubound(all,1)  .ne.        5 ) ERROR STOP 15
end subroutine check_all

subroutine check_ptr(ptr) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none
    complex(c_float_complex), pointer :: ptr(:)

    if(        .not. associated(ptr) ) ERROR STOP 20
    if(     .not. is_contiguous(ptr) ) ERROR STOP 21
    if( size(ptr)      .ne.        5 ) ERROR STOP 22
    if( any(shape(ptr) .ne.     [5]) ) ERROR STOP 23
    if( lbound(ptr,1)  .ne.        0 ) ERROR STOP 24
    if( ubound(ptr,1)  .ne.        4 ) ERROR STOP 25
end subroutine check_ptr

subroutine check_fptr(ptr) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none
    complex(c_float_complex), pointer :: ptr(:)

    if(        .not. associated(ptr) ) ERROR STOP 26
    if( size(ptr)      .ne.        5 ) ERROR STOP 27
    if( lbound(ptr,1)  .ne.        1 ) ERROR STOP 28
    if( ubound(ptr,1)  .ne.        5 ) ERROR STOP 29
end subroutine check_fptr

subroutine update_all(arg) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none
    complex(c_float_complex), allocatable :: arg(:)
    integer(c_int) i

    ! array reallocalation
    if(         .not. allocated(arg) ) ERROR STOP 30
    arg = [(int_2_complex(i),i=1,2*size(arg))]

    contains

    function int_2_complex(sel)
        complex(c_float_complex) :: int_2_complex
        integer(c_int), value :: sel

        int_2_complex = CMPLX(sel, sel+1)
    end function int_2_complex
end subroutine update_all

subroutine alloc_new_obj(src) bind(C)
    use, intrinsic :: iso_c_binding
    implicit none

    complex(c_float_complex), allocatable :: src(:)
    complex(c_float_complex), allocatable :: tgt(:)
    complex(c_float_complex), allocatable :: new(:,:), tran(:,:)
    integer                :: st
    character(200)         :: msg
    logical                :: res
    logical precision_x8
    integer i

    if ( .not. allocated(src))  ERROR STOP 31
    if (  lbound(src,1) /= 1 )  ERROR STOP 32
    if ( ubound(src,1) /= 10 )  ERROR STOP 33
    if ( size(src)     /= 10 )  ERROR STOP 34

    allocate(tgt, source=src, stat=st, errmsg=msg)

    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 35
    endif

    if ( .not. allocated(tgt))  ERROR STOP 36
    if (  lbound(tgt,1) /= 1 )  ERROR STOP 37
    if ( ubound(tgt,1) /= 10 )  ERROR STOP 38
    if ( size(tgt)     /= 10 )  ERROR STOP 39
    do i = lbound(tgt,1), ubound(tgt,1)
        if ( .not.   precision_x8(tgt(i), (i*1.0,i+1.0) )) ERROR STOP 40
    end do
    if( any(tgt%re .ne. [(i*1.0, i=1, 10)]) ) ERROR STOP 41
    if( any(tgt%im .ne. [(i*1.0, i=2, 11)]) ) ERROR STOP 42


    allocate(new(2,2))
    do i = lbound(new,1), ubound(new,1)
        new(i,:) = src(i:i+2:2)
    end do

    if ( .not. allocated(new))  ERROR STOP 43
    if (  lbound(new,1) /= 1 )  ERROR STOP 44
    if (  ubound(new,1) /= 2 )  ERROR STOP 45
    if (  size(new)     /= 4 )  ERROR STOP 46
    if ( .not.   precision_x8(new(1,1), (1.0,2.0) )) ERROR STOP 47
    if ( .not.   precision_x8(new(2,1), (2.0,3.0) )) ERROR STOP 48
    if ( .not.   precision_x8(new(1,2), (3.0,4.0) )) ERROR STOP 49
    if ( .not.   precision_x8(new(2,2), (4.0,5.0) )) ERROR STOP 50

    tran = transpose(new)
    if ( .not. allocated(tran))  ERROR STOP 51
    if (  lbound(tran,1) /= 1 )  ERROR STOP 52
    if (  ubound(tran,1) /= 2 )  ERROR STOP 53
    if (  size(tran)     /= 4 )  ERROR STOP 54
    if ( .not.   precision_x8(tran(1,1), (1.0,2.0) )) ERROR STOP 55
    if ( .not.   precision_x8(tran(1,2), (2.0,3.0) )) ERROR STOP 56
    if ( .not.   precision_x8(tran(2,1), (3.0,4.0) )) ERROR STOP 57
    if ( .not.   precision_x8(tran(2,2), (4.0,5.0) )) ERROR STOP 58
end subroutine alloc_new_obj

subroutine associate_new_obj(src) bind(C)
    use, intrinsic :: iso_c_binding
    implicit none

    complex(c_float_complex), allocatable :: tgt(:)
    complex(c_float_complex), pointer     :: src(:)
    integer                :: st, i
    character(200)         :: msg
    logical                :: res
    logical                :: precision_x8

    if ( .not.   associated(src) )  ERROR STOP 60
    if (          allocated(tgt) )  ERROR STOP 61
    if ( lbound(src,1) /= 0 )  ERROR STOP 62
    if ( ubound(src,1) /= 3 )  ERROR STOP 63
    if ( size(src)     /= 4 )  ERROR STOP 64
    do i = 1, size(src)
        if ( .not. precision_x8(src(i), ((i+1)*2.0,(i+1)*2.0+1.0) )) ERROR STOP 65
    end do

    allocate(tgt(5:(5+size(src)-1)), source=src, stat=st, errmsg=msg)

    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 66
    endif

    if ( .not. allocated(tgt) )  ERROR STOP 67
    if (   lbound(tgt,1) /= 5 )  ERROR STOP 68
    if (   ubound(tgt,1) /= 8 )  ERROR STOP 69
    if (   size(tgt)     /= 4 )  ERROR STOP 70
    do i = 0, size(tgt)-1
        if ( .not. precision_x8(tgt(i+lbound(tgt,1)), src(i) )) ERROR STOP 71
    end do
end subroutine associate_new_obj
