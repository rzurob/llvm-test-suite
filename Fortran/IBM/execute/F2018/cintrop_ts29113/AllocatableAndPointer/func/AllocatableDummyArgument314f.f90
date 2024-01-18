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
!*                                - Allocate (a, source=b) with only b having a C descriptor
!*                                - derived type
!*                                - Nesting of calls
!*                                   Bind(c) ==> Non-bind(c)
!*                                   Bind(c) ==> bind(c) ==> Non-bind(c)
!*                                   Bind(c) ==> bind(c) ==> bind(c)
!*                                - Matmul: the last dimension of the first
!*                                          array must be equal to the first
!*                                          dimension of the second array
!*                                - Verify values both in Fortran and C
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
    use, intrinsic :: iso_c_binding
    implicit none

    type, bind(c) :: shape
      character(c_char) :: name
      integer(c_int)    :: area
      integer(c_int)    :: sides(2)
    end type
end module mod

subroutine check_all(all) bind(c)
    use mod, my_shape => shape
    implicit none
    type(my_shape), allocatable :: all(:,:)

    if(         .not. allocated(all) ) ERROR STOP 10
    if(     .not. is_contiguous(all) ) ERROR STOP 11
    if( size(all)      .ne.        4 ) ERROR STOP 12
    if( any(shape(all) .ne.  [2, 2]) ) ERROR STOP 13
    if( lbound(all,1)  .ne.        1 ) ERROR STOP 14
    if( lbound(all,2)  .ne.        1 ) ERROR STOP 15
    if( ubound(all,1)  .ne.        2 ) ERROR STOP 16
    if( ubound(all,2)  .ne.        2 ) ERROR STOP 17
end subroutine check_all

subroutine fill_all(all) bind(c)
    use mod
    implicit none
    type(shape), allocatable :: all(:,:)

    if(.not. allocated(all)) ERROR STOP 18

    all = reshape([shape("a",4,[2,2]), shape("t",6,[2,3]),  &
            &      shape("d",9,[3,3]), shape("r",6,[3,2])], [2,2])

    print*, all(1,1)
    print*, all(2,1)
    print*, all(1,2)
    print*, all(2,2)
end subroutine fill_all

subroutine check_ptr(ptr) bind(c)
    use mod, my_shape => shape
    implicit none
    type(my_shape), pointer :: ptr(:)

    if(     .not. associated(ptr) ) ERROR STOP 20
    if(  .not. is_contiguous(ptr) ) ERROR STOP 21
    if( size(ptr)      .ne.     2 ) ERROR STOP 22
    if( any(shape(ptr) .ne.  [2]) ) ERROR STOP 23
    if( lbound(ptr,1)  .ne.     0 ) ERROR STOP 24
    if( ubound(ptr,1)  .ne.     1 ) ERROR STOP 25
    if( (ptr(1).area .ne. -99) .or. (ptr(0).area .ne. -99) ) ERROR STOP 26
    if( (ptr(1).sides(1) .ne. -1) .or. (ptr(0).sides(1) .ne. -1) )  ERROR STOP 27
    if( (ptr(1).sides(2) .ne. -1) .or. (ptr(0).sides(2) .ne. -1) )  ERROR STOP 28

end subroutine check_ptr

subroutine alloc_new_obj(src) bind(C)
    use mod
    implicit none

    type(shape), allocatable :: src(:,:)
    type(shape), allocatable :: tgt(:,:)
    integer                :: st
    character(200)         :: msg
    logical                :: res

    if (      allocated(tgt))  ERROR STOP 30
    if (.not. allocated(src))  ERROR STOP 31
    if ( lbound(src,1) /= 1 )  ERROR STOP 32
    if ( lbound(src,2) /= 1 )  ERROR STOP 33
    if ( ubound(src,2) /= 2 )  ERROR STOP 34
    if ( ubound(src,1) /= 2 )  ERROR STOP 35
    if ( size(src)     /= 4 )  ERROR STOP 36

    allocate(tgt, source=src, stat=st, errmsg=msg)

    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 37
    endif

    if (.not. allocated(tgt))  ERROR STOP 38
    if ( any(lbound(tgt)   /= [1,1]) )  ERROR STOP 39
    if ( any(ubound(tgt)   /= [2,2]) )  ERROR STOP 40
    if ( size(tgt)         /=      4 )  ERROR STOP 41
end subroutine alloc_new_obj

subroutine associate_new_obj(src) bind(C)
    use mod
    implicit none

    type(shape), allocatable :: tgt(:)
    type(shape), pointer     :: src(:)
    integer                :: st
    character(200)         :: msg
    logical                :: res

    if ( .not.   associated(src) )  ERROR STOP 50
    if (          allocated(tgt) )  ERROR STOP 51
    if ( lbound(src,1) /= 1 )  ERROR STOP 52
    if ( ubound(src,1) /= 2 )  ERROR STOP 53
    if ( size(src)     /= 2 )  ERROR STOP 54

    allocate(tgt, source=src, stat=st, errmsg=msg)

    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 55
    endif

    if ( .not. allocated(tgt) )  ERROR STOP 56
    if ( lbound(tgt,1) /=   1 )  ERROR STOP 57
    if ( ubound(tgt,1) /=   2 )  ERROR STOP 58
    if ( size(tgt)     /=   2 )  ERROR STOP 59

    if ( tgt(1).name     /=  'a' )  ERROR STOP 59
    if ( tgt(1).area     /=    4 )  ERROR STOP 59
    if ( tgt(1).sides(1) /=    2 )  ERROR STOP 59
    if ( tgt(1).sides(2) /=    2 )  ERROR STOP 59
    if ( tgt(2).name     /=  'd' )  ERROR STOP 59
    if ( tgt(2).area     /=    9 )  ERROR STOP 59
    if ( tgt(2).sides(1) /=    3 )  ERROR STOP 59
    if ( tgt(2).sides(2) /=    3 )  ERROR STOP 59

end subroutine associate_new_obj

subroutine alloc_obj(arg) bind(C)
    use mod
    implicit none

    type(shape), allocatable :: arg(:,:)
    integer                :: st
    character(200)         :: msg
    logical                :: res

    if (allocated(arg))  ERROR STOP 30

    allocate(arg(7,3), stat=st, errmsg=msg)
    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 27
    endif

    if (.not. allocated(arg) )  ERROR STOP 28
    if ( lbound(arg,1) /=  1 )  ERROR STOP 29
    if ( lbound(arg,2) /=  1 )  ERROR STOP 29
    if ( ubound(arg,1) /=  7 )  ERROR STOP 30
    if ( ubound(arg,2) /=  3 )  ERROR STOP 30
    if ( size(arg)     /= 21 )  ERROR STOP 31
end subroutine alloc_obj
