! *********************************************************************
!* ===================================================================
!*
!* DATE                         : October 27, 2013
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*                               (use -D_DEBUG for a debug version)
!*
!* DESCRIPTION                  : Calling a BIND(C) procedure defined in Fortran from C
!*                                - argay is rank 1
!*                                - CFI_attribute_other: Lower bound is always 1 on the Fortran side
!*                                - type c_int
!*                                - nested with bind(c)=>bind(c) call
!*
!* Actual Argument:
!*
!* Dummy Argument:
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine fcheck(flag, arg) bind(c)
    use :: iso_c_binding, only: c_int
    implicit none
    integer :: i
    integer(c_int) :: flag
    integer(c_int), dimension(..), intent(in) :: arg

    interface
        subroutine sub(r, s, lb, ub, arg)
            implicit none
            integer :: r, s, lb, ub
            integer, dimension(..), optional :: arg
        end subroutine sub
        subroutine sub_bind_c(r, s, lb, ub, arg) bind(c)
            implicit none
            integer, intent(in), optional :: arg(..)
            integer :: r, s, lb, ub
        end subroutine sub_bind_c
    end interface

!      /*
!       flag set to 0 when lbound is 1
!       flag set to 1 when lbound is 1
!       flag set to 2 when lbound is 1
!       flag set to -1 when argay is not contiguous and lbound is 1
!       */

    if ( flag .eq. -1 ) then
        if(           is_contiguous(arg) ) ERROR STOP 11
        if( rank(arg)      .ne.        1 ) ERROR STOP 12
        if( size(arg)      .ne.        4 ) ERROR STOP 13
        if( any(shape(arg) .ne.     [4]) ) ERROR STOP 14
        if( lbound(arg,1)  .ne.        1 ) ERROR STOP 15
        if( ubound(arg,1)  .ne.        4 ) ERROR STOP 16
        call sub_bind_c(1, 4, 1, 4, arg)
        call sub(1, 4, 1, 4, arg)
    elseif ( flag .eq. 0 ) then
        if(     .not. is_contiguous(arg) ) ERROR STOP 21
        if( rank(arg)      .ne.        1 ) ERROR STOP 22
        if( size(arg)      .ne.       10 ) ERROR STOP 23
        if( any(shape(arg) .ne.    [10]) ) ERROR STOP 24
        if( lbound(arg,1)  .ne.        1 ) ERROR STOP 25
        if( ubound(arg,1)  .ne.       10 ) ERROR STOP 26
        call sub_bind_c(1, 10, 1, 10, arg)
        call sub(1, 10, 1, 10, arg)
    elseif ( flag .eq. 1 ) then
        if(     .not. is_contiguous(arg) ) ERROR STOP 31
        if( rank(arg)      .ne.        1 ) ERROR STOP 32
        if( size(arg)      .ne.       10 ) ERROR STOP 33
        if( any(shape(arg) .ne.    [10]) ) ERROR STOP 34
        if( lbound(arg,1)  .ne.        1 ) ERROR STOP 35
        if( ubound(arg,1)  .ne.       10 ) ERROR STOP 36
        call sub_bind_c(1, 10, 1, 10, arg)
        call sub(1, 10, 1, 10, arg)
    elseif ( flag .eq. 2 ) then
        if(     .not. is_contiguous(arg) ) ERROR STOP 41
        if( rank(arg)      .ne.        1 ) ERROR STOP 42
        if( size(arg)      .ne.       10 ) ERROR STOP 43
        if( any(shape(arg) .ne.    [10]) ) ERROR STOP 44
        if( lbound(arg,1)  .ne.        1 ) ERROR STOP 45
        if( ubound(arg,1)  .ne.       10 ) ERROR STOP 46
        call sub_bind_c(1, 10, 1, 10, arg)
        call sub(1, 10, 1, 10, arg)
    else
        if(     .not. is_contiguous(arg) ) ERROR STOP 51
        if( rank(arg)      .ne.        1 ) ERROR STOP 52
        if( size(arg)      .ne.       10 ) ERROR STOP 53
        if( any(shape(arg) .ne.    [10]) ) ERROR STOP 54
        if( lbound(arg,1)  .ne.        0 ) ERROR STOP 55
        if( ubound(arg,1)  .ne.        9 ) ERROR STOP 56
        call sub_bind_c(1, 10, 0, 9, arg)
        call sub(1, 10, 0, 9, arg)
    end if
end subroutine fcheck

subroutine sub(r, s, lb, ub, arg)
    implicit none
    integer, dimension(..), optional :: arg
    integer :: r, s, lb, ub

    if ( present(arg) ) then
        if( rank(arg)      .ne.        r ) ERROR STOP 101
        if( size(arg)      .ne.        s ) ERROR STOP 102
        if( any(shape(arg) .ne.     [s]) ) ERROR STOP 103
        if( lbound(arg,1)  .ne.       lb ) ERROR STOP 104
        if( ubound(arg,1)  .ne.       ub ) ERROR STOP 105
    endif
end subroutine sub

subroutine sub_bind_c(r, s, lb, ub, arg) bind(c)
    implicit none
    integer, intent(in), optional :: arg(..)
    integer :: r, s, lb, ub

    if ( present(arg) ) then
        if( rank(arg)      .ne.        r ) ERROR STOP 101
        if( size(arg)      .ne.        s ) ERROR STOP 102
        if( any(shape(arg) .ne.     [s]) ) ERROR STOP 103
        if( lbound(arg,1)  .ne.       lb ) ERROR STOP 104
        if( ubound(arg,1)  .ne.       ub ) ERROR STOP 105
    endif
end subroutine sub_bind_c
