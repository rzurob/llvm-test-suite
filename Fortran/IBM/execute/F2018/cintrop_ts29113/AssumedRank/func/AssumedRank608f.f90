! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AssumedRank603f.f
!*
!* PROGRAMMER                   : Dorra Bouchiha
!* DATE                         : October 27, 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    : 
!*                               (use -D_DEBUG for a debug version)
!*
!* DESCRIPTION                  : Calling a BIND(C) procedure defined in Fortran from C
!*                                - argay of various ranks 
!*                                - CFI_attribute_other: Lower bound is always 1 on the Fortran side 
!*                                - type c_int_least8_t 
!*                                - nested with bind(c)=>bind(c) / non-bind(c) call
!*
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
    use iso_c_binding
    implicit none
    integer  i 
    integer(c_int) flag
    integer(c_int_least8_t) arg
    dimension arg(..)

    interface 
        subroutine sub_bind_c(arg, r, s, lb, ub) bind(c)  
            use iso_c_binding
            implicit none
            integer(c_int_least8_t) arg
            dimension arg(..)
            integer :: r, s, lb, ub
        end subroutine sub_bind_c
        subroutine sub(arg, r, s, lb, ub)
            use iso_c_binding
            implicit none
            integer(c_int_least8_t) arg
            dimension arg(..)
            integer :: r, s, lb, ub
        end subroutine sub
    end interface 

!    /* Non-allocatable non-pointer assumed rank object:
!       lower boud is 0 on the C-side and 1 on the Fortran side
!       flag set to 0: rank 0
!       flag set to 1: rank 1
!       flag set to 2: rank 2
!       flag set to 15: rank 15
!       */
print*, rank(arg), size(arg), shape(arg), lbound(arg), ubound(arg)

    if ( flag .eq. 0 ) then
        if( rank(arg)          .ne.    0 ) ERROR STOP 10
        if( size(arg)          .ne.    1 ) ERROR STOP 11
        if( size(shape(arg))   .ne.    0 ) ERROR STOP 12
        if( size(lbound(arg))  .ne.    0 ) ERROR STOP 13
        if( size(ubound(arg))  .ne.    0 ) ERROR STOP 14
    elseif ( flag .eq. 1 ) then
        if(     .not. is_contiguous(arg) ) ERROR STOP 20
        if( rank(arg)      .ne.        1 ) ERROR STOP 21
        if( size(arg)      .ne.       10 ) ERROR STOP 22
        if( any(shape(arg) .ne.    [10]) ) ERROR STOP 23
        if( lbound(arg,1)  .ne.        1 ) ERROR STOP 24
        if( ubound(arg,1)  .ne.       10 ) ERROR STOP 25
        call sub(arg, 1, 10, 1, 10)
        call sub_bind_c(arg, 1, 10, 1, 10)
    elseif ( flag .eq. 2 ) then
        if(     .not. is_contiguous(arg) ) ERROR STOP 30
        if( rank(arg)      .ne.        2 ) ERROR STOP 31
        if( size(arg)      .ne.        6 ) ERROR STOP 32
        if( any(shape(arg) .ne.   [2,3]) ) ERROR STOP 33
        if( lbound(arg,1)  .ne.        1 ) ERROR STOP 34
        if( ubound(arg,1)  .ne.        2 ) ERROR STOP 35
        if( lbound(arg,2)  .ne.        1 ) ERROR STOP 36
        if( ubound(arg,2)  .ne.        3 ) ERROR STOP 37
        if( any(lbound(arg)  .ne. [1,1]) ) ERROR STOP 38
        if( any(ubound(arg)  .ne. [2,3]) ) ERROR STOP 39
        call sub(arg, 2, 6, 1, 2)
        call sub_bind_c(arg, 2, 6, 1, 2)
    elseif ( flag .eq. 15 ) then
        if(     .not. is_contiguous(arg) ) ERROR STOP 40
        if( rank(arg)      .ne.       15 ) ERROR STOP 41
        if( size(arg)      .ne.        3 ) ERROR STOP 42
        if( any(shape(arg)   .ne.  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 43
        if( any(lbound(arg)  .ne.  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) ) ERROR STOP 44
        if( any(ubound(arg)  .ne.  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 45
        call sub(arg, 15, 3, 1, 1)
        call sub_bind_c(arg, 15, 3, 1, 1)
    else
        print*, "This case is unexpected"
        ERROR STOP 1
    end if
end subroutine fcheck

subroutine sub(arg, r, s, lb, ub) 
    use iso_c_binding
    implicit none
    integer(c_int_least8_t) arg
    dimension arg(..)
    integer :: r, s, lb, ub

    if( rank(arg)      .ne.        r ) ERROR STOP 101
    if( size(arg)      .ne.        s ) ERROR STOP 102
    if( lbound(arg,1)  .ne.       lb ) ERROR STOP 103
    if( ubound(arg,1)  .ne.       ub ) ERROR STOP 104
end subroutine sub

subroutine sub_bind_c(arg, r, s, lb, ub) bind(c)
    use iso_c_binding
    implicit none
    integer(c_int_least8_t) arg
    dimension arg(..)
    integer :: r, s, lb, ub

    if( rank(arg)      .ne.        r ) ERROR STOP 111
    if( size(arg)      .ne.        s ) ERROR STOP 112
    if( lbound(arg,1)  .ne.       lb ) ERROR STOP 113
    if( ubound(arg,1)  .ne.       ub ) ERROR STOP 114
    !if( any(shape(arg) .ne.     [s]) ) ERROR STOP 113
end subroutine sub_bind_c
