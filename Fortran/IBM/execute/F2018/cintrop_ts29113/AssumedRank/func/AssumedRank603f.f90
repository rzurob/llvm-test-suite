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
!*                                - array of various ranks 
!*                                - CFI_attribute_other: Lower bound is always 1 on the Fortran side 
!*                                - type c_int8_t 
!*                                - nested with bind(c)=>bind(c) call
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
subroutine fcheck(flag, arr) bind(c)
    use iso_c_binding
    implicit none
    integer  i 
    integer(c_int) flag
    integer(c_int8_t) arr(..)

    interface 
        subroutine sub(arr, r, s, lb, ub) bind(c)  
            use iso_c_binding
            implicit none
            integer(c_int8_t) arr(..)
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
print*, rank(arr), size(arr), shape(arr), lbound(arr), ubound(arr)

    if ( flag .eq. 0 ) then
        if( rank(arr)          .ne.    0 ) ERROR STOP 10
        if( size(arr)          .ne.    1 ) ERROR STOP 11
        if( size(shape(arr))   .ne.    0 ) ERROR STOP 12
        if( size(lbound(arr))  .ne.    0 ) ERROR STOP 13
        if( size(ubound(arr))  .ne.    0 ) ERROR STOP 14
    elseif ( flag .eq. 1 ) then
        if(     .not. is_contiguous(arr) ) ERROR STOP 20
        if( rank(arr)      .ne.        1 ) ERROR STOP 21
        if( size(arr)      .ne.       10 ) ERROR STOP 22
        if( any(shape(arr) .ne.    [10]) ) ERROR STOP 23
        if( lbound(arr,1)  .ne.        1 ) ERROR STOP 24
        if( ubound(arr,1)  .ne.       10 ) ERROR STOP 25
        call sub(arr, 1, 10, 1, 10)
    elseif ( flag .eq. 2 ) then
        if(     .not. is_contiguous(arr) ) ERROR STOP 30
        if( rank(arr)      .ne.        2 ) ERROR STOP 31
        if( size(arr)      .ne.        6 ) ERROR STOP 32
        if( any(shape(arr) .ne.    [2,3]) ) ERROR STOP 33
        if( lbound(arr,1)  .ne.        1 ) ERROR STOP 34
        if( ubound(arr,1)  .ne.        2 ) ERROR STOP 35
        if( lbound(arr,2)  .ne.        1 ) ERROR STOP 36
        if( ubound(arr,2)  .ne.        3 ) ERROR STOP 37
        if( any(lbound(arr)  .ne.   [1,1]) ) ERROR STOP 38
        if( any(ubound(arr)  .ne.   [2,3]) ) ERROR STOP 39
        call sub(arr, 2, 6, 1, 2)
    elseif ( flag .eq. 15 ) then
        if(     .not. is_contiguous(arr) ) ERROR STOP 40
        if( rank(arr)      .ne.       15 ) ERROR STOP 41
        if( size(arr)      .ne.        3 ) ERROR STOP 42
        if( any(shape(arr)   .ne.  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 43
        if( any(lbound(arr)  .ne.  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) ) ERROR STOP 44
        if( any(ubound(arr)  .ne.  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 45
        call sub(arr, 15, 3, 1, 1)
    else
        print*, "This case is unexpected"
        ERROR STOP 1
    end if
end subroutine fcheck

subroutine sub(arr, r, s, lb, ub) bind(c)
    use iso_c_binding
    implicit none
    integer(c_int8_t) arr(..)
    integer :: r, s, lb, ub

    if( rank(arr)      .ne.        r ) ERROR STOP 101
    if( size(arr)      .ne.        s ) ERROR STOP 102
    !if( any(shape(arr) .ne.     [s]) ) ERROR STOP 103
    if( lbound(arr,1)  .ne.       lb ) ERROR STOP 104
    if( ubound(arr,1)  .ne.       ub ) ERROR STOP 105
end subroutine sub
