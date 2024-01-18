! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AssumedRank604f.f
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
!*                                - CFI_attribute_allocatable
!*                                - type c_int
!*                                - various ranks 
!*                                - nested with bind(c)=>bind(c) call
!*
!* If the actual argument  has rank zero, the dummy argument has rank zero;
!* the shape is a zero-sized array and the LBOUND and UBOUND intrinsic functions,
!*  with no DIM argument, return zero-sized arrays.
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
    use :: iso_c_binding, only: c_int
    implicit none
    integer :: i 
    integer(c_int) :: flag
    integer(c_int), allocatable :: arr(..)  

    interface 
        subroutine sub(r, s, arr) bind(c) 
            implicit none
            integer :: r, s
            integer, allocatable :: arr(..)
        end subroutine sub
    end interface 

!      /* 
!       flag set to 0: unallocated scalar
!       flag set to 1: allocated scalar 
!       flag set to 2: rank 1 and lbound is -19
!       flag set to 3: rank 1 and lbound is 2  
!       flag set to 4: rank 2 and lbound is 1  
!       flag set to 5: rank 15 and lbound is 1  
!       */
   
    if ( flag .eq. 0 ) then
        if(               allocated(arr) ) ERROR STOP 10
    elseif ( flag .eq. 1 ) then
        if(        .not. allocated(arr) ) ERROR STOP 20
        if(    .not. is_contiguous(arr) ) ERROR STOP 21
        if( rank(arr)         .ne.    0 ) ERROR STOP 22
        if( size(arr)         .ne.    1 ) ERROR STOP 23
        if( size(shape(arr))  .ne.    0 ) ERROR STOP 24
        if( size(lbound(arr)) .ne.    0 ) ERROR STOP 25
        if( size(ubound(arr)) .ne.    0 ) ERROR STOP 26
        call sub(0, 1, arr)
    elseif ( flag .eq. 2 ) then
        if(         .not. allocated(arr) ) ERROR STOP 30
        if(     .not. is_contiguous(arr) ) ERROR STOP 31
        if( rank(arr)      .ne.        1 ) ERROR STOP 32
        if( size(arr)      .ne.       40 ) ERROR STOP 33
        if( any(shape(arr) .ne.    [40]) ) ERROR STOP 34
        if( lbound(arr,1)  .ne.      -19 ) ERROR STOP 35
        if( ubound(arr,1)  .ne.       20 ) ERROR STOP 36
        call sub(1, 40, arr)
    elseif ( flag .eq. 3 ) then
        if(         .not. allocated(arr) ) ERROR STOP 40
        if(     .not. is_contiguous(arr) ) ERROR STOP 41
        if( rank(arr)      .ne.        1 ) ERROR STOP 42
        if( size(arr)      .ne.        4 ) ERROR STOP 43
        if( any(shape(arr) .ne.     [4]) ) ERROR STOP 44
        if( lbound(arr,1)  .ne.        2 ) ERROR STOP 45
        if( ubound(arr,1)  .ne.        5 ) ERROR STOP 46
        call sub(1, 4, arr)
    elseif ( flag .eq. 4 ) then
        if(         .not. allocated(arr) ) ERROR STOP 50
        if(     .not. is_contiguous(arr) ) ERROR STOP 51
        if( rank(arr)        .ne.        2 ) ERROR STOP 52
        if( size(arr)        .ne.        6 ) ERROR STOP 53
        if( any(shape(arr) .ne.    [2,3]) ) ERROR STOP 54
        if( lbound(arr,1)    .ne.        1 ) ERROR STOP 55
        if( lbound(arr,2)    .ne.        1 ) ERROR STOP 56
        if( ubound(arr,1)    .ne.        2 ) ERROR STOP 57
        if( ubound(arr,2)    .ne.        3 ) ERROR STOP 58
        if( any(lbound(arr)  .ne.   [1,1]) ) ERROR STOP 59
        if( any(ubound(arr)  .ne.   [2,3]) ) ERROR STOP 60
        call sub(2, 6, arr)
    elseif ( flag .eq. 5 ) then
        if(         .not. allocated(arr) ) ERROR STOP 61
        if(     .not. is_contiguous(arr) ) ERROR STOP 62
        if( rank(arr)        .ne.       15 ) ERROR STOP 63
        if( size(arr)        .ne.        3 ) ERROR STOP 64
        if( any(shape(arr)   .ne.  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 65
        if( any(lbound(arr)  .ne.  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) ) ERROR STOP 66
        if( any(ubound(arr)  .ne.  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 67
        call sub(15, 3, arr)
    else
       print*, "This case is unexpected"
       ERROR STOP 1
    end if
end subroutine fcheck

subroutine sub(r, s, arr) bind(c)   
    implicit none
    integer :: r, s
    integer, allocatable :: arr(..)

    print*, "In sub:"
    print*, rank(arr), size(arr), shape(arr), lbound(arr), ubound(arr)
    if( .not. allocated(arr) ) ERROR STOP 100
    if( rank(arr)  .ne.    r ) ERROR STOP 101
    if( size(arr)  .ne.    s ) ERROR STOP 102
end subroutine sub
