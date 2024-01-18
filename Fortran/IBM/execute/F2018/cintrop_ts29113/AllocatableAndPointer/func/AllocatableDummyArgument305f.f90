! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument301f.f
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
!*                                - Allocate in C / deallocate in C
!*                                - Set values in Fortran
!*                                - Verify values both in Fortran and C
!*                                - type c_int
!*                                - actual argument is an array and the dummy argument is allocatable or a pointer, 
!*                                  the bounds of the dummy argument are assumed from the actual argument
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine fsub(arg, flag, value) bind(C)
    use iso_c_binding
    implicit none
    logical(c_bool), optional   :: flag
    real(c_float), optional    :: value
    real(c_float), allocatable :: arg(:)

    if( present(value)) then 
       if ( allocated(arg) ) then
          if( arg(1) .NE. value) ERROR STOP 10  
          if ( present(flag) ) flag = .true._C_BOOL
       else 
         if ( present(flag) ) flag = .false._C_BOOL
       endif
    else 
       if ( allocated(arg) ) then
         if ( present(flag) ) flag = .true._C_BOOL
         arg = -1
       else 
         if ( present(flag) ) flag = .false._C_BOOL
       endif
    end if
end subroutine fsub

subroutine fill_array(arg, value) bind(C)
    use iso_c_binding
    implicit none

    interface
       real*4 function Func(arg)
            implicit none
            real*4 :: arg
       end     
    end interface

    real(c_float), allocatable :: arg(:), src
    real(c_float)              :: value
    integer i, lb, ub 

    lb = lbound(arg,1)
    ub = ubound(arg,1)
    allocate(src, source = value)

    if (.not. allocated(arg)) ERROR STOP 20
    arg = [(i*Func(src), i=lb,ub,1)] 
end subroutine fill_array

real*4 function Func(arg)
    implicit none
    real*4 arg

    Func = arg
end function Func
