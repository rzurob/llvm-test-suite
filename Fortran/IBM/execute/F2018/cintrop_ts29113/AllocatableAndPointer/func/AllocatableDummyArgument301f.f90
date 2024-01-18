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
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine sub_int(arg, flag, value) bind(C)
    use iso_c_binding
    implicit none
    integer(c_int), allocatable :: arg
    integer(c_int), optional    :: value
    logical(c_bool), optional   :: flag


    if( present(value)) then 
       if ( allocated(arg) ) then
          if( arg .NE. value) ERROR STOP 10 
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
end subroutine sub_int

subroutine get_val(arg, value) bind(C)
    use iso_c_binding
    implicit none

    interface
       integer(c_int) function Func(arg)
            use iso_c_binding
            implicit none
            integer(c_int) :: arg
       end     
    end interface

    integer(c_int), allocatable :: arg, src
    integer(c_int)              :: value

    allocate(src, source = value)
    if ( allocated(arg) ) then
       arg = Func(src)
    else 
       STOP 20
    endif
end subroutine get_val

integer(c_int) function Func(arg)
    use iso_c_binding
    implicit none
    integer(c_int) :: arg

    Func = arg
end function Func
