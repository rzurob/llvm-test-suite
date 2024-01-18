! *********************************************************************
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
!*                                - Allocate in C / deallocate in C
!*                                - verify that pointer is associated to global var with the "associated" intrinsic
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
    integer(c_int), pointer :: arg
    integer(c_int), optional    :: value
    logical(c_bool), optional   :: flag


    if( present(value)) then
       if ( associated(arg) ) then
          if( arg .NE. value) ERROR STOP 10
          if ( present(flag) ) flag = .true._C_BOOL
       else
         if ( present(flag) ) flag = .false._C_BOOL
       endif
    else
       if ( associated(arg) ) then
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

    integer(c_int), pointer :: arg, src
    integer(c_int)              :: value

    allocate(src, source = value)
    if ( associated(arg) ) then
       arg = Func(src)
    else
       ERROR STOP 20
    endif
end subroutine get_val

integer(c_int) function Func(arg)
    use iso_c_binding
    implicit none
    integer(c_int) :: arg

    Func = arg
end function Func

subroutine is_associated(ptr, tgt) bind(C)
    use iso_c_binding
    implicit none
    integer(c_int), pointer :: ptr
    integer(c_int), target  :: tgt

    if( .not. associated(ptr, tgt) ) ERROR STOP 30
end subroutine is_associated
