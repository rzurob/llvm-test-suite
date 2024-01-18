! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument303f.f
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
!*                                - Allocate in Fortran / deallocate in C
!*                                - Set values in Fortran
!*                                - Verify values both in Fortran and C
!*                                - type c_float 
!*                                - Optional Allocatable dummy argument 
!*
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine sub_alloc(value, arg) bind(C)
    use iso_c_binding
    implicit none

    interface
       real(c_float) function Func(arg)
            use iso_c_binding
            implicit none
            real(c_float) :: arg
       end     
    end interface

    real(c_float), allocatable :: arg
    real(c_float), optional    :: value
    integer :: i = 0                        ! i has implictly the SAVE attribute

    if ( allocated(arg) ) then
      print*, "arg is already allocated!"
      ERROR STOP 10
    else 
       if ( present(value) ) then 
         allocate(arg, source=Func(value))
       else 
         allocate(arg, source=Func(-88.0))
       endif 
    endif

    if ( .not. allocated(arg) ) ERROR STOP 11

    if (  i .eq. 0 ) then
      if ( arg   .NE.     -88.0 ) ERROR STOP 12
    else 
      if ( arg   .NE.     -99.0 ) ERROR STOP 13
    endif

    i = 2

end subroutine sub_alloc

subroutine sub_alloc_clean(value, arg) bind(C)
    use iso_c_binding
    implicit none

    interface
       real(c_float) function Func(arg)
            use iso_c_binding
            implicit none
            real(c_float) :: arg
       end     
    end interface

    real(c_float), allocatable :: arg
    real(c_float)              :: value
    integer                    :: st
    character(200)             :: msg

    allocate( arg, source=Func(value), stat=st, errmsg=msg )
    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 13
    endif

    if ( .not. allocated(arg) ) ERROR STOP 14
    if ( arg   .NE.      55.0 ) ERROR STOP 15
end subroutine sub_alloc_clean

real(c_float) function Func(arg)
    use iso_c_binding
    implicit none
    real(c_float) :: arg

    Func = arg
end function Func

function compute(arg,opt) result(res) bind(C)
    use iso_c_binding
    implicit none
    real(c_float) :: res
    real(c_float), allocatable :: opt, arg

    res = opt * arg 
end function compute
