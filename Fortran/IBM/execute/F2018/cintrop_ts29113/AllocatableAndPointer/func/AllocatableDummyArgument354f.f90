! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument354f.f
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
!*                                - Associate pointer to a target in Fortran 
!*                                  using a pointer assignment statement
!*                                - NULLIFY the pointer
!*                                - Set values in Fortran
!*                                - Verify values both in Fortran and C
!*                                - type c_int
!*                                - Optional Allocatable dummy argument 
!*
!*
!* If the dummy argument has the TARGET attribute and the VALUE attribute, any pointers associated with the
!* dummy argument become undefined when execution of the procedure completes.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine fassoc(arg, value) bind(C)
    use iso_c_binding
    implicit none

    interface
       real(c_float) function to_real(arg, opt) bind(C)
           use iso_c_binding
           implicit none
           integer(c_int), target :: arg
           integer(c_int), pointer :: opt
           optional opt
       end function to_real
    end interface

    real(c_float) :: res
    integer(c_int), pointer :: arg
    integer(c_int), target, value, intent(in) :: value

    if ( associated(arg) ) then
      print*, "arg is already associated!"
      ERROR STOP 10
    else 
      arg => value            
    endif

    if ( .not. associated(arg) ) ERROR STOP 11
    if ( arg    .NE.       -99 ) ERROR STOP 12

    res = to_real(value, arg)
    if ( res    .NE.      -99. ) ERROR STOP 13
end subroutine fassoc

subroutine fnullify(arg) bind(C)
    use iso_c_binding
    implicit none

    integer(c_int), pointer :: arg

    if ( .not. associated(arg) ) then
      print*, "arg is not associated!"
      ERROR STOP 20
    else 
      nullify(arg)
    endif

    if ( associated(arg) ) ERROR STOP 21
end subroutine fnullify
real(c_float) function to_real(arg, ptr_opt) bind(C)
    use iso_c_binding
    implicit none
    integer(c_int), target :: arg
    integer(c_int), pointer :: ptr_opt
    optional ptr_opt

    if( present(ptr_opt) ) then
      if( .NOT. associated(ptr_opt, arg) ) ERROR STOP 30
      to_real = real(ptr_opt)
      call sub(arg, ptr_opt)
    else 
      print*, "Missing pointer dummy argument:"
    endif


    contains

    subroutine sub(a, p)
        integer(c_int), target  :: a
        integer(c_int), pointer :: p

        if( .NOT. associated(p, a) ) ERROR STOP 31
    end subroutine sub
end function
