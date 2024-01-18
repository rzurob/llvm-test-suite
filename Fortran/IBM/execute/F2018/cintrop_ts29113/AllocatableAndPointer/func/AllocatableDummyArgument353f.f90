! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument353f.f
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
!* If the dummy argument has the TARGET attribute and the effective argument does not have the TARGET
!* attribute or is an array section with a vector subscript, any pointers associated with the dummy argument
!* become undefined when execution of the procedure completes.
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

    integer(c_int), pointer :: arg
    integer(c_int), target  :: value

    if ( associated(arg) ) then
      print*, "arg is already associated!"
      ERROR STOP 10
    else 
      arg => value            
    endif

    if ( .not. associated(arg) ) ERROR STOP 11
    if ( arg    .NE.       -99 ) ERROR STOP 12

    call sub(value,arg)

    contains 

    subroutine sub(arg,ptr_opt) 
        integer(c_int), target :: arg
        integer(c_int), pointer, optional :: ptr_opt

        if( present(ptr_opt) ) then
          if( .NOT. associated(ptr_opt, arg) ) ERROR STOP 13
        endif
end subroutine sub
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
