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
!*                                - array is rank 0, 1, 2, 15
!*                                - allocatable
!*                                - type c_float
!*                                - nested with bind(c)=>bind(c) call
!*                                - INTENT(OUT)
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
subroutine fcheck(arr) bind(c)
    use :: iso_c_binding, only: c_float
    implicit none
    real(c_float), allocatable, intent(out) :: arr(..)

    interface
        subroutine sub(arr) bind(c)
            use :: iso_c_binding, only: c_float
            implicit none
            real(c_float), allocatable, intent(in) :: arr(..)
        end subroutine sub
    end interface

    if( allocated(arr) ) ERROR STOP 10
    call sub(arr)
end subroutine fcheck

subroutine sub(arr) bind(c)
    use :: iso_c_binding, only: c_float
    implicit none
    real(c_float), allocatable, intent(in) :: arr(..)

    if(  allocated(arr) ) ERROR STOP 100
end subroutine sub