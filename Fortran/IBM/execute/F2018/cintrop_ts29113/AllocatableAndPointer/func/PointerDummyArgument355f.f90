! *********************************************************************
!* ===================================================================
!*
!* DATE                         : July 27, 2013
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*                               (use -D_DEBUG for a debug version)
!*
!* DESCRIPTION                  : Calling a BIND(C) procedure defined in Fortran from C
!*                                - Assoociate in C using CFI_setpointer
!*                                - De-associate in Fortran using NULLIFY
!*                                - Verify association status and values in both C and Fortran
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
subroutine fcheck(flag, ptr) bind(c)
    use :: iso_c_binding, only: c_int
    implicit none
    integer :: i
    integer(c_int) :: flag
    integer(c_int), pointer :: ptr(:)

    if( flag .eq. 0 ) then
        if(          associated(ptr) ) ERROR STOP 10
    elseif ( flag .eq. -1 ) then
        if(        .not. associated(ptr) ) ERROR STOP 20
        if(           is_contiguous(ptr) ) ERROR STOP 21
        if( size(ptr)      .ne.        5 ) ERROR STOP 22
        if( any(shape(ptr) .ne.     [5]) ) ERROR STOP 23
        if( lbound(ptr,1)  .ne.        1 ) ERROR STOP 24
        if( ubound(ptr,1)  .ne.        5 ) ERROR STOP 25
        if( any(ptr   .ne. [2,4,6,8,10]) ) ERROR STOP 26
    elseif ( flag .eq. 2 ) then
        if(    .not. associated(ptr) ) ERROR STOP 30
        if( .not. is_contiguous(ptr) ) ERROR STOP 31
        if( size(ptr)      .ne.       10 ) ERROR STOP 32
        if( any(shape(ptr) .ne.    [10]) ) ERROR STOP 33
        if( lbound(ptr,1)  .ne.        1 ) ERROR STOP 34
        if( ubound(ptr,1)  .ne.       10 ) ERROR STOP 35
        if( any(ptr  .ne. [(i, i=1,10)]) ) ERROR STOP 36
    else
        if(        .not. associated(ptr) ) ERROR STOP 40
        if(     .not. is_contiguous(ptr) ) ERROR STOP 41
        if( size(ptr)      .ne.       10 ) ERROR STOP 42
        if( any(shape(ptr) .ne.    [10]) ) ERROR STOP 43
        if( lbound(ptr,1)  .ne.        0 ) ERROR STOP 44
        if( ubound(ptr,1)  .ne.        9 ) ERROR STOP 45
        if( any(ptr  .ne. [(i, i=1,10)]) ) ERROR STOP 46
    end if
end subroutine fcheck

subroutine fdisassociate(ptr) bind(c)
    use :: iso_c_binding, only: c_int
    implicit none
    integer(c_int), pointer :: ptr(:)

    if( .not. associated(ptr) ) ERROR STOP 50
    NULLIFY(ptr)
    if(       associated(ptr) ) ERROR STOP 51
end subroutine fdisassociate
