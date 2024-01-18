! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument304f.f
!*
!* PROGRAMMER                   : Dorra Bouchiha
!* DATE                         : January 25, 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy arg1ument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a Fortran BIND(C) procedure from C
!*
!*                                - Allocate in C / deallocate in Fortran
!*                                - Set values in C
!*                                - Intent(In) dummy argument 
!*                                - Optinal dummy argument 
!*                                - Verify values both in Fortran and C
!*                                - type c_double
!*
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine sub_dealloc(arg1) bind(C)
    use iso_c_binding
    implicit none

    real(c_double), allocatable :: arg1

    if ( .not. allocated(arg1) ) then
      print*, "arg1 is not allocated!"
      ERROR STOP 20
    else 
      deallocate(arg1)
    endif

    if ( allocated(arg1) ) ERROR STOP 21
end subroutine sub_dealloc

subroutine sub_dealloc_clean(arg1) bind(C)
    use iso_c_binding
    implicit none

    real(c_double), allocatable :: arg1
    integer                     :: st
    character(200)              :: msg


    deallocate( arg1, stat=st, errmsg=msg )
    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 22
    endif

    if ( allocated(arg1) ) ERROR STOP 23
end subroutine sub_dealloc_clean

subroutine compute(arg1, arg2) bind(C)
    use iso_c_binding
    implicit none
    real(c_double), allocatable, intent(in) :: arg1, arg2
    real(c_double), allocatable :: obj1, obj2, obj3, obj4
    real(c_double), allocatable :: all1, all2, all3, all4
    real(c_double) :: res1, res2, res3, res4

    print*, "Entry values:"
    print*, arg1, arg2

    all1 = arg1 * arg2
    res1 = arg1 * arg2
    print*, "all1, res1:"
    print*, all1, res1

    allocate(obj1, obj2)
    obj1 = arg1 
    obj2 = arg2 
    all2 = obj1 * obj2
    res2 = obj1 * obj2
    print*, "all2, res2:"
    print*, all2, res2

    allocate(obj3, source=arg1)
    allocate(obj4, source=arg2)
    all3 = obj3 * obj4
    res3 = obj3 * obj4
    print*, "all3, res3:"
    print*, all3, res3

    all4 = obj1 * obj4
    res4 = obj1 * obj4
    print*, "all4, res4:"
    print*, all4, res4
end subroutine

subroutine set_value(arg, opt1, opt2, opt3, opt4, value, val1, val2, val3, val4) bind(C)
    use iso_c_binding
    implicit none

    real(c_double)              :: value
    real(c_double), optional    :: val1, val2, val3, val4
    real(c_double), allocatable :: arg
    real(c_double), allocatable, optional :: opt1, opt2, opt3, opt4

    if ( .not. allocated(arg) ) ERROR STOP 10
    arg = value 

    if ( present(opt1) ) then
       if ( .not. allocated(opt1) ) ERROR STOP 11
       opt1 = val1
    endif

    if ( present(opt2) ) then
       if ( .not. allocated(opt2) ) ERROR STOP 12
       opt2 = val2
    endif

    if ( present(opt3) ) then
       if ( .not. allocated(opt3) ) ERROR STOP 13
       opt3 = val3
    endif

    if ( present(opt4) ) then
       if ( .not. allocated(opt4) ) ERROR STOP 14
       opt4 = val4
    endif
end subroutine set_value
