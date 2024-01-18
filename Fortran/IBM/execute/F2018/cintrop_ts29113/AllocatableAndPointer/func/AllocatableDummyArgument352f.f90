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
!*                                - Allocate in Fortran / deallocate in Fortran
!*                                - Set values in Fortran
!*                                - Verify values both in Fortran and C
!*                                - type c_int
!*                                - Optional Allocatable dummy argument
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine sub_alloc(arg, value) bind(C)
    use iso_c_binding
    implicit none

    interface
       integer(c_int) function Func(arg)
            use iso_c_binding
            implicit none
            integer(c_int) :: arg
       end
    end interface

    integer(c_int), pointer :: arg
    integer(c_int)          :: value

    if ( associated(arg) ) then
      print*, "arg is already associated!"
      ERROR STOP 10
    else
      allocate(arg, source=Func(value))
    endif

    if ( .not. associated(arg) ) ERROR STOP 11
    if ( arg   .NE.       -99 ) ERROR STOP 12
end subroutine sub_alloc

subroutine sub_alloc_clean(arg, value) bind(C)
    use iso_c_binding
    implicit none

    interface
       integer(c_int) function Func(arg)
            use iso_c_binding
            implicit none
            integer(c_int) :: arg
       end
    end interface

    integer(c_int), pointer :: arg
    integer(c_int)          :: value
    integer                 :: st
    character(200)          :: msg

    allocate( arg, source=Func(value), stat=st, errmsg=msg )
    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 13
    endif

    if ( .not. associated(arg) ) ERROR STOP 14
    if ( arg   .NE.        55 ) ERROR STOP 15
end subroutine sub_alloc_clean

subroutine sub_dealloc(arg) bind(C)
    use iso_c_binding
    implicit none

    integer(c_int), pointer :: arg

    if ( .not. associated(arg) ) then
      print*, "arg is not associated!"
      ERROR STOP 20
    else
      deallocate(arg)
    endif

    if ( associated(arg) ) ERROR STOP 21
end subroutine sub_dealloc

subroutine sub_dealloc_clean(arg) bind(C)
    use iso_c_binding
    implicit none

    integer(c_int), pointer :: arg
    integer                 :: st
    character(200)          :: msg


    deallocate( arg, stat=st, errmsg=msg )
    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 22
    endif

    if ( associated(arg) ) ERROR STOP 23
end subroutine sub_dealloc_clean

integer(c_int) function Func(arg)
    use iso_c_binding
    implicit none
    integer(c_int) :: arg

    Func = arg
end function Func

integer(c_int) function compute(arg,opt) bind(C)
    use iso_c_binding
    implicit none
    integer(c_int) :: arg
    integer(c_int), pointer :: opt
    optional opt

    if(present(opt)) then
        if(arg .eq. opt) then
          compute = 2*opt
        else
          compute = opt
        endif
    else
        compute = 0
    endif
end function
