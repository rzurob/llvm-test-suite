! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument302f.f
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
!*                                - Allocate in Fortran / deallocate in Fortran
!*                                - Set values in Fortran
!*                                - Verify values both in Fortran and C
!*                                - type c_double
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
subroutine sub_alloc(arg) bind(C)
    use iso_c_binding
    implicit none

    interface
       real(c_double) function Func(arg)
            use iso_c_binding
            implicit none
            real(c_double) :: arg
       end     
    end interface

    real(c_double), allocatable :: arg(:,:,:,:,:)
    integer i

    if ( allocated(arg) ) then
      print*, "arg is already allocated!"
      ERROR STOP 10
    else 
      allocate(arg(2:7,3:7,4:7,1:3,-1:0))
    endif

    arg = reshape([(Func(i*1._c_double), i=1,size(arg),1)], [6,5,4,3,2])

    if ( .not. allocated(arg) ) ERROR STOP 11
end subroutine sub_alloc

subroutine sub_alloc_clean(arg) bind(C)
    use iso_c_binding
    implicit none

    interface
       real(c_double) function Func(arg)
            use iso_c_binding
            implicit none
            real(c_double) :: arg
       end     
    end interface

    real(c_double), allocatable :: arg(:,:,:,:,:)
    integer                     :: st, i
    character(200)              :: msg

    allocate( arg(6,5,4,3,2), source=reshape([(Func(i*1._c_double), i=1,size(arg),1)], &
                                             & [6,5,4,3,2]), stat=st, errmsg=msg )
    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 13
    endif

    if ( .not. allocated(arg) ) ERROR STOP 14
end subroutine sub_alloc_clean

subroutine sub_dealloc(arg) bind(C)
    use iso_c_binding
    implicit none

    real(c_double), allocatable :: arg(:,:,:,:,:)

    if ( .not. allocated(arg) ) then
      print*, "arg is not allocated!"
      ERROR STOP 20
    else 
      deallocate(arg)
    endif

    if ( allocated(arg) ) ERROR STOP 21
end subroutine sub_dealloc

subroutine sub_dealloc_clean(arg) bind(C)
    use iso_c_binding
    implicit none

    real(c_double), allocatable :: arg(:,:,:,:,:)
    integer                     :: st
    character(200)              :: msg


    deallocate( arg, stat=st, errmsg=msg )
    if( st .NE. 0 ) then
          print *, msg
          ERROR STOP 22
    endif

    if ( allocated(arg) ) ERROR STOP 23
end subroutine sub_dealloc_clean

real(c_double) function Func(arg)
    use iso_c_binding
    implicit none
    real(c_double) :: arg

    Func = arg
end function Func

real(c_double) function compute(opt) bind(C)
    use iso_c_binding
    implicit none
    real(c_double), allocatable, optional :: opt(:,:,:,:,:)

    if(present(opt)) then
        compute = sum(opt)
    else
        compute = 0 
    endif
end function compute
