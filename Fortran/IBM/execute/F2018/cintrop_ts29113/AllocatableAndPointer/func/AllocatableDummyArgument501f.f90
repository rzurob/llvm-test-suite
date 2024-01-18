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
!* DESCRIPTION                  : Calling a BIND(C) procedure defined in C from Fortran
!*                                - Allocate in C using CFI_allocate
!*                                - De-associate/deallocate in Fortran using => NULL(), deallocate and NULLIFY
!*                                - Verify allocation status and values in both C and Fortran
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
program AllocatableDummyArgument501f
use, intrinsic :: iso_c_binding
implicit none

interface
    subroutine c_associate(p) bind(c)
        import :: c_int
        integer(c_int), pointer :: p
    end
    subroutine c_allocate(a) bind(c)
        import :: c_int
        integer(c_int), allocatable :: a
    end
end interface

integer(c_int), allocatable :: al
integer(c_int), pointer :: ptr => NULL()
integer(c_int), target :: t = 6
integer(c_int) :: q

! call C procedures with non-allocated, non-associated vars
call c_associate(ptr)
call c_allocate(al)

! ptr and al are now allocated/associated
if( .not.   allocated(al) ) ERROR STOP 10
if( .not. associated(ptr) ) ERROR STOP 11

deallocate(al)
allocate(al, source=5)
ptr => t

! call C procedures with allocated, associated vars
call c_associate(ptr)
call c_allocate(al)

ptr => NULL()
deallocate(al)

! call C procedures with non-allocated, non-associated vars
call c_associate(ptr)
call c_allocate(al)
if( .not.   allocated(al) ) ERROR STOP 12
if( .not. associated(ptr) ) ERROR STOP 13

allocate(ptr)
ptr = 6
al = 5

! call C procedures with allocated, associated vars
call c_associate(ptr)
call c_allocate(al)

NULLIFY(ptr)
deallocate(al)

! call C procedures with non-allocated, non-associated vars
call c_associate(ptr)
call c_allocate(al)
if( .not.   allocated(al) ) ERROR STOP 14
if( .not. associated(ptr) ) ERROR STOP 15

q = 6
allocate(ptr, source=q)
al =q-1

! call C procedures with allocated, associated vars
call c_associate(ptr)
call c_allocate(al)

deallocate(ptr, al)

end program AllocatableDummyArgument501f
