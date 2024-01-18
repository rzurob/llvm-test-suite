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
!*                                - Assoociate in C using CFI_setpointer
!*                                - Assoociate a pointer with a C-descriptor to a pointer that also has a C-descriptor
!*                                - Assoociate a pointer with a Fortran-descriptor to a pointer with a C-descriptor
!*                                - De-associate in Fortran using => NULL()
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
program PointerDummyArgument552f
use, intrinsic :: iso_c_binding
implicit none

interface
    subroutine c_associate(a) bind(c)
        import :: c_int
        integer(c_int), pointer :: a(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    end
    subroutine c_verify(a) bind(c)
        import :: c_int
        integer(c_int), pointer :: a(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    end
    subroutine c_sum(a) bind(c)
        import :: c_int
        integer(c_int), pointer :: a(:)
    end
end interface

integer(c_int), pointer :: al(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
integer(c_int), pointer :: ptr1D(:)
integer(c_int), allocatable :: al1D(:)

! call C procedures with non-associated
call c_associate(al)

! verify association status, size and bounds
if(                               .not. associated(al) ) ERROR STOP 10
if(                            .not. is_contiguous(al) ) ERROR STOP 11
if( size(al)       /=                                3 ) ERROR STOP 12
if( any(shape(al)  /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 13
if( any(lbound(al) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) ) ERROR STOP 14
if( any(ubound(al) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 15
if( any(al(1,1,1,1,1,1,1,1,1,1,1,1,1,1,:) /= [3,-2,4]) ) ERROR STOP 16

! Do some computation, take an array section and send it to C

al(:,:,:,:,:,:,:,:,:,:,:,:,:,:,2) = 2
if( any(al(1,1,1,1,1,1,1,1,1,1,1,1,1,1,:) /= [3,2,4]) ) ERROR STOP 17
!----> send the value back to C and verify it there and verify that the array is_contiguous
call c_verify(al)

ptr1D => al(1,1,1,1,1,1,1,1,1,1,1,1,1,1,:)

if( .not.      associated(ptr1D) ) ERROR STOP 20
if( size(ptr1D)      /=        3 ) ERROR STOP 21
if( any(shape(ptr1D) /=     [3]) ) ERROR STOP 22
if( lbound(ptr1D,1)  /=        1 ) ERROR STOP 23
if( ubound(ptr1D,1)  /=        3 ) ERROR STOP 24
if( any(ptr1D        /= [3,2,4]) ) ERROR STOP 25
! ----> send this one to C and check contiguity/values, size and other things
ptr1D => NULL()

allocate(ptr1D(3))
ptr1D = al(1,1,1,1,1,1,1,1,1,1,1,1,1,1,:)

if( .not.      associated(ptr1D) ) ERROR STOP 30
if( size(ptr1D)      /=        3 ) ERROR STOP 31
if( any(shape(ptr1D) /=     [3]) ) ERROR STOP 32
if( lbound(ptr1D,1)  /=        1 ) ERROR STOP 33
if( ubound(ptr1D,1)  /=        3 ) ERROR STOP 34
if( any(ptr1D        /= [3,2,4]) ) ERROR STOP 35
! ----> send this one to C and check contiguity/values, size and other things
deallocate(ptr1D)

al1D = al(1,1,1,1,1,1,1,1,1,1,1,1,1,1,:)

if( .not.       allocated(al1D) ) ERROR STOP 40
if( size(al1D)      /=        3 ) ERROR STOP 41
if( any(shape(al1D) /=     [3]) ) ERROR STOP 42
if( lbound(al1D,1)  /=        1 ) ERROR STOP 43
if( ubound(al1D,1)  /=        3 ) ERROR STOP 44
if( any(al1D        /= [3,2,4]) ) ERROR STOP 45
deallocate(al1D)

end program PointerDummyArgument552f
