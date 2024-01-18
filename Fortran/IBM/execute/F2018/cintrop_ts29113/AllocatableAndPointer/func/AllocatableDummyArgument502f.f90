! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument502f.f
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
!* DESCRIPTION                  : Calling a BIND(C) procedure defined in C from Fortran 
!*                                - Allocate in C using CFI_allocate 
!*                                - De-associate/deallocate in Fortran using => NULL(), deallocate and NULLIFY
!*                                - Verify allocation status and values in both C and Fortran 
!*                                  
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
program AllocatableDummyArgument502f
use, intrinsic :: iso_c_binding
implicit none

interface
    subroutine c_allocate(a) bind(c)
        import :: c_int
        integer(c_int), allocatable :: a(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    end
    subroutine c_verify(a) bind(c)
        import :: c_int
        integer(c_int), allocatable :: a(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    end
    subroutine c_sum(a) bind(c)
        import :: c_int
        integer(c_int), allocatable :: a(:)
    end
end interface

integer(c_int), allocatable :: al(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
integer(c_int), allocatable :: tmp(:)
     
! call C procedures with non-allocated
call c_allocate(al)

! verify allocationj status, size and bounds 
if(                                .not. allocated(al) ) ERROR STOP 10
if(                            .not. is_contiguous(al) ) ERROR STOP 11
if( size(al)       /=                                3 ) ERROR STOP 12
if( any(shape(al)  /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 13
if( any(lbound(al) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) ) ERROR STOP 14
if( any(ubound(al) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 15
if( any(al(1,1,1,1,1,1,1,1,1,1,1,1,1,1,:) /= [3,-2,4]) ) ERROR STOP 16

! Do some computation, take an array section and send it to C

al(:,:,:,:,:,:,:,:,:,:,:,:,:,:,2) = 2
if( any(al(1,1,1,1,1,1,1,1,1,1,1,1,1,1,:) /= [3,2,4]) ) ERROR STOP 17
call c_verify(al)
!----> send the value back to C and verify it there and verify that the array is_contiguous
tmp = al(1,1,1,1,1,1,1,1,1,1,1,1,1,1,:)
if( .not.       allocated(tmp) ) ERROR STOP 20
if( size(tmp)      /=        3 ) ERROR STOP 21
if( any(shape(tmp) /=     [3]) ) ERROR STOP 22
if( lbound(tmp,1)  /=        1 ) ERROR STOP 23
if( ubound(tmp,1)  /=        3 ) ERROR STOP 24
if( any(tmp        /= [3,2,4]) ) ERROR STOP 25
! ----> send this one to C and check contiguity/values, size and other things 

end program AllocatableDummyArgument502f
