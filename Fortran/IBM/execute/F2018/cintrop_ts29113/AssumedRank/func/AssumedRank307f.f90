! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AssumedRank307f.f
!*
!* PROGRAMMER                   : Dorra Bouchiha
!* DATE                         : October 27, 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    : 
!*                               (use -D_DEBUG for a debug version)
!*
!* DESCRIPTION                  : Calling a BIND(C) procedure defined in C from Fortran 
!*                                - array is rank 0, 1, 2, 15
!*                                - allocatable
!*                                - intent(out) 
!*                                - type c_long
!*
!* When a C function is invoked from a Fortran procedure via an interface with an INTENT(OUT) allocatable
!* dummy argument, and the actual argument in the reference to the C function is an allocated allocatable variable,
!* the variable is deallocated on invocation (before execution of the C function begins).
!*
!* Actua15 Argument:
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
program AssumedRank307f
use, intrinsic :: iso_c_binding
implicit none

interface
    subroutine c_check(a) bind(c)
        import :: c_long
        integer(c_long), allocatable, intent(out) :: a(..)
    end
end interface

integer        :: st, i, j, k 
character(200) :: msg
integer(c_long), allocatable :: a0
integer(c_long), allocatable :: a1(:), a2(:,:), a3(:,:,:)
integer(c_long), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)

     
! Allocate all the allocatable arrays 
! 
a0 = -1

allocate(a1(10), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 1
endif
a1 = [(i**2, i=1, 10)]

allocate(a2(2,3), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 2
endif

allocate(a3(-4:5,-2:3,0:0), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 3
endif

allocate(a15(1,1,1,1,1,1,1,1,1,1,1,1,1,1,3), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 4
endif

! Verify alllocation status, rank, shape, size, bounds and contiguity in Fortran
if(                                .not. allocated(a0) ) ERROR STOP 10
if( a0             /=                               -1 ) ERROR STOP 11

if(                                .not. allocated(a1) ) ERROR STOP 20
if(                            .not. is_contiguous(a1) ) ERROR STOP 21
if( rank(a1)       /=                                1 ) ERROR STOP 22
if( size(a1)       /=                               10 ) ERROR STOP 23
if( any(shape(a1)  /=                            [10]) ) ERROR STOP 24
if( any(lbound(a1) /=                             [1]) ) ERROR STOP 25
if( any(ubound(a1) /=                            [10]) ) ERROR STOP 26

if(                                .not. allocated(a2) ) ERROR STOP 30
if(                            .not. is_contiguous(a2) ) ERROR STOP 31
if( rank(a2)       /=                                2 ) ERROR STOP 32
if( size(a2)       /=                                6 ) ERROR STOP 33
if( any(shape(a2)  /=                           [2,3]) ) ERROR STOP 34
if( any(lbound(a2) /=                           [1,1]) ) ERROR STOP 35
if( any(ubound(a2) /=                           [2,3]) ) ERROR STOP 36

if(                                .not. allocated(a3) ) ERROR STOP 40
if(                            .not. is_contiguous(a3) ) ERROR STOP 41
if( rank(a3)       /=                                3 ) ERROR STOP 42
if( size(a3)       /=                               60 ) ERROR STOP 43
if( any(shape(a3)  /=                        [10,6,1]) ) ERROR STOP 44
if( any(lbound(a3) /=                       [-4,-2,0]) ) ERROR STOP 45
if( any(ubound(a3) /=                         [5,3,0]) ) ERROR STOP 46

if(                                .not. allocated(a15) ) ERROR STOP 50
if(                            .not. is_contiguous(a15) ) ERROR STOP 51
if( rank(a15)       /=                               15 ) ERROR STOP 52
if( size(a15)       /=                                3 ) ERROR STOP 53
if( any(shape(a15)  /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 54
if( any(lbound(a15) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) ) ERROR STOP 55
if( any(ubound(a15) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 56

! Verify alllocation status
call c_check(a0)
call c_check(a1)
call c_check(a2)
call c_check(a3)
call c_check(a15)

! Verify again 
if(  allocated(a0) ) ERROR STOP 110
if(  allocated(a1) ) ERROR STOP 120
if(  allocated(a2) ) ERROR STOP 130
if(  allocated(a3) ) ERROR STOP 140
if( allocated(a15) ) ERROR STOP 150

end program AssumedRank307f
