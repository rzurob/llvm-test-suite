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
!* DESCRIPTION                  : Calling a BIND(C) procedure defined in C from Fortran
!*                                - array is rank 0, 1, 2, 15
!*                                - allocatable
!*                                - intent(out)
!*                                - type c_float
!*
!* When a C function is invoked from a Fortran procedure via an interface with an INTENT(OUT) allocatable
!* dummy argument, and the actual argument in the reference to the C function is an allocated allocatable variable,
!* the variable is deallocated on invocation (before execution of the C function begins).
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
program AssumedRank308f
use, intrinsic :: iso_c_binding
implicit none

interface
    subroutine c_modify(a) bind(c)
        import :: c_float
        real(c_float), allocatable, intent(out) :: a(..)
    end
end interface

integer        :: st, i, j, k
character(200) :: msg
real(c_float), allocatable :: a0
real(c_float), allocatable :: a1(:), a2(:,:)
real(c_float), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
logical precision_r4


! Allocate all the allocatable objects
allocate(a0, stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 1
endif

allocate(a1(10), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 2
endif

allocate(a2(2,3), stat=st, errmsg=msg)
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

if(                                .not. allocated(a15) ) ERROR STOP 40
if(                            .not. is_contiguous(a15) ) ERROR STOP 41
if( rank(a15)       /=                               15 ) ERROR STOP 42
if( size(a15)       /=                                3 ) ERROR STOP 43
if( any(shape(a15)  /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 44
if( any(lbound(a15) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) ) ERROR STOP 45
if( any(ubound(a15) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 46

! Allocate in C with different values / bounds
call c_modify(a0)
call c_modify(a1)
call c_modify(a2)
call c_modify(a15)

! Verify the new allocation status and rank, shape, size, bounds and contiguity
if( .not.         allocated(a0) ) ERROR STOP 100
if( .not. precision_r4(a0, 3.1) ) ERROR STOP 101

if(         .not. allocated(a1) ) ERROR STOP 110
if(     .not. is_contiguous(a1) ) ERROR STOP 111
if( rank(a1)         /=       1 ) ERROR STOP 112
if( size(a1)         /=      11 ) ERROR STOP 113
if( any(shape(a1)    /=   [11]) ) ERROR STOP 114
if( any(lbound(a1)   /=   [-5]) ) ERROR STOP 115
if( any(ubound(a1)   /=    [5]) ) ERROR STOP 116

do i = lbound(a1,1), ubound(a1,1)
  if( .not. precision_r4(a1(i), i*1.0) ) ERROR STOP 117
end do

if(         .not. allocated(a2) ) ERROR STOP 120
if(     .not. is_contiguous(a2) ) ERROR STOP 121
if( rank(a2)       /=         2 ) ERROR STOP 122
if( size(a2)       /=         6 ) ERROR STOP 123
if( any(shape(a2)  /=    [2,3]) ) ERROR STOP 124
if( any(lbound(a2) /=   [-1,0]) ) ERROR STOP 125
if( any(ubound(a2) /=    [0,2]) ) ERROR STOP 126
if( .not. precision_r4(a2(-1,0),  3.0) ) ERROR STOP 127
if( .not. precision_r4(a2(-1,1),  2.0) ) ERROR STOP 128
if( .not. precision_r4(a2(-1,2),  1.0) ) ERROR STOP 129
if( .not. precision_r4(a2(0,0),  -2.0) ) ERROR STOP 130
if( .not. precision_r4(a2(0,1),   4.0) ) ERROR STOP 131
if( .not. precision_r4(a2(0,2),   1.0) ) ERROR STOP 132

if(                                .not. allocated(a15) ) ERROR STOP 140
if(                            .not. is_contiguous(a15) ) ERROR STOP 141
if( rank(a15)       /=                               15 ) ERROR STOP 142
if( size(a15)       /=                                3 ) ERROR STOP 143
if( any(shape(a15)  /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 144
if( any(lbound(a15) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) ) ERROR STOP 145
if( any(ubound(a15) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 146
if( any(a15(1,1,1,1,1,1,1,1,1,1,1,1,1,1,:) /= [3,-2,4]) ) ERROR STOP 147

end program AssumedRank308f
