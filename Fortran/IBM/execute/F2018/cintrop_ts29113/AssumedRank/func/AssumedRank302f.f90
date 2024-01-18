! *********************************************************************
!* ===================================================================
!*
!* DATE                         : October 27, 2013
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed rank dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*                               (use -D_DEBUG for a debug version)
!*
!* DESCRIPTION                  : Calling a BIND(C) procedure defined in C from Fortran
!*                                - array is rank 0, 1, 2, 3
!*                                - allocatable
!*                                - Verify value / Change value on the C-side
!*                                - Generic resolution based on type
!*
!* Actua1 Argument:
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
program AssumedRank302f
use, intrinsic :: iso_c_binding
implicit none

interface c_check
    subroutine check_int(arg) bind(c)
        import :: c_int
        integer(c_int), allocatable :: arg(..)
    end
    subroutine check_short(arg) bind(c)
        import :: c_short
        integer(c_short), allocatable :: arg(..)
    end
    subroutine check_complex(arg) bind(c)
        import :: c_float_complex
        complex(c_float_complex), allocatable :: arg(..)
    end
    subroutine check_float(arg) bind(c)
        import :: c_float
        real(c_float), allocatable :: arg(..)
    end
end interface

integer        :: st, i, j, k
character(200) :: msg
integer(c_int), allocatable :: a0, a1(:)
integer(c_short), allocatable :: s3(:,:,:)
real(c_float), allocatable :: r0, r2(:,:)
complex(c_float_complex), allocatable :: z0, z1(:)
logical precision_r4, precision_x8

! Allocate all the allocatable arrays
a0 = -1

allocate(a1(10), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 1
endif
a1 = [(i**2, i=1, 10)]

allocate(s3(-4:5,-2:3,0:0), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 2
endif
do i = lbound(s3,1), ubound(s3,1), 1
   do j = lbound(s3,2), ubound(s3,2), 1
      do k = lbound(s3,3), ubound(s3,3), 1
         s3(i,j,k)= i + j + k
      end do
   end do
end do

allocate(r2(2,3), stat=st, errmsg=msg)
r2 = reshape([3., -2., 2., 4., 1., 1.], [2,3])
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 3
endif

allocate(r0, source=r2(2,2), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 4
endif

allocate(z0, source=CMPLX(-1.0,4.0), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 5
endif

allocate(z1(-2:-1), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 6
endif
z1 = [(CMPLX(i, i+1), i=1, size(z1))]

! Verify alllocation status, rank, shape, size, bounds and contiguity in Fortran
if(                      .not. allocated(a0) ) ERROR STOP 10
if( a0             /=                     -1 ) ERROR STOP 11

if(                      .not. allocated(a1) ) ERROR STOP 20
if(                  .not. is_contiguous(a1) ) ERROR STOP 21
if( rank(a1)       /=                      1 ) ERROR STOP 22
if( size(a1)       /=                     10 ) ERROR STOP 23
if( any(shape(a1)  /=                  [10]) ) ERROR STOP 24
if( any(lbound(a1) /=                   [1]) ) ERROR STOP 25
if( any(ubound(a1) /=                  [10]) ) ERROR STOP 26
if( any(a1         /=     [(i**2, i=1, 10)]) ) ERROR STOP 27

if(                      .not. allocated(r0) ) ERROR STOP 30
if(              .not.  precision_r4(r0, 4.) ) ERROR STOP 31
if(                      .not. allocated(r2) ) ERROR STOP 32
if(                  .not. is_contiguous(r2) ) ERROR STOP 33
if( rank(r2)       /=                      2 ) ERROR STOP 34
if( size(r2)       /=                      6 ) ERROR STOP 35
if( any(shape(r2)  /=                 [2,3]) ) ERROR STOP 36
if( any(lbound(r2) /=                 [1,1]) ) ERROR STOP 37
if( any(ubound(r2) /=                 [2,3]) ) ERROR STOP 38
if(         .not.  precision_r4(r2(1,1), 3.) ) ERROR STOP 39
if(         .not. precision_r4(r2(2,1), -2.) ) ERROR STOP 40
if(         .not.  precision_r4(r2(1,2), 2.) ) ERROR STOP 41
if(         .not.  precision_r4(r2(2,2), 4.) ) ERROR STOP 42
if(         .not.  precision_r4(r2(1,3), 1.) ) ERROR STOP 43
if(         .not.  precision_r4(r2(2,3), 1.) ) ERROR STOP 44

if(                      .not. allocated(s3) ) ERROR STOP 50
if(                  .not. is_contiguous(s3) ) ERROR STOP 51
if( rank(s3)       /=                      3 ) ERROR STOP 52
if( size(s3)       /=                     60 ) ERROR STOP 53
if( any(shape(s3)  /=              [10,6,1]) ) ERROR STOP 54
if( any(lbound(s3) /=             [-4,-2,0]) ) ERROR STOP 55
if( any(ubound(s3) /=               [5,3,0]) ) ERROR STOP 56
do i = lbound(s3,1), ubound(s3,1), 1
   do j = lbound(s3,2), ubound(s3,2), 1
      do k = lbound(s3,3), ubound(s3,3), 1
         if( s3(i,j,k)     /=    (i + j + k) ) ERROR STOP 57
      end do
   end do
end do

if(                      .not. allocated(z0) ) ERROR STOP 60
if(       .not. precision_x8(z0, (-1.0,4.0)) ) ERROR STOP 61
if(                      .not. allocated(z1) ) ERROR STOP 62
if(                  .not. is_contiguous(z1) ) ERROR STOP 63
if( rank(z1)       /=                      1 ) ERROR STOP 64
if( size(z1)       /=                      2 ) ERROR STOP 65
if( any(shape(z1)  /=                   [2]) ) ERROR STOP 66
if( any(lbound(z1) /=                  [-2]) ) ERROR STOP 67
if( any(ubound(z1) /=                  [-1]) ) ERROR STOP 68
do i = lbound(z1,1), ubound(z1,1)
    if( .not. precision_x8(z1(i), (i+3.0,i+4.0)) ) ERROR STOP 69
end do

! Verify alllocation status, rank, shape, size, bounds and contiguity in C
call c_check(a0)
call c_check(a1)
call c_check(s3)
call c_check(r0)
call c_check(r2)
call c_check(z0)
call c_check(z1)

! Verify again
if(                                .not. allocated(a0) ) ERROR STOP 110
if( a0             /=                              -99 ) ERROR STOP 111

if(                                .not. allocated(a1) ) ERROR STOP 120
if(                            .not. is_contiguous(a1) ) ERROR STOP 121
if( rank(a1)       /=                                1 ) ERROR STOP 122
if( size(a1)       /=                               10 ) ERROR STOP 123
if( any(shape(a1)  /=                            [10]) ) ERROR STOP 124
if( any(lbound(a1) /=                             [1]) ) ERROR STOP 125
if( any(ubound(a1) /=                            [10]) ) ERROR STOP 126
if( any(a1         /=                  [(i, i=-5, 4)]) ) ERROR STOP 127

if(                                .not. allocated(r0) ) ERROR STOP 130
if(                       .not.  precision_r4(r0, 3.1) ) ERROR STOP 131
if(                                .not. allocated(r2) ) ERROR STOP 132
if(                            .not. is_contiguous(r2) ) ERROR STOP 133
if( rank(r2)       /=                                2 ) ERROR STOP 134
if( size(r2)       /=                                6 ) ERROR STOP 135
if( any(shape(r2)  /=                           [2,3]) ) ERROR STOP 136
if( any(lbound(r2) /=                           [1,1]) ) ERROR STOP 137
if( any(ubound(r2) /=                           [2,3]) ) ERROR STOP 138

if(                                .not. allocated(s3) ) ERROR STOP 140
if(                            .not. is_contiguous(s3) ) ERROR STOP 141
if( rank(s3)       /=                                3 ) ERROR STOP 142
if( size(s3)       /=                               60 ) ERROR STOP 143
if( any(shape(s3)  /=                        [10,6,1]) ) ERROR STOP 144
if( any(lbound(s3) /=                       [-4,-2,0]) ) ERROR STOP 145
if( any(ubound(s3) /=                         [5,3,0]) ) ERROR STOP 146

if(                                .not. allocated(z0) ) ERROR STOP 160
if(                 .not. precision_x8(z0, (1.01,2.2)) ) ERROR STOP 161
if(                                .not. allocated(z1) ) ERROR STOP 162
if(                            .not. is_contiguous(z1) ) ERROR STOP 163
if( rank(z1)       /=                                1 ) ERROR STOP 164
if( size(z1)       /=                                2 ) ERROR STOP 165
if( any(shape(z1)  /=                             [2]) ) ERROR STOP 166
if( any(lbound(z1) /=                            [-2]) ) ERROR STOP 167
if( any(ubound(z1) /=                            [-1]) ) ERROR STOP 168

end program AssumedRank302f
