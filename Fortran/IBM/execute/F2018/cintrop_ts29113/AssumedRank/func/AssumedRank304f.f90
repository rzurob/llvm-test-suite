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
!*                                - array is rank 0, 1, 2, 15
!*                                - allocatable
!*                                - type c_int
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
program AssumedRank304f
use, intrinsic :: iso_c_binding
implicit none

interface
    subroutine c_check(a) bind(c)
        import :: c_int
        integer(c_int), allocatable :: a(..)
    end
end interface

integer        :: st, i, j, k
character(200) :: msg
integer(c_int), allocatable :: a0
integer(c_int), allocatable :: a1(:), a2(:,:), a3(:,:,:)
integer(c_int), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)


! Allocate all the allocatable arrays
!
a0 = -1

allocate(a1(10), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 1
endif
a1 = [(i**2, i=1, 10)]

allocate(a2(2,3), source=reshape([3, -2, 2, 4, 1, 1], [2,3]), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 2
endif

allocate(a3(-4:5,-2:3,0:0), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 3
endif

do i = lbound(a3,1), ubound(a3,1), 1
   do j = lbound(a3,2), ubound(a3,2), 1
      do k = lbound(a3,3), ubound(a3,3), 1
         a3(i,j,k)= i + j + k
      end do
   end do
end do


allocate(a15(1,1,1,1,1,1,1,1,1,1,1,1,1,1,3), stat=st, errmsg=msg)
if( st .NE. 0 ) then
   print *, msg
   ERROR STOP 4
endif
a15(1,1,1,1,1,1,1,1,1,1,1,1,1,1,:) = [3,-2,4]

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
if( any(a1         /=               [(i**2, i=1, 10)]) ) ERROR STOP 27

if(                                .not. allocated(a2) ) ERROR STOP 30
if(                            .not. is_contiguous(a2) ) ERROR STOP 31
if( rank(a2)       /=                                2 ) ERROR STOP 32
if( size(a2)       /=                                6 ) ERROR STOP 33
if( any(shape(a2)  /=                           [2,3]) ) ERROR STOP 34
if( any(lbound(a2) /=                           [1,1]) ) ERROR STOP 35
if( any(ubound(a2) /=                           [2,3]) ) ERROR STOP 36
if( any(a2     /= reshape([3, -2, 2, 4, 1, 1], [2,3])) ) ERROR STOP 37

if(                                .not. allocated(a3) ) ERROR STOP 40
if(                            .not. is_contiguous(a3) ) ERROR STOP 41
if( rank(a3)       /=                                3 ) ERROR STOP 42
if( size(a3)       /=                               60 ) ERROR STOP 43
if( any(shape(a3)  /=                        [10,6,1]) ) ERROR STOP 44
if( any(lbound(a3) /=                       [-4,-2,0]) ) ERROR STOP 45
if( any(ubound(a3) /=                         [5,3,0]) ) ERROR STOP 46
do i = lbound(a3,1), ubound(a3,1), 1
   do j = lbound(a3,2), ubound(a3,2), 1
      do k = lbound(a3,3), ubound(a3,3), 1
         if( a3(i,j,k)     /=              (i + j + k) ) ERROR STOP 47
      end do
   end do
end do

if(                                .not. allocated(a15) ) ERROR STOP 50
if(                            .not. is_contiguous(a15) ) ERROR STOP 51
if( rank(a15)       /=                               15 ) ERROR STOP 52
if( size(a15)       /=                                3 ) ERROR STOP 53
if( any(shape(a15)  /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 54
if( any(lbound(a15) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) ) ERROR STOP 55
if( any(ubound(a15) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 56
if( any(a15(1,1,1,1,1,1,1,1,1,1,1,1,1,1,:) /= [3,-2,4]) ) ERROR STOP 57

! Verify alllocation status, rank, shape, size, bounds and contiguity in C
call c_check(a0)
call c_check(a1)
call c_check(a2)
call c_check(a3)
call c_check(a15)

! Verify again
if(                                .not. allocated(a0) ) ERROR STOP 110
if( a0             /=                               -1 ) ERROR STOP 111

if(                                .not. allocated(a1) ) ERROR STOP 120
if(                            .not. is_contiguous(a1) ) ERROR STOP 121
if( rank(a1)       /=                                1 ) ERROR STOP 122
if( size(a1)       /=                               10 ) ERROR STOP 123
if( any(shape(a1)  /=                            [10]) ) ERROR STOP 124
if( any(lbound(a1) /=                             [1]) ) ERROR STOP 125
if( any(ubound(a1) /=                            [10]) ) ERROR STOP 126
if( any(a1         /=               [(i**2, i=1, 10)]) ) ERROR STOP 127

if(                                .not. allocated(a2) ) ERROR STOP 130
if(                            .not. is_contiguous(a2) ) ERROR STOP 131
if( rank(a2)       /=                                2 ) ERROR STOP 132
if( size(a2)       /=                                6 ) ERROR STOP 133
if( any(shape(a2)  /=                           [2,3]) ) ERROR STOP 134
if( any(lbound(a2) /=                           [1,1]) ) ERROR STOP 135
if( any(ubound(a2) /=                           [2,3]) ) ERROR STOP 136
if( any(a2     /= reshape([3, -2, 2, 4, 1, 1], [2,3])) ) ERROR STOP 137

if(                                .not. allocated(a3) ) ERROR STOP 140
if(                            .not. is_contiguous(a3) ) ERROR STOP 141
if( rank(a3)       /=                                3 ) ERROR STOP 142
if( size(a3)       /=                               60 ) ERROR STOP 143
if( any(shape(a3)  /=                        [10,6,1]) ) ERROR STOP 144
if( any(lbound(a3) /=                       [-4,-2,0]) ) ERROR STOP 145
if( any(ubound(a3) /=                         [5,3,0]) ) ERROR STOP 146
do i = lbound(a3,1), ubound(a3,1), 1
   do j = lbound(a3,2), ubound(a3,2), 1
      do k = lbound(a3,3), ubound(a3,3), 1
         if( a3(i,j,k)     /=              (i + j + k) ) ERROR STOP 147
      end do
   end do
end do

if(                                .not. allocated(a15) ) ERROR STOP 150
if(                            .not. is_contiguous(a15) ) ERROR STOP 151
if( rank(a15)       /=                               15 ) ERROR STOP 152
if( size(a15)       /=                                3 ) ERROR STOP 153
if( any(shape(a15)  /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 154
if( any(lbound(a15) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) ) ERROR STOP 155
if( any(ubound(a15) /= [1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]) ) ERROR STOP 156
if( any(a15(1,1,1,1,1,1,1,1,1,1,1,1,1,1,:) /= [3,-2,4]) ) ERROR STOP 157

end program AssumedRank304f
