!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : January 22, 2002
!
!*  PRIMARY FUNCTIONS TESTED   : Allocatable attribute
!*
!*  SECONDARY FUNCTIONS TESTED : ALLOCATE, ALLOCATED, DEALLOCATE
!*
!*  DESCRIPTION                : Tests allocatable attribute on real,
!*                               the allocated intrinsic procedure, along
!*                               with deallocate and allocate
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  : Allocatable real
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/21/01   1.0    -Initial Version
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program allocins001
use,intrinsic :: iso_fortran_env

implicit none
logical precision_r8

real,parameter, dimension(4) :: index_array = (/0.000093E+1,0.700693E+30,1.797693E+37,1.225074E-31/)
real(real32),allocatable :: r1
real(2),allocatable :: r2
real(4),allocatable :: r4

! Test 1 - Before allocate the reals

if(allocated(r1) .or. allocated(r2) .or. &
   allocated(r4) ) error stop 1

! Test 2 - Allocate the reals and make sure allocation
!          status changes

allocate (r1,r2,r4)

if ((.not.allocated(r1)) .or. (.not.allocated(r2)) .or. &
    (.not.allocated(r4)) ) then
error stop 2
endif

r1= index_array(1)
r2= index_array(2)
r4= index_array(3)

if (precision_r8(r1,0.000093E+1) .or. precision_r8(r2,0.700693E+30) .or. &
   precision_r8(r4 , 1.225074E-31) ) error stop 3

! Test 3 - Deallocate the reals and make sure that the
!          the reals have been deallocated

deallocate (r1, r2, r4)

if(allocated(r1) .or. allocated(r2) .or. &
   allocated(r4) ) error stop 4

end program allocins001
