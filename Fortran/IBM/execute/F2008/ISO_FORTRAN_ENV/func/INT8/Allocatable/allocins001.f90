!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : January 22, 2002
!
!*  PRIMARY FUNCTIONS TESTED   : Allocatable attribute
!*
!*  SECONDARY FUNCTIONS TESTED : ALLOCATE, ALLOCATED, DEALLOCATE
!*
!*  DESCRIPTION                : Tests allocatable attribute on integer,
!*                               the allocated intrinsic procedure, along
!*                               with deallocate and allocate
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  : Allocatable integer
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

integer,parameter, dimension(4) :: index_array = (/10,100,1000,10**4/)
integer(int8),allocatable :: int1
integer(2),allocatable :: int2
integer(4),allocatable :: int4

! Test 1 - Before allocate the integers

if(allocated(int1) .or. allocated(int2) .or. &
   allocated(int4) ) error stop 1

! Test 2 - Allocate the integers and make sure allocation
!          status changes

allocate (int1,int2,int4)

if ((.not.allocated(int1)) .or. (.not.allocated(int2)) .or. &
    (.not.allocated(int4)) ) then
error stop 2
endif

int1= index_array(1)
int2= index_array(2)
int4= index_array(3)

if ((int1 .ne. 10) .or. (int2 .ne. 100) .or. &
   (int4 .ne.1000) ) error stop 3

! Test 3 - Deallocate the integers and make sure that the
!          the integers have been deallocated

deallocate (int1, int2, int4)

if(allocated(int1) .or. allocated(int2) .or. &
   allocated(int4) ) error stop 4

end program allocins001

