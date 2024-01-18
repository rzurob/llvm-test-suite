!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocFromIsUnallocated01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 29 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. FROM IS UNALLOCATED,CALL MOVE_ALLOC IN MAIN PROGRAM
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
      integer,kind :: k
      integer,len  :: l
      integer(k)   :: i(l)
   end type
end module

program move_allocFromIsUnallocated01

  use m
  implicit none

  integer :: i

  type(dtp(2,4)),allocatable :: dtp1
  type(dtp(2,4)),allocatable :: dtp2
  type(dtp(2,:)),allocatable :: dtp3

  class(dtp(2,4)),allocatable :: dtp4
  class(dtp(2,:)),allocatable :: dtp5 

  if(allocated(dtp1))                    error stop 10_4
  if(allocated(dtp2))                    error stop 11_4
  if(allocated(dtp3))                    error stop 12_4
  if(allocated(dtp4))                    error stop 13_4
  if(allocated(dtp5))                    error stop 14_4

  call move_alloc(dtp1,dtp2)
  call move_alloc(dtp1,dtp3)
  call move_alloc(dtp1,dtp4)
  call move_alloc(dtp1,dtp5)
  
  if(allocated(dtp2))                    error stop 15_4
  if(allocated(dtp3))                    error stop 16_4
  if(allocated(dtp4))                    error stop 17_4
  if(allocated(dtp5))                    error stop 18_4

  call move_alloc(dtp3,dtp2)
  call move_alloc(dtp3,dtp1)
  call move_alloc(dtp3,dtp4)
  call move_alloc(dtp3,dtp5)

  if(allocated(dtp1))                    error stop 19_4 
  if(allocated(dtp2))                    error stop 20_4
  if(allocated(dtp3))                    error stop 21_4
  if(allocated(dtp4))                    error stop 22_4
  if(allocated(dtp5))                    error stop 23_4

  allocate(dtp1,source=dtp(2,4)([1,2,3,4]))

  dtp2=dtp1
  dtp3=dtp1

  allocate(dtp4,source=dtp1)
  allocate(dtp5,source=dtp1)

  if(.not. allocated(dtp1))              error stop 24_4
  if(.not. allocated(dtp2))              error stop 25_4
  if(.not. allocated(dtp3))              error stop 26_4
  if(.not. allocated(dtp4))              error stop 27_4
  if(.not. allocated(dtp5))              error stop 28_4

  if(any(dtp1%i /= [1,2,3,4] ))          error stop 29_4
  if(any(dtp2%i /= [1,2,3,4] ))          error stop 30_4
  if(any(dtp3%i /= [1,2,3,4] ))          error stop 31_4
  if(any(dtp4%i /= [1,2,3,4] ))          error stop 32_4
  if(any(dtp5%i /= [1,2,3,4] ))          error stop 33_4

  deallocate(dtp1)
  call move_alloc(dtp1,dtp2)
  call move_alloc(dtp1,dtp3)
  call move_alloc(dtp1,dtp4)
  call move_alloc(dtp1,dtp5)

  if(allocated(dtp1))                    error stop 34_4
  if(allocated(dtp2))                    error stop 35_4
  if(allocated(dtp3))                    error stop 36_4
  if(allocated(dtp4))                    error stop 37_4
  if(allocated(dtp5))                    error stop 38_4  

end program

