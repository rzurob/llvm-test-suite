!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocToHasTargetAttribute02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 2 2008 
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
!*  2. IF TO HAS THE TARGET ATTRIBUTE,ANY POINTER ASSOCIATED WITH FROM ON ENTRY MOVE_ALLOC BECOMES CORRESPONDINGLY ASSOCIATED WITH TO. 
!*  3. FROM AND TO ARE ARRAY
!*  5. CALL MOVE_ALLOC IN SUBROUTINE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
      integer,kind :: k
      integer,len  :: l
      integer(2*k+2) :: i(l-3:l+3)
   end type
end module

program move_allocToHasTargetAttribute02

  use m
  implicit none
  
  interface

     subroutine sub1(arg1,arg2,arg3)
        import dtp
        type(dtp(1,2)),target,allocatable,intent(inout) :: arg1(:)
        type(dtp(1,:)),target,allocatable,intent(out) :: arg2(:)
        type(dtp(1,:)),pointer  :: arg3(:)
     end subroutine
     subroutine sub2(arg1,arg2,arg3)
        import dtp 
        type(dtp(1,2)),target,allocatable,intent(inout) :: arg1(:,:)
        type(dtp(1,2)),target,allocatable,intent(out) :: arg2(:,:)
        type(dtp(1,2)),pointer  :: arg3(:,:)

     end subroutine
  end interface 

  integer ::i,j,k 
  type(dtp(1,:)),pointer  :: dtp1(:)
  type(dtp(1,2)),target,allocatable :: dtp2(:)
  type(dtp(1,:)),target,allocatable :: dtp3(:) 

  type(dtp(1,2)),pointer  :: dtp4(:,:)
  type(dtp(1,2)),target,allocatable :: dtp5(:,:)
  type(dtp(1,2)),target,allocatable :: dtp6(:,:)

  call sub1(dtp2,dtp3,dtp1)
  
  if(allocated(dtp2))                                     error stop 10_4
  if(.not. allocated(dtp3))                               error stop 11_4
  if(.not. associated(dtp1))                              error stop 12_4
  if(dtp3%k /= 1)                                         error stop 13_4
  if(dtp3%l /= 2)                                         error stop 14_4
  do j=2,5
     if(dtp3(j)%i%kind /= 4)                              error stop 15_4
     if(lbound(dtp3(j)%i,1) /= -1)                        error stop 16_4
     if(ubound(dtp3(j)%i,1) /= 5)                         error stop 17_4 
     if(any(dtp3(j)%i /= j))                              error stop 18_4
  end do
  if(lbound(dtp3,1) /= 2)                                 error stop 19_4
  if(ubound(dtp3,1) /= 5)                                 error stop 20_4 

  if(dtp1%k /= 1)                                         error stop 21_4
  if(dtp1%l /= 2)                                         error stop 22_4
  do j=2,5
     if(dtp1(j)%i%kind /= 4)                              error stop 23_4
     if(lbound(dtp1(j)%i,1) /= -1)                        error stop 24_4
     if(ubound(dtp1(j)%i,1) /= 5)                         error stop 25_4
     if(any(dtp1(j)%i /= j))                              error stop 26_4
  end do
  if(lbound(dtp1,1) /= 2)                                 error stop 27_4
  if(ubound(dtp1,1) /= 5)                                 error stop 28_4

  allocate(dtp2(9),source=(/( dtp(1,2)(j),j=-1,-9,-1) /)  )
  dtp5=reshape(dtp2,(/3,3/))
  
  call sub2(dtp5,dtp6,dtp4)

  if(allocated(dtp5))                                     error stop 30_4
  if(.not. allocated(dtp6))                               error stop 31_4
  if(.not. associated(dtp4))                              error stop 32_4
  if(dtp6%k /= 1)                                         error stop 33_4
  if(dtp6%l /= 2)                                         error stop 34_4
  if(ubound(dtp6,1) /= 3)                                 error stop 35_4
  if(ubound(dtp6,2) /= 3)                                 error stop 36_4
 
  k=-1 
  do i=1,3
     do j=1,3
       if(any(dtp6(j,i)%i /= k))                          error stop 37_4
       k=k-1
     end do
  end do   

  if(dtp4%k /= 1)                                         error stop 38_4
  if(dtp4%l /= 2)                                         error stop 39_4
  if(ubound(dtp4,1) /= 3)                                 error stop 40_4
  if(ubound(dtp4,2) /= 3)                                 error stop 41_4

  k=-1
  do i=1,3
     do j=1,3
       if(any(dtp4(j,i)%i /= k))                          error stop 42_4
       k=k-1
     end do
  end do

end program
  subroutine sub1(arg1,arg2,arg3)
     use m
     type(dtp(1,2)),target,allocatable,intent(inout) :: arg1(:)
     type(dtp(1,:)),target,allocatable,intent(out) :: arg2(:)
     type(dtp(1,:)),pointer  :: arg3(:)
     
     allocate(arg1(2:5),source=(/( dtp(1,2)(i),i=2,5) /)  )
     arg3=>arg1
     call move_alloc(arg1,arg2)
  end subroutine

  subroutine sub2(arg1,arg2,arg3)
     use m
     type(dtp(1,2)),target,allocatable,intent(inout) :: arg1(:,:)
     type(dtp(1,2)),target,allocatable,intent(out) :: arg2(:,:)
     type(dtp(1,2)),pointer  :: arg3(:,:)

     arg3=>arg1     
     call move_alloc(arg1,arg2)
         
  end subroutine
