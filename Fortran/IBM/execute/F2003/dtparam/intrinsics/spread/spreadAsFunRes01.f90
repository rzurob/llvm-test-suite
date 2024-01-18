!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadAsFunRes01.f
!*
!*  DATE                       : Oct. 20 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. USE SPREAD AS FUNCTION RESULT,PASS SOURCE,DIM,NCOPIES AS ACTUAL ARGUMENT
!*  3. USE DIFFERENT FUNCTIONS (INTERAL,EXTERNAL)
!*  4. SOURCE HAS DIFFERENT DIMENSIONS
!*  5. DERIVED TYPE HAS INTEGER AND CHARACTER ARRAY COMPONENT
!*  6. DEFECT 357751
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
     integer(2),kind :: k
     integer,len     :: l
     integer(k)      :: i(l)
     character(l)    :: c(l-1:l+1)
   end type

   contains
      function getSpreadResult3(source,dim,ncopies)
          type(dtp(2,*)),intent(in) :: source(:,:)
          type(dtp(2,:)),allocatable:: getSpreadResult3(:,:,:)
          integer :: dim,ncopies
          getSpreadResult3=spread(source,dim,ncopies)
      end function

end module

program spreadAsFunRes01
  use m
  implicit none

  interface
     function getSpreadResult2(source,dim,ncopies)
        import dtp
        type(dtp(2,*)),intent(in) :: source(:)
        integer,intent(in) :: dim,ncopies
        type(dtp(2,source%l)),allocatable :: getSpreadResult2(:,:)
    end function
  end interface

  integer :: i

  type(dtp(2,3)) :: dtp1=dtp(2,3)(i=[1,2,3],c=["1","2","3"])
  type(dtp(2,:)),allocatable :: dtp2(:)
  type(dtp(2,:)),allocatable :: dtp3(:,:)
  type(dtp(2,:)),allocatable :: dtp4(:,:,:)

  dtp2=getSpreadResult1(dtp1,1,4)

  if(.not. allocated(dtp2))                          error stop 10_4
  if(size(dtp2,1) /= 4)                              error stop 11_4
  if(dtp2%k /= 2)                                    error stop 12_4
  if(dtp2%l /= 3)                                    error stop 13_4

  do i=1,4
    if(any(dtp2(i)%i /= [1,2,3]))                    error stop 14_4
    if(any(dtp2(i)%c /= ["1","2","3"]))              error stop 15_4
    if(dtp2(i)%i%kind /= 2)                          error stop 16_4
    if(lbound(dtp2(i)%c,1) /= 2)                     error stop 17_4
    if(ubound(dtp2(i)%c,1) /= 4)                     error stop 18_4
    if(dtp2(i)%c%len /= 3)                           error stop 19_4
  end do


  dtp3=getSpreadResult2([dtp1,dtp(2,3)(i=[3,4,5],c=["3","4","5"])],1,5)

  if(dtp3%k /= 2)                                     error stop 20_4
  if(dtp3%l /= 3)                                     error stop 21_4
  do i=1,5
     if(dtp3(i,1)%i%kind /= 2)                        error stop 22_4
     if(dtp3(i,2)%i%kind /= 2)                        error stop 23_4
     if(size(dtp3(i,1)%i,1) /= 3)                     error stop 24_4
     if(size(dtp3(i,2)%i,1) /= 3)                     error stop 25_4
     if(any(dtp3(i,1)%i /= [1,2,3]))                  error stop 26_4
     if(any(dtp3(i,2)%i /= [3,4,5]))                  error stop 27_4
     if(any(dtp3(i,1)%c /= ["1","2","3"]))            error stop 28_4
     if(any(dtp3(i,2)%c /= ["3","4","5"]))            error stop 29_4

     if(dtp3(i,1)%c%len /= 3)                         error stop 30_4
     if(dtp3(i,2)%c%len /= 3)                         error stop 31_4
     if(lbound(dtp3(i,1)%c,1) /= 2)                   error stop 32_4
     if(lbound(dtp3(i,2)%c,1) /= 2)                   error stop 33_4
     if(ubound(dtp3(i,1)%c,1) /= 4)                   error stop 34_4
     if(ubound(dtp3(i,2)%c,1) /= 4)                   error stop 35_4

  end do

  if(allocated(dtp3))  deallocate(dtp3)

  dtp3=getSpreadResult2([dtp1,dtp(2,3)(i=[3,4,5],c=["3","4","5"])],2,5)

  if(dtp3%k /= 2)                                     error stop 36_4
  if(dtp3%l /= 3)                                     error stop 37_4
  do i=1,5
     if(dtp3(1,i)%i%kind /= 2)                        error stop 38_4
     if(dtp3(2,i)%i%kind /= 2)                        error stop 39_4
     if(size(dtp3(1,i)%i,1) /= 3)                     error stop 40_4
     if(size(dtp3(2,i)%i,1) /= 3)                     error stop 41_4
     if(any(dtp3(1,i)%i /= [1,2,3]))                  error stop 42_4
     if(any(dtp3(2,i)%i /= [3,4,5]))                  error stop 43_4
     if(any(dtp3(1,i)%c /= ["1","2","3"]))            error stop 44_4
     if(any(dtp3(2,i)%c /= ["3","4","5"]))            error stop 45_4

     if(dtp3(1,i)%c%len /= 3)                         error stop 46_4
     if(dtp3(2,i)%c%len /= 3)                         error stop 47_4
     if(lbound(dtp3(1,i)%c,1) /= 2)                   error stop 48_4
     if(lbound(dtp3(2,i)%c,1) /= 2)                   error stop 49_4
     if(ubound(dtp3(1,i)%c,1) /= 4)                   error stop 50_4
     if(ubound(dtp3(2,i)%c,1) /= 4)                   error stop 51_4

  end do

  if(allocated(dtp2)) deallocate(dtp2)
  if(allocated(dtp3)) deallocate(dtp3)

  allocate(dtp2(4),source=[dtp(2,3)([1,2,3],["1","2","3"]), &
                           dtp(2,3)([-1,-2,-3],["-1","-2","-3"]), &
                           dtp(2,3)([4,5,6],["4","5","6"]),&
                           dtp(2,3)([-4,-5,-6],["-4","-5","-6"]) ] )

  allocate(dtp3(2,2),source=reshape(dtp2,(/2,2/)) )

  dtp4=getSpreadResult3(dtp3,1,5)

  if(dtp4%k /= 2)                                     error stop 52_4
  if(dtp4%l /= 3)                                     error stop 53_4
  do i=1,5
     if(any(dtp4(i,1,1)%i /= [1,2,3]))                error stop 54_4
     print *,dtp4(i,2,1)%i
     if(any(dtp4(i,2,1)%i /= [-1,-2,-3]))             error stop 55_4
     if(any(dtp4(i,1,2)%i /= [4,5,6]))                error stop 56_4
     if(any(dtp4(i,2,2)%i /= [-4,-5,-6]))             error stop 57_4

     if(any(dtp4(i,1,1)%c /= ["1","2","3"]))          error stop 58_4
     if(any(dtp4(i,2,1)%c /= ["-1","-2","-3"]))       error stop 59_4
     if(any(dtp4(i,1,2)%c /= ["4","5","6"]))          error stop 60_4
     if(any(dtp4(i,2,2)%c /= ["-4","-5","-6"]))       error stop 61_4
  end do


  if(allocated(dtp4))   deallocate(dtp4)

  dtp4=getSpreadResult3(dtp3,2,5)

  if(dtp4%k /= 2)                                     error stop 62_4
  if(dtp4%l /= 3)                                     error stop 63_4
  do i=1,5
     if(any(dtp4(1,i,1)%i /= [1,2,3]))                error stop 64_4
     if(any(dtp4(2,i,1)%i /= [-1,-2,-3]))             error stop 65_4
     if(any(dtp4(1,i,2)%i /= [4,5,6]))                error stop 66_4
     if(any(dtp4(2,i,2)%i /= [-4,-5,-6]))             error stop 67_4

     if(any(dtp4(1,i,1)%c /= ["1","2","3"]))          error stop 68_4
     if(any(dtp4(2,i,1)%c /= ["-1","-2","-3"]))       error stop 69_4
     if(any(dtp4(1,i,2)%c /= ["4","5","6"]))          error stop 70_4
     if(any(dtp4(2,i,2)%c /= ["-4","-5","-6"]))       error stop 71_4
  end do

  if(allocated(dtp4))   deallocate(dtp4)

  dtp4=getSpreadResult3(dtp3,3,5)

  if(dtp4%k /= 2)                                     error stop 72_4
  if(dtp4%l /= 3)                                     error stop 73_4
  do i=1,5
     if(any(dtp4(1,1,i)%i /= [1,2,3]))                error stop 74_4
     if(any(dtp4(2,1,i)%i /= [-1,-2,-3]))             error stop 75_4
     if(any(dtp4(1,2,i)%i /= [4,5,6]))                error stop 76_4
     if(any(dtp4(2,2,i)%i /= [-4,-5,-6]))             error stop 77_4

     if(any(dtp4(1,1,i)%c /= ["1","2","3"]))          error stop 78_4
     if(any(dtp4(2,1,i)%c /= ["-1","-2","-3"]))       error stop 79_4
     if(any(dtp4(1,2,i)%c /= ["4","5","6"]))          error stop 80_4
     if(any(dtp4(2,2,i)%c /= ["-4","-5","-6"]))       error stop 81_4
  end do
  contains

     function getSpreadResult1(source,dim,ncopies)

          type(dtp(2,*)),intent(in)  :: source
          integer,intent(in) :: dim,ncopies
          type(dtp(2,source%l)),allocatable :: getSpreadResult1(:)

          getSpreadResult1=spread(source,dim,ncopies)
     end function
end program

function getSpreadResult2(source,dim,ncopies)
   use m,only : dtp
   type(dtp(2,*)),intent(in) :: source(:)
   integer,intent(in) :: dim,ncopies
   type(dtp(2,source%l)),allocatable :: getSpreadResult2(:,:)

   getSpreadResult2=spread(source,dim,ncopies)
end function
