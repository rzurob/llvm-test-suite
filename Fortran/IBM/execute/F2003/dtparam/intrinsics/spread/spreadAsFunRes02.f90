!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadAsFunRes02.f
!*
!*  DATE                       : Oct. 21 2008
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
!*  5. SOURCE HAS DERIVED TYPE COMPONENT
!*  6. DEFECT 357751
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k1,l1)
      integer,kind :: k1
      integer,len  :: l1
      integer(k1)   :: i
      character(l1) :: c
   end type
   type contain(k2,l2)
     integer(2),kind :: k2
     integer,len     :: l2
     type(dtp(k2+k2,l2+1)) :: dtp1
   end type

   contains
      function getSpreadResult3(source,dim,ncopies)
          type(contain(1,*)),intent(in) :: source(:,:)
          type(contain(1,:)),allocatable:: getSpreadResult3(:,:,:)
          integer :: dim,ncopies
          getSpreadResult3=spread(source,dim,ncopies)
      end function

end module

program spreadAsFunRes02
  use m
  implicit none

  interface
     function getSpreadResult2(source,dim,ncopies)
        import contain
        type(contain(1,*)),intent(in) :: source(:)
        integer,intent(in) :: dim,ncopies
        type(contain(1,source%l2)),allocatable :: getSpreadResult2(:,:)
    end function
  end interface

  integer :: i

  type(contain(1,2)) :: contain1=contain(1,2)(dtp1=dtp(2,3)(i=1,c="1"))

  type(contain(1,:)),allocatable :: contain2(:)
  type(contain(1,:)),allocatable :: contain3(:,:)
  type(contain(1,:)),allocatable :: contain4(:,:,:)

  contain2=getSpreadResult1(contain1,1,5)

  if(.not. allocated(contain2))                          error stop 10_4
  if(size(contain2,1) /= 5)                              error stop 11_4
  if(contain2%k2 /= 1)                                   error stop 12_4
  if(contain2%l2 /= 2)                                   error stop 13_4

  do i=1,5
    if(contain2(i)%dtp1%k1 /= 2)                         error stop 14_4
    if(contain2(i)%dtp1%l1 /= 3)                         error stop 15_4
    if(contain2(i)%dtp1%i /= 1)                          error stop 16_4
    if(contain2(i)%dtp1%c /= "1")                        error stop 17_4
    if(contain2(i)%dtp1%i%kind /= 2)                     error stop 18_4
    if(contain2(i)%dtp1%c%len /= 3)                      error stop 19_4
  end do


  contain3=getSpreadResult2([contain1,contain(1,2)(dtp(2,3)(i=2,c="2"))],1,5)

  if(contain3%k2 /= 1)                                    error stop 20_4
  if(contain3%l2 /= 2)                                    error stop 21_4
  do i=1,5
     if(contain3(i,1)%dtp1%k1 /= 2)                       error stop 22_4
     if(contain3(i,1)%dtp1%l1 /= 3)                       error stop 23_4

     if(contain3(i,2)%dtp1%k1 /= 2)                       error stop 24_4
     if(contain3(i,2)%dtp1%l1 /= 3)                       error stop 25_4

     if(contain3(i,1)%dtp1%i%kind /= 2)                   error stop 26_4
     if(contain3(i,2)%dtp1%c%len  /= 3)                   error stop 27_4

     if(contain3(i,1)%dtp1%i%kind /= 2)                   error stop 28_4
     if(contain3(i,2)%dtp1%c%len  /= 3)                   error stop 29_4
     if(contain3(i,1)%dtp1%i /= 1)                        error stop 30_4
     if(contain3(i,2)%dtp1%i /= 2)                        error stop 31_4

     if(contain3(i,1)%dtp1%c /= "1")                      error stop 32_4
     if(contain3(i,2)%dtp1%c /= "2")                      error stop 33_4

  end do

  if(allocated(contain3))  deallocate(contain3)

  contain3=getSpreadResult2([contain1,contain(1,2)(dtp(2,3)(i=2,c="2"))],2,5)

  if(contain3%k2 /= 1)                                    error stop 34_4
  if(contain3%l2 /= 2)                                    error stop 35_4
  do i=1,5
     if(contain3(1,i)%dtp1%k1 /= 2)                       error stop 36_4
     if(contain3(1,i)%dtp1%l1 /= 3)                       error stop 37_4

     if(contain3(2,i)%dtp1%k1 /= 2)                       error stop 38_4
     if(contain3(2,i)%dtp1%l1 /= 3)                       error stop 39_4

     if(contain3(1,i)%dtp1%i%kind /= 2)                   error stop 40_4
     if(contain3(2,i)%dtp1%c%len  /= 3)                   error stop 41_4

     if(contain3(1,i)%dtp1%i%kind /= 2)                   error stop 42_4
     if(contain3(2,i)%dtp1%c%len  /= 3)                   error stop 43_4

     if(contain3(1,i)%dtp1%i /= 1)                        error stop 44_4
     if(contain3(2,i)%dtp1%i /= 2)                        error stop 45_4

     if(contain3(1,i)%dtp1%c /= "1")                      error stop 46_4
     if(contain3(2,i)%dtp1%c /= "2")                      error stop 47_4

  end do

  if(allocated(contain2)) deallocate(contain2)
  if(allocated(contain3)) deallocate(contain3)

  allocate(contain2(4),source=[contain(1,2)(dtp(2,3)(1,"+1")), &
                               contain(1,2)(dtp(2,3)(-1,"-1")), &
                               contain(1,2)(dtp(2,3)(2,"+2")),&
                               contain(1,2)(dtp(2,3)(-2,"-2")) ] )

  allocate(contain3(2,2),source=reshape(contain2,(/2,2/)) )

  contain4=getSpreadResult3(contain3,1,5)

  if(contain4%k2 /= 1)                                    error stop 48_4
  if(contain4%l2 /= 2)                                    error stop 49_4
  do i=1,5
     if(contain4(i,1,1)%dtp1%k1 /= 2)                     error stop 50_4
     if(contain4(i,1,1)%dtp1%l1 /= 3)                     error stop 51_4
     if(contain4(i,2,1)%dtp1%k1 /= 2)                     error stop 52_4
     if(contain4(i,2,1)%dtp1%l1 /= 3)                     error stop 53_4
     if(contain4(i,1,2)%dtp1%k1 /= 2)                     error stop 54_4
     if(contain4(i,1,2)%dtp1%l1 /= 3)                     error stop 55_4
     if(contain4(i,2,2)%dtp1%k1 /= 2)                     error stop 56_4
     if(contain4(i,2,2)%dtp1%l1 /= 3)                     error stop 57_4

     if(contain4(i,1,1)%dtp1%i /= 1)                      error stop 58_4
     if(contain4(i,2,1)%dtp1%i /= -1)                     error stop 59_4
     if(contain4(i,1,2)%dtp1%i /= 2)                      error stop 60_4
     if(contain4(i,2,2)%dtp1%i /= -2)                     error stop 61_4

     if(contain4(i,1,1)%dtp1%c /= "+1")                   error stop 62_4
     if(contain4(i,2,1)%dtp1%c /= "-1")                   error stop 63_4
     if(contain4(i,1,2)%dtp1%c /= "+2")                   error stop 64_4
     if(contain4(i,2,2)%dtp1%c /= "-2")                   error stop 65_4

  end do


  if(allocated(contain4))   deallocate(contain4)

  contain4=getSpreadResult3(contain3,2,5)

  if(contain4%k2 /= 1)                                    error stop 66_4
  if(contain4%l2 /= 2)                                    error stop 67_4
  do i=1,5
     if(contain4(1,i,1)%dtp1%k1 /= 2)                     error stop 68_4
     if(contain4(1,i,1)%dtp1%l1 /= 3)                     error stop 69_4
     if(contain4(2,i,1)%dtp1%k1 /= 2)                     error stop 70_4
     if(contain4(2,i,1)%dtp1%l1 /= 3)                     error stop 71_4
     if(contain4(1,i,2)%dtp1%k1 /= 2)                     error stop 72_4
     if(contain4(1,i,2)%dtp1%l1 /= 3)                     error stop 73_4
     if(contain4(2,i,2)%dtp1%k1 /= 2)                     error stop 74_4
     if(contain4(2,i,2)%dtp1%l1 /= 3)                     error stop 75_4

     if(contain4(1,i,1)%dtp1%i /= 1)                      error stop 76_4
     if(contain4(2,i,1)%dtp1%i /= -1)                     error stop 77_4
     if(contain4(1,i,2)%dtp1%i /= 2)                      error stop 78_4
     if(contain4(2,i,2)%dtp1%i /= -2)                     error stop 79_4

     if(contain4(1,i,1)%dtp1%c /= "+1")                   error stop 80_4
     if(contain4(2,i,1)%dtp1%c /= "-1")                   error stop 81_4
     if(contain4(1,i,2)%dtp1%c /= "+2")                   error stop 82_4
     if(contain4(2,i,2)%dtp1%c /= "-2")                   error stop 83_4

  end do

  if(allocated(contain4))   deallocate(contain4)

  contain4=getSpreadResult3(contain3,3,5)

  if(contain4%k2 /= 1)                                    error stop 84_4
  if(contain4%l2 /= 2)                                    error stop 85_4
  do i=1,5
     if(contain4(1,1,i)%dtp1%k1 /= 2)                     error stop 86_4
     if(contain4(1,1,i)%dtp1%l1 /= 3)                     error stop 87_4
     if(contain4(2,1,i)%dtp1%k1 /= 2)                     error stop 88_4
     if(contain4(2,1,i)%dtp1%l1 /= 3)                     error stop 89_4
     if(contain4(1,2,i)%dtp1%k1 /= 2)                     error stop 90_4
     if(contain4(1,2,i)%dtp1%l1 /= 3)                     error stop 91_4
     if(contain4(2,2,i)%dtp1%k1 /= 2)                     error stop 92_4
     if(contain4(2,2,i)%dtp1%l1 /= 3)                     error stop 93_4

     if(contain4(1,1,i)%dtp1%i /= 1)                      error stop 94_4
     if(contain4(2,1,i)%dtp1%i /= -1)                     error stop 95_4
     if(contain4(1,2,i)%dtp1%i /= 2)                      error stop 96_4
     if(contain4(2,2,i)%dtp1%i /= -2)                     error stop 97_4

     if(contain4(1,1,i)%dtp1%c /= "+1")                   error stop 98_4
     if(contain4(2,1,i)%dtp1%c /= "-1")                   error stop 99_4
     if(contain4(1,2,i)%dtp1%c /= "+2")                   error stop 100_4
     if(contain4(2,2,i)%dtp1%c /= "-2")                   error stop 101_4

  end do

  contains

     function getSpreadResult1(source,dim,ncopies)

          type(contain(1,*)),intent(in)  :: source
          integer,intent(in) :: dim,ncopies
          type(contain(1,source%l2)),allocatable :: getSpreadResult1(:)

          getSpreadResult1=spread(source,dim,ncopies)
     end function
end program

function getSpreadResult2(source,dim,ncopies)
   use m,only : contain
   type(contain(1,*)),intent(in) :: source(:)
   integer,intent(in) :: dim,ncopies
   type(contain(1,source%l2)),allocatable :: getSpreadResult2(:,:)

   getSpreadResult2=spread(source,dim,ncopies)
end function
