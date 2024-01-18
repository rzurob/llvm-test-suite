!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 6 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DERIVED TYPE HAS INTEGER AND CHARACTER ARRAY COMPONENT
!*  2. DUMMY ARGUMENT IS SCALAR OR ARRAY
!*  3. USE MODULE SUBROUTINE, EXTERNAL SUBROUTINE,INTERNAL SUBROUTINE
!*  4. VERIFY ALLOCATION AND ASSOCIATION STATUS AND COMPONENT VALUES
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k1,k2,l1,l2)
     integer,kind    :: k1
     integer(2),kind :: k2
     integer,len     :: l1
     integer(2),len  :: l2

     integer(k1)      :: i1(k1:k2)
     character(l1+l2) :: c1(l1:l2)

  end type

  type(dtp(2,4,2,3)),target      :: tar1=&
                   dtp(2,4,2,3)(i1= [1,2,3],c1=["123","456"])

  contains

   subroutine modsub1(arg)
      type(dtp(2,4,l1=:,l2=:)),allocatable :: arg

      if(.not. allocated(arg)) then
         allocate(dtp(2,4,2,3) :: arg)
         arg%i1=[1,2,3]
         arg%c1=["123","456"]
      else
         deallocate(arg)
      endif
   end subroutine

   subroutine modsub2(arg)
      type(dtp(2,4,:,:)),pointer :: arg(:)

      if(.not. associated(arg)) then
         allocate(dtp(2,4,2,3)  :: arg(3))
         arg = &
              [dtp(2,4,2,3)(i1=[1,2,3],c1=["000","111"]), &
               dtp(2,4,2,3)(i1=[4,5,6],c1=["222","333"]), &
               dtp(2,4,2,3)(i1=[7,8,9],c1=["444","555"]) ]
      else
         deallocate(arg)
      endif
   end subroutine
end module

program dummyArgDeferNonPolyBasic02
  use m
  implicit none

  interface

  subroutine extsub1(arg)
     import

     type(dtp(2,4,l1=:,l2=:)),allocatable :: arg(:)

  end subroutine

  subroutine extsub2(arg)
     import

     type(dtp(2,4,:,:)),pointer       :: arg

  end subroutine

  end interface
  integer :: i
  type(dtp(2,4,:,:)),allocatable :: dtp1
  type(dtp(2,4,:,:)),pointer     :: dtp2(:)=>null()
  type(dtp(2,4,:,:)),allocatable :: dtp3(:)
  type(dtp(2,4,:,:)),pointer     :: dtp4=>null()


  call modsub1(dtp1)
  if(.not. allocated(dtp1))                  error stop 10_4
  if(dtp1%l1 /= 2)                           error stop 11_4
  if(dtp1%l2 /= 3)                           error stop 12_4
  if(any(dtp1%i1 /= [1,2,3]))                error stop 13_4
  if(any(dtp1%c1 /= ["123","456"]))          error stop 14_4
  if(lbound(dtp1%i1,1) /= 2)                 error stop 15_4
  if(ubound(dtp1%i1,1) /= 4)                 error stop 16_4
  if(lbound(dtp1%c1,1) /= 2)                 error stop 17_4
  if(ubound(dtp1%c1,1) /= 3)                 error stop 18_4
  if(dtp1%c1%len /= 5)                       error stop 19_4

  call modsub1(dtp1)
  if(allocated(dtp1))                        error stop 20_4

  call modsub2(dtp2)
  if(.not. associated(dtp2))                 error stop 21_4

  if(dtp2%l1 /= 2)                           error stop 22_4
  if(dtp2%l2 /= 3)                           error stop 23_4
  if(any(dtp2(1)%i1 /= [1,2,3]))             error stop 24_4
  if(any(dtp2(2)%i1 /= [4,5,6]))             error stop 25_4
  if(any(dtp2(3)%i1 /= [7,8,9]))             error stop 26_4
  if(any(dtp2(1)%c1 /= ["000","111"]))       error stop 27_4
  if(any(dtp2(2)%c1 /= ["222","333"]))       error stop 28_4
  if(any(dtp2(3)%c1 /= ["444","555"]))       error stop 29_4

  do i=1,3
    if(lbound(dtp2(i)%i1,1) /= 2)            error stop 30_4
    if(ubound(dtp2(i)%i1,1) /= 4)            error stop 31_4
    if(lbound(dtp2(i)%c1,1) /= 2)            error stop 32_4
    if(ubound(dtp2(i)%c1,1) /= 3)            error stop 33_4
    if(dtp2(i)%c1%len /= 5)                  error stop 34_4
  end do

  call modsub2(dtp2)
  if(associated(dtp2))                       error stop 35_4

  call extsub1(dtp3)
  if(.not. allocated(dtp3))                  error stop 36_4
  if(dtp3%l1 /= 2)                           error stop 37_4
  if(dtp3%l2 /= 3)                           error stop 38_4

  if(any(dtp3(1)%i1 /= [1,2,3]))             error stop 39_4
  if(any(dtp3(2)%i1 /= [4,5,6]))             error stop 40_4
  if(any(dtp3(3)%i1 /= [7,8,9]))             error stop 41_4
  if(any(dtp3(1)%c1 /= ["000","111"]))       error stop 42_4
  if(any(dtp3(2)%c1 /= ["222","333"]))       error stop 43_4
  if(any(dtp3(3)%c1 /= ["444","555"]))       error stop 44_4

  do i=1,3
    if(lbound(dtp3(i)%i1,1) /= 2)            error stop 45_4
    if(ubound(dtp3(i)%i1,1) /= 4)            error stop 46_4
    if(lbound(dtp3(i)%c1,1) /= 2)            error stop 47_4
    if(ubound(dtp3(i)%c1,1) /= 3)            error stop 48_4
    if(dtp3(i)%c1%len /= 5)                  error stop 49_4
  end do

  call extsub1(dtp3)
  if(allocated(dtp3))                        error stop 50_4

  call extsub2(dtp4)
  if(.not. associated(dtp4))                 error stop 51_4

  if(dtp4%l1 /= 2)                           error stop 52_4
  if(dtp4%l2 /= 3)                           error stop 53_4
  if(any(dtp4%i1 /= [1,2,3]))                error stop 54_4
  if(any(dtp4%c1 /= ["123","456"]))          error stop 55_4

  if(lbound(dtp4%i1,1) /= 2)                 error stop 56_4
  if(ubound(dtp4%i1,1) /= 4)                 error stop 57_4
  if(lbound(dtp4%c1,1) /= 2)                 error stop 58_4
  if(ubound(dtp4%c1,1) /= 3)                 error stop 59_4
  if(dtp4%c1%len /= 5)                       error stop 60_4

  call extsub2(dtp4)
  if(associated(dtp4))                       error stop 61_4

  call intsub1(dtp3)

  if(.not. allocated(dtp3))                  error stop 62_4
  if(dtp3%l1 /= 2)                           error stop 63_4
  if(dtp3%l2 /= 3)                           error stop 64_4

  if(any(dtp3(1)%i1 /= [1,2,3]))             error stop 65_4
  if(any(dtp3(2)%i1 /= [4,5,6]))             error stop 66_4
  if(any(dtp3(3)%i1 /= [7,8,9]))             error stop 67_4
  if(any(dtp3(1)%c1 /= ["000","111"]))       error stop 68_4
  if(any(dtp3(2)%c1 /= ["222","333"]))       error stop 69_4
  if(any(dtp3(3)%c1 /= ["444","555"]))       error stop 70_4

  do i=1,3
    if(lbound(dtp3(i)%i1,1) /= 2)            error stop 71_4
    if(ubound(dtp3(i)%i1,1) /= 4)            error stop 72_4
    if(lbound(dtp3(i)%c1,1) /= 2)            error stop 73_4
    if(ubound(dtp3(i)%c1,1) /= 3)            error stop 74_4
    if(dtp3(i)%c1%len /= 5)                  error stop 75_4
  end do

  call intsub1(dtp3)
  if(allocated(dtp3))                        error stop 76_4

  call intsub2(dtp4)
  if(.not. associated(dtp4))                 error stop 77_4

  if(dtp4%l1 /= 2)                           error stop 78_4
  if(dtp4%l2 /= 3)                           error stop 79_4
  if(any(dtp4%i1 /= [1,2,3]))                error stop 80_4
  if(any(dtp4%c1 /= ["123","456"]))          error stop 81_4

  if(lbound(dtp4%i1,1) /= 2)                 error stop 82_4
  if(ubound(dtp4%i1,1) /= 4)                 error stop 83_4
  if(lbound(dtp4%c1,1) /= 2)                 error stop 84_4
  if(ubound(dtp4%c1,1) /= 3)                 error stop 85_4
  if(dtp4%c1%len /= 5)                       error stop 86_4

  call intsub2(dtp4)
  if(associated(dtp4))                       error stop 87_4

  contains

  subroutine intsub1(arg)
    type(dtp(k1=2,k2=4,l1=:,l2=:)),allocatable :: arg(:)

    if(.not. allocated(arg)) then
       allocate(arg(3),source= &
         [dtp(2,4,2,3)(i1=[1,2,3],c1=["000","111"]), &
          dtp(2,4,2,3)(i1=[4,5,6],c1=["222","333"]), &
          dtp(2,4,2,3)(i1=[7,8,9],c1=["444","555"]) ])
    else
       deallocate(arg)
    end if
  end subroutine

  subroutine intsub2(arg)
    type(dtp(2,4,:,:)),pointer :: arg

    if(.not. associated(arg) ) then
       allocate(arg,source= &
                   dtp(2,4,2,3)(i1=[1,2,3],c1=["123","456"]) )
    else
       deallocate(arg)
    end if
  end subroutine
end program

  subroutine extsub1(arg)
     use m

     type(dtp(2,4,l1=:,l2=:)),allocatable :: arg(:)

     if(.not. allocated(arg)) then
        arg=[dtp(2,4,2,3)(i1=[1,2,3],c1=["000","111"]), &
             dtp(2,4,2,3)(i1=[4,5,6],c1=["222","333"]), &
             dtp(2,4,2,3)(i1=[7,8,9],c1=["444","555"])]
     else
        deallocate(arg)
     end if
  end subroutine

  subroutine extsub2(arg)
     use m

     type(dtp(2,4,:,:)),pointer       :: arg

     if(.not. associated(arg)) then
        arg=>tar1
     else

        arg=>null()
     end if
  end subroutine
