!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPolyBasic03.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 6 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. DERIVED TYPE HAS CHARACTER ALLOCATABLE AND POINTER COMPONENT 
!*  2. DUMMY ARGUMENT IS SCALAR OR ARRAY
!*  3. USE MODULE SUBROUTINE, EXTERNAL SUBROUTINE,INTERNAL SUBROUTINE 
!*  4. VERIFY ALLOCATION AND ASSOCIATION STATUS AND COMPONENT VALUES 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(l1,l2)
     integer,len     :: l1 
     integer(2),len  :: l2
     character(:),allocatable :: c1(:)
     character(:),pointer     :: c2=>null()
  end type

  character(3),target        :: ctar0="xlf"
  character(3),target        :: ctar1="a1"
  character(3),target        :: ctar2="b2"
  character(3),target        :: ctar3="c3"
  type(dtp(2,3)),target      :: tar1=dtp(2,3)(null(),null())

  contains

   subroutine modsub1(arg)
      type(dtp(l1=:,l2=:)),allocatable :: arg

      if(.not. allocated(arg)) then
         allocate(dtp(2,3) :: arg)
         arg%c1=["123","456"]
         arg%c2=>ctar0
      else
         deallocate(arg)
      endif
   end subroutine   

   subroutine modsub2(arg)
      type(dtp(:,:)),pointer :: arg(:)
      
      if(.not. associated(arg)) then
         allocate(dtp(2,3)  :: arg(3))
         arg=&
            [dtp(2,3)(["000","111"],ctar1), &
             dtp(2,3)(["222","333"],ctar2), &
             dtp(2,3)(["444","555"],ctar3) ]
           
      else
         deallocate(arg)
      endif      
   end subroutine 
end module

program dummyArgDeferNonPolyBasic03
  use m
  implicit none

  interface

  subroutine extsub1(arg)
     import

     type(dtp(l1=:,l2=:)),allocatable :: arg(:)

  end subroutine

  subroutine extsub2(arg)
     import

     type(dtp(:,:)),pointer       :: arg

  end subroutine

  end interface  
  type(dtp(:,:)),allocatable :: dtp1
  type(dtp(:,:)),pointer     :: dtp2(:)=>null()   
  type(dtp(:,:)),allocatable :: dtp3(:)
  type(dtp(:,:)),pointer     :: dtp4=>null()

  tar1%c1=["123","456"]
  tar1%c2=>ctar0
 
  call modsub1(dtp1)
  if(.not. allocated(dtp1))             error stop 10_4
  if(dtp1%l1 /= 2)                      error stop 11_4
  if(dtp1%l2 /= 3)                      error stop 12_4
  if(any(dtp1%c1 /= ["123","456"]))     error stop 13_4
  if(dtp1%c2 /= "xlf")                  error stop 14_4

  call modsub1(dtp1)
  if(allocated(dtp1))                   error stop 15_4

  call modsub2(dtp2)
  if(.not. associated(dtp2))            error stop 16_4
  if(dtp2%l1 /= 2)                      error stop 17_4
  if(dtp2%l2 /= 3)                      error stop 18_4

  if(any(dtp2(1)%c1 /= ["000","111"]))  error stop 19_4
  if(any(dtp2(2)%c1 /= ["222","333"]))  error stop 20_4
  if(any(dtp2(3)%c1 /= ["444","555"]))  error stop 21_4
  if(dtp2(1)%c2 /= "a1")                error stop 22_4
  if(dtp2(2)%c2 /= "b2")                error stop 23_4
  if(dtp2(3)%c2 /= "c3")                error stop 24_4
  
  call modsub2(dtp2)
  if(associated(dtp2))                  error stop 25_4

  call extsub1(dtp3)
  if(.not. allocated(dtp3))             error stop 26_4    
  if(dtp3%l1 /= 2)                      error stop 27_4
  if(dtp3%l2 /= 3)                      error stop 28_4

  if(any(dtp3(1)%c1 /= ["000","111"]))  error stop 29_4
  if(any(dtp3(2)%c1 /= ["222","333"]))  error stop 30_4
  if(any(dtp3(3)%c1 /= ["444","555"]))  error stop 31_4
  if(dtp3(1)%c2 /= "a1")                error stop 32_4
  if(dtp3(2)%c2 /= "b2")                error stop 33_4
  if(dtp3(3)%c2 /= "c3")                error stop 34_4

  call extsub1(dtp3)
  if(allocated(dtp3))                   error stop 35_4                 

  call extsub2(dtp4)
  if(.not. associated(dtp4))            error stop 36_4
  if(dtp4%l1 /= 2)                      error stop 37_4
  if(dtp4%l2 /= 3)                      error stop 38_4
  if(any(dtp4%c1 /= ["123","456"]))     error stop 39_4
  if(dtp4%c2 /= "xlf")                  error stop 40_4

  call extsub2(dtp4)  
  if(associated(dtp4))                  error stop 41_4

  call intsub1(dtp3)
  if(.not. allocated(dtp3))             error stop 42_4
  if(dtp3%l1 /= 2)                      error stop 43_4
  if(dtp3%l2 /= 3)                      error stop 44_4

  if(any(dtp3(1)%c1 /= ["000","111"]))  error stop 45_4
  if(any(dtp3(2)%c1 /= ["222","333"]))  error stop 46_4
  if(any(dtp3(3)%c1 /= ["444","555"]))  error stop 47_4
  if(dtp3(1)%c2 /= "a1")                error stop 48_4
  if(dtp3(2)%c2 /= "b2")                error stop 49_4
  if(dtp3(3)%c2 /= "c3")                error stop 50_4
  
  call intsub1(dtp3)
  if(allocated(dtp3))                   error stop 51_4

  call intsub2(dtp4)
  if(.not. associated(dtp4))            error stop 52_4
  if(dtp4%l1 /= 2)                      error stop 53_4
  if(dtp4%l2 /= 3)                      error stop 54_4
  if(any(dtp4%c1 /= ["123","456"]))     error stop 55_4
  if(dtp4%c2 /= "xlf")                  error stop 56_4

  call intsub2(dtp4)
  if(associated(dtp4))                  error stop 57_4

  contains

  subroutine intsub1(arg)
    type(dtp(l1=:,l2=:)),allocatable :: arg(:)

    if(.not. allocated(arg)) then
       allocate(arg(3),source= &
            [dtp(2,3)(["000","111"],ctar1), &
             dtp(2,3)(["222","333"],ctar2), &
             dtp(2,3)(["444","555"],ctar3) ] )
    else
       deallocate(arg)
    end if
  end subroutine

  subroutine intsub2(arg)
    type(dtp(:,:)),pointer :: arg
    
    if(.not. associated(arg) ) then
       allocate(arg,source= dtp(2,3)(["123","456"],ctar0))
    else
       deallocate(arg)
    end if
  end subroutine   
end program

  subroutine extsub1(arg)
     use m

     type(dtp(l1=:,l2=:)),allocatable :: arg(:)

     if(.not. allocated(arg)) then
         arg=&
            [dtp(2,3)(["000","111"],ctar1), &
             dtp(2,3)(["222","333"],ctar2), &
             dtp(2,3)(["444","555"],ctar3) ]
     else
        deallocate(arg) 
     end if
  end subroutine

  subroutine extsub2(arg)
     use m

     type(dtp(:,:)),pointer       :: arg
     if(.not. associated(arg)) then
        arg=>tar1
     else
        arg=>null() 
     end if 
  end subroutine
