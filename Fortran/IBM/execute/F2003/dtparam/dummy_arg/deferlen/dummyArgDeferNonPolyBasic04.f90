!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPolyBasic04.f   
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
!*  1. DERIVED TYPE HAS NESTED DRIVED TYPE COMPONENT 
!*  2. DUMMY ARGUMENT IS SCALAR OR ARRAY
!*  3. USE MODULE SUBROUTINE, EXTERNAL SUBROUTINE,INTERNAL SUBROUTINE 
!*  4. VERIFY ALLOCATION AND ASSOCIATION STATUS AND TYPE PARAMETER VALUE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type first(k1,l1)
     integer,kind    :: k1=2
     integer,len     :: l1=3 
     integer(k1)     :: i1(l1-1:l1+1)=k1
  end type

  type second(k2,l2)
     integer,kind    :: k2=2
     integer,len     :: l2=3
     type(first(k2,l2)) :: first1=first(k2,3)()
  end type

  type third(k3,l3)
     integer,kind    :: k3=2
     integer,len     :: l3=3
     type(second(k3,l3)) :: second1=second(k3,3)()
  end type


  type(third(2,3)),target      :: tar1= &
              third(2,3)(second(2,3)(first(2,3)([1,2,3])))

  contains

   subroutine modsub1(arg)
      type(third(2,l3=:)),allocatable :: arg

      if(.not. allocated(arg)) then
         allocate(third(2,3) :: arg)
         arg=third(2,3)(second(2,3)(first(2,3)([1,2,3])))
      else
         deallocate(arg)
      endif
   end subroutine   

   subroutine modsub2(arg)
      type(third(2,:)),pointer :: arg(:)
      
      if(.not. associated(arg)) then
         allocate(third(2,3)  :: arg(3))
         arg=[third(2,3)(second(2,3)(first(2,3)([1,2,3]))), &
              third(2,3)(second(2,3)(first(2,3)([4,5,6]))),& 
              third(2,3)(second(2,3)(first(2,3)([7,8,9])))] 
      else
         deallocate(arg)
      endif      
   end subroutine 
end module

program dummyArgDeferNonPolyBasic04
  use m
  implicit none

  interface

  subroutine extsub1(arg)
     import

     type(third(2,l3=:)),allocatable :: arg(:)

  end subroutine

  subroutine extsub2(arg)
     import

     type(third(2,:)),pointer       :: arg

  end subroutine

  end interface  
  type(third(2,:)),allocatable :: third1
  type(third(2,:)),pointer     :: third2(:)=>null()   
  type(third(2,:)),allocatable :: third3(:)
  type(third(2,:)),pointer     :: third4=>null()

 
  call modsub1(third1)
  if(.not. allocated(third1))                              error stop 10_4
  if(any(third1%second1%first1%i1 /= [1,2,3]))             error stop 11_4
  if(lbound(third1%second1%first1%i1,1) /= 2)              error stop 12_4
  if(ubound(third1%second1%first1%i1,1) /= 4)              error stop 13_4

  call modsub1(third1)
  if(allocated(third1))                                    error stop 14_4

  call modsub2(third2)
  if(.not. associated(third2))                             error stop 15_4
  if(any(third2(1)%second1%first1%i1 /= [1,2,3]))          error stop 16_4
  if(any(third2(2)%second1%first1%i1 /= [4,5,6]))          error stop 17_4
  if(any(third2(3)%second1%first1%i1 /= [7,8,9]))          error stop 18_4
  if(lbound(third2(2)%second1%first1%i1,1) /= 2)           error stop 19_4
  if(ubound(third2(2)%second1%first1%i1,1) /= 4)           error stop 20_4
  
  call modsub2(third2)
  if(associated(third2))                                   error stop 21_4

  call extsub1(third3)
  if(.not. allocated(third3))                              error stop 22_4   
  if(any(third3(1)%second1%first1%i1 /= [1,2,3]))          error stop 23_4
  if(any(third3(2)%second1%first1%i1 /= [4,5,6]))          error stop 24_4
  if(any(third3(3)%second1%first1%i1 /= [7,8,9]))          error stop 25_4
  if(lbound(third3(2)%second1%first1%i1,1) /= 2)           error stop 26_4
  if(ubound(third3(2)%second1%first1%i1,1) /= 4)           error stop 27_4 

  call extsub1(third3)
  if(allocated(third3))                                    error stop 28_4

  call extsub2(third4)
  if(.not. associated(third4))                             error stop 29_4
  if(any(third4%second1%first1%i1 /= [1,2,3]))             error stop 30_4
  if(lbound(third4%second1%first1%i1,1) /= 2)              error stop 31_4
  if(ubound(third4%second1%first1%i1,1) /= 4)              error stop 32_4

  call extsub2(third4)  
  if(associated(third4))                                   error stop 33_4

  call intsub1(third3)
  if(.not. allocated(third3))                              error stop 34_4
  if(any(third3(1)%second1%first1%i1 /= [1,2,3]))          error stop 35_4
  if(any(third3(2)%second1%first1%i1 /= [4,5,6]))          error stop 36_4
  if(any(third3(3)%second1%first1%i1 /= [7,8,9]))          error stop 37_4
  if(lbound(third3(2)%second1%first1%i1,1) /= 2)           error stop 38_4
  if(ubound(third3(2)%second1%first1%i1,1) /= 4)           error stop 39_4
  
  call intsub1(third3)
  if(allocated(third3))                                    error stop 40_4

  call intsub2(third4)
  if(.not. associated(third4))                             error stop 41_4
  if(any(third4%second1%first1%i1 /= [1,2,3]))             error stop 42_4
  if(lbound(third4%second1%first1%i1,1) /= 2)              error stop 43_4
  if(ubound(third4%second1%first1%i1,1) /= 4)              error stop 44_4

  call intsub2(third4)
  if(associated(third4))                                   error stop 45_4

  contains

  subroutine intsub1(arg)
    type(third(k3=2,l3=:)),allocatable :: arg(:)

    if(.not. allocated(arg)) then
       allocate(arg(3),source= &
             [third(2,3)(second(2,3)(first(2,3)([1,2,3]))), &
              third(2,3)(second(2,3)(first(2,3)([4,5,6]))),&
              third(2,3)(second(2,3)(first(2,3)([7,8,9])))] )
    else
       deallocate(arg)
    end if
  end subroutine

  subroutine intsub2(arg)
    type(third(2,:)),pointer :: arg
    
    if(.not. associated(arg) ) then
       allocate(arg,source= third(2,3)(second(2,3)(first(2,3)([1,2,3]))) )
    else
       deallocate(arg)
    end if
  end subroutine   
end program

  subroutine extsub1(arg)
     use m

     type(third(2,l3=:)),allocatable :: arg(:)

     if(.not. allocated(arg)) then
         arg=[third(2,3)(second(2,3)(first(2,3)([1,2,3]))), &
              third(2,3)(second(2,3)(first(2,3)([4,5,6]))),&
              third(2,3)(second(2,3)(first(2,3)([7,8,9])))]
     else
        deallocate(arg) 
     end if
  end subroutine

  subroutine extsub2(arg)
     use m

     type(third(2,:)),pointer       :: arg
     
     if(.not. associated(arg)) then
        arg=>tar1
     else

        arg=>null() 
     end if 
  end subroutine
