!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 21 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. actual pointer becomes associated with actual target when corresponding dummy pointer associated with dummy target.
!*  2. 2 pointers are associated with 1 array target,after modify dummy target, 2 pointers will associated with updated actual target.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1,l2)
     integer,len :: l1,l2
     integer     :: i1(l1:l2)
  end type
  type,extends(base) :: child(k1,k2)
     integer,kind :: k1,k2
     integer      :: i2(k1:k2)
  end type

  type(child(2,3,2,4)),target :: tar(2)

  contains

    subroutine sub1(arg1,arg2)
       class(base(:,:)),pointer,intent(inout) :: arg1(:)
       class(base(*,*)),target,intent(in)     :: arg2(:)

       arg1=>arg2

    end subroutine

    subroutine sub2(arg1,arg2)
       class(base(*,*)),target,intent(inout) :: arg1(:)
       class(base(*,*)),target,intent(inout) :: arg2(:)

       select type(arg1)
         type is(child(*,*,2,4))
           arg1=child(2,3,2,4)(i1=[11,12],i2=[13,14,15])
         class default
           error stop 50_4
       end select

       select type(arg2)
          type is(child(*,*,2,4))
            arg2=child(2,3,2,4)(i1=[-11,-12],i2=[-13,-14,-15])
          class default
            error stop 51_4
       end select
    end subroutine

end module

program dummyArgDeferPoly2Dummy01
  use m
  implicit none

  class(base(:,:)),pointer :: pbase1(:)=>null(),pbase2(:)=>null()


  tar=[child(2,3,2,4)(i1=[1,2],i2=[3,4,5]), &
       child(2,3,2,4)(i1=[-1,-2],i2=[-3,-4,-5]) ]

  pbase2(3:)=>tar

  call sub1(pbase1,tar)

  if(.not. associated(pbase1,tar))                error stop 10_4
  if(.not. associated(pbase2,tar))                error stop 11_4

  select type(pbase1)
     type is(child(*,*,2,4))
       if(pbase1%l1 /= 2)                         error stop 12_4
       if(pbase1%l2 /= 3)                         error stop 13_4
       if(pbase1%k1 /= 2)                         error stop 14_4
       if(pbase1%k2 /= 4)                         error stop 15_4
       if(any(pbase1(1)%i1 /= [1,2]))             error stop 16_4
       if(any(pbase1(2)%i1 /= [-1,-2]))           error stop 17_4
       if(any(pbase1(1)%i2 /= [3,4,5]))           error stop 18_4
       if(any(pbase1(2)%i2 /= [-3,-4,-5]))        error stop 19_4
     class default
       error stop 52_4

  end select

  select type(pbase2)
     type is(child(*,*,2,4))
       if(pbase2%l1 /= 2)                         error stop 20_4
       if(pbase2%l2 /= 3)                         error stop 21_4
       if(pbase2%k1 /= 2)                         error stop 22_4
       if(pbase2%k2 /= 4)                         error stop 23_4
       if(any(pbase2(3)%i1 /= [1,2]))             error stop 24_4
       if(any(pbase2(4)%i1 /= [-1,-2]))           error stop 25_4
       if(any(pbase2(3)%i2 /= [3,4,5]))           error stop 26_4
       if(any(pbase2(4)%i2 /= [-3,-4,-5]))        error stop 27_4
      class default
       error stop 53_4
  end select

  nullify(pbase1,pbase2)

  pbase1(0:)=>tar
  pbase2(2:3)=>tar

  call sub2(tar(1:1),tar(2:2))

  select type(pbase1)
     type is(child(*,*,2,4))
       if(pbase1%l1 /= 2)                         error stop 28_4
       if(pbase1%l2 /= 3)                         error stop 29_4
       if(pbase1%k1 /= 2)                         error stop 30_4
       if(pbase1%k2 /= 4)                         error stop 31_4
       if(any(pbase1(0)%i1 /= [11,12]))           error stop 32_4
       if(any(pbase1(1)%i1 /= [-11,-12]))         error stop 33_4
       if(any(pbase1(0)%i2 /= [13,14,15]))        error stop 34_4
       if(any(pbase1(1)%i2 /= [-13,-14,-15]))     error stop 35_4
      class default
       error stop 54_4
  end select

  select type(pbase2)
     type is(child(*,*,2,4))
       if(pbase2%l1 /= 2)                         error stop 36_4
       if(pbase2%l2 /= 3)                         error stop 37_4
       if(pbase2%k1 /= 2)                         error stop 38_4
       if(pbase2%k2 /= 4)                         error stop 39_4
       if(any(pbase2(2)%i1 /= [11,12]))           error stop 40_4
       if(any(pbase2(3)%i1 /= [-11,-12]))         error stop 41_4
       if(any(pbase2(2)%i2 /= [13,14,15]))        error stop 42_4
       if(any(pbase2(3)%i2 /= [-13,-14,-15]))     error stop 43_4
      class default
       error stop 55_4
  end select

end program
