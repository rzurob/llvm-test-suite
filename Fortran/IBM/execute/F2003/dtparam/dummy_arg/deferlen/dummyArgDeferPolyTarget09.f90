!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 20 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*   pointer is associated with target, and pass pointer and target as actual argument, pointer dummy argument and target dummy argument becomes associated, inside procedure, dummy argument pointer is associated with another target, verify association status after execuation of procedure is completed.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len  :: l1
      integer      :: i(l1)
   end type

   type,extends(base) :: child(l2)
      integer,len  :: l2
      type(child(l1,l2)),pointer :: childcomp=>null()
   end type
end module

program dummyArgDeferPolyTarget09
  use m
  implicit none

  class(base(:)),pointer :: pbase1(:)=>null(),pbase2(:)=>null()
  class(base(:)),target,allocatable  :: tar1(:)
  type(child(2,3)),target  :: tar2(2:3)


  allocate(tar1(5:7),source= &
          [child(2,3)(i=[1,2]),child(2,3)([3,4]),child(2,3)([5,6])] )

  select type(tar1)
    type is(child(*,*))
      tar1(5)%childcomp=>tar1(7)
      tar1(6)%childcomp=>tar1(6)
      tar1(7)%childcomp=>tar1(5)
    class default
      error stop 100_4
  end select

  tar2=[child(2,3)([-1,-2]),child(2,3)([-3,-4])]

  tar2(2)%childcomp=>tar2(3)
  tar2(3)%childcomp=>tar2(2)

  pbase1(0:)=>tar1
  pbase2=>tar1

  call sub(pbase1,tar1)

  if(.not. associated(pbase2,tar1))                        error stop 24_4
  if(.not. associated(pbase1,tar2))                        error stop 25_4
  if(lbound(pbase1,1) /= 10)                               error stop 26_4
  if(ubound(pbase1,1) /= 11)                               error stop 27_4
  if(any(pbase1(10)%i /= [-1,-2]))                         error stop 28_4
  if(any(pbase1(11)%i /= [-3,-4]))                         error stop 29_4
  select type(pbase1)
    type is(child(*,*))
      if(.not. associated(pbase1(10)%childcomp,tar2(3)))   error stop 30_4
      if(.not. associated(pbase1(11)%childcomp,tar2(2)))   error stop 31_4
    class default
      error stop 101_4
  end select

  if(lbound(pbase2,1) /= 5)                                error stop 32_4
  if(ubound(pbase2,1) /= 7)                                error stop 33_4

  if(any(pbase2(5)%i /= [1,2]))                            error stop 34_4
  if(any(pbase2(6)%i /= [3,4]))                            error stop 35_4
  if(any(pbase2(7)%i /= [5,6]))                            error stop 36_4

  contains

      subroutine sub(ptr,target)
         class(base(:)),pointer     :: ptr(:)
         class(base(:)),target,allocatable      :: target(:)

         if(.not. associated(ptr,target))                  error stop 10_4
         if(lbound(ptr,1) /= 0)                            error stop 11_4
         if(ubound(ptr,1) /= 2)                            error stop 12_4
         if(any(ptr(0)%i /= [1,2]))                        error stop 13_4
         if(any(ptr(1)%i /= [3,4]))                        error stop 14_4
         if(any(ptr(2)%i /= [5,6]))                        error stop 15_4

         if(lbound(target,1) /= 5)                         error stop 16_4
         if(ubound(target,1) /= 7)                         error stop 17_4
         if(any(target(5)%i /= [1,2]))                     error stop 18_4
         if(any(target(6)%i /= [3,4]))                     error stop 19_4
         if(any(target(7)%i /= [5,6]))                     error stop 20_4

         select type(ptr)
           type is(child(*,*))
             select type(target)
               type is(child(*,*))
             if(.not. associated(ptr(0)%childcomp,target(7))) error stop 21_4
             if(.not. associated(ptr(1)%childcomp,target(6))) error stop 22_4
             if(.not. associated(ptr(2)%childcomp,target(5))) error stop 23_4
               class default
                 error stop 102_4
             end select
         end select

         ptr(10:11)=>tar2

      end subroutine

end program
