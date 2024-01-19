!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 18 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.75
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK)
!* 3. TSOURCE AND FSOURCE ARE POLYMORPHIC TYPE
!* 4. DERIVED TYPE HAS TYPE-BOUND PROCEDURE
!* 5. PASS MERGE AS ARGUMENT
!* 6. DEFECT 355942
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,l1)
      integer,kind   :: k1
      integer(2),len :: l1
      integer(k1) :: i1(2)
      character(:),allocatable :: c1(:)
   end type
   type,extends(base) :: child(k2,l2)
      integer(8),kind :: k2
      integer(2),len  :: l2
      integer(k2)     :: i2(2)
      character(:),allocatable :: c2(:)
      contains
         procedure :: getChildInfo
         procedure :: resetChild
   end type

   contains
      function resetChild(dt)
          class(child(2,*,4,*)),intent(in) :: dt
          type(child(2,dt%l1,4,dt%l2))  :: resetChild

              resetChild%i1=[2*dt%i1(1),3*dt%i1(2)]
              resetChild%c1=["00","11","22"]
              resetChild%i2=[2*dt%i2(1),3*dt%i2(2)]
              resetChild%c2=["333","444","555","666"]

      end function

      function getChildInfo(dt)
          class(child(2,*,4,*)),intent(in) :: dt
          type(child(2,dt%l1,4,dt%l2))  :: getChildInfo
          getChildInfo=dt
      end function
end module

program mergeAsActualArg02
   use m
   implicit none

   class(base(2,:)),allocatable :: b1
   class(base(2,:)),pointer     :: b2
   type(child(2,:,4,:)),allocatable :: ch

   allocate(b1,source=child(2,3,4,5)([1,2],["ab","de"], &
                      [3,4],["x","y"]) )
   allocate(b2,source=child(2,3,4,5)([11,22],["red  ","blue ","green"], &
                                     [33,44],["aaa","bbb","ccc"]))


   call check(merge(b1,b2,.true.),1)
   call check(merge(b1,b2,.false.),2)

   contains

      subroutine check(dt,flag)
          class(base(2,*)),intent(in) :: dt
          integer :: flag

          select type(dt)
              type is(child(2,*,4,*))
                  if(dt%k1 /= 2)                         error stop 10_4
                  if(dt%l1 /= 3)                         error stop 11_4
                  if(dt%k2 /= 4)                         error stop 12_4
                  if(dt%l2 /= 5)                         error stop 13_4
                  if(dt%i1%kind /= 2)                    error stop 14_4
                  if(dt%i2%kind /= 4)                    error stop 15_4
                  if(dt%l1%kind /= 2)                    error stop 16_4
                  if(dt%l2%kind /= 2)                    error stop 17_4
                  if(dt%k1%kind /= 4)                    error stop 18_4
                  if(dt%k2%kind /= 8)                    error stop 19_4

                  if(flag .eq. 1) then
                      if(any(dt%i1 /= [1,2]))            error stop 20_4
                      if(any(dt%i2 /= [3,4]))            error stop 21_4
                      if(any(dt%c1 /= ["ab","de"]))      error stop 22_4
                      if(any(dt%c2 /= ["x","y"]))        error stop 23_4
                      if(dt%c1%len /= 2)                 error stop 24_4
                      if(dt%c2%len /= 1)                 error stop 25_4

                  else
                      if(any(dt%i1 /= [11,22]))          error stop 26_4
                      if(any(dt%i2 /= [33,44]))          error stop 27_4
                      if(any(dt%c1 /= &
                        ["red  ","blue ","green"]))      error stop 28_4
                      if(any(dt%c2 /= &
                        ["aaa","bbb","ccc"]))            error stop 29_4
                      if(dt%c1%len /= 5)                 error stop 30_4
                      if(dt%c2%len /= 3)                 error stop 31_4
                  endif

                  ch=dt%getChildInfo()
                  if(ch%k1 /= 2)                         error stop 40_4
!                  if(ch%l1 /= 3)                         error stop 41_4
                  if(ch%k2 /= 4)                         error stop 42_4
!                  if(ch%l2 /= 5)                         error stop 43_4
                  if(ch%i1%kind /= 2)                    error stop 44_4
                  if(ch%i2%kind /= 4)                    error stop 45_4
                  if(ch%l1%kind /= 2)                    error stop 46_4
                  if(ch%l2%kind /= 2)                    error stop 47_4
                  if(ch%k1%kind /= 4)                    error stop 48_4
                  if(ch%k2%kind /= 8)                    error stop 49_4

                  if(flag .eq. 1) then
                      if(any(ch%i1 /= [1,2]))            error stop 50_4
                      if(any(ch%i2 /= [3,4]))            error stop 51_4
                      if(any(ch%c1 /= ["ab","de"]))      error stop 52_4
                      if(any(ch%c2 /= ["x","y"]))        error stop 53_4
                      if(ch%c1%len /= 2)                 error stop 54_4
                      if(ch%c2%len /= 1)                 error stop 55_4

                  else
                      if(any(ch%i1 /= [11,22]))          error stop 56_4
                      if(any(ch%i2 /= [33,44]))          error stop 57_4
                      if(any(ch%c1 /= &
                        ["red  ","blue ","green"]))      error stop 58_4
                      if(any(ch%c2 /= &
                        ["aaa","bbb","ccc"]))            error stop 59_4
                      if(ch%c1%len /= 5)                 error stop 60_4
                      if(ch%c2%len /= 3)                 error stop 61_4
                  end if

                  ch=dt%resetChild()

                  if(ch%k1 /= 2)                         error stop 70_4
!                  if(ch%l1 /= 3)                         error stop 71_4
                  if(ch%k2 /= 4)                         error stop 72_4
!                  if(ch%l2 /= 5)                         error stop 73_4
                  if(ch%i1%kind /= 2)                    error stop 74_4
                  if(ch%i2%kind /= 4)                    error stop 75_4
                  if(ch%l1%kind /= 2)                    error stop 76_4
                  if(ch%l2%kind /= 2)                    error stop 77_4
                  if(ch%k1%kind /= 4)                    error stop 78_4
                  if(ch%k2%kind /= 8)                    error stop 79_4

                  if (flag == 1) then
                    if(any(ch%i1 /= [2,6]))                error stop 80_4
                    if(any(ch%i2 /= [6,12]))               error stop 81_4
                  else
                    if(any(ch%i1 /= [22,66]))                error stop 90_4
                    if(any(ch%i2 /= [66,132]))               error stop 91_4
                  end if
                  if(any(ch%c1 /= ["00","11","22"]))     error stop 82_4
                  if(any(ch%c2 /= ["333","444","555","666"]))  error stop 83_4
                  if(ch%c1%len /= 2)                     error stop 84_4
                  if(ch%c2%len /= 3)                    error stop 85_4
              class default
                 error stop 100_4

          end select

      end subroutine

end program
