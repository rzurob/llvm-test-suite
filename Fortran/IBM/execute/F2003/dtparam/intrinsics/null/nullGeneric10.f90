!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 27 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD])
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.88
!* 2. NULL([MOLD])
!* 3. MOLD IS POINTER OR ALLOCATABLE
!* 4. NULL(MOLD) IS USED AS ACTUAL ARGUMENT OF GENERIC TYPE-BOUND PROCEDURE
!* 5. ACTUAL ARGUMENTS ARE DIFFERENT TYPE , TYPE PARAMETER AND RANK
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type typeA(k)
      integer,kind :: k=4
      integer(k)   :: i=1
   end type
   type typeB(k)
      integer,kind :: k=4
      integer(k)   :: i=-1
   end type

   type any
      contains
        procedure,nopass :: proc1=>sub1
        procedure,nopass :: proc2=>sub2
        procedure,nopass :: proc3=>sub3
        procedure,nopass :: proc4=>sub4
        procedure,nopass :: proc5=>sub5
        procedure,nopass :: proc6=>sub6
        generic :: proc=>proc1,proc2,proc3,proc4,proc5,proc6
   end type

   contains
      subroutine sub1(dt)
         type(typeA(4)),pointer :: dt
         print *,"in sub1"
         if(associated(dt)) then
            print *,"typeA is associated"
            print *,dt%k,dt%i
         else
            print *,"typeA is not associated"
         endif
      end subroutine

      subroutine sub2(dt)
         type(typeA(2)),allocatable :: dt
         print *,"in sub2"
         if(allocated(dt)) then
            print *,"typeA is allocated"
            print *,dt%k,dt%i
         else
            print *,"typeA is not allocated"
         endif

      end subroutine

      subroutine sub3(dt)
         type(typeB(4)),pointer :: dt
         print *,"in sub3"
         if(associated(dt)) then
            print *,"typeB is associated"
            print *,dt%k,dt%i
         else
            print *,"typeB is not associated"
         endif

      end subroutine

      subroutine sub4(dt)
         type(typeB(2)),allocatable :: dt
         print *,"in sub4"
         if(allocated(dt)) then
            print *,"typeB is allocated"
            print *,dt%k,dt%i
         else
            print *,"typeB is not allocated"
         endif

      end subroutine

      subroutine sub5(dt)
         type(typeB(4)),pointer :: dt(:)
         print *,"in sub5"
         if(associated(dt)) then
            print *,"typeB is associated"
            print *,dt%k,dt%i
         else
            print *,"typeB is not associated"
         endif

      end subroutine

      subroutine sub6(dt)
         type(typeB(4)),pointer :: dt(:,:)
         print *,"in sub6"
         if(associated(dt)) then
            print *,"typeB is associated"
            print *,dt%k,dt(:,1)%i,dt(:,2)%i
         else
            print *,"typeB is not associated"
         endif

      end subroutine
end module

program nullGeneric09
   use m
   implicit none

   integer :: i

   type(typeA(4)),pointer     :: a1=>null()
   type(typeA(2)),allocatable :: a2
   type(typeB(4)),pointer     :: b1=>null()
   type(typeB(2)),allocatable :: b2
   type(typeB(4)),pointer     :: b3(:)=>null()
   type(typeB(4)),pointer     :: b4(:,:)=>null()
   type(any) :: any1

   type(typeA(4)),target      :: ta1
   type(typeA(2)),target      :: ta2
   type(typeB(4)),target      :: tb1
   type(typeB(2)),target      :: tb2

   if(associated(a1))                         error stop 10_4
   if(allocated(a2))                          error stop 11_4
   if(associated(b1))                         error stop 12_4
   if(allocated(b2))                          error stop 13_4
   if(associated(b3))                         error stop 14_4
   if(associated(b4))                         error stop 15_4

   a1=>ta1
   a2=ta2
   b1=>tb1
   b2=tb2

   allocate(b3(4),source=(/ (typeB(4)(i),i=1,4)/) )
   allocate(b4(2,2),source=reshape(b3,(/2,2/)) )

   if(.not. associated(a1,ta1))               error stop 16_4
   if(.not. allocated(a2))                    error stop 17_4
   if(.not. associated(b1,tb1))               error stop 18_4
   if(.not. allocated(b2))                    error stop 19_4
   if(.not. associated(b3))                   error stop 20_4
   if(.not. associated(b4))                   error stop 21_4

   call any1%proc(a1)
   call any1%proc(a2)
   call any1%proc(b1)
   call any1%proc(b2)
   call any1%proc(b3)
   call any1%proc(b4)

   call any1%proc(null(a1))
   call any1%proc(null(a2))
   call any1%proc(null(b1))
   call any1%proc(null(b2))
   call any1%proc(null(b3))
   call any1%proc(null(b4))

end program


      subroutine sub5(dt)
         use m,only : typeB
         type(typeB(4)),pointer :: dt(:)
         print *,"in sub5"
         if(associated(dt)) then
            print *,"typeB is associated"
            print *,dt%k,dt%i
         else
            print *,"typeB is not associated"
         endif

      end subroutine

      subroutine sub6(dt)
         use m,only : typeB
         type(typeB(4)),pointer :: dt(:,:)
         print *,"in sub6"
         if(associated(dt)) then
            print *,"typeB is associated"
            print *,dt%k,dt(:,1)%i,dt(:,2)%i
         else
            print *,"typeB is not associated"
         endif

      end subroutine
