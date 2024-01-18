!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullGeneric06.f
!*
!*  DATE                       : Sept. 23 2008
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
!* 3. MOLD IS POLYMORPHIC ALLOCATABLE
!* 4. NULL(MOLD) IS USED AS ACTUAL ARGUMENT OF GENERIC TYPE-BOUND PROCEDURE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1)
      integer,kind :: k1=2
      integer(k1)  :: i=1
      contains
         procedure,nopass :: bsub1
         procedure,nopass :: bsub2
         procedure,nopass :: bsub3
         generic :: sub=>bsub1,bsub2,bsub3
   end type
   type,extends(base) :: child(k2)
       integer,kind :: k2=1
       integer(k2)  :: j=-1
   end type

   contains

       subroutine bsub1(dt)
          class(base(2)),allocatable :: dt
          print *,"bsub1"
          if(allocated(dt)) then
             select type(dt)
                type is(base(2))
                   print *,"dt is allocated"
                   print *,"dt is base"
                   print *,dt%k1,dt%i
                type is(child(2,1))
                   print *,"dt is allocated"
                   print *,"dt is child"
                   print *,dt%k1,dt%k2,dt%i,dt%j
                class default
                   error stop 100_4
             end select
          else
             print *,"dt is not allocated"
          endif
       end subroutine

       subroutine bsub2(dt)
          class(base(4)),allocatable :: dt
          print *,"bsub2"
          if(allocated(dt)) then
             select type(dt)
                type is(base(4))
                   print *,"dt is allocated"
                   print *,"dt is base"
                   print *,dt%k1,dt%i
                type is(child(4,8))
                   print *,"dt is allocated"
                   print *,"dt is child"
                   print *,dt%k1,dt%k2,dt%i,dt%j
                class default
                   error stop 101_4
             end select
          else
             print *,"dt is not allocated"
          endif
       end subroutine

       subroutine bsub3(dt1,dt2)
          class(base(2)),allocatable :: dt1
          class(base(4)),allocatable :: dt2

          print *,"bsub3"
          if(allocated(dt1)) then
             select type(dt1)
                type is(base(2))
                   print *,"dt1 is allocated"
                   print *,"dt1 is base"
                   print *,dt1%k1,dt1%i
                type is(child(2,1))
                   print *,"dt1 is allocated"
                   print *,"dt1 is child"
                   print *,dt1%k1,dt1%k2,dt1%i,dt1%j
                class default
                   error stop 102_4
             end select
          else
             print *,"dt1 is not allocated"
          endif

          if(allocated(dt2)) then
             select type(dt2)
                type is(base(4))
                   print *,"dt2 is allocated"
                   print *,"dt2 is base"
                   print *,dt2%k1,dt2%i
                type is(child(4,8))
                   print *,"dt2 is allocated"
                   print *,"dt2 is child"
                   print *,dt2%k1,dt2%k2,dt2%i,dt2%j
                class default
                   error stop 103_4
             end select
          else
             print *,"dt2 is not allocated"
          endif
       end subroutine

end module

program nullGeneric06
   use m
   implicit none

   class(base),allocatable :: b1

   type(child(2,4)) :: c1

   class(base(2)),allocatable  :: dtp1
   class(base(4)),allocatable  :: dtp2

   allocate(dtp1,source=base(2)())
   allocate(dtp2,source=base(4)(i=2))

   allocate(b1,source=base())

   call b1%sub(dtp1)
   call b1%sub(dtp2)
   call b1%sub(dtp1,dtp2)

   call b1%sub(null(dtp1))
   call b1%sub(null(dtp2))
   call b1%sub(null(dtp1),null(dtp2))

   if(allocated(dtp1))  deallocate(dtp1)
   if(allocated(dtp2))  deallocate(dtp2)

   allocate(dtp1,source=child(2,1)(i=3,j=-3))
   allocate(dtp2,source=child(4,8)(i=4,j=-4))

   call b1%sub(dtp1)
   call b1%sub(dtp2)
   call b1%sub(dtp1,dtp2)

   call b1%sub(null(dtp1))
   call b1%sub(null(dtp2))
   call b1%sub(null(dtp1),null(dtp2))

end program

