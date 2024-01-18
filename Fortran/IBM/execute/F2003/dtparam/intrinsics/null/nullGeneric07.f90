!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullGeneric07.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 24 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD]) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.88 
!* 2. NULL([MOLD])
!* 3. MOLD IS POLYMORPHIC POINTER 
!* 4. NULL(MOLD) IS USED AS ACTUAL ARGUMENT OF GENERIC PROCEDURE 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1)
      integer,kind :: k1=2
      integer(k1)  :: i=1
   end type
   type,extends(base) :: child(k2)
       integer,kind :: k2=1
       integer(k2)  :: j=-1
   end type
end module

program nullGeneric07
   use m
   implicit none

   interface sub

       subroutine bsub1(dt)
          import
          class(base(2)),pointer :: dt
       end subroutine
       subroutine bsub2(dt)
          import
          class(base(4)),pointer :: dt
       end subroutine
       subroutine bsub3(dt1,dt2)
          import
          class(base(2)),pointer :: dt1
          class(base(4)),pointer :: dt2
       end subroutine

   end interface

   class(base(2)),pointer  :: dtp1=>null()
   class(base(4)),pointer  :: dtp2=>null()

   allocate(dtp1,source=base(2)())
   allocate(dtp2,source=base(4)(i=2))

   call sub(dtp1)
   call sub(dtp2)
   call sub(dtp1,dtp2)
   
   call sub(null(dtp1))
   call sub(null(dtp2))
   call sub(null(dtp1),null(dtp2))

   allocate(dtp1,source=child(2,1)(i=3,j=-3))
   allocate(dtp2,source=child(4,8)(i=4,j=-4))
 
   call sub(dtp1)
   call sub(dtp2) 
   call sub(dtp1,dtp2)

   call sub(null(dtp1))
   call sub(null(dtp2))
   call sub(null(dtp1),null(dtp2))

end program

       subroutine bsub1(dt)
          use m
          class(base(2)),pointer :: dt
          print *,"bsub1"
          if(associated(dt)) then
             select type(dt)
                type is(base(2))
                   print *,"dt is associated"
                   print *,"dt is base"
                   print *,dt%k1,dt%i
                type is(child(2,1))
                   print *,"dt is associated"
                   print *,"dt is child"
                   print *,dt%k1,dt%k2,dt%i,dt%j
                class default
                    error stop 100_4
             end select
          else
             print *,"dt is not associated"
          endif
       end subroutine

       subroutine bsub2(dt)
          use m
          class(base(4)),pointer :: dt
          print *,"bsub2"
          if(associated(dt)) then
             select type(dt)
                type is(base(4))
                   print *,"dt is associated"
                   print *,"dt is base"
                   print *,dt%k1,dt%i
                type is(child(4,8))
                   print *,"dt is associated"
                   print *,"dt is child"
                   print *,dt%k1,dt%k2,dt%i,dt%j
                class default
                   error stop 101_4
             end select
          else
             print *,"dt is not associated"
          endif
       end subroutine

       subroutine bsub3(dt1,dt2)
          use m
          class(base(2)),pointer :: dt1
          class(base(4)),pointer :: dt2

          print *,"bsub3"
          if(associated(dt1)) then
             select type(dt1)
                type is(base(2))
                   print *,"dt1 is associated"
                   print *,"dt1 is base"
                   print *,dt1%k1,dt1%i
                type is(child(2,1))
                   print *,"dt1 is associated"
                   print *,"dt1 is child"
                   print *,dt1%k1,dt1%k2,dt1%i,dt1%j
                class default
                   error stop 103_4
             end select
          else
             print *,"dt1 is not associated"
          endif

          if(associated(dt2)) then
             select type(dt2)
                type is(base(4))
                   print *,"dt2 is associated"
                   print *,"dt2 is base"
                   print *,dt2%k1,dt2%i
                type is(child(4,8))
                   print *,"dt2 is associated"
                   print *,"dt2 is child"
                   print *,dt2%k1,dt2%k2,dt2%i,dt2%j
                class default
                   error stop 104_4
             end select
          else
             print *,"dt2 is not associated"
          endif
       end subroutine
