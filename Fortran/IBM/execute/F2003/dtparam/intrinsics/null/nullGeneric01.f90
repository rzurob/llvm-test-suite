!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullGeneric01.f
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
!* 3. MOLD IS POINTER
!* 4. NULL(MOLD) IS USED AS ACTUAL ARGUMENT OF GENERIC PROCEDURE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
      integer,kind :: k
      integer,len  :: l

      integer(k)   :: int
      character(l) :: ch
   end type

end module

program nullGeneric01
   use m
   implicit none

   interface sub
     subroutine sub1(dt)
       import
       type(dtp(2,*)),pointer  :: dt
     end subroutine
     subroutine sub2(dt)
       import
       type(dtp(4,*)),pointer  :: dt
     end subroutine
   end interface

   type(dtp(2,3)),pointer :: dtp1=>null()
   type(dtp(4,6)),pointer :: dtp2=>null()
   type(dtp(2,5)),pointer :: dtp3=>null()
   type(dtp(4,10)),pointer :: dtp4=>null()

   type(dtp(2,3)),target   :: tar1=dtp(2,3)(int=1,ch="123")
   type(dtp(4,6)),target   :: tar2=dtp(4,6)(int=2,ch="456")
   type(dtp(2,5)),target   :: tar3=dtp(2,5)(int=11,ch="000")
   type(dtp(4,10)),target  :: tar4=dtp(4,10)(int=22,ch="111")

   call sub(dtp1)
   call sub(dtp2)
   call sub(dtp3)
   call sub(dtp4)

   dtp1=>tar1
   dtp2=>tar2
   dtp3=>tar3
   dtp4=>tar4

   call sub(dtp1)
   call sub(dtp2)
   call sub(dtp3)
   call sub(dtp4)

   call sub(null(dtp1))
   call sub(null(dtp2))
   call sub(null(dtp3))
   call sub(null(dtp4))

end program

      subroutine sub1(dt)
         use m,only : dtp
         type(dtp(2,*)),pointer  :: dt

         print *,"in sub1"
         if(associated(dt)) then
            print *,"dtp is associated"
            print *,dt%k,dt%l
            print *,dt%int,dt%ch
         else
            print *,"dtp is not associated"
         endif

      end subroutine

      subroutine sub2(dt)
         use m,only : dtp
         type(dtp(4,*)),pointer  :: dt

         print *,"in sub2"
         if(associated(dt)) then
            print *,"dtp is associated"
            print *,dt%k,dt%l
            print *,dt%int,dt%ch
         else
            print *,"dtp is not associated"
         endif

      end subroutine
