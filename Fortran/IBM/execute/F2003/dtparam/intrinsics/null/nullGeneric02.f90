!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullGeneric02.f
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
!* 3. MOLD IS ALLOCATABLE
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

program nullGeneric02
   use m
   implicit none

   interface sub
     subroutine sub1(dt)
       import
       type(dtp(2,*)),allocatable  :: dt
     end subroutine
     subroutine sub2(dt)
       import
       type(dtp(4,*)),allocatable  :: dt
     end subroutine
   end interface

   type(dtp(2,3)),allocatable :: dtp1
   type(dtp(4,6)),allocatable :: dtp2
   type(dtp(2,5)),allocatable :: dtp3
   type(dtp(4,10)),allocatable :: dtp4

   type(dtp(2,3))   :: tar1=dtp(2,3)(int=1,ch="123")
   type(dtp(4,6))   :: tar2=dtp(4,6)(int=2,ch="456")
   type(dtp(2,5))   :: tar3=dtp(2,5)(int=11,ch="000")
   type(dtp(4,10))  :: tar4=dtp(4,10)(int=22,ch="111")

   call sub(dtp1)
   call sub(dtp2)
   call sub(dtp3)
   call sub(dtp4)

   dtp1=tar1
   dtp2=tar2
   dtp3=tar3
   dtp4=tar4

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
         type(dtp(2,*)),allocatable  :: dt

         print *,"in sub1"
         if(allocated(dt)) then
            print *,"dtp is allocated"
            print *,dt%k,dt%l
            print *,dt%int,dt%ch
         else
            print *,"dtp is not allocated"
         endif

      end subroutine

      subroutine sub2(dt)
         use m,only : dtp
         type(dtp(4,*)),allocatable  :: dt

         print *,"in sub2"
         if(allocated(dt)) then
            print *,"dtp is allocated"
            print *,dt%k,dt%l
            print *,dt%int,dt%ch
         else
            print *,"dtp is not allocated"
         endif

      end subroutine
