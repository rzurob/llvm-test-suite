!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullGeneric03.f
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
!* 4. NULL(MOLD) IS USED AS ACTUAL ARGUMENT OF GENERIC TYPE-BOUND PROCEDURE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
      integer,kind :: k=8
      integer,len  :: l=20

      integer(k)   :: i=99
      contains
         procedure,nopass :: sub1
         procedure,nopass :: sub2
         procedure,nopass :: sub3
         generic :: sub => sub1,sub2,sub3
   end type

   contains

   subroutine sub1(dt)
       type(dtp(2,*)),pointer      :: dt(:)

         print *,"in sub1"
         if(associated(dt)) then
            print *,"dtp is associated"
            print *,dt%k,dt%l,dt%i
         else
            print *,"dtp is not associated"
         endif

    end subroutine

    subroutine sub2(dt)
         type(dtp(4,*)),pointer  :: dt(:)

         print *,"in sub2"
         if(associated(dt)) then
            print *,"dtp is associated"
            print *,dt%k,dt%l,dt%i
         else
            print *,"dtp is not associated"
         endif

    end subroutine

    subroutine sub3(dt1,dt2)
         type(dtp(2,*)),pointer  :: dt1(:)
         type(dtp(4,*)),pointer  :: dt2(:)

         print *,"in sub3"
         if(associated(dt1)) then
            print *,"dtp1 is associated"
            print *,dt1%k,dt1%l,dt1%i
         else
            print *,"dtp1 is not associated"
         endif
         if(associated(dt2)) then
            print *,"dtp2 is associated"
            print *,dt2%k,dt2%l,dt2%i
         else
            print *,"dtp2 is not associated"
         endif
    end subroutine
end module

program nullGeneric03
   use m
   implicit none

   integer :: j

   type(dtp)  :: obj

   type(dtp(2,3)),target  :: tar1(3)=(/ (dtp(2,3)(j),j=1,3) /)
   type(dtp(4,6)),target  :: tar2(3)=(/ (dtp(4,6)(j),j=11,13) /)
   type(dtp(2,3)),pointer :: dtp1(:)=>null()
   type(dtp(4,6)),pointer :: dtp2(:)=>null()

   print *,"1---"
   call obj%sub(null(dtp1))
   call obj%sub(null(dtp2))
   call obj%sub(null(dtp1),null(dtp2))

   print *,"2---"
   dtp1=>tar1
   dtp2=>tar2

   call obj%sub(dtp1)
   call obj%sub(dtp2)
   call obj%sub(dtp1,dtp2)

   print *,"3---"
   call obj%sub(null(dtp1))
   call obj%sub(null(dtp2))
   call obj%sub(null(dtp1),null(dtp2))
end program

