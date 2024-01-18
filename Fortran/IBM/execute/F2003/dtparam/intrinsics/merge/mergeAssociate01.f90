!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergegAssociate01.f
!*
!*  DATE                       : Sept. 22 2008
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
!* 3. TSOURCE AND FSOURCE ARE SCALR OR ARRAY
!* 4. USE ASSOCIATE
!* 5. DEFECT 356156
!234567890123456789012345678901234567890123456789012345678901234567890
module m

  type dtp(k,l)
     integer(2),kind :: k
     integer(8),len  :: l
     integer(k)      :: i
     character(l)    :: c
  end type

end module

program mergegAssociate01
   use m
   implicit none

   integer :: k

   logical :: mask1(6),mask2(2,3)

   type(dtp(4,3)),target  :: dtp1(6)=[( dtp(4,3)(i=k,c=char(k+64)),k=1,6 )]
   type(dtp(4,3)),target  :: dtp2(6)=[( dtp(4,3)(i=-k,c=char(k+96)),k=1,6 )]
   type(dtp(4,3)),target  :: dtp3(2,3),dtp4(2,3)

   type(dtp(4,:)),allocatable  :: dtp5(:),dtp6(:),dtp7(:,:),dtp8(:,:)
   type(dtp(4,:)),pointer      :: dtp9(:),dtp10(:),dtp11(:,:),dtp12(:,:)

   mask1=[.true.,.false.,.true.,.false.,.true.,.false.]

   mask2=reshape(mask1,(/2,3/))

   dtp3=reshape(dtp1,(/2,3/))
   dtp4=reshape(dtp2,(/2,3/))

   dtp5=dtp1
   dtp6=dtp2

   dtp9=>dtp1
   dtp10=>dtp2

   allocate(dtp11(2,3),source=dtp3)
   allocate(dtp12(2,3),source=dtp4)


   associate(x=>merge(dtp1(1),dtp1(2),.true.))
      if(x%k /= 4)                                  error stop 10_4
      if(x%l /= 3)                                  error stop 11_4
      if(x%i /= 1)                                  error stop 12_4
      if(x%c /= "A")                                error stop 13_4
   end associate

   associate(x=>merge(dtp1(1),dtp1(2),.false.))
      if(x%i /= 2)                                  error stop 14_4
      if(x%c /= "B")                                error stop 15_4
   end associate

   associate(x=>merge(dtp1,dtp2,mask1))
      if(x%k /= 4)                                  error stop 16_4
      if(x%l /= 3)                                  error stop 17_4
      if(any(x%i /= [1,-2,3,-4,5,-6]))              error stop 18_4
      if(any(x%c /= ["A","b","C","d","E","f"]))     error stop 19_4

   end associate

   associate(x=>merge(dtp1,dtp2,.not. mask1))
      if(x%k /= 4)                                  error stop 20_4
      if(x%l /= 3)                                  error stop 21_4
      if(any(x%i /= [-1,2,-3,4,-5,6]))              error stop 22_4
      if(any(x%c /= ["a","B","c","D","e","F"]))     error stop 23_4
   end associate

   associate(x=>merge(dtp3,dtp4,mask2))
      if(x%k /= 4)                                  error stop 24_4
      if(x%l /= 3)                                  error stop 25_4
      if(any(x(1,:)%i /= [1,3,5]))                  error stop 26_4
      if(any(x(2,:)%i /= [-2,-4,-6]))               error stop 27_4
      if(any(x(1,:)%c /= ["A","C","E"]))            error stop 28_4
      if(any(x(2,:)%c /= ["b","d","f"]))            error stop 29_4
   end associate

   associate(x=>merge(dtp3,dtp4,.not. mask2))
      if(x%k /= 4)                                  error stop 30_4
      if(x%l /= 3)                                  error stop 31_4
      if(any(x(1,:)%i /= [-1,-3,-5]))               error stop 32_4
      if(any(x(2,:)%i /= [2,4,6]))                  error stop 33_4
      if(any(x(1,:)%c /= ["a","c","e"]))            error stop 34_4
      if(any(x(2,:)%c /= ["B","D","F"]))            error stop 35_4
   end associate

   !--- test allocatable---!

   call associate36(merge(dtp5(1),dtp5(2),.true.))

   call associate40 (merge(dtp5(1),dtp5(2),.false.))

   call associate42(x=merge(dtp5,dtp6,mask1))

   call associate46(merge(dtp5,dtp6,.not. mask1))

!   call associate50(x=merge(dtp7,dtp8,mask2))
!
!   call associate56(merge(dtp7,dtp8,.not. mask2))

   !--- test pointer---!

   call associate62(merge(dtp9(1),dtp9(2),.true.))

   call associate66(merge(dtp9(1),dtp9(2),.false.))

   call associate68(x=merge(dtp9,dtp10,mask1))

   call associate72(merge(dtp9,dtp10,.not. mask1))

   call associate76(merge(dtp11,dtp12,mask2))

   call associate82(merge(dtp11,dtp12,.not. mask2))

   contains

!   associate(x=>merge(dtp5(1),dtp5(2),.true.))
   subroutine associate36(x)
      type(dtp(4,*)), intent(in) :: x

      if(x%k /= 4)                                  error stop 36_4
      if(x%l /= 3)                                  error stop 37_4
      if(x%i /= 1)                                  error stop 38_4
      if(x%c /= "A")                                error stop 39_4
   end subroutine

!   associate(x=>merge(dtp5(1),dtp5(2),.false.))
   subroutine associate40(x)
      type(dtp(4,*)), intent(in) :: x

      if(x%i /= 2)                                  error stop 40_4
      if(x%c /= "B")                                error stop 41_4
   end subroutine

!   associate(x=>merge(dtp5,dtp6,mask1))
   subroutine associate42(x)
      type(dtp(4,*)), intent(in) :: x(:)

      if(x%k /= 4)                                  error stop 42_4
      if(x%l /= 3)                                  error stop 43_4
      if(any(x%i /= [1,-2,3,-4,5,-6]))              error stop 44_4
      if(any(x%c /= ["A","b","C","d","E","f"]))     error stop 45_4
   end subroutine

!   associate(x=>merge(dtp5,dtp6,.not. mask1))
   subroutine associate46(x)
      type(dtp(4,*)), intent(in) :: x(:)

      if(x%k /= 4)                                  error stop 46_4
      if(x%l /= 3)                                  error stop 47_4
      if(any(x%i /= [-1,2,-3,4,-5,6]))              error stop 48_4
      if(any(x%c /= ["a","B","c","D","e","F"]))     error stop 49_4
   end subroutine

!   associate(x=>merge(dtp7,dtp8,mask2))
   subroutine associate50(x)
      type(dtp(4,*)), intent(in) :: x(:,:)

      if(x%k /= 4)                                  error stop 50_4
      if(x%l /= 3)                                  error stop 51_4
      if(any(x(1,:)%i /= [1,3,5]))                  error stop 52_4
      if(any(x(2,:)%i /= [-2,-4,-6]))               error stop 53_4
      if(any(x(1,:)%c /= ["A","C","E"]))            error stop 54_4
      if(any(x(2,:)%c /= ["b","d","f"]))            error stop 55_4
   end subroutine

!   associate(x=>merge(dtp7,dtp8,.not. mask2))
   subroutine associate56(x)
      type (dtp(4,*)), intent(in) :: x(:,:)

      if(x%k /= 4)                                  error stop 56_4
      if(x%l /= 3)                                  error stop 57_4
      if(any(x(1,:)%i /= [-1,-3,-5]))               error stop 58_4
      if(any(x(2,:)%i /= [2,4,6]))                  error stop 59_4
      if(any(x(1,:)%c /= ["a","c","e"]))            error stop 60_4
      if(any(x(2,:)%c /= ["B","D","F"]))            error stop 61_4
   end subroutine

!   associate(x=>merge(dtp9(1),dtp9(2),.true.))
   subroutine associate62(x)
      type (dtp(4,*)), intent(in) :: x
      if(x%k /= 4)                                  error stop 62_4
      if(x%l /= 3)                                  error stop 63_4
      if(x%i /= 1)                                  error stop 64_4
      if(x%c /= "A")                                error stop 65_4
   end subroutine

!   associate(x=>merge(dtp9(1),dtp9(2),.false.))
   subroutine associate66(x)
      type(dtp(4,*)), intent(in) :: x

      if(x%i /= 2)                                  error stop 66_4
      if(x%c /= "B")                                error stop 67_4
   end subroutine

!   associate(x=>merge(dtp9,dtp10,mask1))
   subroutine associate68(x)
      type(dtp(4,*)), intent(in) :: x(:)

      if(x%k /= 4)                                  error stop 68_4
      if(x%l /= 3)                                  error stop 69_4
      if(any(x%i /= [1,-2,3,-4,5,-6]))              error stop 70_4
      if(any(x%c /= ["A","b","C","d","E","f"]))     error stop 71_4
   end subroutine

!   associate(x=>merge(dtp9,dtp10,.not. mask1))
   subroutine associate72(x)
      type(dtp(4,*)), intent(in) :: x(:)

      if(x%k /= 4)                                  error stop 72_4
      if(x%l /= 3)                                  error stop 73_4
      if(any(x%i /= [-1,2,-3,4,-5,6]))              error stop 74_4
      if(any(x%c /= ["a","B","c","D","e","F"]))     error stop 75_4
   end subroutine

!   associate(x=>merge(dtp11,dtp12,mask2))
   subroutine associate76(x)
      type(dtp(4,*)), intent(in) :: x(:,:)

      if(x%k /= 4)                                  error stop 76_4
      if(x%l /= 3)                                  error stop 77_4
      if(any(x(1,:)%i /= [1,3,5]))                  error stop 78_4
      if(any(x(2,:)%i /= [-2,-4,-6]))               error stop 79_4
      if(any(x(1,:)%c /= ["A","C","E"]))            error stop 80_4
      if(any(x(2,:)%c /= ["b","d","f"]))            error stop 81_4
   end subroutine

!   associate(x=>merge(dtp11,dtp12,.not. mask2))
   subroutine associate82(x)
      type(dtp(4,*)), intent(in) :: x(:,:)

      if(x%k /= 4)                                  error stop 82_4
      if(x%l /= 3)                                  error stop 83_4
      if(any(x(1,:)%i /= [-1,-3,-5]))               error stop 84_4
      if(any(x(2,:)%i /= [2,4,6]))                  error stop 85_4
      if(any(x(1,:)%c /= ["a","c","e"]))            error stop 86_4
      if(any(x(2,:)%c /= ["B","D","F"]))            error stop 87_4
   end subroutine

end program
