!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPolyGenericName01.f
!*
!*  DATE                       : Nov. 22 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  dummy argument in type-bound procedure is allocatable with deferred length parameter, invoke different specific procedure when passing different number of dummy argument through generic name
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k1,l1)
      integer,kind :: k1=4
      integer,len  :: l1=3
      integer(k1)  :: i(l1)=99
      contains
         procedure,nopass :: define1
         procedure,nopass :: define2
         procedure,nopass :: define3
         generic :: define=>define1,define2,define3
   end type

   contains
      subroutine define1(arg)
         type(dtp(2,:)),allocatable,intent(out) :: arg(:)

         allocate(dtp(2,4) :: arg(-3:-1))
         arg=[dtp(2,4)([1,2,3,4]),&
              dtp(2,4)([-1,-2,-3,-4]),&
              dtp(2,4)([-5,-2,-1,-4])]
      end subroutine

      subroutine define2(arg1,arg2)
         type(dtp(2,:)),allocatable,intent(out) :: arg1(:)
         type(dtp(2,*)),intent(in)              :: arg2(:)

         arg1=arg2
      end subroutine

      subroutine define3(arg1,arg2,int)
         type(dtp(2,:)),allocatable,intent(out) :: arg1(:)
         type(dtp(2,*)),intent(in)              :: arg2(:)
         integer :: int(:),j

         arg1=arg2
         do j=lbound(arg1,1),ubound(arg1,1)
           arg1(j)%i=arg1(j)%i+int
         end do

      end subroutine

end module

program dummyArgDeferNonPolyGenericName01
  use m
  implicit none

  type(dtp)  :: dt

  type(dtp(2,:)),allocatable :: dtp1(:)
  type(dtp(2,3))             :: dtp2(2)

  dtp2=[dtp(2,3)([10,11,12]),dtp(2,3)([-10,-11,-12])]

  call dt%define(dtp1)

  if(dtp1%l1 /= 4)                                 error stop 10_4
  if(any(dtp1(-3)%i /= [1,2,3,4]))                 error stop 11_4
  if(any(dtp1(-2)%i /= [-1,-2,-3,-4]))             error stop 12_4
  if(any(dtp1(-1)%i /= [-5,-2,-1,-4]))             error stop 13_4

  call dt%define(dtp1,dtp2)

  if(dtp1%l1 /= 3)                                 error stop 14_4
  if(any(dtp1(1)%i /= [10,11,12]))                 error stop 15_4
  if(any(dtp1(2)%i /= [-10,-11,-12]))              error stop 16_4

  call dt%define(dtp1,dtp2,[1,2,3])

  if(dtp1%l1 /= 3)                                 error stop 17_4
  if(any(dtp1(1)%i /= [11,13,15]))                 error stop 18_4
  if(any(dtp1(2)%i /= -9))                         error stop 19_4

end program
