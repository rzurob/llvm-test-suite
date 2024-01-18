!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPolyGenericName02.f
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
!*   dummy argument in type-bound procedure is pointer with deferred length parameter,invoke different specific procedure when passing different numbers of argument through generic name
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
      subroutine define1(ptr,tar)
         type(dtp(2,:)),pointer,intent(inout)  :: ptr(:)
         type(dtp(2,*)),target,intent(in)      :: tar(:)

         ptr(3:)=>tar
      end subroutine

      subroutine define2(ptr)
         type(dtp(2,:)),pointer,intent(out)    :: ptr(:)

         allocate(ptr(-1:0),source= &
           [dtp(2,5)(i=[-1,-2,-3,-4,-5]), &
            dtp(2,5)(i=[-11,-12,-13,-14,-15])])

      end subroutine

      subroutine define3(ptr1,tar1,tar2,tar3)
         type(dtp(2,:)),pointer,intent(inout)  :: ptr1(:)
         type(dtp(2,*)),target,intent(in)      :: tar1,tar2,tar3
         type(dtp(2,3)),save,target            :: tar4(3)

         tar4=[tar3,tar2,tar1]

         ptr1(0:)=>tar4

      end subroutine

end module

program dummyArgDeferNonPolyGenericName02
  use m
  implicit none

  type(dtp) :: dt

  type(dtp(2,:)),pointer :: ptr1(:)=>null(),ptr2(:)=>null()

  type(dtp(2,3)),target  :: tar1(3)

  tar1=[dtp(2,3)(i=[1,2,3]),dtp(2,3)(i=[11,12,13]) ,dtp(2,3)(i=[21,22,23])]

  call dt%define(ptr1,tar1)

  if(lbound(ptr1,1) /= 3)                     error stop 10_4
  if(ubound(ptr1,1) /= 5)                     error stop 11_4
  if(any(ptr1(3)%i /= [1,2,3]))               error stop 12_4
  if(any(ptr1(4)%i /= [11,12,13]))            error stop 13_4
  if(any(ptr1(5)%i /= [21,22,23]))            error stop 14_4

  call dt%define(ptr2)

  if(lbound(ptr2,1) /= -1)                        error stop 15_4
  if(ubound(ptr2,1) /= 0)                         error stop 16_4
  if(any(ptr2(-1)%i /= [-1,-2,-3,-4,-5]))         error stop 17_4
  if(any(ptr2(0)%i /= [-11,-12,-13,-14,-15]))     error stop 18_4

  call dt%define(ptr1,tar1(1),tar1(2),tar1(3))

  if(lbound(ptr1,1) /= 0)                         error stop 19_4
  if(ubound(ptr1,1) /= 2)                         error stop 20_4
  if(any(ptr1(0)%i /= [21,22,23]))                error stop 21_4
  if(any(ptr1(1)%i /= [11,12,13]))                error stop 22_4
  if(any(ptr1(2)%i /= [1,2,3]))                   error stop 23_4

end program
