!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 29 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. FROM IS UNALLOCATED,CALL MOVE_ALLOC IN SUBROUTINE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
      integer,kind :: k
      integer,len  :: l
      integer(k)   :: i(l)
   end type
end module

program move_allocFromIsUnallocated02

  use m
  implicit none

  integer :: i

  type(dtp(2,4)),allocatable  :: dtp1
  type(dtp(2,4)),allocatable  :: dtp2
  type(dtp(2,:)),allocatable  :: dtp3
  class(dtp(2,4)),allocatable :: dtp4
  class(dtp(2,:)),allocatable :: dtp5

  dtp2=dtp(2,4)(i=[1,2,3,4])
  dtp3=dtp2
  allocate(dtp4,source=dtp(2,4)(i=[11,12,13,14]))
  allocate(dtp5,source=dtp4)

  if(.not. allocated(dtp2))                       error stop 10_4
  if(.not. allocated(dtp3))                       error stop 11_4
  if(.not. allocated(dtp4))                       error stop 12_4
  if(.not. allocated(dtp5))                       error stop 13_4

  call move_alloc1(dtp1,dtp2)
  call move_alloc2(dtp1,dtp3)
  call move_alloc3(dtp1,dtp4)
  call move_alloc4(dtp1,dtp5)

  if(allocated(dtp2))                             error stop 14_4
  if(allocated(dtp3))                             error stop 15_4
  if(allocated(dtp4))                             error stop 16_4
  if(allocated(dtp5))                             error stop 17_4

  contains

     subroutine move_alloc1(from,to)
         type(dtp(2,*)),intent(inout),allocatable :: from
         type(dtp(2,4)),intent(out),allocatable   :: to

         call move_alloc(from,to)

     end subroutine

     subroutine move_alloc2(from,to)
         type(dtp(2,*)),intent(inout),allocatable :: from
         type(dtp(2,:)),intent(out),allocatable   :: to

         call move_alloc(from,to)

     end subroutine

     subroutine move_alloc3(from,to)
         type(dtp(2,*)),intent(inout),allocatable :: from
         class(dtp(2,4)),intent(out),allocatable   :: to

         call move_alloc(from,to)

     end subroutine

     subroutine move_alloc4(from,to)
         type(dtp(2,*)),intent(inout),allocatable :: from
         class(dtp(2,:)),intent(out),allocatable   :: to

         call move_alloc(from,to)

     end subroutine

end program

