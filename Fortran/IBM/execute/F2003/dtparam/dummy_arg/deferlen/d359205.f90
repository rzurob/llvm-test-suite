!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359205.f
!*
!*  DATE                       : Nov. 20 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 359205
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len :: l1
      integer     :: i(l1)
   end type

   type,extends(base) :: child(l2)
      integer,len  :: l2
      type(base(3)),pointer :: basecomp=>null()
   end type
end module
program d359205
  use m
  implicit none

  class(base(:)),target,allocatable :: tarchild(:)

  type(base(3)),target    :: tarbase(2:3)

  tarbase=[base(3)(i=[3,4,5]),base(3)(i=[-3,-4,-5])]

  allocate(tarchild(2),source=[child(2,3)(i=[1,2]),child(2,3)(i=[-1,-2])])

  select type(tarchild)
      type is(child(*,*))
         tarchild(1)%basecomp=>tarbase(2)
         tarchild(2)%basecomp=>tarbase(3)
      class default
         error stop 10_4
  end select

  call sub1(tarchild)

  contains

    subroutine sub1(arg)
       class(base(:)),target,allocatable :: arg(:)

       print *,arg(1)%i,arg(2)%i
    end subroutine

end program

