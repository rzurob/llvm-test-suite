!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359210.f
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
!*  DEFECT 359210
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len :: l1
      integer     :: i(l1)
   end type

   type,extends(base) :: child(l2)
      integer,len  :: l2
      type(base(2)),pointer :: basecomp=>null()
   end type
end module

program d359210
  use m
  implicit none

  class(base(:)),target,allocatable :: tarchild(:)

  type(base(2)),target    :: tarbase(2:3)

  tarbase=[base(2)([1,2]),base(2)([3,4])]
  allocate(tarchild(2),source=[child(2,3)([1,2]),child(2,3)([3,4])])
  select type(tarchild)
      type is(child(*,*))
         tarchild(1)%basecomp=>tarbase(2)
         tarchild(2)%basecomp=>tarbase(3)
         print *,associated(tarchild(2)%basecomp)
         print *,associated(tarchild(1)%basecomp)
      class default
         error stop 10_4
  end select

end program
