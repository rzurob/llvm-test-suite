!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356506.f
!*
!*  DATE                       : Sept. 21 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356506
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k,l)
     integer,kind :: k
     integer,len  :: l
  end type
end module

program d356506
   use m
   implicit none

   integer :: i1(6)=[1,2,3,4,5,6]
   integer :: i2(2,3)
   integer,allocatable :: i3(:,:)

   type(dtp(4,3)) :: dtp2(2,3)
   type(dtp(4,:)),allocatable :: dtp3(:,:)

   logical :: mask1(2)
   logical :: mask2(2,3,2)

   i2=reshape(i1,(/2,3/))

   i3=merge(i2,i2,[.true.,.false.])!<=== compile pass at this line
   i3=merge(i2,i2,mask1)           !<=== compile pass at this line
   i3=merge(i2,i2,mask2)           !<=== compile fail at this line

   dtp3=merge(dtp2,dtp2,[.true.,.false.]) !<=== compile pass at this line
   dtp3=merge(dtp2,dtp2,mask1)    !<=== compile pass at this line
   dtp3=merge(dtp2,dtp2,mask2)    !<=== compile fail at this line

end program

