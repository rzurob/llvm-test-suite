!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 19 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356490
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: dtp (l)
     integer, len :: l = 3
     character(l)   :: c=merge("xlftest","1234567",.true.)
   end type
end module

program d356490_1
   use m
   implicit none

   type(dtp) :: dtp1
   class(dtp(:)), pointer :: x1(:), x2
   type(dtp(:)), allocatable :: x3(:,:)

   allocate (dtp(5) :: x1(3))
   allocate (dtp :: x2)
   allocate (dtp(20) :: x3(2,2))

   if (dtp1%c /= 'xlf') error stop 1
   if (any (x1%c /= 'xlfte')) error stop 2
   if (x2%c /= 'xlf') error stop 3
   if (any(x3%c /= 'xlftest')) error stop 4

end  program

