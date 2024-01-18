!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 5 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 361801
!234567490123456749012345674901234567490123456749012345674901234567490
module m
  type inner1(l1)
     integer,len   :: l1
     character(l1) :: c1
  end type
  type outer(l2)
    integer,len :: l2
    type(inner1(l2))  :: inn1comp(2)
  end type
  contains
      function getFun(tar)
         type(outer(*)),intent(in),target :: tar
         type(outer(3)),pointer :: getFun

         print *,"|",tar,"|"
         print *,"|",tar%inn1comp%c1,"|"
         print *,"|",tar%inn1comp,"|"

         getFun=>tar
      end function

end module

program d361801
  use m
  implicit none

   type(outer(3)),target  :: tar
   type(outer(3)),pointer :: ptr=>null()

   tar%inn1comp=[inner1(3)(c1="abc"),inner1(3)(c1="ABC") ]

   print *,"|",tar,"|",tar%inn1comp,"|",tar%inn1comp%c1,"|"

   ptr=>getFun(tar)

   print *,"|",ptr,"|",ptr%inn1comp,"|",ptr%inn1comp%c1,"|"

end program
