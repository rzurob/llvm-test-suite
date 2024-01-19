!**********************************************************************
!*  ===================================================================
!*
!*                               (derived from abstracti005kl_dlp_rb)
!*
!*  DATE                       : 2007-10-09
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: No Conversion from an Extended Type
!*                               in a Structure Constructor for an Allocatable
!*                               Polymorphic Component
!*
!*  DESCRIPTION                :
!*  The Reduced Code below fails during the Compile Step on an Intrinsic
!*  Assignment statement where a Structure Constructor is assigned to a
!*  variable of Derived Type.  The Derived Type contains an allocatable
!*  polymorphic component.
!*
!*  NOTE:  This Defect may be a duplicate of Defect 339738.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type :: abstractdata(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
   end type

   type, extends(abstractdata) :: data    ! (4,20)
      integer(k1) :: i = 1
   end type

   type :: base(k2)    ! (3,4)
      integer, kind                          :: k2
      class(abstractdata(k2,:)), allocatable :: d
   end type

end module

program d342451
   use m

   type(base(4)) :: b1

   b1 =  base(4)(d=data(4,1)())

end program d342451
