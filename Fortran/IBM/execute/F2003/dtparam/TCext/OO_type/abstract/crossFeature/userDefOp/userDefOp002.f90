! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/userDefOp/userDefOp002.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: userDefOp002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  User-defined operator and assignment
!*                               a) both operands are polymorphic abstract type for the operator in the interface and supplying
!*                                  2) scalar abstract declared type and extension declared type
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type

   interface operator(+)
      type(child(4,4,20)) function myAdd1(a,b)
         import base, child
         class(base(4)), intent(in) :: a, b
      end function
   end interface

   interface assignment(=)
      subroutine myAsgn1(a,b)
         import base, child
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in)  :: b
      end subroutine
   end interface

end module

program userDefOp002
   use m

   class(base(4)), allocatable :: b1,b3,b4
   class(child(4,4,20)), allocatable :: c2

   allocate(b1, source=child(4,4,20)(3) )
   allocate(c2, source=child(4,4,20)(1) )
   allocate(b3, source=(b1+c2) )
   allocate( b4, source=(b1+c2+b3) )

   if ( b3%id .ne. 4 ) error stop 1_4
   if ( b4%id .ne. 8 ) error stop 2_4

end program

type(child(4,4,20)) function myAdd1(a,b)
   use m, only: base, child
   class(base(4)), intent(in) :: a, b
   myAdd1%id = a%id + b%id
end function

subroutine myAsgn1(a,b)
   use m, only: base, child
   class(base(4)), intent(out) :: a
   class(base(4)), intent(in)  :: b
   a%id = b%id
end subroutine
