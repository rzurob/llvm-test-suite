! GM DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/abstracti/functional/abstracti020.f

!************************************************************************
!* ======================================================================
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-26 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Testing: interface block
!*  poly abstract type return, interface of a deferred binding
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!* =====================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

type, abstract :: base(l1,k1)    ! (20,4)
   integer, kind :: k1
   integer, len  :: l1
   integer(k1)   :: id
contains
   procedure(itf), pass, deferred :: getbase
end type

abstract interface
   class(base(:,4)) function itf(a)
      import base
      class(base(*,4)), intent(in) :: a
      pointer :: itf
   end function
end interface

type, extends(base) :: child    ! (20,4)
contains
   procedure, pass :: getbase
end type

contains

class(base(:,4)) function getbase(a)
   class(child(*,4)), intent(in) :: a
   pointer getbase
   allocate (getbase,source=a)
end function

end module

program abstracti020l_dlp
   use m

   class(base(:,4)), pointer :: b1
   type(child(20,4)) :: c1 = child(20,4)(5)

   b1 => c1%getbase()

   if(b1%id .ne. 5) error stop 1_4

end program abstracti020l_dlp
