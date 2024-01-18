! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C611/userDefAssgn001.f
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        R614: structure-component is data-ref
!*                                        non-polymorphic abstract type data-ref assigned data, user defined assignment
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
   end type

end module

program userDefAssgn001
   use m

   interface assignment (=)
      subroutine aaa ( a, b )
         import base
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in) :: b
      end subroutine
   end interface


   class(base(4)), allocatable :: a, b
   type(child(4,4)) :: c

   allocate ( a, source = child(4,4)(1,2.3))
   allocate ( b, source = child(4,4)(2,3.4))

   a = b             !<- legal
   select type ( b )
      class is ( child(4,4) )
         a = b%base  !<- illegal
         c%base = b  !<- illegal
   end select

end program

subroutine aaa ( a, b )
   use m
   class(base(4)), intent(out) :: a
   class(base(4)), intent(in) :: b

   a%id = b%id
end subroutine
