! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/syntax/C459/genericC459Operator004d.f
! opt variations: -qnol

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C459:  using private-binding-stmt
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)      i
      contains
         private
         generic :: operator(.myadd.) => add_i
         procedure, pass ( passobj ) :: add_i
         generic, public :: operator(.myadd.) => add_base
         procedure, pass, public :: add_base
   end type

   contains

      function add_i ( int, passobj )
         class(base(*,4)), intent(in) :: passobj
         integer, intent(in) :: int
         type(base(20,4)) :: add_i
         add_i%i = passobj%i + int
      end function

      function add_base ( passobj, base )
         class(base(*,4)), intent(in) :: passobj, base
         type(base(20,4)) :: add_base
         add_base%i = passobj%i + base%i
      end function

end module

program genericC459Operator004d
end program
