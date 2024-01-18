! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/syntax/C459/genericC459Operator001d.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : C459: define generic TB with same generic name with different access-spec
!*                                     within the same derived type
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
         generic :: operator(+) => add_i
         procedure, pass ( passobj ) :: add_i
         generic, public :: operator(+) => add_base
         procedure, pass, private :: add_base
         generic, private :: operator(+) => add_base_array
         procedure, pass :: add_base_array
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

      function add_base_array ( passobj, basearray )
         class(base(*,4)), intent(in) :: passobj, basearray(:)
         type(base(20,4)) :: add_base_array
         add_base_array%i = passobj%i + basearray(1)%i
      end function

end module

program genericC459Operator001d
end program
