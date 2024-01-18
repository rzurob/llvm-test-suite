! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/syntax/C459/genericC459Operator003.f
! opt variations: -ql

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
!*  DESCRIPTION                : C459: functional TC, private and public specific typebound
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)      i
      contains
         generic :: operator(.myadd.) => add_i
         procedure :: add_i
         generic, public :: operator(.myadd.) => add_base
         procedure, pass, private :: add_base
   end type

   contains

      function add_i ( passobj , int )
         class(base(4)), intent(in) :: passobj
         integer, intent(in) :: int
         type(base(4)) :: add_i
         add_i%i = passobj%i + int
      end function

      function add_base ( passobj, base )
         class(base(4)), intent(in) :: passobj, base
         type(base(4)) :: add_base
         add_base%i = passobj%i + base%i
      end function

end module

program genericC459Operator003
   use m

   type(base(4)), allocatable :: b1, b2

   allocate ( base(4) :: b1, b2 )

   b1%i = 10
   b2%i = 20

   b1 = b1 .myadd. b2
   if ( b1%i /= 30 ) error stop 1_4

   b2 = b1 .myadd. 10
   if ( b2%i /= 40 ) error stop 2_4

end program
