! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentDeferred004.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

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
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: with deferred binding in type components
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

   type, abstract :: inner(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure(itf), deferred, pass :: assignmt
         generic, private :: assignment(=) => assignmt
   end type

   type, extends(inner) :: cinner    ! (4)
      integer(k1) :: j
      contains
         procedure, pass :: assignmt => cassignmt
   end type

   interface
      subroutine itf ( a, b )
         import inner
         class(inner(4)), intent(out) :: a
         class(inner(4)), intent(in)  :: b
      end subroutine
   end interface

   type base(k2)    ! (4)
      integer, kind                 :: k2
      class(inner(k2)), allocatable :: in1
      class(inner(k2)), allocatable :: in2
      contains
         procedure, pass :: bassignmt
         generic :: assignment(=) => bassignmt
   end type

   contains

   subroutine cassignmt ( a, b )
      class(cinner(4)), intent(out) :: a
      class(inner(4)), intent(in)   :: b

      a%i = b%i

      select type ( b )
         type is ( cinner(4) )
            a%j = b%j
         class default
            error stop 1_4
      end select

   end subroutine

   subroutine bassignmt ( a, b )
      class(base(4)), intent(out) :: a
      class(base(4)), intent(in)   :: b

      if ( .not. allocated(a%in1) ) allocate ( cinner(4):: a%in1 )
      if ( .not. allocated(a%in2) ) allocate ( cinner(4):: a%in2 )

      a%in1 = b%in1
      a%in2 = b%in2

   end subroutine

end module

program genericAssignmentDeferred004
   use m

   class(base(4)), allocatable, target :: b1
   type(base(4)), pointer :: b2
   type(base(4)) :: b3

   allocate ( b1, source = base(4) ( cinner(4)(10,11), cinner(4)(20, 21) ) )
   b2 => b1

   b3 = b2

   select type ( g => b3%in1 )
      type is ( cinner(4) )
         print *, g
   end select
   select type ( g => b3%in2 )
      type is ( cinner(4) )
         print *, g
   end select

   b3%in1%i = -999
   b3%in2%i = -999

   b2 = b3

   select type ( g => b2%in1 )
      type is ( cinner(4) )
         print *, g
   end select

   select type ( g => b2%in2 )
      type is ( cinner(4) )
         print *, g
   end select

   select type ( g => b1%in1 )
      type is ( cinner(4) )
         print *, g
   end select

   select type ( g => b1%in2 )
      type is ( cinner(4) )
         print *, g
   end select

end program
