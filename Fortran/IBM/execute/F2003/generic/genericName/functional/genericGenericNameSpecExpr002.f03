!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic name
!*
!*  DESCRIPTION                : Generic type bound call appearing in spec-expr for character lengths
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

   type base
      integer :: l
      character(30) :: c
      contains
         procedure, pass :: puregetl
         generic :: getl => puregetl
   end type

   contains

      integer pure function puregetl ( dtv )
         class(base), intent(in) :: dtv

         puregetl = dtv%l

      end function

      subroutine sub ( dtv, c )
         class(base), intent(inout) :: dtv
         character(len=dtv%getl()), intent(in) :: c

         dtv%c(1:dtv%getl()) = c

      end subroutine

end module

program genericGenericNameSpecExpr002
   use m

   character(26) :: c = "abcdefghijklmnopqrstuvwxyz"

   type(base) :: b1
   class(base), pointer :: b2

   allocate ( b2, source = base(10,'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx') )
   b1 = base( 12, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' )

   call sub ( b1, c )
   if ( b1%c /= 'abcdefghijklxxxxxxxxxxxxxxxxxx' ) error stop 1_4

   call sub ( b2, c(13:26) )

   if ( b2%c /= 'mnopqrstuvxxxxxxxxxxxxxxxxxxxx' ) error stop 2_4

end program
