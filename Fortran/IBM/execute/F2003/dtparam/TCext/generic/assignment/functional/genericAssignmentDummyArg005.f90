! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentDummyArg005.f
! opt variations: -ql -qreuse=none

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
!*  DESCRIPTION                : assignment: poly (assumed-shaped) array dummy arguments being the operand
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
      integer(k1)   :: i = -999
      contains
         procedure, pass :: bamt
         generic :: assignment(=) => bamt
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j =-999
   end type

   interface
      subroutine bamt ( a, b )
         import base
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in) :: b(:)
      end subroutine
   end interface

end module

program genericAssignmentDummyArg005
   use m

   class(base(4)), allocatable :: b1, b2(:)
   type(child(4)), allocatable :: c1, c2(:)

   allocate ( b1, b2(3), c1, c2(5) )

   b2(1)%i = 100
   b2(2)%i = 200
   b2(3)%i = 300

   c2(1)%i = 100
   c2(2)%i = 200
   c2(3)%i = 300
   c2(4)%i = 400
   c2(5)%i = 500
   c2(1)%j = 1000
   c2(2)%j = 2000
   c2(3)%j = 3000
   c2(4)%j = 4000
   c2(5)%j = 5000

   b1 = b2

   c1 = c2

   print *, b1%i
   print *, c1%i, c1%j

   deallocate ( b1 )
   allocate ( child(4) :: b1 )

   b1 = b2
   select type ( b1 )
      type is ( child(4) )
         print *, b1%i, b1%j
   end select

   b1 = c2
   select type ( b1 )
      type is ( child(4) )
         print *, b1%i, b1%j
   end select

   c1 = b2
   print *, c1%i, c1%j

   contains

      subroutine assignment(a, b)
         type(base(4)), intent(inout) :: a
         type(base(4)), intent(in)  :: b(:)

         print *, 'assignment'
         a = b

      end subroutine

      subroutine assignment1(a, b)
         type(base(4)), intent(inout) :: a
         type(base(4)), intent(in)  :: b(-1:)

         print *, 'assignment1'
         a = b

      end subroutine

end program

subroutine bamt ( a, b )
   use m, only: base, child
   class(base(4)), intent(out) :: a
   class(base(4)), intent(in) :: b(:)

   a%i = b(1)%i
   do k = 2,size(b)
      a%i = b(k)%i + a%i
   end do

   select type ( a )
      type is ( child(4) )
         select type ( b )
            type is ( child(4) )
               a%j = b(1)%j
               do k = 2,size(b)
                  a%j = b(k)%j + a%j
               end do
         end select
    end select


   print *, 'bamt'

end subroutine
