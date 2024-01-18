! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentPure001.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: pure subroutine with scalar and class hierarchy
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
      integer(k1)   :: i
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j
      contains
         procedure, pass :: bassgn => cassgn
         procedure, pass :: cassgnint
         generic :: assignment(=) => cassgnint
   end type

   contains

      pure subroutine bassgn ( a, b )
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in) :: b

         a%i = b%i + 1

      end subroutine

      pure subroutine cassgn ( a, b )
         class(child(4)), intent(out) :: a
         class(base(4)), intent(in)   :: b

         select type ( b )
            type is ( base(4) )
               a%i = b%i + 1
            type is ( child(4) )
               a%base = b%base
               a%j = b%j + 1
         end select

      end subroutine

      pure subroutine cassgnint ( a, b )
         class(child(4)), intent(out) :: a
         integer, intent(in) :: b

         a%i = b + 1
         a%j = b + 1

      end subroutine

end module

program genericAssignmentPure001
   use m

   class(base(4)), pointer  :: b1
   class(child(4)), pointer :: c1

   type(base(4)) :: b2
   type(child(4)) :: c2

   allocate ( b1, c1 )

   b2 = base(4)(100)
   c2 = child(4)(1000, 2000)

   print *, b2
   print *, c2

   b1 = b2
   c1 = c2

   print *, b1%i
   print *, c1%i, c1%j

   c1 = -1000
   c2 = -1000

   print *, c1%i, c1%j
   print *, c2%i, c2%j

   deallocate ( b1 )
   allocate ( child(4) :: b1 )

   b1 = c1
   select type ( b1 )
      type is ( child(4) )
         print *, b1%i, b1%j
         b1 = 1000
         print *, b1%i, b1%j
   end select

end program
