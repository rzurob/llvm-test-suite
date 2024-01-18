! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar013.f
! opt variations: -qnol -qdeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: operands with poly pointer scalar with overridding specific typebound
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
      integer(k1)   :: i = -999
      contains
         procedure, pass :: assgn => ba
         generic :: assignment(=) => assgn
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j = -999
      contains
         procedure, pass :: assgn => ca
   end type

   contains

      subroutine ba ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b

         a%i = b%i
         print *, 'ba'

      end subroutine

      subroutine ca ( a, b )
         class(child(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b

         a%i = b%i

         print *, 'ca'

         select type ( b )
            type is ( child(*,4) )
               a%j = b%j
         end select

      end subroutine

end module

program genericAssignmentScalar013
   use m

   class(base(20,4)), pointer :: b1, b2
   type(child(20,4)), pointer :: c1

   allocate ( b1, source = base(20,4)(20) )
   allocate ( c1 , b2 )

   c1 = b1
   b2 = b1

   if ( ( c1%i /= 20 ) .or. ( c1%j /= -999 ) .or. ( b2%i /= 20 ) ) error stop 1_4

   deallocate ( b1 , b2 )

   allocate ( child(20,4) :: b1 )
   allocate ( b2, source = child(20,4) ( 100, 200 ) )

   b1 = b2

   select type ( b1 )
      type is ( child(*,4) )
         if ( ( b1%i /= 100 ) .or. ( b1%j /= 200 )  ) error stop 2_4
      class default
         error stop 3_4
   end select

end program
