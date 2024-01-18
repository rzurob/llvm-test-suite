! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=none /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar008.f
! opt variations: -qnol -qdeferredlp -qreuse=base

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
!*  DESCRIPTION                : assignment: operands with poly scalar with overridding specific typebound
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


   type, extends(base) :: child(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j = -999
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
         class(child(*,4,*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b

         a%i = b%i

         print *, 'ca'

         select type ( b )
            type is ( child(*,4,*,4) )
               a%j = b%j
         end select

      end subroutine

end module


program genericAssignmentScalar008
   use m

   class(base(20,4)), pointer :: b1
   class(base(20,4)), allocatable :: b2
   type(child(20,4,20,4)) :: c1
   class(child(20,4,20,4)), pointer :: c2

   allocate ( b1, source = base(20,4)(10) )
   allocate ( b2 )

   b2 = b1
   c1 = b1

   print *, b2%i
   print *, c1%i, c1%j

   allocate ( c2 )

   b1 = c1
   c2 = child(20,4,20,4)( 10 , 20 )
   c1 = c2
   
   print *, b1%i
   print *, c2%i, c2%j
   print *, c1%i, c1%j
   
   deallocate ( b1, b2 )
   
   allocate ( b1, source = child(20,4,20,4)() )
   allocate ( b2, source = base(20,4) ( 100 ) )
   
   b1 = b2
   select type ( b1 )
      type is ( child(*,4,*,4) )
         print *, b1%i, b1%j
   end select
   
   b1 = c1
   select type ( b1 )
      type is ( child(*,4,*,4) )
         print *, b1%i, b1%j
   end select
   

end program
