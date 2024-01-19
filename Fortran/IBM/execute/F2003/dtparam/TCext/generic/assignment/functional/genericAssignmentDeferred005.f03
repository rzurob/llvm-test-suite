! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentDeferred005.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: deferred elemental binding being generic assignment
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

   type, abstract :: base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure(itf), deferred, pass :: bagnmt
         generic :: assignment(=) => bagnmt
   end type

   interface
      elemental subroutine itf ( a, b )
         import base
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in)  :: b
      end subroutine
   end interface

end module

module m1
   use m

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j = -999
      contains
         procedure, pass :: bagnmt => cagnmt
   end type

   contains

      elemental subroutine cagnmt ( a, b )
         class(child(*,4)), intent(out) :: a
         class(base(*,4)), intent(in)  :: b

         a%i = b%i + 1
         select type ( b )
            type is ( child(*,4) )
               a%j = b%j+1
         end select

      end subroutine


end module

program genericAssignmentDeferred005
   use m1

   class(base(:,4)), allocatable  :: b1, b2(:), b3(:,:)
   class(child(:,4)), allocatable :: c1, c2(:), c3(:,:)

   allocate ( child(20,4) :: b1, b2(3), b3(2,2), c1, c2(3), c3(2,2) )

   c1%i = 100
   c1%j = 1000

   c2 = c1

   c3 = reshape ( source = (/ c2(1:2), child(20,4)(200, 2000), child(20,4) ( 300, 3000) /), shape = (/2,2/) )

   print *, c2%i
   print *, c2%j

   print *, c3%i
   print *, c3%j

   c2(1:2) = c3(2,1:2)
   print *, c2%i
   print *, c2%j

   b1 = c1

   select type ( b1 )
      type is ( child(*,4) )
         print *, b1%i
         print *, b1%j
   end select

   b2 = c1

   select type ( b2 )
      type is ( child(*,4) )
         print *, b2%i
         print *, b2%j
   end select

   b2 = c3(2, (/1,2,1 /) )

   select type ( b2 )
      type is ( child(*,4) )
         print *, b2%i
         print *, b2%j
   end select

   b3 = c3

   select type ( b3 )
      type is ( child(*,4) )
         print *, b3%i
         print *, b3%j
   end select

end program
