! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar004.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic bindings of two different types pointing to the same
!*                                             procedure
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

   type base1(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, pass :: call => twotypesb1
         generic :: add => call
   end type

   type base2(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: i
      contains
         procedure, pass(b) :: call => twotypesb2
         generic :: add => call
   end type

   contains

      integer function twotypesb1 ( a, b )
         class(base1(*,4)), intent(in) :: a
         class(base2(*,4)), intent(in) :: b

         twotypesb1 = a%i + b%i

         print *, 'twotypesb1'

      end function

      integer function twotypesb2 ( a, b )
         class(base1(*,4)), intent(in) :: a
         class(base2(*,4)), intent(in) :: b

         twotypesb2 = a%i + b%i

         print *, 'twotypesb2'

      end function

end module

program genericGenericNameScalar004
   use m

   type(base1(20,4)) :: b1
   type(base1(:,4)), pointer :: b2

   type(base2(20,4)) :: b3
   type(base2(:,4)), allocatable :: b4

   integer :: i

   allocate ( base1(20,4):: b2 )
   allocate ( base2(20,4):: b4 )

   b1 = base1(20,4)(100)
   b2 = base1(20,4)(200)
   b3 = base2(20,4)(300)
   b4 = base2(20,4)(400)

   i = b1%call(b3)
   print *,i

   i = b2%call(b4)
   print *,i

   i = b3%call(b2)
   print *,i

   i = b4%call(b1)
   print *,i

end program
