! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar003a.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic, specific bindings
!*                                             with pass and nopass specbnd pointing to same procedure (function)
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
      integer(k1)   :: i
      contains
         procedure, pass(a) :: addpass => add
         procedure, nopass  :: addnopass => add
         generic :: add => addpass, addnopass
   end type

   contains

      integer function add ( a, j )
         class(base(*,4)), intent(in) :: a
         integer, intent(in) :: j

         add = a%i + j

         print *, 'add'

      end function

end module

program genericGenericNameScalar003a
   use m

   type(base(20,4)) :: b1
   type(base(:,4)), allocatable :: b2
   type(base(:,4)), pointer :: b3

   integer :: i, j(4),k(2,2)

   allocate ( base(20,4):: b2, b3 )

   b1 = base(20,4)(100)
   b2 = base(20,4)(200)
   b3 = base(20,4)(300)

   ! calling generic name with pass attribute

   i = b1%add(100)
   j = b2%add(200)
   k = b3%add(300)

   print *, i
   print *, j
   print *, k

   ! calling generic name with nopass attribute

   i = b1%add(base(20,4)(10), 100)
   j = b2%add(b1, 200)
   k = b3%add(base(20,4)(30), 300)

   print *, i
   print *, j
   print *, k

end program
