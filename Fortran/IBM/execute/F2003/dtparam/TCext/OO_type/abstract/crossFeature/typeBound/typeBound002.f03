! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qxlpvdtc /tstdev/OO_type/abstract/crossFeature/typeBound/typeBound002.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Type-bound procedures: extension type's component is polymorphic abstract
!*                               type and calling abstract type's type bound
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i = 5
   contains
      procedure, pass :: print => printbase
   end type

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
      integer, kind                   :: k2
      integer, len                    :: n1
      class(child(k2,k2,n1)), pointer :: next
   end type

contains

   integer function printbase(a)
      class(base(4)), intent(in) :: a
      printbase = a%i
   end function

end module

program typeBound002
   use m

   class(child(4,4,20)), pointer :: c1
   class(child(4,4,20)), pointer :: c2
   type(child(4,4,20)), target :: c3

   c3 = child(4,4,20)(3,null())

   allocate(c2,c1)
   c2%next => c3
   c1%next => c2

   if (( c1%print() .ne. 5 ) .or. ( c2%print() .ne. 5 ))   error stop 1_4
   if (( c1%next%print() .ne. 5 ) .or. ( c2%next%print() .ne. 3 ))   error stop 2_4
   if ( c1%next%next%print() .ne. 3 ) error stop 3_4
end program


