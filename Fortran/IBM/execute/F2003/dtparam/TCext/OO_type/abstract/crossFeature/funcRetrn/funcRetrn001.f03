! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_type/abstract/crossFeature/funcRetrn/funcRetrn001.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), class(abstract type)
!*                                        returns polymorphic scalar abstract type
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
      integer(k1)   :: id
   end type

   type, extends(base) :: child    ! (20,4)
   end type

contains

   class(base(:,4)) function foo(a)
      pointer :: foo
      class(base(*,4)), intent(in) :: a
      allocate(foo, source=a )
   end function

   function foo1(a) result (boo)
      class(base(:,4)), pointer :: boo
      class(base(*,4)), intent(in) :: a
      allocate(boo, source=a)
   end function

end module

program funcRetrn001
   use m

   class(base(:,4)), pointer :: c
   class(base(:,4)), allocatable :: b

   allocate (b, source = child(20,4)(4))

   c => foo(b)
   if (c%id .ne. 4) error stop 1_4
   deallocate (c)

   allocate ( c, source = foo1(child(20,4)(5)) )
   if (c%id .ne. 5) error stop 2_4

end program
