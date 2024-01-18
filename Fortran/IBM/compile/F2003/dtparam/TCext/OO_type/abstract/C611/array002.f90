! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C611/array002.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        array non-polymorphic abstract data-ref used in intrinsic functions
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

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
   end type

end module

program array002
   use m

   class(base(4)), pointer, dimension(:) :: b1
   class(child(4,4)), allocatable, target :: c11(:)
   class(child(4,4)), pointer :: c12(:)

   allocate (c11(5))
   c12 => c11
   allocate ( b1(2), source = (/ child(4,4)(1,2.0), child(4,4)(3,4.0) /) )

   if ( same_type_as (b1, c11(2:3)%base) )             error stop 1_4
   if ( same_type_as (b1, c12((/1/))%base) )           error stop 2_4

   if ( extends_type_of ( c11%base, c11 ) )            error stop 3_4
   if ( extends_type_of ( c12(1:2)%base, c12(1:2) ) )  error stop 4_4

end program
