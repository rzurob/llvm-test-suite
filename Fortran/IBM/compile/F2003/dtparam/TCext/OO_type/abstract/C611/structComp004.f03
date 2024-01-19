! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C611/structComp004.f
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        non-polymorphic abstract type data-ref used in intrinsic function
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

program structComp004
   use m

   class(child(4,4)), pointer  :: c1
   class(base(4)), allocatable :: b1
   class(*), pointer           :: u1

   allocate ( b1, source = child(4,4)( 3, 4.0 ) )
   allocate ( c1, source = child(4,4)( 1, 2.0 ) )

   select type ( b1 )
      type is ( child(4,4) )
         if ( same_type_as(b1%base,c1%base) ) error stop 1_4
         if ( extends_type_of(b1%base,b1) )   error stop 2_4
         if ( extends_type_of(b1%base,u1) )   error stop 3_4
   end select

end program
