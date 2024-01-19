!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Select type: printing out associate-name components
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base
      character(3) :: c = ''
   end type
   type, extends(base) :: child
      character(3) :: g = ''
   end type
end module

program misc009
   use m1

   class(base), allocatable  :: dummy(:)
   allocate( dummy(1), source = (/ child('abc','def') /) )

   select type( dummy )
      type is (child)
         print *, dummy%c
         print *, dummy%g
   end select

end program
