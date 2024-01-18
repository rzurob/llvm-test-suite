!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp array003.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        Non-rightmost part-name could be a non-polymorphic and array, with allocate statement
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

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
      real :: rid
   end type

end module

program array003
   use m

   class(base), pointer, dimension(:) :: b1, b2
   type(child) :: c11(5)
   type(child), allocatable, target :: c12(:)
   class(child), pointer :: c13(:)

   allocate(c12(5))
   c13 => c12

   allocate(b1(5), source =  c11%base)
   allocate(b1(3) , source = c12(1:3)%base)
   allocate(b1(0) , source = c13(5:3:1)%base)

   deallocate ( b1 )

   allocate ( b1(1), source = (/ child(123,1.23) /) )

   select type ( b1 )
      type is ( child )
         b2 => b1%base
   end select

end program