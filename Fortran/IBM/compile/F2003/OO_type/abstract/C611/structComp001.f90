!######################################################################
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
! %POSTCMD: dcomp structComp001.f
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
!*                                        scalar non-polymorphic abstract data-ref used in associate construct and actual argument
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

   interface
      subroutine foo(dtv)
         import base
         class(base), intent(in) :: dtv
      end subroutine
   end interface

end module

program structComp001
   use m

   class(base), pointer :: b1
   type(child)          :: c1

   allocate ( b1, source = child(1,2.0))
   c1 = child(2,3.0)

   select type ( g => b1 )
      type is ( child )
         call foo(g%base)
   end select

   associate ( gg => c1%base, g2 => c1 )
     call foo(g2%base)
   end associate

end program

subroutine foo(dtv)
   use m, only: base
   class(base), intent(in) :: dtv
   print *, dtv%id
end subroutine
