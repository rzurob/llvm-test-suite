!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: if the type definition contains or inherits
!*                                        a deferred binding, ABSTRACT shall appear. (C427)
!*                                        vii)	if deferred type bound is map to a generic DTIO type bound, see
!*                                              if extended type not defined to be abstract will be catched by compiler
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

   type, abstract :: b1
      integer :: id
   contains
      procedure(inf), pass, deferred :: print
      generic :: write(unformatted) => print
   end type

   interface
      subroutine inf(dtv, unit, iostat, iomsg)
         import b1
         class(b1),intent(in) :: dtv
         integer, intent(in)  :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   type, extends(b1) :: b2
   end type

end module

program deferred007
end program
