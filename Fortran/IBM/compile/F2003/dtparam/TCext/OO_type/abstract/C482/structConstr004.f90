!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: The derived-type-spec shall not specify an abstract type (C401)
!*                                        Structure Constructor appears in an IO statement (w/ or w/o DTIO)
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

   type, abstract :: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   end type

   type, extends(base) :: child(k2)
      integer, kind :: k2
      real(k2) :: rid
   end type

   type, abstract :: base2(n)
      integer, len :: n
      character(n) :: c
      contains
         procedure, pass :: print
         generic :: write(formatted) => print
   end type

   contains

      subroutine print (dtv, unit, iotype, v_list, iostat, iomsg )
         class(base2(*)), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write ( unit, *) dtv%c
      end subroutine
end module

program structConstr004
   use m, newbase => base

   write (6,*) newbase(4)(10)
   write (6,*) child(4,4)(10, 10.0)
   write (6,*) base2(20)('ibm')

end program
