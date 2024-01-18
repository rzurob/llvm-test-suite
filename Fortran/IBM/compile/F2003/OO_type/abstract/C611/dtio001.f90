!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        R614: structure-component is data-ref
!*                                        non-polymorphic abstract type data-ref appears in IO statements ( with or without dtio )
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

program dtio001
   use m, newb => base

   interface write(unformatted)
      subroutine aaa ( a, unit, iostat, iomsg  )
         import newb
         class(newb), intent(in) :: a
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   class(newb), allocatable :: a
   type(child) :: c

   allocate ( a, source = child(1,2.3))

   select type ( a )
      type is ( child )
         associate ( gg => c )
            write ( 1 ) a%base, 555, gg%base
            print *, a%base, 555, gg%base
         end associate
   end select



end program

subroutine aaa ( a, unit, iostat, iomsg  )
   use m, only: base
   class(base), intent(in) :: a
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit ) a%id

end subroutine
