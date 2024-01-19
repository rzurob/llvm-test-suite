! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-06-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with namelist and object of public/private accessibility (output)
!*                                        child type containing private components
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
   type, abstract :: base (kb)
      integer, kind :: kb
      integer(kb) :: i
      contains
      procedure(inf1), pass, deferred :: getc
      procedure(inf2), pass, deferred :: setc
   end type

   type, extends(base) :: child (lc)
      integer, len :: lc
      character(lc), private :: c
      contains
      procedure, pass :: getc
      procedure, pass :: setc
   end type

   interface
      character(3) function inf1(dtv)
         import base
         class(base(4)), intent(in) :: dtv
      end function
   end interface

   interface
      subroutine inf2(dtv,c)
         import base
         class(base(4)), intent(inout) :: dtv
         character(3), intent(in) :: c
      end subroutine
   end interface

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), allocatable :: b1
   class(base(4)), pointer     :: b2

   namelist /n12/ b1, b2

   contains

   subroutine initialize ()
      allocate ( b1, source = child(4,3)(5, 'ibm') )
      allocate ( b2, source = child(4,3)(7, 'IBM') )
   end subroutine

   character(3) function getc(dtv)
      class(child(4,*)), intent(in) :: dtv
      getc =dtv%c
   end function

   subroutine setc(dtv,c)
      class(child(4,*)), intent(inout) :: dtv
      character(3), intent(in) :: c
      dtv%c = c
   end subroutine

end module

program access002kl
use m
   integer :: stat
   character(150) :: msg
   open (1, file = 'access002kl.1', form='formatted', access='sequential' )
   call initialize()

   write (1, n12, iostat = stat, iomsg = msg)

   select type (b => b1)
      class is (base(4))
         select type ( c => b2 )
            class is (child(4,*))
               write (1, n12, iostat = stat, iomsg = msg)
            class default
               error stop 1_4
         end select
      class default
         error stop 2_4
   end select
end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   write (unit, "('i=',I4,1X)", iostat=iostat )   dtv%i

   select type (dtv)
      type is (child(4,*))
         write (unit, "('c=',A3,1X)", iostat=iostat )   dtv%getC()
   end select
   iomsg = 'dtiowrite'

end subroutine
