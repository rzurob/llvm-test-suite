!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE TITLE            : abstracti003k
!*
!*  PROGRAMMER                 : Glen Mateer (derived from abstracti003
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-09-14 (original: 02/20/2006)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*  Try namelist formatting with namelist and object of public/private
!*  accessibility (output) child type containing private components
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type, abstract :: base
      integer(4) :: i
      contains
      procedure(inf1), pass, deferred :: getc
      procedure(inf2), pass, deferred :: setc
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=1
      integer, kind :: kchild_1
      character(kind=kchild_1,len=3), private :: c
      contains
      procedure, pass :: getc
      procedure, pass :: setc
   end type

   abstract interface
      character(3) function inf1(dtv)
         import base
         class(base), intent(in) :: dtv
      end function
   end interface

   abstract interface
      subroutine inf2(dtv,c)
         import base
         class(base), intent(inout) :: dtv
         character(3), intent(in) :: c
      end subroutine
   end interface

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), allocatable :: b1
   class(base), pointer     :: b2

   namelist /n12/ b1, b2

   contains

   subroutine initialize ()
      allocate ( b1, source = child(1)(5, 'ibm') ) ! tcx: (1)
      allocate ( b2, source = child(1)(7, 'IBM') ) ! tcx: (1)
   end subroutine

   character(3) function getc(dtv)
      class(child(1)), intent(in) :: dtv ! tcx: (1)
      getc =dtv%c
   end function

   subroutine setc(dtv,c)
      class(child(1)), intent(inout) :: dtv ! tcx: (1)
      character(3), intent(in) :: c
      dtv%c = c
   end subroutine

end module

program abstracti003k
use m
   integer :: stat
   character(150) :: msg
   open (1, file = 'abstracti003k.1', form='formatted', access='sequential' )
   call initialize()

   write (1, n12, iostat = stat, iomsg = msg)

   select type (b => b1)
      class is (base)
         select type ( c => b2 )
            class is (child(1)) ! tcx: (1)
               write (1, n12, iostat = stat, iomsg = msg)
            class default
               error stop 11_4
         end select
      class default
         error stop 12_4
   end select
end program abstracti003k


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 13_4
   if ( size(v_list, 1) /= 0 ) error stop 14_4

   write (unit, "('i=',I4,1X)", iostat=iostat )   dtv%i

   select type (dtv)
      type is (child(1)) ! tcx: (1)
         write (unit, "('c=',A3,1X)", iostat=iostat )   dtv%getC()
   end select
   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: child - added parameters (kchild_1) to invoke with (1)/declare with (1) - 6 changes
