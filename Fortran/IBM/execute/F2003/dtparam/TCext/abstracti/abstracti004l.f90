! *********************************************************************
!*  ===================================================================
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-09-24 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*  Try namelist formatting with namelist and object of public/private
!*  accessibility (input) child type containing private components
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
   type, abstract :: base (lchild) ! lchild=3
      integer, len :: lchild
      integer(4) :: i = -9999
      contains
      procedure(inf1), pass, deferred :: getc
      procedure(inf2), pass, deferred :: setc
      procedure(inf3), pass, deferred :: print
   end type

   type, extends(base) :: child
      character(lchild), private :: c = 'xxx'
      contains
      procedure, pass :: getc
      procedure, pass :: setc
      procedure, pass :: print
   end type

   abstract interface
      character(3) function inf1(dtv)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
      end function
   end interface

   abstract interface
      subroutine inf2(dtv,c)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         character(3), intent(in) :: c
      end subroutine
   end interface

   abstract interface
      subroutine inf3(dtv)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
      end subroutine
   end interface

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)

   contains

   subroutine initialize ()
      allocate ( b1, source = child(3)(-5, 'xxx') ) ! tcx: (3)
      allocate ( b2, source = child(3)(-7, 'xxx') ) ! tcx: (3)
   end subroutine

   character(3) function getc(dtv)
      class(child(*)), intent(in) :: dtv ! tcx: (*)
      getc =dtv%c
   end function

   subroutine setc(dtv,c)
      class(child(*)), intent(inout) :: dtv ! tcx: (*)
      character(3), intent(in) :: c
      dtv%c = c
   end subroutine

   subroutine print(dtv)
      class(child(*)), intent(in) :: dtv ! tcx: (*)
      print *, dtv%i, dtv%c
   end subroutine

end module

program abstracti004l
use m
   integer :: stat
   character(150) :: msg
   namelist /n12/ b1, b2

   open (1, file = 'abstracti004l.1', form='formatted', access='sequential' )

   call initialize()

   read (1, n12, iostat = stat, iomsg = msg)
   if (stat /= 0) then
      print *, 'read(', stat, ') ', msg
      error stop 10_4
   end if

   call b1%print()
   call b2%print()

   select type (b => b1)
      class is (base(*)) ! tcx: (*)
         select type ( c => b2 )
            class is (child(*)) ! tcx: (*)
                read (1, n12, iostat = stat, iomsg = msg)
                if (stat /= 0) then
                   print *, 'read(', stat, ') ', msg
                   error stop 11_4
                end if

                call b1%print()
                call b2%print()
            class default
               error stop 12_4
         end select
      class default
         error stop 13_4
   end select
end program abstracti004l


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(3) :: tmpc

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   read (unit, *, iostat=iostat )   dtv%i

   select type (dtv)
      type is (child(*)) ! tcx: (*)
         read (unit, *, iostat=iostat )   tmpc
         call dtv%setc(tmpc)
   end select
   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lchild) to invoke with (3)/declare with (*) - 8 changes
! type: child - added parameters (lchild) to invoke with (3)/declare with (*) - 7 changes
