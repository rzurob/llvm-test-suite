! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : access102kl
!*
!*  PROGRAMMER                 : David Forster (derived from access102 by Robert Ma)
!*  DATE                       : 2007-06-20 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with namelist and object of public/private accessibility (input)
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
      integer(kb) :: i = -9999
      contains
      procedure(inf1), pass, deferred :: getc
      procedure(inf2), pass, deferred :: setc
      procedure(inf3), pass, deferred :: print
   end type

   type, extends(base) :: child (lc)
      integer, len :: lc
      character(lc), private :: c = 'xxx'
      contains
      procedure, pass :: getc
      procedure, pass :: setc
      procedure, pass :: print
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

   interface
      subroutine inf3(dtv)
         import base
         class(base(4)), intent(in) :: dtv
      end subroutine
   end interface

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), allocatable :: b1
   class(base(4)), pointer     :: b2

   contains

   subroutine initialize ()
      allocate ( b1, source = child(4,3)(-5, 'xxx') )
      allocate ( b2, source = child(4,3)(-7, 'xxx') )
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

   subroutine print(dtv)
      class(child(4,*)), intent(in) :: dtv
      print *, dtv%i, dtv%c
   end subroutine

end module

program access102kl
use m
   integer :: stat
   character(150) :: msg
   namelist /n12/ b1, b2

   open (1, file = 'access102kl.1', form='formatted', access='sequential' )

   call initialize()

   read (1, n12, iostat = stat, iomsg = msg)

   call b1%print()
   call b2%print()

   select type (b => b1)
      class is (base(4))
         select type ( c => b2 )
            class is (child(4,*))
               read (1, n12, iostat = stat, iomsg = msg)
                call b1%print()
                call b2%print()
            class default
               error stop 1_4
         end select
      class default
         error stop 2_4
   end select
end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(inout) :: dtv
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
      type is (child(4,*))
         read (unit, *, iostat=iostat )   tmpc
         call dtv%setc(tmpc)
   end select
   iomsg = 'dtioread'

end subroutine
