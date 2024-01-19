! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with data object with protected attribute (input)
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
   type base
      complex(4) :: c
   contains
      procedure, nopass :: readMe
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), pointer, protected :: b1
   class(base), allocatable, protected :: b2
   type(base), protected :: b3

   namelist /n1/ b1
   namelist /n2/ b2
   namelist /n3/ b3

   contains

   subroutine start()
      allocate(b1, b2)
   end subroutine

   subroutine readMe (unit, c)
      integer, intent(in) :: unit, c
      integer :: stat = 0
      character(150) :: msg = ''

      select case (c)
         case(1)
            read (unit, n1, iostat=stat, iomsg=msg)
         case(2)
            read (unit, n2, iostat=stat, iomsg=msg)
         case(3)
            read (unit, n3, iostat=stat, iomsg=msg)
         case default
            error stop 1_4
      end select

      if ( ( stat /= 0 ).or. ( msg /= 'dtioread') ) error stop 2_4

   end subroutine

end module

program protected101
use m

   class(base), allocatable :: dummy
   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'protected101.1', form='formatted', access='sequential' )
   allocate (dummy)
   call start()

   call dummy%readMe(1, 1)   !<- read b1
   call dummy%readMe(1, 2)   !<- read b2
   call dummy%readMe(1, 3)   !<- read b3

   print *, b1%c
   print *, b2%c
   print *, b3%c

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   namelist /nml/ c
   complex(4) :: c

   if ( iotype /= "NAMELIST" ) error stop 1_4
   if ( size(v_list, 1) /= 0 ) error stop 2_4

   read (unit, nml, iostat=iostat )

   dtv%c = c

   iomsg = 'dtioread'

end subroutine
