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

   class(base), pointer :: b1
   class(base), allocatable, protected :: b2
   type(base), pointer, protected :: b3
   type(base), allocatable :: b4

   namelist /b1b3/ b1, b3  !<- part of namelist is protected

   contains

   subroutine assoMe (c)
      complex(4), intent(in), optional :: c

      if ( present(c) ) then
         allocate ( b1, source = base(c) )
         allocate ( b3, source = base(c) )
      else
      	 allocate ( b1, source = base( (1.0,1.0) ) )
      	 allocate ( b3, source = base(c) )
      end if

   end subroutine

   subroutine allocMe (c)
      complex(4), intent(in), optional :: c

      if ( present(c) ) then
         allocate ( b2, source = base(c) )
         allocate ( b4, source = base(c) )
      else
      	 allocate ( b2, source = base( (1.0,3.0) ) )
      	 allocate ( b4, source = base( (2.0,4.0) ) )
      end if

   end subroutine

end module

program protected101d
use m
   namelist /b2b4/ b2, b4
   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'protected101d.1', form='formatted', access='sequential' )

   call assoMe((2.0,3.0))
   call allocMe()

   read (1, b1b3, iostat = stat, iomsg = msg )   !<- illegal
   read (1, b2b4, iostat = stat, iomsg = msg )   !<- illegal

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
