! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with non polymorphic function return variable
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
      character(3) :: c
      contains
         procedure, nopass :: readbase
   end type

   type, extends(base) :: child
      integer(4) :: i
      contains
         procedure, nopass :: readchild
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

   integer :: unit = 1
   integer :: stat
   character(200) :: msg

contains

   type(base) function readBase()
      namelist /WB/ readbase
      read (unit, WB, iostat = stat, iomsg = msg )
   end function

   type(child) function readChild()
      namelist /WC/ readChild
      read (unit, WC, iostat = stat, iomsg = msg )
   end function

end module

program funcRetrn101
   use m

   class(base), allocatable :: b1
   type(base)               :: b2 = base  ('xxx')
   type(child)              :: c1 = child ('xxx',-999)
   class(child), pointer    :: c2

   open (1, file = 'funcRetrn101.1', form='formatted', access='sequential' )

   allocate ( b1, source = b2%readbase()  )
   allocate ( c2, source = c1%readchild() )

   if ( b1%c /= 'IBM' )                           error stop 1_4

   if ( ( c2%c /= 'FTN' ) .or. ( c2%i /= 123 ) )  error stop 2_4
   deallocate (b1)
   allocate(b1, source = child('xxx',-999))

   select type ( b1 )
      type is (child)
         c1 = b1%readchild()
   end select

   if ( ( c1%c /= 'IBM' ) .or. ( c1%i /= 2005 ) )  error stop 3_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base,child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   select type (dtv)
      type is (base)
         read (unit, "(A3)", iostat=iostat )            dtv%c
      type is (child)
         read (unit, *, iostat=iostat )      dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine
