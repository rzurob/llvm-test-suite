! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting implicit array objects and define implicit statement inside DTIO (Output)
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

   type, abstract :: abstractdata
   contains
      procedure(inf),deferred, pass :: get
   end type

   type, extends(abstractdata) :: data
      integer(4) :: i
   contains
      procedure, pass :: get
   end type

   type :: base
      character(3) ::  c
      class(abstractdata), allocatable :: d
   end type

   abstract interface
      integer function inf (dtv)
          import abstractdata
         class(abstractdata), intent(in) :: dtv
      end function
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
      subroutine writeformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import abstractdata
         class(abstractdata), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

contains

   integer function get (dtv)
      class(data), intent(in) :: dtv
      get = dtv%i
   end function

end module

program abstracti002
   use m
   implicit type(base)   (A-M)
   implicit class(base)  (N-Z)

   namelist /nml1/ b1, b2
   namelist /nml2/ z3, z4

   dimension :: b1(2)
   allocatable  :: b2(:)
   pointer  :: z3(:)
   allocatable :: z4(:,:)

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'abstracti002.1', form='formatted', access='sequential' )

   b1 =  (/ base(c='b11',d=data(1001)), base(c='b12',d=data(1002)) /)
   allocate ( b2(3), source = (/ base(c='b21',d=data(2001)), base(c='b22',d=data(2002)), base(c='b23',d=data(2003)) /) )
   allocate ( z3(3), source = b2 )
   allocate ( z4(2,2), source = reshape ( source = (/ b1, b1 /), shape = (/2,2/) ) )

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program abstracti002

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, abstractdata, write(formatted), writeformatteddata

   implicit class(abstractdata) (X)

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   namelist /dtio/ x1
   allocatable :: x1

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   write (unit, *, iostat=iostat )        dtv%c

   if ( iostat /= 0 ) error stop 5_4

   allocate ( x1, source = dtv%d )
   write (unit, dtio, iostat=iostat )

   iomsg = 'dtiowrite'

end subroutine

subroutine writeformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: abstractdata, data

   class(abstractdata), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   select type ( dtv )
      class is (abstractdata)
         error stop 8_4
      type is (data)
         write (unit, *, iostat=iostat )        dtv%i
   end select

end subroutine
