!*  ===================================================================
!*
!*  DATE                       : 02/20/2013
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  KEYWORD(S)                 : F2008 Submodule sub module
!*
!*  TARGET(S)                  :
!*
!*  DESCRIPTION                :
!*  based on F2003/abstracti/abstracti005
!*
!*  Define module m, which uses module n.  Define submodule nsub which
!*   extends n and uses m.  This should not cause a circular reference.
!*
!*  Secondary tests:
!*   - n contains abstract interfaces defined in nsub, m, and not in
!*     the module/submodule.
!*   - through using m, the main program still accesses the function
!*     declared in the interface.
!*
!*  Verify that the results match the values of the original test case.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module n

   type, abstract :: abstractdata
   contains
      procedure(inf),deferred, pass :: get
   end type

   type, extends(abstractdata) :: data
      integer(4) :: i = -999
   contains
      procedure, pass :: get
   end type

   interface
      integer function inf (dtv)
         import abstractdata
         class(abstractdata), intent(in) :: dtv
      end function
   end interface
   interface
      module integer function get (dtv)
         class(data), intent(in) :: dtv
      end function
   end interface

end module n

module m
   use n

   type :: base
      character(3) ::  c = 'xxx'
      class(abstractdata), allocatable :: d
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
      subroutine readformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import abstractdata
         class(abstractdata), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module m


submodule (n) nsub
!should not cause circular reference
use m
contains

   module integer function get (dtv)
      class(data), intent(in) :: dtv
      get = dtv%i
   end function

end submodule

program abstracti005
   use m
   implicit type(base)   (A-M)
   implicit class(base)  (N-Z)

   namelist /nml1/ b1, b2
   namelist /nml2/ z3, z4

   dimension :: b1(2)
   allocatable  :: b2(:)
   pointer  :: z3(:)
   allocatable :: z4(:)

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'submodule12f.1', form='formatted', access='sequential' )

   b1 =  (/ base(d=data()), base(d=data()) /)
   allocate ( b2(3), source = (/ base(d=data()), base(d=data()), base(d=data()) /) )
   allocate ( z3(3), source = b2 )
   allocate ( z4(2), source = b1 )

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   select type ( d1 => b1(1)%d )
      type is (data)
         select type ( d2 => b1(2)%d )
            type is (data)
               if ( ( b1(1)%c /= 'b11' ) .or. ( d1%i /= 1001 ) .or. ( b1(2)%c /= 'b12' ) .or. ( d2%i /= 1002 ))   error stop 2_4
            class default
               error stop 3_4
         end select
      class default
         error stop 4_4
   end select

   select type ( d1 => b2(1)%d )
      type is (data)
         select type ( d2 => b2(2)%d )
            type is (data)
               select type ( d3 => b2(3)%d )
                  type is (data)
                     if ( ( b2(1)%c /= 'b21' ) .or. ( d1%i /= 2001 ) .or. ( b2(2)%c /= 'b22' ) .or. ( d2%i /= 2002 )  .or. &
                          ( b2(3)%c /= 'b23' ) .or. ( d3%i /= 2003 ) )   error stop 5_4
                  class default
                     error stop 6_4
               end select
            class default
               error stop 7_4
         end select
      class default
         error stop 8_4
   end select

   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 9_4


   select type ( d1 => z3(1)%d )
      type is (data)
         select type ( d2 => z3(2)%d )
            type is (data)
               select type ( d3 => z3(3)%d )
                  type is (data)
                     if ( ( z3(1)%c /= 'z31' ) .or. ( d1%i /= 3001 ) .or. ( z3(2)%c /= 'z32' ) .or. ( d2%i /= 3002 )  .or. &
                          ( z3(3)%c /= 'z33' ) .or. ( d3%i /= 3003 ) )   error stop 10_4
                  class default
                     error stop 11_4
               end select
            class default
               error stop 12_4
         end select
      class default
         error stop 13_4
   end select

   select type ( d1 => z4(1)%d )
      type is (data)
         select type ( d2 => z4(2)%d )
            type is (data)
               if ( ( z4(1)%c /= 'z41' ) .or. ( d1%i /= 4001 ) .or. ( z4(2)%c /= 'z42' ) .or. ( d2%i /= 4002 ))   error stop 14_4
            class default
               error stop 15_4
         end select
      class default
         error stop 16_4
   end select

end program abstracti005

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, abstractdata, read(formatted), readformatteddata

   implicit class(abstractdata) (X)

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   namelist /dtio/ x1
   allocatable :: x1

   if ( iotype /= "NAMELIST" ) error stop 17_4
   if ( size(v_list, 1) /= 0 ) error stop 18_4

   read (unit, *, iostat=iostat )        dtv%c

   if ( iostat /= 0 ) error stop 19_4

   allocate ( x1, source = dtv%d )
   read (unit, dtio, iostat=iostat, iomsg = iomsg )

   if ( allocated ( dtv%d ) ) deallocate ( dtv%d )

   allocate ( dtv%d , source = x1 )

   iomsg = 'dtioread'

end subroutine

subroutine readformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: abstractdata, data

   class(abstractdata), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 20_4
   if ( size(v_list, 1) /= 0 ) error stop 21_4

   select type ( dtv )
      class is (abstractdata)
         error stop 22_4
      type is (data)
         read (unit, *, iostat=iostat )        dtv%i
   end select

end subroutine
